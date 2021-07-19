
#' @importFrom stats runif median
#' @importFrom stringr str_match str_split str_sub
#' @importFrom graphics plot
#' @importFrom magrittr mod "%>%"
NULL


# CRAN sometimes issues spurious warnings about undefined variables
utils::globalVariables( c( ".", "%>%", "x", "y", "c", "value" ) )


if( FALSE ){

}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# nimg class ----


nimg = function( im, name ){
  if( is.logical( im ) | is.integer( im ) ){
    im = im + 0.0
  }
  if( length( dim( im ) ) == 2 ){ # gray-scale image
    dim( im ) = c( dim( im ), 1 )
  }
  class( im ) = c( "nimg", "numeric" )
  if( ! base::missing( name ) ){
    attr( im, "name" ) = name
  } else if( is.null( attr( im, "name" ) ) ){
    attr( im, "name" ) = ""
  }
  im
}


is.nimg = function( x ){
  methods::is(x,"nimg")
}


##' @export
print.nimg = function( x, ... ){
  d = dim( x )
  if( attr( x, "name" ) == "" || attr( x, "name" ) == "-" || is.null( attr( x, "name" ) ) ){
    name = "image"
  } else {
    name = attr( x, "name" )
  }
  cat( sprintf( "%s: %i [height] x %i [width] x %i [colour channels]\n", name, d[1], d[2], d[3] ) )
  # cat( sprintf( "image: %i [height] x %i [width] x %i [colour channels]\n", d[1], d[2], d[3] ) )
  invisible( x )
}


#' Display an image
#' @param x an image
#' @param rescale logical. if true, then pixel value is rescaled to range between 0 and 1.
#' @param ... other parameters to be passed to plot.default
#' @return No return value, called for side effects.
#' @examples
#' plot(face)
#' @export
plot.nimg = function( x, rescale = FALSE, ... ){
  old.par = graphics::par( no.readonly = TRUE )
  on.exit( graphics::par( old.par ), add = TRUE )
  if( im_npix( x ) == 0 ){
    stop( "The image is empty." )
  }

  if( im_nchannel( x ) == 1 ){
    # a raster array must have exactly 3 or 4 planes
    x = im_rep( x, 3 )
  }
  im = x[ ,,, drop = FALSE ]
  if( rescale ){
    im = rescaling01( im )
  } else if( max( im ) > 1 || min( im ) < 0 ){
    warning( paste0( "Pixcel value exceeds the range [0,1], and hence it was clamped when plotting.\n",
                     "min = ", min( im ), ", max = ", max( im ) ) )
    im = clamping( im )
  }
  graphics::par( mar = c( 0, 0, 0, 0 ) )
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(1,im_width(x)), ylim = c(im_height(x),1), asp = 1, xaxs = "i", yaxs = "i", ...)
  rst = grDevices::as.raster(
    im, rescale = FALSE, colorscale = NULL, colourscale = NULL, col.na = grDevices::rgb(0,0,0,0) )
  graphics::rasterImage( rst, 1, nrow( rst ), ncol( rst ), 1, interpolate = FALSE )
  invisible( x )
}


#' Load image from file or URL
#' @param file path to file or URL
#' @param name a string for name attribute. if missing, inferred from the file argument.
#' @return an array of image data
#' @examples
#' \dontrun{
#' # load an image from disk
#' im = im_load("path/to/your/image.jpg")
#' plot(im)
#' }
#' # load an image from URL
#' im = im_load("http://placehold.jp/150x150.png")
#' @export
im_load = function( file, name ){
  if( grepl("^(http|ftp)s?://", file) ){ # if URL
    url = file
    ext = stringr::str_extract_all( url, "\\.([A-Za-z0-9]+$)" )[[ 1 ]]
    if( length( ext ) > 0 ){
      file = tempfile( fileext = ext )
    } else {
      file = tempfile()
    }
    downloader::download( url, file, mode = "wb" )
    im = im_load( file, get_image_name_from_file( url ) )
    unlink( file )
    return( im )
  }
  ext = sub( ".*\\.([^.]{3,4})$", "\\1", file ) %>% tolower
  if( ext %in% c( "png", "bmp", "jpg", "jpeg" ) ){
    tryCatch({
      im = readbitmap::read.bitmap( file )
    },
    error = function(e) {
      stop( paste0( e, "Note: im_load() fails for binary (black/white) bmp image." ) )
    })
    # im = readbitmap::read.bitmap( file )
    dim( im )
    if( ! is.null( attr( im, "header" ) ) ){
      im = im / 255
    }
    if( length( dim( im ) ) == 2 ){ # gray-scale image
      dim( im ) = c( dim( im ), 1 )
    } else if( length( dim( im ) ) == 3 ){ # multiple channels
      if( dim( im )[ 3 ] %in% c( 2, 4 ) ){
        # remove alpha channel if it is uninformative
        if( min( im[ , , dim( im )[ 3 ] ] ) == max( im[ , , dim( im )[ 3 ] ] ) ){
          im = im[ , , 1:( dim( im )[ 3 ] - 1 ), drop = FALSE ]
        }
      }
    }
    im = nimg( im, ifelse( base::missing( name ), get_image_name_from_file( file ), name ) )
    return( im )
  } else {
    stop( "Only jpg, png, and bmp formats are supported." )
  }
}


get_image_name_from_file = function( file ){
  tryCatch({
    name = stringr::str_split( file, "/" )[[ 1 ]]
    name = name[ length( name ) ]
    name = stringr::str_split( name, "[.]" )[[ 1 ]]
    return( name[ 1 ] )
  },
  error = function(e) {
    return( "-" )
  })
}


#' Save an image to disk
#' @param im An image.
#' @param name Name of the image file.
#' @param path The image is saved in this direcory. For example, path = getwd()
#' @param format Image format. Either "jpg", "png", "tiff", or "bmp". Default is "png".
#' @param quality (jpg only) default is 0.95. Higher quality means less compression.
#' @return No return value, called for side effects.
#' @examples
#' \dontrun{
#' # face.png is saved to a path (if a path is specified)
#' im_save( face, path = NULL )
#' # img.png is saved to a path (if a path is specified)
#' im_save( face, name = "img", path = NULL )
#' # myimage.jpg is saved to a path (if a path is specified)
#' im_save( face, name = "myimage", path = NULL, format = "jpg" )
#' }
#' @export
im_save = function( im, name, path, format = "png", quality = .95 ){
  if( ! format %in% c( "jpg", "png" ) ){
    warning( "Incorrect imaeg format. Use either jpg or png." )
    return()
  }
  if( base::missing( name ) ){
    name = deparse( substitute( im ) )
  }
  if( im_nchannel( im ) == 1 ){
    im = im_rep( im, 3 )
  }
  if( stringr::str_sub( path, stringr::str_length( path ) ) == "/" ){
    path = stringr::str_sub( path, end = stringr::str_length( path ) - 1 )
  }
  if( max( im ) > 1 || min( im ) < 0 ){
    warning( "Pixcel value exceeds the range [0,1], and hence it was clamped when saving.")
    im = clamping( im )
  }
  base::dir.create( path, showWarnings = FALSE, recursive = TRUE )
  file = paste0( path, "/", name, ".", format )
  if( format == "png" ){
    png::writePNG( im, file )
  } else if ( format == "jpeg" | format == "jpg" ){
    jpeg::writeJPEG( im, file, quality = quality )
  }
}


#' cimg to nimg conversion
#' @param im a cimg object
#' @return an nimg object
cimg2nimg = function( im ){
  if( is.list( im ) ){
    im = lapply( im, function( x ){
      if( "nimg" %in% class( x ) ){
        x
      } else {
        cimg2nimg( x )
      }
    })
    return( im )
  } else if( any( c( "cimg", "pixset" ) %in% class( im ) ) ){
    im = aperm( im, c( 2, 1, 4, 3 ) ) # (x, y, z, cc) to (y, x, cc, z)
    return( nimg( im[,,,1] ) )
  } else if( "nimg" %in% class( im ) ){
    return( im )
  } else {
    return( nimg( im ) )
  }
}


#' nimg to cimg conversion
#' @param im an nimg object
#' @return a cimg object
nimg2cimg = function( im ){
  if( is.list( im ) ){
    im = lapply( im, function(x){
      if( any( c( "cimg", "pixset" ) %in% class( x ) ) ){
        x
      } else {
        nimg2cimg( x )
      }
    })
    return( im )
  } else {
    if( any( c( "cimg", "pixset" ) %in% class( im ) ) ) {
      return( im )
    } else if( length( dim( im ) ) == 2 ){ # (y, x) to (x, y)
      return( imager::as.cimg( t( im ) ) )
    } else if( length( dim( im ) ) == 4 ){ # (y, x, cc, z) to (x, y, z, cc)
      return( imager::as.cimg( aperm( im, c( 2, 1, 4, 3 ) ) ) )
    } else if( length( dim( im ) ) == 3 ){ # (y, x, cc) to (x, y, cc)
      im = aperm( im, c( 2, 1, 3 ) )
      im2 = array( 0, dim = c( dim( im )[ 1 ], dim( im )[ 2 ], 1, dim( im )[ 3 ] ) )
      im2[,,1,] = im
      return( imager::as.cimg( im2 ) )
    }
  }
}


resetPar = function() {
  p = list(
    xlog = FALSE, ylog = FALSE, adj = 0.5, ann = TRUE, ask = FALSE, bg = "white", bty = "o",
    cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1.2, cex.sub = 1,
    col = "black", col.axis = "black", col.lab = "black", col.main = "black", col.sub = "black",
    crt = 0, err = 0, family = "", fg = "black", fig = c(0, 1, 0, 1), fin = c(6.239583, 5.6875),
    font = 1, font.axis = 1, font.lab = 1, font.main = 2, font.sub = 1,
    lab = c(5, 5, 7), las = 0, lend = "round", lheight = 1, ljoin = "round", lmitre = 10,
    lty = "solid", lwd = 1,
    mai = c(1.02, 0.82, 0.82, 0.42), mar = c(5.1, 4.1, 4.1, 2.1), mex = 1,
    mfcol = c(1, 1), mfg = rep(1, 4), mfrow = c(1, 1), mgp = c(3, 1, 0), mkh = 0.001,
    new = FALSE, oma = c(0, 0, 0, 0), omd = c(0, 1, 0, 1), omi = rep(0, 4),
    pch = 1, pin = c(4.999583, 3.8475), plt = c(0.131419, 0.9326878, 0.1793407, 0.8558242),
    ps = 12, pty = "m", smo = 1, srt = 0, tck = NA, tcl = -0.5, usr = c(0, 1, 0, 1),
    xaxp = c(0, 1, 5), xaxs = "r", xaxt = "s", xpd = FALSE,
    yaxp = c(0, 1, 5), yaxs = "r", yaxt = "s", ylbias = 0.2
  )
  graphics::par( p )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# color space ----


sRGB2RGB = function( im ){
  mask = im < 0.04045
  im[ mask ] = im[ mask ] / 12.92
  im[ !mask ] = ( ( im[ !mask ] + 0.055 ) / 1.055 )^2.4
  return( im )
}


RGB2sRGB = function( im ){
  mask = im < 0.0031308
  im[ mask ] = im[ mask ] * 12.92
  im[ !mask ] = 1.055 * im[ !mask ]^( 1 / 2.4 ) - 0.055
  return( im )
}


RGB2XYZ = function( im, use.D65 = TRUE ){
  if( use.D65 ){
    X = 0.4124564 * get_R( im ) + 0.3575761 * get_G( im ) + 0.1804375 * get_B( im )
    Y = 0.2126729 * get_R( im ) + 0.7151522 * get_G( im ) + 0.0721750 * get_B( im )
    Z = 0.0193339 * get_R( im ) + 0.1191920 * get_G( im ) + 0.9503041 * get_B( im )
  } else {
    X = 0.4360747 * get_R( im ) + 0.3850649 * get_G( im ) + 0.1430804 * get_B( im )
    Y = 0.2225045 * get_R( im ) + 0.7168786 * get_G( im ) + 0.0606169 * get_B( im )
    Z = 0.0139322 * get_R( im ) + 0.0971045 * get_G( im ) + 0.7141733 * get_B( im )
  }
  return( merge_color( list( X, Y, Z ) ) )
}


XYZ2RGB = function( im, use.D65 = TRUE ){
  if( use.D65 ){
    R =  3.24045484 * get_R( im ) - 1.5371389 * get_G( im ) - 0.49853155 * get_B( im )
    G = -0.96926639 * get_R( im ) + 1.8760109 * get_G( im ) + 0.04155608 * get_B( im )
    B =  0.05564342 * get_R( im ) - 0.2040259 * get_G( im ) + 1.05722516 * get_B( im )
  } else {
    R =  3.13385637 * get_R( im ) - 1.6168668 * get_G( im ) - 0.49061477 * get_B( im )
    G = -0.97876856 * get_R( im ) + 1.9161416 * get_G( im ) + 0.03345412 * get_B( im )
    B =  0.07194517 * get_R( im ) - 0.2289913 * get_G( im ) + 1.40524267 * get_B( im )
  }
  return( merge_color( list( R, G, B ) ) )
}


sRGB2XYZ = function( im, use.D65 = TRUE ){
  im %>% sRGB2RGB %>% RGB2XYZ( use.D65 )
}


XYZ2sRGB = function( im, use.D65 = TRUE ){
  im %>% XYZ2RGB( use.D65 ) %>% RGB2sRGB
}


XYZ2Lab = function( im, use.D65 = TRUE ){
  # reference white
  if( use.D65 ){
    white = c( 0.95047, 1, 1.08883 )
  } else {
    white = c( 0.96420, 1, 0.82491 )
  }
  im[ ,,1 ] = im[ ,,1, drop = FALSE ] / white[ 1 ]
  im[ ,,3 ] = im[ ,,3, drop = FALSE ] / white[ 3 ]
  #
  mask = 24389 * im > 216
  im[ mask ] = im[ mask ]^( 1 / 3 )
  im[ !mask ] = ( 24389 * im[ !mask ] / 27 + 16 ) / 116
  fx = im[ ,,1, drop = FALSE ]
  fy = im[ ,,2, drop = FALSE ]
  fz = im[ ,,3, drop = FALSE ]
  #
  L = ( 116 * fy - 16 )
  a = 500 * ( fx - fy )
  b = 200 * ( fy - fz )
  return( merge_color( list( L, a, b ) ) )
}


Lab2XYZ = function( im, use.D65 = TRUE ){
  eta = 216 / 24389
  kappa = 24389 / 27
  #
  fy = ( im[,,1, drop = FALSE ] + 16 ) / 116
  fx = 0.002 * im[,,2, drop = FALSE ] + fy
  fz = fy - 0.005 * im[,,3, drop = FALSE ]
  # x = fx^3 > eta ? fx^3 : ( 116 * fx - 16 ) / kappa
  mask = fx^3 > eta
  fx[ mask ] = fx[ mask ]^3
  fx[ !mask ] = ( 116 * fx[ !mask ] - 16 ) / kappa
  # y = L > 8 ? ( ( L + 16 ) / 116 )^3 : L / kappa
  L = im[,,1, drop = FALSE ]
  mask = L > 8
  L[ mask ] = ( ( L[ mask ] + 16 ) / 116 )^3
  L[ !mask ] = L[ !mask ] / kappa
  # z = fz^3 > eta ? fz^3 : ( 116 * fz - 16 ) / kappa
  mask = fz^3 > eta
  fz[ mask ] = fz[ mask ]^3
  fz[ !mask ] = ( 116 * fz[ !mask ] - 16 ) / kappa
  # reference white
  if( use.D65 ){
    white = c( 0.95047, 1, 1.08883 )
  } else {
    white = c( 0.96420, 1, 0.82491 )
  }
  fx = fx * white[ 1 ]
  fz = fz * white[ 3 ]
  return( merge_color( list( fx, L, fz ) ) )
}


sRGB2Lab = function( im, use.D65 = TRUE ){
  XYZ2Lab( sRGB2XYZ( im, use.D65 ), use.D65 )
}


Lab2sRGB = function( im, use.D65 = TRUE ){
  XYZ2sRGB( Lab2XYZ( im, use.D65 ), use.D65 )
}


RGB2Lab = function( im, use.D65 = TRUE ){
  im %>% RGB2XYZ( use.D65 ) %>% XYZ2Lab( use.D65 )
}


Lab2RGB = function( im, use.D65 = TRUE ){
  im %>% Lab2XYZ( use.D65 ) %>% XYZ2RGB( use.D65 )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# math ----


rescaling01 = function( x ){
  if( max( x ) == min( x ) ){
    return( x )
  } else {
    return( ( x - min( x ) ) / ( max( x ) - min( x ) ) )
  }
}


rescaling = function( x, from = 0, to = 1 ){
  if( max( x ) == min( x ) ){
    return( x )
  } else {
    return( from + ( to - from ) * rescaling01( x ) )
  }
}


clamping = function( x, min = 0, max = 1 ){
  x[ x < min ] = min
  x[ x > max ] = max
  return( x )
}


cubic_spline = function( x, low = 0, high = 1 ){
  if( low == high ){
    warning( "low and high must be different!" )
  } else if( low > high ){
    return( 1 - ( cubic_spline( x, high, low ) ) )
  }
  x2 = x
  t = x[ x > low & x < high ]
  t = ( t - low ) / ( high - low )
  x2[ x > low & x < high ] = t^2 * ( 3 - 2 * t )
  x2[ x <= low ] = 0
  x2[ x >= high ] = 1
  return( x2 )
}


MinMax = MaxMin = function( x ){
  return( max( x ) - min( x ) )
}


ramp_threshold = function( x, eta, phi ){
  y = x
  y[ x >= eta ] = 1
  y[ x < eta ] = 1 + tanh( phi * ( y[ x < eta ] - eta ) )
  return( y )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# stats ----


im_diff = function( im1, im2 ){
  if( imager::is.cimg( im1 ) ){
    im1 = cimg2nimg( im1 )
  }
  if( imager::is.cimg( im2 ) ){
    im2 = cimg2nimg( im2 )
  }
  return( mean( ( im1 - im2 )^2 ) )
}


get_moments = function( x, order = 1:4, na.rm = FALSE ){
  m = rep( 0.0, times = length( order ) )
  names( m ) = c( "mean", "sd", "skewness", "kurtosis" )[ order ]
  for( i in 1:length( order ) ){
    if( order[ i ] == 1 ){
      m[ i ] = base::mean( x, na.rm = na.rm )
    } else if( order[ i ] == 2 ){
      m[ i ] = stats::sd( x, na.rm = na.rm )
    } else if( order[ i ] == 3 ){
      m[ i ] = moments::skewness( x, na.rm = na.rm )
    } else if( order[ i ] == 4 ){
      m[ i ] = moments::kurtosis( x, na.rm = na.rm )
    }
  }
  return( m )
}


im_moments = function( im, channel = 1:3, order = 1:4, space = "CIELAB", max_size = 1024, na.rm = FALSE ){
  if( im_nc( im ) == 1 ){
    channel = 1
  }
  df = data.frame()
  im = im_resize_limit( im, max_size )
  if( space == "CIELAB" ){
    if( im_nc( im ) > 2 ){
      im = sRGB2Lab( im )
    }
    clabel = c( "L", "a", "b" )
  } else {
    clabel = c( "R", "G", "B", "A" )
  }
  channel = force_channel_label_to_num( channel )
  for( i in 1:length( channel ) ){
    mmt = get_moments( get_channel( im, channel[ i ] ), order, na.rm = na.rm )
    df = rbind( df, data.frame(
      channel = clabel[ channel[ i ] ], moment = names( mmt ), value = unname( mmt ) ) )
  }
  return( df )
}


im_distribute = function( im, channel, mean = NULL, sd = NULL, space = "CIELAB", clamp = TRUE ){
  channel = force_channel_label_to_num( channel )
  if( space == "CIELAB" && im_nc( im ) > 2 ){
    im = sRGB2Lab( im )
  }
  for( i in 1:length( channel ) ){
    if( is.null( mean[ i ] ) || is.na( mean[ i ] ) ){
      M = base::mean( get_channel( im, channel[ i ] ) )
    } else {
      M = mean[ i ]
    }
    if( is.null( sd[ i ] ) || is.na( sd[ i ] ) ){
      S = stats::sd( get_channel( im, channel[ i ] ) )
    } else {
      S = sd[ i ]
    }
    I = im[ , , channel[ i ], drop = F ]
    im[ , , channel[ i ] ] = S * ( ( I - base::mean( I ) ) / stats::sd( I ) ) + M
  }
  if( space == "CIELAB" && im_nc( im ) > 2 ){
    im = Lab2sRGB( im )
  }
  if( clamp ){
    im = clamping( im )
  }
  return( im )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# image info ----


im_height = function( im ){
  dim( im )[ 1 ]
}


im_width = function( im ){
  dim( im )[ 2 ]
}


im_size = function( im ){
  unname( dim( im )[ 1:2 ] )
}


im_npix = function( im ){
  prod( dim( im ) )
}


im_nchannel = function( im ){
  dim( im )[ 3 ]
}


im_nc = function( im ){
  im_nchannel( im )
}


im_cx = function( im ){
  return( floor( im_width( im ) / 2 ) + 1 )
}


im_cy = function( im ){
  return( floor( im_height( im ) / 2 ) + 1 )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# image slicing ----


force_channel_label_to_num = function( x ){
  if( is.numeric( x ) ){
    return( x )
  }
  y = c()
  for( i in 1:length( x ) ){
    if( x[ i ] %in% c( "R", "r", "L", "l" ) ){
      y = c( y, 1 )
    } else if( x[ i ] %in% c( "G", "g", "a" ) ){
      y = c( y, 2 )
    } else if( x[ i ] %in% c( "B", "b" ) ){
      y = c( y, 3 )
    } else if( x[ i ] %in% c( "A", "alpha", "Alpha" ) ){
      y = c( y, 4 )
    } else {
      y = c( y, 0 )
    }
  }
  return( y )
}


get_channel = function( im, channel ){
  if( length( dim( im ) ) == 2 ){
    return( im )
  } else {
    return( nimg( im[ , , force_channel_label_to_num( channel ), drop = FALSE ] ) )
  }
}


get_R = function( im ){
  return( get_channel( im, 1 ) )
}


get_G = function( im ){
  return( get_channel( im, 2 ) )
}


get_B = function( im ){
  return( get_channel( im, 3 ) )
}


get_A = function( im ){
  return( get_channel( im, 4 ) )
}


split_color = function( im ){
  ls = list()
  for( i in 1:dim( im )[ 3 ] ){
    ls = c( ls, list( nimg( im[ , , i, drop = FALSE ] ) ) )
  }
  return( ls )
}


merge_color = function( imlist ){
  imdim = dim( imlist[[ 1 ]] )
  im = array( 0, c( imdim[ 1 ], imdim[ 2 ], length( imlist ) ) )
  for( i in 1:length( imlist ) ){
    im[,,i] = imlist[[ i ]]
  }
  return( nimg( im ) )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# image transform ----


im_rep = function( im, n = 3, channel = 1 ){
  nimg( array( get_channel( im, channel ), c( im_height( im ), im_width( im ), n ) ) )
}


im_tricolored = function( im ){
  n = im_nc( im )
  if( n < 3 ){
    return( im_rep( im, 3, 1 ) )
  } else if( n > 3 ){
    return( get_channel( im, 1:3 ) )
  } else {
    return( im )
  }
}


im_pad = function( im, n, method = "mirror" ){
  if( n == 0 ) return( im )

  w = im_width( im )
  h = im_height( im )

  if( any( n > c( w, h ) ) ){
    warning( "n must be equal or smaller than image width (and height)." )
    return( im )
  }

  # create an empty matrix
  x = ifelse( is.numeric( method ), method, ifelse( method == "mean", mean( im ), 0 ) )
  mat = array( x, c( h + 2 * n, w + 2 * n, dim( im )[ 3 ] ) )

  # put the image
  mat[ ( n + 1 ):( n + h ), ( n + 1 ):( n + w ), ] = im

  # padding
  if( method == "zero" || method == "mean" || is.numeric( method ) ){
    # do nothing
  } else if( method == "repeat" ){
    # top left
    mat[ 1:n, 1:n, ] = im[ (h-n+1):h, (w-n+1):w, ]
    # top
    mat[ 1:n, (n+1):(n+w), ] = im[ (h-n+1):h, 1:w, ]
    # top right
    mat[ 1:n, (n+w+1):(2*n+w), ] = im[ (h-n+1):h, 1:n, ]
    # left
    mat[ (n+1):(n+h), 1:n, ] = im[ 1:h, (w-n+1):w, ]
    # right
    mat[ (n+1):(n+h), (n+w+1):(2*n+w), ] = im[ 1:h, 1:n, ]
    # bottom left
    mat[ (n+h+1):(2*n+h), 1:n, ] = im[ 1:n, (w-n+1):w, ]
    # bottom
    mat[ (n+h+1):(2*n+h), (n+1):(n+w), ] = im[ 1:n, 1:w, ]
    # bottom right
    mat[ (n+h+1):(2*n+h), (n+w+1):(2*n+w), ] = im[ 1:n, 1:n, ]
  } else if( method == "mirror" ){
    # top left
    mat[ 1:n, 1:n, ] = im[ n:1, n:1, ]
    # top
    mat[ 1:n, (n+1):(n+w), ] = im[ n:1, 1:w, ]
    # top right
    mat[ 1:n, (n+w+1):(2*n+w), ] = im[ n:1, w:(w-n+1), ]
    # left
    mat[ (n+1):(n+h), 1:n, ] = im[ 1:h, n:1, ]
    # right
    mat[ (n+1):(n+h), (n+w+1):(2*n+w), ] = im[ 1:h, w:(w-n+1), ]
    # bottom left
    mat[ (n+h+1):(2*n+h), 1:n, ] = im[ h:(h-n+1), n:1, ]
    # bottom
    mat[ (n+h+1):(2*n+h), (n+1):(n+w), ] = im[ h:(h-n+1), 1:w, ]
    # bottom right
    mat[ (n+h+1):(2*n+h), (n+w+1):(2*n+w), ] = im[ h:(h-n+1), w:(w-n+1), ]
  }

  im = nimg( mat )
  return( im )
}


im_crop = function( im, margin ){
  if( length( margin ) == 1 ){
    top = bottom = left = right = margin
  } else if( length( margin ) == 2 ){
    top = bottom = margin[ 1 ]
    left = right = margin[ 2 ]
  } else if( length( margin ) == 3 ){
    warning( "margin length must be 1, 2, or 4!" )
  } else if( length( margin ) == 4 ){
    top = margin[ 1 ]
    right = margin[ 2 ]
    bottom = margin[ 3 ]
    left = margin[ 4 ]
  }
  im = im[ (1 + top):(im_height( im ) - bottom), (1 + left):(im_width( im ) - right), , drop = FALSE ]
  return( nimg( im ) )
}


im_get = function( im, y1, x1, y2, x2 ){
  im = im[ y1:y2, x1:x2, , drop = FALSE ]
  return( nimg( im ) )
}


im_crop_square = function( im, position = 0.5 ){
  position = clamping( position )
  diff = im_width( im ) - im_height( im )
  position = 2 * position - 1 # range [-1,1]
  size = min( im_size( im ) )
  erode = abs( diff ) / 2
  center = max( im_size( im ) ) / 2
  start = floor( center - size / 2 + erode * position )
  if( start < 1 ) start = 1
  end = start + size - 1
  if( diff > 0 ){ # wide
    im = im_crop( im, c( 0, im_width( im ) - end, 0, start - 1 ) )
  } else { # tall
    im = im_crop( im, c( start - 1, 0, im_height( im ) - end, 0 ) )
  }
  return( nimg( im ) )
}


im_rotate = function(im, angle, expand = FALSE, cx = NULL, cy = NULL, interpolation = 2, pad = "zero"){
  cimg = nimg2cimg( im )
  boundary = 0
  if( pad == "neumann" ){
    boundary = 1
  } else if( pad == "repeat" ){
    boundary = 2
  }
  if( is.null( cx ) && is.null( cy ) ){
    if( expand ){
      im = imager::imrotate( cimg, angle, interpolation = interpolation, boundary = boundary )
    } else {
      im = imager::imrotate( cimg, angle, im_width(im)/2, im_height(im)/2, interpolation, boundary )
    }
  } else if( ! is.null( cx ) && ! is.null( cy ) ){
    im = imager::imrotate( cimg, angle, cx, cy, interpolation, boundary )
  } else {
    warning( "You must specify both cx and cy." )
    return( NULL )
  }
  return( cimg2nimg( clamping( im ) ) )
}


im_resize = function( im, height, width, interpolation = 1 ){
  itype = 1 + 2 * interpolation # 0->1, 1->3, 2->5
  if( base::missing( width ) ){ # scale to height
    width = round( im_width( im ) * ( height / im_height( im ) ) )
  } else if( base::missing( height ) ){ # scale to width
    height = round( im_height( im ) * ( width / im_width( im ) ) )
  }
  im = imager::resize( nimg2cimg( im ), size_x = width, size_y = height, interpolation_type = itype )
  return( cimg2nimg( im ) )
}


im_resize_limit = function( im, bound, interpolation = 1 ){
  if( max( im_size( im ) ) < bound ){
    return( im )
  }
  if( im_width( im ) > im_height( im ) ){
    im_resize( im, width = bound, interpolation = interpolation )
  } else {
    im_resize( im, height = bound, interpolation = interpolation )
  }
}


im_resize_limit_min = function( im, bound, interpolation = 1 ){
  if( min( im_size( im ) ) < bound ){
    return( im )
  }
  if( im_width( im ) > im_height( im ) ){
    im_resize( im, height = bound, interpolation = interpolation )
  } else {
    im_resize( im, width = bound, interpolation = interpolation )
  }
}


im_resize_scale = function( im, scale = 1, interpolation = 1 ){
  itype = 1 + 2 * interpolation # 0->1, 1->3, 2->5
  im = imager::imresize( nimg2cimg( im ), scale, itype )
  return( cimg2nimg( im ) )
}


im_combine = function( im1, im2, y = 0, x = 0, alpha = FALSE, background = 1 ){
  cc = max( im_nc( im1 ), im_nc( im2 ) )
  h = max( im_height( im1 ), y + im_height( im2 ), im_height( im2 ), - y + im_height( im1 ) )
  w = max( im_width( im1 ), x + im_width( im2 ), im_width( im2 ), - x + im_width( im1 ) )
  im = array( rep( background, each = h * w, times = cc ), dim = c( h, w, cc ) )

  y1 = ifelse( y < 0, -y, 0 ) + 1
  y2 = ifelse( y < 0, 0, y ) + 1
  x1 = ifelse( x < 0, -x, 0 ) + 1
  x2 = ifelse( x < 0, 0, x ) + 1
  im[ y1:( y1 + im_height( im1 ) - 1 ), x1:( x1 + im_width( im1 ) - 1 ), 1:cc ] = im1
  im[ y2:( y2 + im_height( im2 ) - 1 ), x2:( x2 + im_width( im2 ) - 1 ), 1:cc ] = im2
  if( ! alpha ){
    return( nimg( im ) )
  } else {
    A = array( 0, dim = c( h, w, 1 ) )
    A[ y1:( y1 + im_height( im1 ) - 1 ), x1:( x1 + im_width( im1 ) - 1 ), 1 ] = 1
    A[ y2:( y2 + im_height( im2 ) - 1 ), x2:( x2 + im_width( im2 ) - 1 ), 1 ] = 1
    return( merge_color( c( split_color( im ), list( A ) ) ) )
  }
}


im_threshold = function( im, thr = "auto", approx = TRUE, adjust = 1 ){
  cimg2nimg( imager::threshold( nimg2cimg( im ), thr, approx, adjust ) )
}


im_raise = function( im, intercept ){
  intercept + ( 1 - intercept ) * im
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# luminance ----


im_gray = function( im, tricolored = FALSE ){
  if( im_nc( im ) < 2 ){
    return( im )
  }
  lab = sRGB2Lab( im )
  L = get_R( lab )
  C0 = array( 0, dim = dim( L ) )
  im = merge_color( list( L, C0, C0 ) ) %>% Lab2sRGB
  if( ! tricolored ){
    im = get_R( im )
  }
  return( im )
}


get_L = function( im, scale = TRUE ){
  if( im_nc( im ) == 1 ){
    return( im )
  } else if( im_nc( im ) == 2 ){
    return( get_R( im ) )
  }
  if( scale ){
    return( get_R( sRGB2Lab( im ) ) / 100 )
  } else {
    return( get_R( sRGB2Lab( im ) ) )
  }
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# spatial filtering ----


box_blur = function( im, radius ){
  if( radius < 1 ){
    warning( "radius should be equal to or larger than 1.")
    return( im )
  }
  r = radius
  if( im_nc( im ) != 1 ){
    imlist = list()
    for( i in 1:im_nc( im ) ){
      imlist = c( imlist, list( box_blur( get_channel( im, i ), r ) ) )
    }
    return( merge_color( imlist ) )
  }
  L = 2 * r + 1
  width = im_width( im )
  height = im_height( im )
  im = im_pad( im, r, method = "mirror" )
  out = array( 0.0, dim( im ) )
  cumsum = rowSums( im[ , 1:(2*r), ] )
  # i = r + 1
  cumsum = cumsum + im[ ,r + 1 + r, ]
  out[ , r + 1, ] = cumsum / L
  for( i in ( r + 2 ):( width + r ) ){
    cumsum = cumsum + im[ ,i + r, ] - im[ ,i - r - 1, ]
    out[ , i, ] = cumsum / L
  }
  im = out
  cumsum = colSums( im[ 1:(2*r), , ] )
  cumsum = cumsum + im[ r + 1 + r, , ]
  out[ r + 1, , ] = cumsum / L
  for( i in ( r + 2 ):( height + r ) ){
    cumsum = cumsum + im[ i + r, , ] - im[ i - r - 1, , ]
    out[ i, , ] = cumsum / L
  }
  out = im_crop( out, r )
  return( out )
}


box_variance = function( im, radius ){
  box_blur( im^2, radius ) - box_blur( im, radius )^2
}


guided_filter = function( p, radius, epsilon = 0.1, I = p ){
  if( radius < 1 ){
    warning( "radius should be equal to or larger than 1.")
    return( p )
  }

  I_mean = box_blur( I, radius )
  I_var = box_variance( I, radius )
  p_mean = box_blur( p, radius )

  a = ( box_blur( I * p, radius ) - I_mean * p_mean ) / ( I_var + epsilon )
  b = p_mean - a * I_mean

  a_mean = box_blur( a, radius )
  b_mean = box_blur( b, radius )

  q = a_mean * I + b_mean
  return( q )
}


stat_filter = function( im, radius, FUN, pad.method = "mirror" ){
  if( radius < 1 ){
    warning( "radius should be equal to or larger than 1.")
    return( im )
  }

  if( im_nc( im ) > 1 ){
    imlist = list()
    for( i in 1:im_nc( im ) ){
      imlist = c( imlist, list( stat_filter( get_channel( im, i ), radius, FUN, pad.method ) ) )
    }
    return( merge_color( imlist ) )
  }

  im = im_pad( im, radius, method = pad.method )[,,]
  im2 = im
  for( cy in ( 1 + radius ):( im_height( im ) - radius ) ){
    for( cx in ( 1 + radius ):( im_width( im ) - radius ) ){
      im2[ cy, cx ] = FUN(
        as.vector( im[ ( cy - radius ):( cy + radius ), ( cx - radius ):( cx + radius ) ] )
      )
    }
  }
  im2 = im_crop( nimg( im2 ), radius )
  return( im2 )
}


im_conv = function( im, kernel, pad.method = "mirror" ){
  if( is.null( kernel ) ){
    return( im )
  }
  if( im_nc( im ) > 1 ){
    imlist = list()
    for( i in 1:im_nc( im ) ){
      imlist = c( imlist, list( im_conv( get_channel( im, i ), kernel, pad.method ) ) )
    }
    return( merge_color( imlist ) )
  }
  npad = floor( max( dim( kernel )[ 1:2 ] ) / 2 )
  im = im_pad( im, n = npad, method = pad.method )
  im = imager::convolve( nimg2cimg( im ), nimg2cimg( kernel ) )
  im = imager::crop.borders( im, nPix = npad )
  return( cimg2nimg( im ) )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# visualization ----


visualize_contrast = function( im, abs.range = NULL, Lcenter = 55 ){
  if( is.null( abs.range ) ){
    abs.range = max( abs( im ) )
  }
  L = clamping( Lcenter + im * ( 100 - Lcenter ) / abs.range, 0, 100 )
  ab = array( 0, dim = dim( L ) )
  clamping( Lab2sRGB( merge_color( list( L, ab, ab ) ) ) )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# material editing ----


#' Scale-space decomposition by the guided filter
#' @param im an image
#' @param log_epsilon offset for log transformation
#' @param filter_epsilon epsilon parameter
#' @return a list of images
gf_decompose = function( im, log_epsilon = 0.0001, filter_epsilon = 0.01 ){
  if( im_nc( im ) == 2 || im_nc( im ) > 3 ){
    warning( "The number of color channel must be either 1 or 3.")
    return( NULL )
  }
  if( im_nc( im ) == 3 ){
    lab = sRGB2Lab( im )
    dec = gf_decompose( get_channel( lab, 1 ) / 100 )
    dec = c( dec, list( a = get_channel( lab, 2 ), b = get_channel( lab, 3 ) ) )
    dec$n.color = 3
    return( dec )
  }

  dec = gf_decompose_scale( im, NULL, log_epsilon, filter_epsilon )
  dec = gf_decompose_parts( dec )


  return( dec )
}


#' Scale-space decomposition by the guided filter
#' @param im a grayscale image
#' @param depth scale depth
#' @param log_epsilon offset for log transformation
#' @param filter_epsilon epsilon parameter
#' @return a list of images
gf_decompose_scale = function( im, depth = NULL, log_epsilon = 0.0001, filter_epsilon = 0.01 ){
  im = get_L( im )
  if( is.null( depth ) ){
    depth = floor( log2( min( im_size( im ) ) ) ) - 1
  }
  L = log( im + log_epsilon )

  if( depth == 0 ) {
    N = 0
    D = list( residual = L )
  } else {
    # L0 = L
    # Lk = guided_filter( Lk-1, filter_epsilon, 2^k ) (k=1~n)
    # Dk = Lk-1 - Lk
    # recon = ∑(Dk)[k=1~n] + Ln
    N = min( depth, floor( log2( min( im_size( L ) ) ) ) )
    L_k_minus_1 = guided_filter( L, 2^1, filter_epsilon ) # L1
    D_k = L - L_k_minus_1 # D1
    D = list( D_k )
    if( N > 1 ){
      for( k in 2:N ){
        L_k = guided_filter( L_k_minus_1, 2^k, filter_epsilon )
        D_k = L_k_minus_1 - L_k
        D = c( D, list( D_k ) )
        if( k == N ){
          names( D ) = paste0( "D", sprintf( paste0( "%0", nchar( N ), "d" ), 1:N ) )
          # add residual
          D = c( D, list( residual = L_k ) )
        } else {
          L_k_minus_1 = L_k
        }
      }
    } else if( N == 1 ) {
      names( D ) = paste0( "D", sprintf( paste0( "%0", nchar( N ), "d" ), 1:N ) )
      D = c( D, list( residual = L_k_minus_1 ) )
    }
  }

  dec = list(
    size = im_size( im ),
    depth = N,
    n.color = 1,
    log_epsilon = log_epsilon,
    filter_epsilon = filter_epsilon,
    L = D
  )
  return( dec )
}


#' Scale-space decomposition
#' @param dec output of gf_decompose_scale function
#' @return a list of images
gf_decompose_parts = function( dec ){
  L = dec$L
  residual = L$residual
  L$residual = NULL
  L = lapply( L, function( im ){
    blur_range = 0.2
    range_lo = 1 - blur_range
    range_hi = 1 + blur_range
    sigma = stats::sd( im )
    hi =
      im * cubic_spline( im, range_lo * sigma, range_hi * sigma ) +
      im * cubic_spline( im, -range_lo * sigma, -range_hi * sigma )
    lo =
      im * pmin( cubic_spline( im, -range_hi * sigma, -range_lo * sigma ),
                 cubic_spline( im, range_hi * sigma, range_lo * sigma ) )
    hip = hi
    hip[ hi < 0 ] = 0
    hin = hi
    hin[ hi > 0 ] = 0
    lop = lo
    lop[ lo < 0 ] = 0
    lon = lo
    lon[ lo > 0 ] = 0
    return( list( highamp_posi = hip, highamp_nega = hin, lowamp_posi = lop, lowamp_nega = lon ) )
  } )
  L = c( L, list( residual = residual ) )
  dec$L = L
  return( dec )
}


gf_get_residual = function( im, Depth, log_epsilon = 0.0001, filter_epsilon = 0.01 ){
  if( Depth < 1 ){
    return( im )
  }

  L = im_gray( im )
  L = log( L + log_epsilon )

  N = min( Depth, floor( log2( min( im_size( L ) ) ) ) )
  L_k_minus_1 = guided_filter( L, 2^1, filter_epsilon ) # L1
  if( N > 1 ){
    for( k in 2:N ){
      L_k = guided_filter( L_k_minus_1, 2^k, filter_epsilon )
      if( k == N ){
        residual = L_k
      } else {
        L_k_minus_1 = L_k
      }
    }
  } else {
    residual = L_k_minus_1
  }

  residual = exp( residual ) - log_epsilon
  return( residual )
}


#' Reconstruct the original image from decomposed data
#' @param dec decomposed data
#' @param scales which spatial scales to use for reconstruction
#' @param ind a numeric vector
#' @param include.residual either TRUE (default) or FALSE
#' @return an image
gf_reconstruct = function( dec, scales, ind, include.residual = TRUE ){
  if( base::missing( scales ) ){
    scales = 1:dec$depth
  }
  if( base::missing( ind ) ){
    ind = 1:4
  }

  recon = array( 0, c( dec$size, 1 ) )
  if( ! any( 0 == scales ) && length( dec$L ) > 1 ){
    for( i in scales ){
      if( "nimg" %in% class( dec$L[[ i ]] ) ){
        # scale-only decomposition
        recon = recon + dec$L[[ i ]]
      } else {
        # scale and parts decomposition
        for( j in ind ){
          recon = recon + dec$L[[ i ]][[ j ]]
        }
      }
    }
  }
  if( include.residual ){
    recon = recon + dec$L$residual
  }
  recon = exp( recon ) - dec$log_epsilon

  if( dec$n.color == 3 ){
    recon = Lab2sRGB( merge_color( list( recon * 100, dec$a, dec$b ) ) )
  }
  return( recon )
}



#' Calculate the BS feature energy
#'
#' @param im An image.
#' @param mask (optional) An image used for mask.
#' @return a data frame
#' @examples
#' data = get_BS_energy(face)
#' @export
get_BS_energy = function( im, mask ){
  if( ! missing( mask ) ){
    if( ! is.logical( mask ) ){
      mask = as.logical( im_threshold( im_gray( mask ), "auto" ) )
    }
  }

  # Image decomposition by the Band-Sift algorithm
  dec = gf_decompose( im )

  # BS feature maps
  maps = list( HHP = dec$L$D1$highamp_posi,
               HHN = dec$L$D1$highamp_nega,
               HLP = dec$L$D1$lowamp_posi,
               HLN = dec$L$D1$lowamp_nega,
               LHP = dec$L[[ dec$depth ]]$highamp_posi,
               LHN = dec$L[[ dec$depth ]]$highamp_nega,
               LLP = dec$L[[ dec$depth ]]$lowamp_posi,
               LLN = dec$L[[ dec$depth ]]$lowamp_nega )
  for( i in 2:( dec$depth - 0 ) ){
    if( i <= ( dec$depth / 2 ) ){
      maps$HHP = maps$HHP + dec$L[[ i ]]$highamp_posi
      maps$HHN = maps$HHN + dec$L[[ i ]]$highamp_nega
      maps$HLP = maps$HLP + dec$L[[ i ]]$lowamp_posi
      maps$HLN = maps$HLN + dec$L[[ i ]]$lowamp_nega
    } else {
      maps$LHP = maps$LHP + dec$L[[ i ]]$highamp_posi
      maps$LHN = maps$LHN + dec$L[[ i ]]$highamp_nega
      maps$LLP = maps$LLP + dec$L[[ i ]]$lowamp_posi
      maps$LLN = maps$LLN + dec$L[[ i ]]$lowamp_nega
    }
  }
  maps$HLA = maps$HLP + maps$HLN
  maps$LAN = maps$LHN + maps$LLN
  maps$aging = maps$HLA + maps$HHN

  # energy calculation
  energy = rep( -1, 11 )
  for( i in 1:11 ){
    D = maps[[ i ]]
    if( ! missing( mask ) ){
      D = D[ mask ]
    }
    energy[ i ] = mean( D^2 )
  }
  total_energy = sum( energy[ 1:8 ] )

  df = data.frame(
    feature = c( names( maps )[ 1:8 ], "total", names( maps )[ 9:11 ]),
    energy = c( energy[ 1:8 ], total_energy, energy[ 9:11 ])
  )
  df$normalized = df$energy / df$energy[ 9 ]

  return( df )
}



#' Apply material editing effect
#'
#' @param im An input image.
#' @param effect A string naming the effect to apply. Either "gloss", "shine", "spots", "blemish", "rough",
#' "stain", "shadow", or "aging".
#' @param strength A numeric, which controls the strength of the effect. Strength values between 0 and 1 will
#' reduce a feature, while strength values larger than 1 will boost a feature. A strength value of 1 does nothing.
#' Negative values are allowed, which will invert a feature.
#' @param max_size If the shorter side of the input image is larger than this value (the default is 1024),
#' input image is resized before applying effects. Because the modif() function is very slow for large-resolution
#' images, it is useful to limit the image resolution to speed-up the image processing.
#' @param log_epsilon Offset for log transformation (default is 0.0001).
#' Need not to change this value in most cases.
#' @param filter_epsilon Epsilon parameter of the Guided filter (default is 0.01).
#' Need not to change this value in most cases.
#' @return an output image
#' @examples
#' \donttest{
#' plot(modif(face, effect = "shine", strength = 2.5)) # Apply the "shine" effect (make objects shiny)
#' plot(modif(face, effect = "shine", strength = 0.2)) # Less shiny effect with a parameter less than 1
#' plot(modif(face, effect = c("shine", "stain"), strength = c(0.2, 3))) # Less shiny and more stain
#' }
#' @export
modif = function( im, effect, strength, max_size = 1024, log_epsilon = 0.0001, filter_epsilon = 0.01 ){
  is_invalid_name = ! effect %in% c( "gloss", "shine", "spots", "blemish", "rough", "stain", "shadow", "aging" )
  if( any( is_invalid_name ) ){
    warning( paste0( "Invalid effect name: ",
                     paste0( effect[ is_invalid_name ], sep = "", collapse = ", " ), sep = "" ) )
    return( im )
  }
  if( all( strength == 1 ) ){
    return( im )
  }
  im = im_resize_limit_min( im, max_size )

  if( im_nc( im ) == 3 ){
    lab = sRGB2Lab( im )
    bs = modif( get_channel( lab, 1 ) / 100, effect, strength, max_size, log_epsilon, filter_epsilon )
    return( clamping( Lab2sRGB( merge_color( list( bs * 100, get_G( lab ), get_B( lab ) ) ) ) ) )
  } else {
    dec = gf_decompose( get_L( im ), log_epsilon, filter_epsilon )
  }

  params = modif_set_params( effect, strength, dec$depth )
  dec = modif_edit_dec( dec, params )
  rec = clamping( gf_reconstruct( dec ) )

  return( rec )
}


#' Apply material editing effect
#'
#' @param im An input image.
#' @param params A list of parameter values. Parameter names are freq, amp, sign, and strength.
#' @param max_size If the shorter side of the input image is larger than this value (the default is 1024),
#' input image is resized. The modif function is very slow for large-resolution images.
#' @param log_epsilon Offset for log transformation (default is 0.0001).
#' Need not to change this value in most cases.
#' @param filter_epsilon Epsilon parameter of the Guided filter (default is 0.01).
#' Need not to change this value in most cases.
#' @return an output image
#' @examples
#' \donttest{
#' # shine effect
#' shine = list(freq = "H", amp = "H", sign = "P", strength = 2)
#' plot(modif2(face, params = shine))
#'
#' # shine effect (equivalent to the above)
#' shine2 = list(freq = 1:4, amp = "H", sign = "P", strength = 2)
#' plot(modif2(face, params = shine2))
#'
#' # you can specify a feature name directly, instead of specifying freq/amp/sign separately
#' plot( modif2( face, params = list( feature = "HHA", strength = 2 ) ) )
#' plot( modif2( face, params = list( feature = "1HP", strength = 3 ) ) )
#'
#' # apply multiple effects at the same time
#' blemish = list(feature = "HLA", strength = 0.1) # less blemish
#' smooth = list(feature = "HHN", strength = 0.2) # smoother
#' plot(modif2(face, params = list(blemish, smooth)))
#' }
#' @export
modif2 = function( im, params, max_size = 1024, log_epsilon = 0.0001, filter_epsilon = 0.01 ){
  im = im_resize_limit_min( im, max_size )

  if( im_nc( im ) == 3 ){
    lab = sRGB2Lab( im )
    bs = modif2( get_channel( lab, 1 ) / 100, params, max_size, log_epsilon, filter_epsilon )
    return( clamping( Lab2sRGB( merge_color( list( bs * 100, get_G( lab ), get_B( lab ) ) ) ) ) )
  } else {
    dec = gf_decompose( get_L( im ), log_epsilon, filter_epsilon )
  }

  params = modif_set_custom_params( params, dec$depth )
  dec = modif_edit_dec( dec, params )
  rec = clamping( gf_reconstruct( dec ) )

  return( rec )
}


modif_set_params = function( effects, strength, depth ){
  if( any( ! effects %in% c( "gloss", "shine", "spots", "blemish", "rough", "stain", "shadow", "aging" ) ) ){
    warning( "Invalid effect name." )
    return( NULL )
  }
  if( "aging" %in% effects ){
    if( length( effects ) == 1 ){
      if( missing( strength ) ){
        strength = 2.5
      }
      c(
        modif_set_params( "spots", strength, depth ),
        modif_set_params( "blemish", strength, depth )
      ) %>% return()
    } else {
      index = which(effects == "aging")
      strength_aging = strength[ index ]
      strength_other = strength[ -index ]
      c(
        modif_set_params( effects[ effects != "aging" ], strength_other, depth ),
        modif_set_params( "aging", strength_aging, depth )
      ) %>% return()
    }
  } else {
    defaults = data.frame(
      effect = c( "blemish", "gloss", "rough", "shadow", "shine", "spots", "stain" ),
      freq = c( "H", "H", "H", "L", "H", "H", "H" ),
      amp  = c( "L", "H", "L", "A", "H", "H", "L" ),
      sign = c( "A", "A", "P", "N", "P", "N", "N" ),
      strength = c( 2.5, 2, 2.5, 3.5, 2, 3.5, 2.5 ),
      stringsAsFactors = FALSE
    )
    params = defaults[ defaults$effect %in% effects, ]
    if( ! missing( strength ) ){
      params$strength = strength[ base::sort( effects, index.return = TRUE )$ix ]
    }
    params = do.call( function(...) base::Map(list, ...), params ) # rows to a list
    params = unname( params )
    #
    for( i in 1:length( params ) ){
      # freq_num
      if( params[[ i ]]$freq == "H" ){
        params[[ i ]]$freq_num = 1:floor( depth / 2 )
      } else if( params[[ i ]]$freq == "L" ){
        params[[ i ]]$freq_num = ( floor( depth / 2 ) + 1 ):depth
      }
      # ind
      if( params[[ i ]]$amp == "A" ){
        amp = c( 1, 1, 1, 1 )
      } else if( params[[ i ]]$amp == "H" ){
        amp = c( 1, 1, 0, 0 )
      } else {
        amp = c( 0, 0, 1, 1 )
      }
      if( params[[ i ]]$sign == "A" ){
        sign = c( 1, 1, 1, 1 )
      } else if( params[[ i ]]$sign == "P" ){
        sign = c( 1, 0, 1, 0 )
      } else {
        sign = c( 0, 1, 0, 1 )
      }
      params[[ i ]]$ind = which( amp & sign )
    }
    return( params )
  }
}


modif_set_custom_params = function( params, depth ){
  if( ! class( params[[ 1 ]] ) == "list" ){
    params = list( params )
  }
  for( i in 1:length( params ) ){
    # effect name
    if( is.null( params[[ i ]]$effect ) ){
      params[[ i ]]$effect = "custom"
    }
    #
    if( ! is.null( params[[ i ]]$feature ) ){
      params[[ i ]]$freq = stringr::str_sub( params[[ i ]]$feature, 1, 1 )
      params[[ i ]]$amp  = stringr::str_sub( params[[ i ]]$feature, 2, 2 )
      params[[ i ]]$sign = stringr::str_sub( params[[ i ]]$feature, 3, 3 )
    }
    # strength
    if( is.null( params[[ i ]]$strength ) ){
      # set the default values of strength
      params[[ i ]]$strength = c( 2, 2, 3.5, 2.5, 2.5, 2.5, 3.5, 2.5, 2 )[
        which( params[[ i ]]$effect ==
                 c( "gloss","shine","spots","blemish","rough","stain","shadow", "aging", "custom" ) ) ]
    }
    # freq_num
    if( is.character( params[[ i ]]$freq ) ){
      if( params[[ i ]]$freq == "A" ){
        params[[ i ]]$freq_num = 1:depth
      } else if( params[[ i ]]$freq == "H" ){
        params[[ i ]]$freq_num = 1:floor( depth / 2 )
      } else if( params[[ i ]]$freq == "L" ){
        params[[ i ]]$freq_num = ( floor( depth / 2 ) + 1 ):depth
      } else if( ! is.na( as.numeric( params[[ i ]]$freq ) ) ){
        params[[ i ]]$freq_num = as.numeric( params[[ i ]]$freq )
      }
    } else {
      params[[ i ]]$freq_num = params[[ i ]]$freq
    }
    # ind
    if( params[[ i ]]$amp == "A" ){
      amp = c( 1, 1, 1, 1 )
    } else if( params[[ i ]]$amp == "H" ){
      amp = c( 1, 1, 0, 0 )
    } else {
      amp = c( 0, 0, 1, 1 )
    }
    if( params[[ i ]]$sign == "A" ){
      sign = c( 1, 1, 1, 1 )
    } else if( params[[ i ]]$sign == "P" ){
      sign = c( 1, 0, 1, 0 )
    } else {
      sign = c( 0, 1, 0, 1 )
    }
    params[[ i ]]$ind = which( amp & sign )
  }
  return( params )
}


modif_edit_dec = function( dec, params ){
  for( p in 1:length( params ) ){
    for( f in params[[ p ]]$freq_num ){
      for( i in params[[ p ]]$ind ){
        dec$L[[ f ]][[ i ]] = dec$L[[ f ]][[ i ]] * params[[ p ]]$strength
      }
    }
  }
  return( dec )
}

