{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ForwardedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ForwardedValues
  ( ForwardedValues (..),

    -- * Smart constructor
    mkForwardedValues,

    -- * Lenses
    fvQueryStringCacheKeys,
    fvHeaders,
    fvQueryString,
    fvCookies,
  )
where

import Network.AWS.CloudFront.Types.CookiePreference
import Network.AWS.CloudFront.Types.Headers
import Network.AWS.CloudFront.Types.QueryStringCacheKeys
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field.
--
-- If you want to include values in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send values to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
--
-- /See:/ 'mkForwardedValues' smart constructor.
data ForwardedValues = ForwardedValues'
  { queryStringCacheKeys ::
      Lude.Maybe QueryStringCacheKeys,
    headers :: Lude.Maybe Headers,
    queryString :: Lude.Bool,
    cookies :: CookiePreference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ForwardedValues' with the minimum fields required to make a request.
--
-- * 'cookies' - This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field.
--
-- If you want to include cookies in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send cookies to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html How CloudFront Forwards, Caches, and Logs Cookies> in the /Amazon CloudFront Developer Guide/ .
-- * 'headers' - This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field.
--
-- If you want to include headers in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send headers to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- A complex type that specifies the @Headers@ , if any, that you want CloudFront to forward to the origin for this cache behavior (whitelisted headers). For the headers that you specify, CloudFront also caches separate versions of a specified object that is based on the header values in viewer requests.
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/header-caching.html Caching Content Based on Request Headers> in the /Amazon CloudFront Developer Guide/ .
-- * 'queryString' - This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field.
--
-- If you want to include query strings in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send query strings to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- Indicates whether you want CloudFront to forward query strings to the origin that is associated with this cache behavior and cache based on the query string parameters. CloudFront behavior depends on the value of @QueryString@ and on the values that you specify for @QueryStringCacheKeys@ , if any:
-- If you specify true for @QueryString@ and you don't specify any values for @QueryStringCacheKeys@ , CloudFront forwards all query string parameters to the origin and caches based on all query string parameters. Depending on how many query string parameters and values you have, this can adversely affect performance because CloudFront must forward more requests to the origin.
-- If you specify true for @QueryString@ and you specify one or more values for @QueryStringCacheKeys@ , CloudFront forwards all query string parameters to the origin, but it only caches based on the query string parameters that you specify.
-- If you specify false for @QueryString@ , CloudFront doesn't forward any query string parameters to the origin, and doesn't cache based on query string parameters.
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/QueryStringParameters.html Configuring CloudFront to Cache Based on Query String Parameters> in the /Amazon CloudFront Developer Guide/ .
-- * 'queryStringCacheKeys' - This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field.
--
-- If you want to include query strings in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send query strings to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- A complex type that contains information about the query string parameters that you want CloudFront to use for caching for this cache behavior.
mkForwardedValues ::
  -- | 'queryString'
  Lude.Bool ->
  -- | 'cookies'
  CookiePreference ->
  ForwardedValues
mkForwardedValues pQueryString_ pCookies_ =
  ForwardedValues'
    { queryStringCacheKeys = Lude.Nothing,
      headers = Lude.Nothing,
      queryString = pQueryString_,
      cookies = pCookies_
    }

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field.
--
-- If you want to include query strings in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send query strings to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- A complex type that contains information about the query string parameters that you want CloudFront to use for caching for this cache behavior.
--
-- /Note:/ Consider using 'queryStringCacheKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fvQueryStringCacheKeys :: Lens.Lens' ForwardedValues (Lude.Maybe QueryStringCacheKeys)
fvQueryStringCacheKeys = Lens.lens (queryStringCacheKeys :: ForwardedValues -> Lude.Maybe QueryStringCacheKeys) (\s a -> s {queryStringCacheKeys = a} :: ForwardedValues)
{-# DEPRECATED fvQueryStringCacheKeys "Use generic-lens or generic-optics with 'queryStringCacheKeys' instead." #-}

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field.
--
-- If you want to include headers in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send headers to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- A complex type that specifies the @Headers@ , if any, that you want CloudFront to forward to the origin for this cache behavior (whitelisted headers). For the headers that you specify, CloudFront also caches separate versions of a specified object that is based on the header values in viewer requests.
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/header-caching.html Caching Content Based on Request Headers> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fvHeaders :: Lens.Lens' ForwardedValues (Lude.Maybe Headers)
fvHeaders = Lens.lens (headers :: ForwardedValues -> Lude.Maybe Headers) (\s a -> s {headers = a} :: ForwardedValues)
{-# DEPRECATED fvHeaders "Use generic-lens or generic-optics with 'headers' instead." #-}

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field.
--
-- If you want to include query strings in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send query strings to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- Indicates whether you want CloudFront to forward query strings to the origin that is associated with this cache behavior and cache based on the query string parameters. CloudFront behavior depends on the value of @QueryString@ and on the values that you specify for @QueryStringCacheKeys@ , if any:
-- If you specify true for @QueryString@ and you don't specify any values for @QueryStringCacheKeys@ , CloudFront forwards all query string parameters to the origin and caches based on all query string parameters. Depending on how many query string parameters and values you have, this can adversely affect performance because CloudFront must forward more requests to the origin.
-- If you specify true for @QueryString@ and you specify one or more values for @QueryStringCacheKeys@ , CloudFront forwards all query string parameters to the origin, but it only caches based on the query string parameters that you specify.
-- If you specify false for @QueryString@ , CloudFront doesn't forward any query string parameters to the origin, and doesn't cache based on query string parameters.
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/QueryStringParameters.html Configuring CloudFront to Cache Based on Query String Parameters> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fvQueryString :: Lens.Lens' ForwardedValues Lude.Bool
fvQueryString = Lens.lens (queryString :: ForwardedValues -> Lude.Bool) (\s a -> s {queryString = a} :: ForwardedValues)
{-# DEPRECATED fvQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field.
--
-- If you want to include cookies in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send cookies to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html How CloudFront Forwards, Caches, and Logs Cookies> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'cookies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fvCookies :: Lens.Lens' ForwardedValues CookiePreference
fvCookies = Lens.lens (cookies :: ForwardedValues -> CookiePreference) (\s a -> s {cookies = a} :: ForwardedValues)
{-# DEPRECATED fvCookies "Use generic-lens or generic-optics with 'cookies' instead." #-}

instance Lude.FromXML ForwardedValues where
  parseXML x =
    ForwardedValues'
      Lude.<$> (x Lude..@? "QueryStringCacheKeys")
      Lude.<*> (x Lude..@? "Headers")
      Lude.<*> (x Lude..@ "QueryString")
      Lude.<*> (x Lude..@ "Cookies")

instance Lude.ToXML ForwardedValues where
  toXML ForwardedValues' {..} =
    Lude.mconcat
      [ "QueryStringCacheKeys" Lude.@= queryStringCacheKeys,
        "Headers" Lude.@= headers,
        "QueryString" Lude.@= queryString,
        "Cookies" Lude.@= cookies
      ]
