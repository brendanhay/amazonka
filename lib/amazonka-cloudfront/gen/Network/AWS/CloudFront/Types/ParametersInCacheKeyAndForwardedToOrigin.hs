{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin
  ( ParametersInCacheKeyAndForwardedToOrigin (..),

    -- * Smart constructor
    mkParametersInCacheKeyAndForwardedToOrigin,

    -- * Lenses
    pickaftoEnableAcceptEncodingBrotli,
    pickaftoEnableAcceptEncodingGzip,
    pickaftoHeadersConfig,
    pickaftoCookiesConfig,
    pickaftoQueryStringsConfig,
  )
where

import Network.AWS.CloudFront.Types.CachePolicyCookiesConfig
import Network.AWS.CloudFront.Types.CachePolicyHeadersConfig
import Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This object determines the values that CloudFront includes in the cache key. These values can include HTTP headers, cookies, and URL query strings. CloudFront uses the cache key to find an object in its cache that it can return to the viewer.
--
-- The headers, cookies, and query strings that are included in the cache key are automatically included in requests that CloudFront sends to the origin. CloudFront sends a request when it can’t find an object in its cache that matches the request’s cache key. If you want to send values to the origin but /not/ include them in the cache key, use @OriginRequestPolicy@ .
--
-- /See:/ 'mkParametersInCacheKeyAndForwardedToOrigin' smart constructor.
data ParametersInCacheKeyAndForwardedToOrigin = ParametersInCacheKeyAndForwardedToOrigin'
  { enableAcceptEncodingBrotli ::
      Lude.Maybe
        Lude.Bool,
    enableAcceptEncodingGzip ::
      Lude.Bool,
    headersConfig ::
      CachePolicyHeadersConfig,
    cookiesConfig ::
      CachePolicyCookiesConfig,
    queryStringsConfig ::
      CachePolicyQueryStringsConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParametersInCacheKeyAndForwardedToOrigin' with the minimum fields required to make a request.
--
-- * 'cookiesConfig' - An object that determines whether any cookies in viewer requests (and if so, which cookies) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
-- * 'enableAcceptEncodingBrotli' - A flag that can affect whether the @Accept-Encoding@ HTTP header is included in the cache key and included in requests that CloudFront sends to the origin.
--
-- This field is related to the @EnableAcceptEncodingGzip@ field. If one or both of these fields is @true@ /and/ the viewer request includes the @Accept-Encoding@ header, then CloudFront does the following:
--
--     * Normalizes the value of the viewer’s @Accept-Encoding@ header
--
--
--     * Includes the normalized header in the cache key
--
--
--     * Includes the normalized header in the request to the origin, if a request is necessary
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support> in the /Amazon CloudFront Developer Guide/ .
-- If you set this value to @true@ , and this cache behavior also has an origin request policy attached, do not include the @Accept-Encoding@ header in the origin request policy. CloudFront always includes the @Accept-Encoding@ header in origin requests when the value of this field is @true@ , so including this header in an origin request policy has no effect.
-- If both of these fields are @false@ , then CloudFront treats the @Accept-Encoding@ header the same as any other HTTP header in the viewer request. By default, it’s not included in the cache key and it’s not included in origin requests. In this case, you can manually add @Accept-Encoding@ to the headers whitelist like any other HTTP header.
-- * 'enableAcceptEncodingGzip' - A flag that can affect whether the @Accept-Encoding@ HTTP header is included in the cache key and included in requests that CloudFront sends to the origin.
--
-- This field is related to the @EnableAcceptEncodingBrotli@ field. If one or both of these fields is @true@ /and/ the viewer request includes the @Accept-Encoding@ header, then CloudFront does the following:
--
--     * Normalizes the value of the viewer’s @Accept-Encoding@ header
--
--
--     * Includes the normalized header in the cache key
--
--
--     * Includes the normalized header in the request to the origin, if a request is necessary
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support> in the /Amazon CloudFront Developer Guide/ .
-- If you set this value to @true@ , and this cache behavior also has an origin request policy attached, do not include the @Accept-Encoding@ header in the origin request policy. CloudFront always includes the @Accept-Encoding@ header in origin requests when the value of this field is @true@ , so including this header in an origin request policy has no effect.
-- If both of these fields are @false@ , then CloudFront treats the @Accept-Encoding@ header the same as any other HTTP header in the viewer request. By default, it’s not included in the cache key and it’s not included in origin requests. In this case, you can manually add @Accept-Encoding@ to the headers whitelist like any other HTTP header.
-- * 'headersConfig' - An object that determines whether any HTTP headers (and if so, which headers) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
-- * 'queryStringsConfig' - An object that determines whether any URL query strings in viewer requests (and if so, which query strings) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
mkParametersInCacheKeyAndForwardedToOrigin ::
  -- | 'enableAcceptEncodingGzip'
  Lude.Bool ->
  -- | 'headersConfig'
  CachePolicyHeadersConfig ->
  -- | 'cookiesConfig'
  CachePolicyCookiesConfig ->
  -- | 'queryStringsConfig'
  CachePolicyQueryStringsConfig ->
  ParametersInCacheKeyAndForwardedToOrigin
mkParametersInCacheKeyAndForwardedToOrigin
  pEnableAcceptEncodingGzip_
  pHeadersConfig_
  pCookiesConfig_
  pQueryStringsConfig_ =
    ParametersInCacheKeyAndForwardedToOrigin'
      { enableAcceptEncodingBrotli =
          Lude.Nothing,
        enableAcceptEncodingGzip = pEnableAcceptEncodingGzip_,
        headersConfig = pHeadersConfig_,
        cookiesConfig = pCookiesConfig_,
        queryStringsConfig = pQueryStringsConfig_
      }

-- | A flag that can affect whether the @Accept-Encoding@ HTTP header is included in the cache key and included in requests that CloudFront sends to the origin.
--
-- This field is related to the @EnableAcceptEncodingGzip@ field. If one or both of these fields is @true@ /and/ the viewer request includes the @Accept-Encoding@ header, then CloudFront does the following:
--
--     * Normalizes the value of the viewer’s @Accept-Encoding@ header
--
--
--     * Includes the normalized header in the cache key
--
--
--     * Includes the normalized header in the request to the origin, if a request is necessary
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support> in the /Amazon CloudFront Developer Guide/ .
-- If you set this value to @true@ , and this cache behavior also has an origin request policy attached, do not include the @Accept-Encoding@ header in the origin request policy. CloudFront always includes the @Accept-Encoding@ header in origin requests when the value of this field is @true@ , so including this header in an origin request policy has no effect.
-- If both of these fields are @false@ , then CloudFront treats the @Accept-Encoding@ header the same as any other HTTP header in the viewer request. By default, it’s not included in the cache key and it’s not included in origin requests. In this case, you can manually add @Accept-Encoding@ to the headers whitelist like any other HTTP header.
--
-- /Note:/ Consider using 'enableAcceptEncodingBrotli' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pickaftoEnableAcceptEncodingBrotli :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin (Lude.Maybe Lude.Bool)
pickaftoEnableAcceptEncodingBrotli = Lens.lens (enableAcceptEncodingBrotli :: ParametersInCacheKeyAndForwardedToOrigin -> Lude.Maybe Lude.Bool) (\s a -> s {enableAcceptEncodingBrotli = a} :: ParametersInCacheKeyAndForwardedToOrigin)
{-# DEPRECATED pickaftoEnableAcceptEncodingBrotli "Use generic-lens or generic-optics with 'enableAcceptEncodingBrotli' instead." #-}

-- | A flag that can affect whether the @Accept-Encoding@ HTTP header is included in the cache key and included in requests that CloudFront sends to the origin.
--
-- This field is related to the @EnableAcceptEncodingBrotli@ field. If one or both of these fields is @true@ /and/ the viewer request includes the @Accept-Encoding@ header, then CloudFront does the following:
--
--     * Normalizes the value of the viewer’s @Accept-Encoding@ header
--
--
--     * Includes the normalized header in the cache key
--
--
--     * Includes the normalized header in the request to the origin, if a request is necessary
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-policy-compressed-objects Compression support> in the /Amazon CloudFront Developer Guide/ .
-- If you set this value to @true@ , and this cache behavior also has an origin request policy attached, do not include the @Accept-Encoding@ header in the origin request policy. CloudFront always includes the @Accept-Encoding@ header in origin requests when the value of this field is @true@ , so including this header in an origin request policy has no effect.
-- If both of these fields are @false@ , then CloudFront treats the @Accept-Encoding@ header the same as any other HTTP header in the viewer request. By default, it’s not included in the cache key and it’s not included in origin requests. In this case, you can manually add @Accept-Encoding@ to the headers whitelist like any other HTTP header.
--
-- /Note:/ Consider using 'enableAcceptEncodingGzip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pickaftoEnableAcceptEncodingGzip :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin Lude.Bool
pickaftoEnableAcceptEncodingGzip = Lens.lens (enableAcceptEncodingGzip :: ParametersInCacheKeyAndForwardedToOrigin -> Lude.Bool) (\s a -> s {enableAcceptEncodingGzip = a} :: ParametersInCacheKeyAndForwardedToOrigin)
{-# DEPRECATED pickaftoEnableAcceptEncodingGzip "Use generic-lens or generic-optics with 'enableAcceptEncodingGzip' instead." #-}

-- | An object that determines whether any HTTP headers (and if so, which headers) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- /Note:/ Consider using 'headersConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pickaftoHeadersConfig :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin CachePolicyHeadersConfig
pickaftoHeadersConfig = Lens.lens (headersConfig :: ParametersInCacheKeyAndForwardedToOrigin -> CachePolicyHeadersConfig) (\s a -> s {headersConfig = a} :: ParametersInCacheKeyAndForwardedToOrigin)
{-# DEPRECATED pickaftoHeadersConfig "Use generic-lens or generic-optics with 'headersConfig' instead." #-}

-- | An object that determines whether any cookies in viewer requests (and if so, which cookies) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- /Note:/ Consider using 'cookiesConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pickaftoCookiesConfig :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin CachePolicyCookiesConfig
pickaftoCookiesConfig = Lens.lens (cookiesConfig :: ParametersInCacheKeyAndForwardedToOrigin -> CachePolicyCookiesConfig) (\s a -> s {cookiesConfig = a} :: ParametersInCacheKeyAndForwardedToOrigin)
{-# DEPRECATED pickaftoCookiesConfig "Use generic-lens or generic-optics with 'cookiesConfig' instead." #-}

-- | An object that determines whether any URL query strings in viewer requests (and if so, which query strings) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- /Note:/ Consider using 'queryStringsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pickaftoQueryStringsConfig :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin CachePolicyQueryStringsConfig
pickaftoQueryStringsConfig = Lens.lens (queryStringsConfig :: ParametersInCacheKeyAndForwardedToOrigin -> CachePolicyQueryStringsConfig) (\s a -> s {queryStringsConfig = a} :: ParametersInCacheKeyAndForwardedToOrigin)
{-# DEPRECATED pickaftoQueryStringsConfig "Use generic-lens or generic-optics with 'queryStringsConfig' instead." #-}

instance Lude.FromXML ParametersInCacheKeyAndForwardedToOrigin where
  parseXML x =
    ParametersInCacheKeyAndForwardedToOrigin'
      Lude.<$> (x Lude..@? "EnableAcceptEncodingBrotli")
      Lude.<*> (x Lude..@ "EnableAcceptEncodingGzip")
      Lude.<*> (x Lude..@ "HeadersConfig")
      Lude.<*> (x Lude..@ "CookiesConfig")
      Lude.<*> (x Lude..@ "QueryStringsConfig")

instance Lude.ToXML ParametersInCacheKeyAndForwardedToOrigin where
  toXML ParametersInCacheKeyAndForwardedToOrigin' {..} =
    Lude.mconcat
      [ "EnableAcceptEncodingBrotli" Lude.@= enableAcceptEncodingBrotli,
        "EnableAcceptEncodingGzip" Lude.@= enableAcceptEncodingGzip,
        "HeadersConfig" Lude.@= headersConfig,
        "CookiesConfig" Lude.@= cookiesConfig,
        "QueryStringsConfig" Lude.@= queryStringsConfig
      ]
