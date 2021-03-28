{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin
  ( ParametersInCacheKeyAndForwardedToOrigin (..)
  -- * Smart constructor
  , mkParametersInCacheKeyAndForwardedToOrigin
  -- * Lenses
  , pickaftoEnableAcceptEncodingGzip
  , pickaftoHeadersConfig
  , pickaftoCookiesConfig
  , pickaftoQueryStringsConfig
  , pickaftoEnableAcceptEncodingBrotli
  ) where

import qualified Network.AWS.CloudFront.Types.CachePolicyCookiesConfig as Types
import qualified Network.AWS.CloudFront.Types.CachePolicyHeadersConfig as Types
import qualified Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This object determines the values that CloudFront includes in the cache key. These values can include HTTP headers, cookies, and URL query strings. CloudFront uses the cache key to find an object in its cache that it can return to the viewer.
--
-- The headers, cookies, and query strings that are included in the cache key are automatically included in requests that CloudFront sends to the origin. CloudFront sends a request when it can’t find an object in its cache that matches the request’s cache key. If you want to send values to the origin but /not/ include them in the cache key, use @OriginRequestPolicy@ .
--
-- /See:/ 'mkParametersInCacheKeyAndForwardedToOrigin' smart constructor.
data ParametersInCacheKeyAndForwardedToOrigin = ParametersInCacheKeyAndForwardedToOrigin'
  { enableAcceptEncodingGzip :: Core.Bool
    -- ^ A flag that can affect whether the @Accept-Encoding@ HTTP header is included in the cache key and included in requests that CloudFront sends to the origin.
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
  , headersConfig :: Types.CachePolicyHeadersConfig
    -- ^ An object that determines whether any HTTP headers (and if so, which headers) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
  , cookiesConfig :: Types.CachePolicyCookiesConfig
    -- ^ An object that determines whether any cookies in viewer requests (and if so, which cookies) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
  , queryStringsConfig :: Types.CachePolicyQueryStringsConfig
    -- ^ An object that determines whether any URL query strings in viewer requests (and if so, which query strings) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
  , enableAcceptEncodingBrotli :: Core.Maybe Core.Bool
    -- ^ A flag that can affect whether the @Accept-Encoding@ HTTP header is included in the cache key and included in requests that CloudFront sends to the origin.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParametersInCacheKeyAndForwardedToOrigin' value with any optional fields omitted.
mkParametersInCacheKeyAndForwardedToOrigin
    :: Core.Bool -- ^ 'enableAcceptEncodingGzip'
    -> Types.CachePolicyHeadersConfig -- ^ 'headersConfig'
    -> Types.CachePolicyCookiesConfig -- ^ 'cookiesConfig'
    -> Types.CachePolicyQueryStringsConfig -- ^ 'queryStringsConfig'
    -> ParametersInCacheKeyAndForwardedToOrigin
mkParametersInCacheKeyAndForwardedToOrigin enableAcceptEncodingGzip
  headersConfig cookiesConfig queryStringsConfig
  = ParametersInCacheKeyAndForwardedToOrigin'{enableAcceptEncodingGzip,
                                              headersConfig, cookiesConfig, queryStringsConfig,
                                              enableAcceptEncodingBrotli = Core.Nothing}

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
pickaftoEnableAcceptEncodingGzip :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin Core.Bool
pickaftoEnableAcceptEncodingGzip = Lens.field @"enableAcceptEncodingGzip"
{-# INLINEABLE pickaftoEnableAcceptEncodingGzip #-}
{-# DEPRECATED enableAcceptEncodingGzip "Use generic-lens or generic-optics with 'enableAcceptEncodingGzip' instead"  #-}

-- | An object that determines whether any HTTP headers (and if so, which headers) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- /Note:/ Consider using 'headersConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pickaftoHeadersConfig :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin Types.CachePolicyHeadersConfig
pickaftoHeadersConfig = Lens.field @"headersConfig"
{-# INLINEABLE pickaftoHeadersConfig #-}
{-# DEPRECATED headersConfig "Use generic-lens or generic-optics with 'headersConfig' instead"  #-}

-- | An object that determines whether any cookies in viewer requests (and if so, which cookies) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- /Note:/ Consider using 'cookiesConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pickaftoCookiesConfig :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin Types.CachePolicyCookiesConfig
pickaftoCookiesConfig = Lens.field @"cookiesConfig"
{-# INLINEABLE pickaftoCookiesConfig #-}
{-# DEPRECATED cookiesConfig "Use generic-lens or generic-optics with 'cookiesConfig' instead"  #-}

-- | An object that determines whether any URL query strings in viewer requests (and if so, which query strings) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- /Note:/ Consider using 'queryStringsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pickaftoQueryStringsConfig :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin Types.CachePolicyQueryStringsConfig
pickaftoQueryStringsConfig = Lens.field @"queryStringsConfig"
{-# INLINEABLE pickaftoQueryStringsConfig #-}
{-# DEPRECATED queryStringsConfig "Use generic-lens or generic-optics with 'queryStringsConfig' instead"  #-}

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
pickaftoEnableAcceptEncodingBrotli :: Lens.Lens' ParametersInCacheKeyAndForwardedToOrigin (Core.Maybe Core.Bool)
pickaftoEnableAcceptEncodingBrotli = Lens.field @"enableAcceptEncodingBrotli"
{-# INLINEABLE pickaftoEnableAcceptEncodingBrotli #-}
{-# DEPRECATED enableAcceptEncodingBrotli "Use generic-lens or generic-optics with 'enableAcceptEncodingBrotli' instead"  #-}

instance Core.ToXML ParametersInCacheKeyAndForwardedToOrigin where
        toXML ParametersInCacheKeyAndForwardedToOrigin{..}
          = Core.toXMLElement "EnableAcceptEncodingGzip"
              enableAcceptEncodingGzip
              Core.<> Core.toXMLElement "HeadersConfig" headersConfig
              Core.<> Core.toXMLElement "CookiesConfig" cookiesConfig
              Core.<> Core.toXMLElement "QueryStringsConfig" queryStringsConfig
              Core.<>
              Core.maybe Core.mempty
                (Core.toXMLElement "EnableAcceptEncodingBrotli")
                enableAcceptEncodingBrotli

instance Core.FromXML ParametersInCacheKeyAndForwardedToOrigin
         where
        parseXML x
          = ParametersInCacheKeyAndForwardedToOrigin' Core.<$>
              (x Core..@ "EnableAcceptEncodingGzip") Core.<*>
                x Core..@ "HeadersConfig"
                Core.<*> x Core..@ "CookiesConfig"
                Core.<*> x Core..@ "QueryStringsConfig"
                Core.<*> x Core..@? "EnableAcceptEncodingBrotli"
