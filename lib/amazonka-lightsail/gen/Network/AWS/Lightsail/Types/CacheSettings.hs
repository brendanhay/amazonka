{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CacheSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.CacheSettings
  ( CacheSettings (..)
  -- * Smart constructor
  , mkCacheSettings
  -- * Lenses
  , csAllowedHTTPMethods
  , csCachedHTTPMethods
  , csDefaultTTL
  , csForwardedCookies
  , csForwardedHeaders
  , csForwardedQueryStrings
  , csMaximumTTL
  , csMinimumTTL
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.CookieObject as Types
import qualified Network.AWS.Lightsail.Types.HeaderObject as Types
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Lightsail.Types.QueryStringObject as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the cache settings of an Amazon Lightsail content delivery network (CDN) distribution.
--
-- These settings apply only to your distribution's @cacheBehaviors@ (including the @defaultCacheBehavior@ ) that have a @behavior@ of @cache@ .
--
-- /See:/ 'mkCacheSettings' smart constructor.
data CacheSettings = CacheSettings'
  { allowedHTTPMethods :: Core.Maybe Types.NonEmptyString
    -- ^ The HTTP methods that are processed and forwarded to the distribution's origin.
--
-- You can specify the following options:
--
--     * @GET,HEAD@ - The distribution forwards the @GET@ and @HEAD@ methods.
--
--
--     * @GET,HEAD,OPTIONS@ - The distribution forwards the @GET@ , @HEAD@ , and @OPTIONS@ methods.
--
--
--     * @GET,HEAD,OPTIONS,PUT,PATCH,POST,DELETE@ - The distribution forwards the @GET@ , @HEAD@ , @OPTIONS@ , @PUT@ , @PATCH@ , @POST@ , and @DELETE@ methods.
--
--
-- If you specify the third option, you might need to restrict access to your distribution's origin so users can't perform operations that you don't want them to. For example, you might not want users to have permission to delete objects from your origin.
  , cachedHTTPMethods :: Core.Maybe Types.NonEmptyString
    -- ^ The HTTP method responses that are cached by your distribution.
--
-- You can specify the following options:
--
--     * @GET,HEAD@ - The distribution caches responses to the @GET@ and @HEAD@ methods.
--
--
--     * @GET,HEAD,OPTIONS@ - The distribution caches responses to the @GET@ , @HEAD@ , and @OPTIONS@ methods.
--
--
  , defaultTTL :: Core.Maybe Core.Integer
    -- ^ The default amount of time that objects stay in the distribution's cache before the distribution forwards another request to the origin to determine whether the content has been updated.
  , forwardedCookies :: Core.Maybe Types.CookieObject
    -- ^ An object that describes the cookies that are forwarded to the origin. Your content is cached based on the cookies that are forwarded.
  , forwardedHeaders :: Core.Maybe Types.HeaderObject
    -- ^ An object that describes the headers that are forwarded to the origin. Your content is cached based on the headers that are forwarded.
  , forwardedQueryStrings :: Core.Maybe Types.QueryStringObject
    -- ^ An object that describes the query strings that are forwarded to the origin. Your content is cached based on the query strings that are forwarded.
  , maximumTTL :: Core.Maybe Core.Integer
    -- ^ The maximum amount of time that objects stay in the distribution's cache before the distribution forwards another request to the origin to determine whether the object has been updated.
--
-- The value specified applies only when the origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects.
  , minimumTTL :: Core.Maybe Core.Integer
    -- ^ The minimum amount of time that objects stay in the distribution's cache before the distribution forwards another request to the origin to determine whether the object has been updated.
--
-- A value of @0@ must be specified for @minimumTTL@ if the distribution is configured to forward all headers to the origin.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CacheSettings' value with any optional fields omitted.
mkCacheSettings
    :: CacheSettings
mkCacheSettings
  = CacheSettings'{allowedHTTPMethods = Core.Nothing,
                   cachedHTTPMethods = Core.Nothing, defaultTTL = Core.Nothing,
                   forwardedCookies = Core.Nothing, forwardedHeaders = Core.Nothing,
                   forwardedQueryStrings = Core.Nothing, maximumTTL = Core.Nothing,
                   minimumTTL = Core.Nothing}

-- | The HTTP methods that are processed and forwarded to the distribution's origin.
--
-- You can specify the following options:
--
--     * @GET,HEAD@ - The distribution forwards the @GET@ and @HEAD@ methods.
--
--
--     * @GET,HEAD,OPTIONS@ - The distribution forwards the @GET@ , @HEAD@ , and @OPTIONS@ methods.
--
--
--     * @GET,HEAD,OPTIONS,PUT,PATCH,POST,DELETE@ - The distribution forwards the @GET@ , @HEAD@ , @OPTIONS@ , @PUT@ , @PATCH@ , @POST@ , and @DELETE@ methods.
--
--
-- If you specify the third option, you might need to restrict access to your distribution's origin so users can't perform operations that you don't want them to. For example, you might not want users to have permission to delete objects from your origin.
--
-- /Note:/ Consider using 'allowedHTTPMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAllowedHTTPMethods :: Lens.Lens' CacheSettings (Core.Maybe Types.NonEmptyString)
csAllowedHTTPMethods = Lens.field @"allowedHTTPMethods"
{-# INLINEABLE csAllowedHTTPMethods #-}
{-# DEPRECATED allowedHTTPMethods "Use generic-lens or generic-optics with 'allowedHTTPMethods' instead"  #-}

-- | The HTTP method responses that are cached by your distribution.
--
-- You can specify the following options:
--
--     * @GET,HEAD@ - The distribution caches responses to the @GET@ and @HEAD@ methods.
--
--
--     * @GET,HEAD,OPTIONS@ - The distribution caches responses to the @GET@ , @HEAD@ , and @OPTIONS@ methods.
--
--
--
-- /Note:/ Consider using 'cachedHTTPMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCachedHTTPMethods :: Lens.Lens' CacheSettings (Core.Maybe Types.NonEmptyString)
csCachedHTTPMethods = Lens.field @"cachedHTTPMethods"
{-# INLINEABLE csCachedHTTPMethods #-}
{-# DEPRECATED cachedHTTPMethods "Use generic-lens or generic-optics with 'cachedHTTPMethods' instead"  #-}

-- | The default amount of time that objects stay in the distribution's cache before the distribution forwards another request to the origin to determine whether the content has been updated.
--
-- /Note:/ Consider using 'defaultTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultTTL :: Lens.Lens' CacheSettings (Core.Maybe Core.Integer)
csDefaultTTL = Lens.field @"defaultTTL"
{-# INLINEABLE csDefaultTTL #-}
{-# DEPRECATED defaultTTL "Use generic-lens or generic-optics with 'defaultTTL' instead"  #-}

-- | An object that describes the cookies that are forwarded to the origin. Your content is cached based on the cookies that are forwarded.
--
-- /Note:/ Consider using 'forwardedCookies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csForwardedCookies :: Lens.Lens' CacheSettings (Core.Maybe Types.CookieObject)
csForwardedCookies = Lens.field @"forwardedCookies"
{-# INLINEABLE csForwardedCookies #-}
{-# DEPRECATED forwardedCookies "Use generic-lens or generic-optics with 'forwardedCookies' instead"  #-}

-- | An object that describes the headers that are forwarded to the origin. Your content is cached based on the headers that are forwarded.
--
-- /Note:/ Consider using 'forwardedHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csForwardedHeaders :: Lens.Lens' CacheSettings (Core.Maybe Types.HeaderObject)
csForwardedHeaders = Lens.field @"forwardedHeaders"
{-# INLINEABLE csForwardedHeaders #-}
{-# DEPRECATED forwardedHeaders "Use generic-lens or generic-optics with 'forwardedHeaders' instead"  #-}

-- | An object that describes the query strings that are forwarded to the origin. Your content is cached based on the query strings that are forwarded.
--
-- /Note:/ Consider using 'forwardedQueryStrings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csForwardedQueryStrings :: Lens.Lens' CacheSettings (Core.Maybe Types.QueryStringObject)
csForwardedQueryStrings = Lens.field @"forwardedQueryStrings"
{-# INLINEABLE csForwardedQueryStrings #-}
{-# DEPRECATED forwardedQueryStrings "Use generic-lens or generic-optics with 'forwardedQueryStrings' instead"  #-}

-- | The maximum amount of time that objects stay in the distribution's cache before the distribution forwards another request to the origin to determine whether the object has been updated.
--
-- The value specified applies only when the origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects.
--
-- /Note:/ Consider using 'maximumTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMaximumTTL :: Lens.Lens' CacheSettings (Core.Maybe Core.Integer)
csMaximumTTL = Lens.field @"maximumTTL"
{-# INLINEABLE csMaximumTTL #-}
{-# DEPRECATED maximumTTL "Use generic-lens or generic-optics with 'maximumTTL' instead"  #-}

-- | The minimum amount of time that objects stay in the distribution's cache before the distribution forwards another request to the origin to determine whether the object has been updated.
--
-- A value of @0@ must be specified for @minimumTTL@ if the distribution is configured to forward all headers to the origin.
--
-- /Note:/ Consider using 'minimumTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csMinimumTTL :: Lens.Lens' CacheSettings (Core.Maybe Core.Integer)
csMinimumTTL = Lens.field @"minimumTTL"
{-# INLINEABLE csMinimumTTL #-}
{-# DEPRECATED minimumTTL "Use generic-lens or generic-optics with 'minimumTTL' instead"  #-}

instance Core.FromJSON CacheSettings where
        toJSON CacheSettings{..}
          = Core.object
              (Core.catMaybes
                 [("allowedHTTPMethods" Core..=) Core.<$> allowedHTTPMethods,
                  ("cachedHTTPMethods" Core..=) Core.<$> cachedHTTPMethods,
                  ("defaultTTL" Core..=) Core.<$> defaultTTL,
                  ("forwardedCookies" Core..=) Core.<$> forwardedCookies,
                  ("forwardedHeaders" Core..=) Core.<$> forwardedHeaders,
                  ("forwardedQueryStrings" Core..=) Core.<$> forwardedQueryStrings,
                  ("maximumTTL" Core..=) Core.<$> maximumTTL,
                  ("minimumTTL" Core..=) Core.<$> minimumTTL])

instance Core.FromJSON CacheSettings where
        parseJSON
          = Core.withObject "CacheSettings" Core.$
              \ x ->
                CacheSettings' Core.<$>
                  (x Core..:? "allowedHTTPMethods") Core.<*>
                    x Core..:? "cachedHTTPMethods"
                    Core.<*> x Core..:? "defaultTTL"
                    Core.<*> x Core..:? "forwardedCookies"
                    Core.<*> x Core..:? "forwardedHeaders"
                    Core.<*> x Core..:? "forwardedQueryStrings"
                    Core.<*> x Core..:? "maximumTTL"
                    Core.<*> x Core..:? "minimumTTL"
