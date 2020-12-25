{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyCookiesConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyCookiesConfig
  ( CachePolicyCookiesConfig (..),

    -- * Smart constructor
    mkCachePolicyCookiesConfig,

    -- * Lenses
    cpccCookieBehavior,
    cpccCookies,
  )
where

import qualified Network.AWS.CloudFront.Types.CachePolicyCookieBehavior as Types
import qualified Network.AWS.CloudFront.Types.CookieNames as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that determines whether any cookies in viewer requests (and if so, which cookies) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- /See:/ 'mkCachePolicyCookiesConfig' smart constructor.
data CachePolicyCookiesConfig = CachePolicyCookiesConfig'
  { -- | Determines whether any cookies in viewer requests are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:
    --
    --
    --     * @none@ – Cookies in viewer requests are not included in the cache key and are not automatically included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any cookies that are listed in an @OriginRequestPolicy@ /are/ included in origin requests.
    --
    --
    --     * @whitelist@ – The cookies in viewer requests that are listed in the @CookieNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.
    --
    --
    --     * @allExcept@ – All cookies in viewer requests that are /__not__ / listed in the @CookieNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.
    --
    --
    --     * @all@ – All cookies in viewer requests are included in the cache key and are automatically included in requests that CloudFront sends to the origin.
    cookieBehavior :: Types.CachePolicyCookieBehavior,
    cookies :: Core.Maybe Types.CookieNames
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CachePolicyCookiesConfig' value with any optional fields omitted.
mkCachePolicyCookiesConfig ::
  -- | 'cookieBehavior'
  Types.CachePolicyCookieBehavior ->
  CachePolicyCookiesConfig
mkCachePolicyCookiesConfig cookieBehavior =
  CachePolicyCookiesConfig' {cookieBehavior, cookies = Core.Nothing}

-- | Determines whether any cookies in viewer requests are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:
--
--
--     * @none@ – Cookies in viewer requests are not included in the cache key and are not automatically included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any cookies that are listed in an @OriginRequestPolicy@ /are/ included in origin requests.
--
--
--     * @whitelist@ – The cookies in viewer requests that are listed in the @CookieNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
--
--     * @allExcept@ – All cookies in viewer requests that are /__not__ / listed in the @CookieNames@ type are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
--
--     * @all@ – All cookies in viewer requests are included in the cache key and are automatically included in requests that CloudFront sends to the origin.
--
--
--
-- /Note:/ Consider using 'cookieBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpccCookieBehavior :: Lens.Lens' CachePolicyCookiesConfig Types.CachePolicyCookieBehavior
cpccCookieBehavior = Lens.field @"cookieBehavior"
{-# DEPRECATED cpccCookieBehavior "Use generic-lens or generic-optics with 'cookieBehavior' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cookies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpccCookies :: Lens.Lens' CachePolicyCookiesConfig (Core.Maybe Types.CookieNames)
cpccCookies = Lens.field @"cookies"
{-# DEPRECATED cpccCookies "Use generic-lens or generic-optics with 'cookies' instead." #-}

instance Core.ToXML CachePolicyCookiesConfig where
  toXML CachePolicyCookiesConfig {..} =
    Core.toXMLNode "CookieBehavior" cookieBehavior
      Core.<> Core.toXMLNode "Cookies" Core.<$> cookies

instance Core.FromXML CachePolicyCookiesConfig where
  parseXML x =
    CachePolicyCookiesConfig'
      Core.<$> (x Core..@ "CookieBehavior") Core.<*> (x Core..@? "Cookies")
