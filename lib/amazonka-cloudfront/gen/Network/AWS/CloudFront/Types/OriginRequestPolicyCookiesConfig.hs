{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyCookiesConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyCookiesConfig
  ( OriginRequestPolicyCookiesConfig (..),

    -- * Smart constructor
    mkOriginRequestPolicyCookiesConfig,

    -- * Lenses
    orpccCookieBehavior,
    orpccCookies,
  )
where

import qualified Network.AWS.CloudFront.Types.CookieNames as Types
import qualified Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that determines whether any cookies in viewer requests (and if so, which cookies) are included in requests that CloudFront sends to the origin.
--
-- /See:/ 'mkOriginRequestPolicyCookiesConfig' smart constructor.
data OriginRequestPolicyCookiesConfig = OriginRequestPolicyCookiesConfig'
  { -- | Determines whether cookies in viewer requests are included in requests that CloudFront sends to the origin. Valid values are:
    --
    --
    --     * @none@ – Cookies in viewer requests are not included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any cookies that are listed in a @CachePolicy@ /are/ included in origin requests.
    --
    --
    --     * @whitelist@ – The cookies in viewer requests that are listed in the @CookieNames@ type are included in requests that CloudFront sends to the origin.
    --
    --
    --     * @all@ – All cookies in viewer requests are included in requests that CloudFront sends to the origin.
    cookieBehavior :: Types.OriginRequestPolicyCookieBehavior,
    cookies :: Core.Maybe Types.CookieNames
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OriginRequestPolicyCookiesConfig' value with any optional fields omitted.
mkOriginRequestPolicyCookiesConfig ::
  -- | 'cookieBehavior'
  Types.OriginRequestPolicyCookieBehavior ->
  OriginRequestPolicyCookiesConfig
mkOriginRequestPolicyCookiesConfig cookieBehavior =
  OriginRequestPolicyCookiesConfig'
    { cookieBehavior,
      cookies = Core.Nothing
    }

-- | Determines whether cookies in viewer requests are included in requests that CloudFront sends to the origin. Valid values are:
--
--
--     * @none@ – Cookies in viewer requests are not included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any cookies that are listed in a @CachePolicy@ /are/ included in origin requests.
--
--
--     * @whitelist@ – The cookies in viewer requests that are listed in the @CookieNames@ type are included in requests that CloudFront sends to the origin.
--
--
--     * @all@ – All cookies in viewer requests are included in requests that CloudFront sends to the origin.
--
--
--
-- /Note:/ Consider using 'cookieBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpccCookieBehavior :: Lens.Lens' OriginRequestPolicyCookiesConfig Types.OriginRequestPolicyCookieBehavior
orpccCookieBehavior = Lens.field @"cookieBehavior"
{-# DEPRECATED orpccCookieBehavior "Use generic-lens or generic-optics with 'cookieBehavior' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cookies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpccCookies :: Lens.Lens' OriginRequestPolicyCookiesConfig (Core.Maybe Types.CookieNames)
orpccCookies = Lens.field @"cookies"
{-# DEPRECATED orpccCookies "Use generic-lens or generic-optics with 'cookies' instead." #-}

instance Core.ToXML OriginRequestPolicyCookiesConfig where
  toXML OriginRequestPolicyCookiesConfig {..} =
    Core.toXMLNode "CookieBehavior" cookieBehavior
      Core.<> Core.toXMLNode "Cookies" Core.<$> cookies

instance Core.FromXML OriginRequestPolicyCookiesConfig where
  parseXML x =
    OriginRequestPolicyCookiesConfig'
      Core.<$> (x Core..@ "CookieBehavior") Core.<*> (x Core..@? "Cookies")
