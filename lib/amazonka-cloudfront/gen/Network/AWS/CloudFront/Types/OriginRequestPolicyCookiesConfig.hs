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
    orpccCookies,
    orpccCookieBehavior,
  )
where

import Network.AWS.CloudFront.Types.CookieNames
import Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that determines whether any cookies in viewer requests (and if so, which cookies) are included in requests that CloudFront sends to the origin.
--
-- /See:/ 'mkOriginRequestPolicyCookiesConfig' smart constructor.
data OriginRequestPolicyCookiesConfig = OriginRequestPolicyCookiesConfig'
  { cookies ::
      Lude.Maybe CookieNames,
    cookieBehavior ::
      OriginRequestPolicyCookieBehavior
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginRequestPolicyCookiesConfig' with the minimum fields required to make a request.
--
-- * 'cookieBehavior' - Determines whether cookies in viewer requests are included in requests that CloudFront sends to the origin. Valid values are:
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
-- * 'cookies' - Undocumented field.
mkOriginRequestPolicyCookiesConfig ::
  -- | 'cookieBehavior'
  OriginRequestPolicyCookieBehavior ->
  OriginRequestPolicyCookiesConfig
mkOriginRequestPolicyCookiesConfig pCookieBehavior_ =
  OriginRequestPolicyCookiesConfig'
    { cookies = Lude.Nothing,
      cookieBehavior = pCookieBehavior_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cookies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orpccCookies :: Lens.Lens' OriginRequestPolicyCookiesConfig (Lude.Maybe CookieNames)
orpccCookies = Lens.lens (cookies :: OriginRequestPolicyCookiesConfig -> Lude.Maybe CookieNames) (\s a -> s {cookies = a} :: OriginRequestPolicyCookiesConfig)
{-# DEPRECATED orpccCookies "Use generic-lens or generic-optics with 'cookies' instead." #-}

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
orpccCookieBehavior :: Lens.Lens' OriginRequestPolicyCookiesConfig OriginRequestPolicyCookieBehavior
orpccCookieBehavior = Lens.lens (cookieBehavior :: OriginRequestPolicyCookiesConfig -> OriginRequestPolicyCookieBehavior) (\s a -> s {cookieBehavior = a} :: OriginRequestPolicyCookiesConfig)
{-# DEPRECATED orpccCookieBehavior "Use generic-lens or generic-optics with 'cookieBehavior' instead." #-}

instance Lude.FromXML OriginRequestPolicyCookiesConfig where
  parseXML x =
    OriginRequestPolicyCookiesConfig'
      Lude.<$> (x Lude..@? "Cookies") Lude.<*> (x Lude..@ "CookieBehavior")

instance Lude.ToXML OriginRequestPolicyCookiesConfig where
  toXML OriginRequestPolicyCookiesConfig' {..} =
    Lude.mconcat
      [ "Cookies" Lude.@= cookies,
        "CookieBehavior" Lude.@= cookieBehavior
      ]
