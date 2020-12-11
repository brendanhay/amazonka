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
    cpccCookies,
    cpccCookieBehavior,
  )
where

import Network.AWS.CloudFront.Types.CachePolicyCookieBehavior
import Network.AWS.CloudFront.Types.CookieNames
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that determines whether any cookies in viewer requests (and if so, which cookies) are included in the cache key and automatically included in requests that CloudFront sends to the origin.
--
-- /See:/ 'mkCachePolicyCookiesConfig' smart constructor.
data CachePolicyCookiesConfig = CachePolicyCookiesConfig'
  { cookies ::
      Lude.Maybe CookieNames,
    cookieBehavior ::
      CachePolicyCookieBehavior
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CachePolicyCookiesConfig' with the minimum fields required to make a request.
--
-- * 'cookieBehavior' - Determines whether any cookies in viewer requests are included in the cache key and automatically included in requests that CloudFront sends to the origin. Valid values are:
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
-- * 'cookies' - Undocumented field.
mkCachePolicyCookiesConfig ::
  -- | 'cookieBehavior'
  CachePolicyCookieBehavior ->
  CachePolicyCookiesConfig
mkCachePolicyCookiesConfig pCookieBehavior_ =
  CachePolicyCookiesConfig'
    { cookies = Lude.Nothing,
      cookieBehavior = pCookieBehavior_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cookies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpccCookies :: Lens.Lens' CachePolicyCookiesConfig (Lude.Maybe CookieNames)
cpccCookies = Lens.lens (cookies :: CachePolicyCookiesConfig -> Lude.Maybe CookieNames) (\s a -> s {cookies = a} :: CachePolicyCookiesConfig)
{-# DEPRECATED cpccCookies "Use generic-lens or generic-optics with 'cookies' instead." #-}

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
cpccCookieBehavior :: Lens.Lens' CachePolicyCookiesConfig CachePolicyCookieBehavior
cpccCookieBehavior = Lens.lens (cookieBehavior :: CachePolicyCookiesConfig -> CachePolicyCookieBehavior) (\s a -> s {cookieBehavior = a} :: CachePolicyCookiesConfig)
{-# DEPRECATED cpccCookieBehavior "Use generic-lens or generic-optics with 'cookieBehavior' instead." #-}

instance Lude.FromXML CachePolicyCookiesConfig where
  parseXML x =
    CachePolicyCookiesConfig'
      Lude.<$> (x Lude..@? "Cookies") Lude.<*> (x Lude..@ "CookieBehavior")

instance Lude.ToXML CachePolicyCookiesConfig where
  toXML CachePolicyCookiesConfig' {..} =
    Lude.mconcat
      [ "Cookies" Lude.@= cookies,
        "CookieBehavior" Lude.@= cookieBehavior
      ]
