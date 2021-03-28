{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CookieObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.CookieObject
  ( CookieObject (..)
  -- * Smart constructor
  , mkCookieObject
  -- * Lenses
  , coCookiesAllowList
  , coOption
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.ForwardValues as Types
import qualified Network.AWS.Prelude as Core

-- | Describes whether an Amazon Lightsail content delivery network (CDN) distribution forwards cookies to the origin and, if so, which ones.
--
-- For the cookies that you specify, your distribution caches separate versions of the specified content based on the cookie values in viewer requests.
--
-- /See:/ 'mkCookieObject' smart constructor.
data CookieObject = CookieObject'
  { cookiesAllowList :: Core.Maybe [Core.Text]
    -- ^ The specific cookies to forward to your distribution's origin.
  , option :: Core.Maybe Types.ForwardValues
    -- ^ Specifies which cookies to forward to the distribution's origin for a cache behavior: @all@ , @none@ , or @allow-list@ to forward only the cookies specified in the @cookiesAllowList@ parameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CookieObject' value with any optional fields omitted.
mkCookieObject
    :: CookieObject
mkCookieObject
  = CookieObject'{cookiesAllowList = Core.Nothing,
                  option = Core.Nothing}

-- | The specific cookies to forward to your distribution's origin.
--
-- /Note:/ Consider using 'cookiesAllowList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCookiesAllowList :: Lens.Lens' CookieObject (Core.Maybe [Core.Text])
coCookiesAllowList = Lens.field @"cookiesAllowList"
{-# INLINEABLE coCookiesAllowList #-}
{-# DEPRECATED cookiesAllowList "Use generic-lens or generic-optics with 'cookiesAllowList' instead"  #-}

-- | Specifies which cookies to forward to the distribution's origin for a cache behavior: @all@ , @none@ , or @allow-list@ to forward only the cookies specified in the @cookiesAllowList@ parameter.
--
-- /Note:/ Consider using 'option' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coOption :: Lens.Lens' CookieObject (Core.Maybe Types.ForwardValues)
coOption = Lens.field @"option"
{-# INLINEABLE coOption #-}
{-# DEPRECATED option "Use generic-lens or generic-optics with 'option' instead"  #-}

instance Core.FromJSON CookieObject where
        toJSON CookieObject{..}
          = Core.object
              (Core.catMaybes
                 [("cookiesAllowList" Core..=) Core.<$> cookiesAllowList,
                  ("option" Core..=) Core.<$> option])

instance Core.FromJSON CookieObject where
        parseJSON
          = Core.withObject "CookieObject" Core.$
              \ x ->
                CookieObject' Core.<$>
                  (x Core..:? "cookiesAllowList") Core.<*> x Core..:? "option"
