{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CookieObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CookieObject
  ( CookieObject (..),

    -- * Smart constructor
    mkCookieObject,

    -- * Lenses
    coCookiesAllowList,
    coOption,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ForwardValues
import qualified Network.AWS.Prelude as Lude

-- | Describes whether an Amazon Lightsail content delivery network (CDN) distribution forwards cookies to the origin and, if so, which ones.
--
-- For the cookies that you specify, your distribution caches separate versions of the specified content based on the cookie values in viewer requests.
--
-- /See:/ 'mkCookieObject' smart constructor.
data CookieObject = CookieObject'
  { cookiesAllowList ::
      Lude.Maybe [Lude.Text],
    option :: Lude.Maybe ForwardValues
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CookieObject' with the minimum fields required to make a request.
--
-- * 'cookiesAllowList' - The specific cookies to forward to your distribution's origin.
-- * 'option' - Specifies which cookies to forward to the distribution's origin for a cache behavior: @all@ , @none@ , or @allow-list@ to forward only the cookies specified in the @cookiesAllowList@ parameter.
mkCookieObject ::
  CookieObject
mkCookieObject =
  CookieObject'
    { cookiesAllowList = Lude.Nothing,
      option = Lude.Nothing
    }

-- | The specific cookies to forward to your distribution's origin.
--
-- /Note:/ Consider using 'cookiesAllowList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCookiesAllowList :: Lens.Lens' CookieObject (Lude.Maybe [Lude.Text])
coCookiesAllowList = Lens.lens (cookiesAllowList :: CookieObject -> Lude.Maybe [Lude.Text]) (\s a -> s {cookiesAllowList = a} :: CookieObject)
{-# DEPRECATED coCookiesAllowList "Use generic-lens or generic-optics with 'cookiesAllowList' instead." #-}

-- | Specifies which cookies to forward to the distribution's origin for a cache behavior: @all@ , @none@ , or @allow-list@ to forward only the cookies specified in the @cookiesAllowList@ parameter.
--
-- /Note:/ Consider using 'option' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coOption :: Lens.Lens' CookieObject (Lude.Maybe ForwardValues)
coOption = Lens.lens (option :: CookieObject -> Lude.Maybe ForwardValues) (\s a -> s {option = a} :: CookieObject)
{-# DEPRECATED coOption "Use generic-lens or generic-optics with 'option' instead." #-}

instance Lude.FromJSON CookieObject where
  parseJSON =
    Lude.withObject
      "CookieObject"
      ( \x ->
          CookieObject'
            Lude.<$> (x Lude..:? "cookiesAllowList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "option")
      )

instance Lude.ToJSON CookieObject where
  toJSON CookieObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cookiesAllowList" Lude..=) Lude.<$> cookiesAllowList,
            ("option" Lude..=) Lude.<$> option
          ]
      )
