{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CookieObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CookieObject where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ForwardValues

-- | Describes whether an Amazon Lightsail content delivery network (CDN)
-- distribution forwards cookies to the origin and, if so, which ones.
--
-- For the cookies that you specify, your distribution caches separate
-- versions of the specified content based on the cookie values in viewer
-- requests.
--
-- /See:/ 'newCookieObject' smart constructor.
data CookieObject = CookieObject'
  { -- | The specific cookies to forward to your distribution\'s origin.
    cookiesAllowList :: Core.Maybe [Core.Text],
    -- | Specifies which cookies to forward to the distribution\'s origin for a
    -- cache behavior: @all@, @none@, or @allow-list@ to forward only the
    -- cookies specified in the @cookiesAllowList@ parameter.
    option :: Core.Maybe ForwardValues
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CookieObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cookiesAllowList', 'cookieObject_cookiesAllowList' - The specific cookies to forward to your distribution\'s origin.
--
-- 'option', 'cookieObject_option' - Specifies which cookies to forward to the distribution\'s origin for a
-- cache behavior: @all@, @none@, or @allow-list@ to forward only the
-- cookies specified in the @cookiesAllowList@ parameter.
newCookieObject ::
  CookieObject
newCookieObject =
  CookieObject'
    { cookiesAllowList = Core.Nothing,
      option = Core.Nothing
    }

-- | The specific cookies to forward to your distribution\'s origin.
cookieObject_cookiesAllowList :: Lens.Lens' CookieObject (Core.Maybe [Core.Text])
cookieObject_cookiesAllowList = Lens.lens (\CookieObject' {cookiesAllowList} -> cookiesAllowList) (\s@CookieObject' {} a -> s {cookiesAllowList = a} :: CookieObject) Core.. Lens.mapping Lens._Coerce

-- | Specifies which cookies to forward to the distribution\'s origin for a
-- cache behavior: @all@, @none@, or @allow-list@ to forward only the
-- cookies specified in the @cookiesAllowList@ parameter.
cookieObject_option :: Lens.Lens' CookieObject (Core.Maybe ForwardValues)
cookieObject_option = Lens.lens (\CookieObject' {option} -> option) (\s@CookieObject' {} a -> s {option = a} :: CookieObject)

instance Core.FromJSON CookieObject where
  parseJSON =
    Core.withObject
      "CookieObject"
      ( \x ->
          CookieObject'
            Core.<$> (x Core..:? "cookiesAllowList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "option")
      )

instance Core.Hashable CookieObject

instance Core.NFData CookieObject

instance Core.ToJSON CookieObject where
  toJSON CookieObject' {..} =
    Core.object
      ( Core.catMaybes
          [ ("cookiesAllowList" Core..=)
              Core.<$> cookiesAllowList,
            ("option" Core..=) Core.<$> option
          ]
      )
