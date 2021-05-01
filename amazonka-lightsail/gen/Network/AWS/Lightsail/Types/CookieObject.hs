{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ForwardValues
import qualified Network.AWS.Prelude as Prelude

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
    cookiesAllowList :: Prelude.Maybe [Prelude.Text],
    -- | Specifies which cookies to forward to the distribution\'s origin for a
    -- cache behavior: @all@, @none@, or @allow-list@ to forward only the
    -- cookies specified in the @cookiesAllowList@ parameter.
    option :: Prelude.Maybe ForwardValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { cookiesAllowList = Prelude.Nothing,
      option = Prelude.Nothing
    }

-- | The specific cookies to forward to your distribution\'s origin.
cookieObject_cookiesAllowList :: Lens.Lens' CookieObject (Prelude.Maybe [Prelude.Text])
cookieObject_cookiesAllowList = Lens.lens (\CookieObject' {cookiesAllowList} -> cookiesAllowList) (\s@CookieObject' {} a -> s {cookiesAllowList = a} :: CookieObject) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies which cookies to forward to the distribution\'s origin for a
-- cache behavior: @all@, @none@, or @allow-list@ to forward only the
-- cookies specified in the @cookiesAllowList@ parameter.
cookieObject_option :: Lens.Lens' CookieObject (Prelude.Maybe ForwardValues)
cookieObject_option = Lens.lens (\CookieObject' {option} -> option) (\s@CookieObject' {} a -> s {option = a} :: CookieObject)

instance Prelude.FromJSON CookieObject where
  parseJSON =
    Prelude.withObject
      "CookieObject"
      ( \x ->
          CookieObject'
            Prelude.<$> ( x Prelude..:? "cookiesAllowList"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "option")
      )

instance Prelude.Hashable CookieObject

instance Prelude.NFData CookieObject

instance Prelude.ToJSON CookieObject where
  toJSON CookieObject' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("cookiesAllowList" Prelude..=)
              Prelude.<$> cookiesAllowList,
            ("option" Prelude..=) Prelude.<$> option
          ]
      )
