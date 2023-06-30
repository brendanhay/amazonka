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
-- Module      : Amazonka.WAFV2.Types.CookieMatchPattern
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.CookieMatchPattern where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.All

-- | The filter to use to identify the subset of cookies to inspect in a web
-- request.
--
-- You must specify exactly one setting: either @All@, @IncludedCookies@,
-- or @ExcludedCookies@.
--
-- Example JSON:
-- @\"MatchPattern\": { \"IncludedCookies\": {\"KeyToInclude1\", \"KeyToInclude2\", \"KeyToInclude3\"} }@
--
-- /See:/ 'newCookieMatchPattern' smart constructor.
data CookieMatchPattern = CookieMatchPattern'
  { -- | Inspect all cookies.
    all :: Prelude.Maybe All,
    -- | Inspect only the cookies whose keys don\'t match any of the strings
    -- specified here.
    excludedCookies :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Inspect only the cookies that have a key that matches one of the strings
    -- specified here.
    includedCookies :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CookieMatchPattern' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'all', 'cookieMatchPattern_all' - Inspect all cookies.
--
-- 'excludedCookies', 'cookieMatchPattern_excludedCookies' - Inspect only the cookies whose keys don\'t match any of the strings
-- specified here.
--
-- 'includedCookies', 'cookieMatchPattern_includedCookies' - Inspect only the cookies that have a key that matches one of the strings
-- specified here.
newCookieMatchPattern ::
  CookieMatchPattern
newCookieMatchPattern =
  CookieMatchPattern'
    { all = Prelude.Nothing,
      excludedCookies = Prelude.Nothing,
      includedCookies = Prelude.Nothing
    }

-- | Inspect all cookies.
cookieMatchPattern_all :: Lens.Lens' CookieMatchPattern (Prelude.Maybe All)
cookieMatchPattern_all = Lens.lens (\CookieMatchPattern' {all} -> all) (\s@CookieMatchPattern' {} a -> s {all = a} :: CookieMatchPattern)

-- | Inspect only the cookies whose keys don\'t match any of the strings
-- specified here.
cookieMatchPattern_excludedCookies :: Lens.Lens' CookieMatchPattern (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
cookieMatchPattern_excludedCookies = Lens.lens (\CookieMatchPattern' {excludedCookies} -> excludedCookies) (\s@CookieMatchPattern' {} a -> s {excludedCookies = a} :: CookieMatchPattern) Prelude.. Lens.mapping Lens.coerced

-- | Inspect only the cookies that have a key that matches one of the strings
-- specified here.
cookieMatchPattern_includedCookies :: Lens.Lens' CookieMatchPattern (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
cookieMatchPattern_includedCookies = Lens.lens (\CookieMatchPattern' {includedCookies} -> includedCookies) (\s@CookieMatchPattern' {} a -> s {includedCookies = a} :: CookieMatchPattern) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CookieMatchPattern where
  parseJSON =
    Data.withObject
      "CookieMatchPattern"
      ( \x ->
          CookieMatchPattern'
            Prelude.<$> (x Data..:? "All")
            Prelude.<*> (x Data..:? "ExcludedCookies")
            Prelude.<*> (x Data..:? "IncludedCookies")
      )

instance Prelude.Hashable CookieMatchPattern where
  hashWithSalt _salt CookieMatchPattern' {..} =
    _salt
      `Prelude.hashWithSalt` all
      `Prelude.hashWithSalt` excludedCookies
      `Prelude.hashWithSalt` includedCookies

instance Prelude.NFData CookieMatchPattern where
  rnf CookieMatchPattern' {..} =
    Prelude.rnf all
      `Prelude.seq` Prelude.rnf excludedCookies
      `Prelude.seq` Prelude.rnf includedCookies

instance Data.ToJSON CookieMatchPattern where
  toJSON CookieMatchPattern' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("All" Data..=) Prelude.<$> all,
            ("ExcludedCookies" Data..=)
              Prelude.<$> excludedCookies,
            ("IncludedCookies" Data..=)
              Prelude.<$> includedCookies
          ]
      )
