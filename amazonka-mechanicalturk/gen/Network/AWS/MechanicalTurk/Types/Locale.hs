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
-- Module      : Network.AWS.MechanicalTurk.Types.Locale
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.Locale where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The Locale data structure represents a geographical region or location.
--
-- /See:/ 'newLocale' smart constructor.
data Locale = Locale'
  { -- | The state or subdivision of the locale. A valid ISO 3166-2 subdivision
    -- code. For example, the code WA refers to the state of Washington.
    subdivision :: Prelude.Maybe Prelude.Text,
    -- | The country of the locale. Must be a valid ISO 3166 country code. For
    -- example, the code US refers to the United States of America.
    country :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Locale' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subdivision', 'locale_subdivision' - The state or subdivision of the locale. A valid ISO 3166-2 subdivision
-- code. For example, the code WA refers to the state of Washington.
--
-- 'country', 'locale_country' - The country of the locale. Must be a valid ISO 3166 country code. For
-- example, the code US refers to the United States of America.
newLocale ::
  -- | 'country'
  Prelude.Text ->
  Locale
newLocale pCountry_ =
  Locale'
    { subdivision = Prelude.Nothing,
      country = pCountry_
    }

-- | The state or subdivision of the locale. A valid ISO 3166-2 subdivision
-- code. For example, the code WA refers to the state of Washington.
locale_subdivision :: Lens.Lens' Locale (Prelude.Maybe Prelude.Text)
locale_subdivision = Lens.lens (\Locale' {subdivision} -> subdivision) (\s@Locale' {} a -> s {subdivision = a} :: Locale)

-- | The country of the locale. Must be a valid ISO 3166 country code. For
-- example, the code US refers to the United States of America.
locale_country :: Lens.Lens' Locale Prelude.Text
locale_country = Lens.lens (\Locale' {country} -> country) (\s@Locale' {} a -> s {country = a} :: Locale)

instance Prelude.FromJSON Locale where
  parseJSON =
    Prelude.withObject
      "Locale"
      ( \x ->
          Locale'
            Prelude.<$> (x Prelude..:? "Subdivision")
            Prelude.<*> (x Prelude..: "Country")
      )

instance Prelude.Hashable Locale

instance Prelude.NFData Locale

instance Prelude.ToJSON Locale where
  toJSON Locale' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Subdivision" Prelude..=) Prelude.<$> subdivision,
            Prelude.Just ("Country" Prelude..= country)
          ]
      )
