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
-- Module      : Amazonka.Chime.Types.PhoneNumberCountry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.PhoneNumberCountry where

import Amazonka.Chime.Types.PhoneNumberType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The phone number country.
--
-- /See:/ 'newPhoneNumberCountry' smart constructor.
data PhoneNumberCountry = PhoneNumberCountry'
  { -- | The phone number country code. Format: ISO 3166-1 alpha-2.
    countryCode :: Prelude.Maybe Prelude.Text,
    -- | The supported phone number types.
    supportedPhoneNumberTypes :: Prelude.Maybe [PhoneNumberType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberCountry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'countryCode', 'phoneNumberCountry_countryCode' - The phone number country code. Format: ISO 3166-1 alpha-2.
--
-- 'supportedPhoneNumberTypes', 'phoneNumberCountry_supportedPhoneNumberTypes' - The supported phone number types.
newPhoneNumberCountry ::
  PhoneNumberCountry
newPhoneNumberCountry =
  PhoneNumberCountry'
    { countryCode = Prelude.Nothing,
      supportedPhoneNumberTypes = Prelude.Nothing
    }

-- | The phone number country code. Format: ISO 3166-1 alpha-2.
phoneNumberCountry_countryCode :: Lens.Lens' PhoneNumberCountry (Prelude.Maybe Prelude.Text)
phoneNumberCountry_countryCode = Lens.lens (\PhoneNumberCountry' {countryCode} -> countryCode) (\s@PhoneNumberCountry' {} a -> s {countryCode = a} :: PhoneNumberCountry)

-- | The supported phone number types.
phoneNumberCountry_supportedPhoneNumberTypes :: Lens.Lens' PhoneNumberCountry (Prelude.Maybe [PhoneNumberType])
phoneNumberCountry_supportedPhoneNumberTypes = Lens.lens (\PhoneNumberCountry' {supportedPhoneNumberTypes} -> supportedPhoneNumberTypes) (\s@PhoneNumberCountry' {} a -> s {supportedPhoneNumberTypes = a} :: PhoneNumberCountry) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PhoneNumberCountry where
  parseJSON =
    Data.withObject
      "PhoneNumberCountry"
      ( \x ->
          PhoneNumberCountry'
            Prelude.<$> (x Data..:? "CountryCode")
            Prelude.<*> ( x
                            Data..:? "SupportedPhoneNumberTypes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PhoneNumberCountry where
  hashWithSalt _salt PhoneNumberCountry' {..} =
    _salt
      `Prelude.hashWithSalt` countryCode
      `Prelude.hashWithSalt` supportedPhoneNumberTypes

instance Prelude.NFData PhoneNumberCountry where
  rnf PhoneNumberCountry' {..} =
    Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf supportedPhoneNumberTypes
