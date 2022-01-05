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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.PhoneNumberCountry where

import Amazonka.Chime.Types.PhoneNumberType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The phone number country.
--
-- /See:/ 'newPhoneNumberCountry' smart constructor.
data PhoneNumberCountry = PhoneNumberCountry'
  { -- | The supported phone number types.
    supportedPhoneNumberTypes :: Prelude.Maybe [PhoneNumberType],
    -- | The phone number country code. Format: ISO 3166-1 alpha-2.
    countryCode :: Prelude.Maybe Prelude.Text
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
-- 'supportedPhoneNumberTypes', 'phoneNumberCountry_supportedPhoneNumberTypes' - The supported phone number types.
--
-- 'countryCode', 'phoneNumberCountry_countryCode' - The phone number country code. Format: ISO 3166-1 alpha-2.
newPhoneNumberCountry ::
  PhoneNumberCountry
newPhoneNumberCountry =
  PhoneNumberCountry'
    { supportedPhoneNumberTypes =
        Prelude.Nothing,
      countryCode = Prelude.Nothing
    }

-- | The supported phone number types.
phoneNumberCountry_supportedPhoneNumberTypes :: Lens.Lens' PhoneNumberCountry (Prelude.Maybe [PhoneNumberType])
phoneNumberCountry_supportedPhoneNumberTypes = Lens.lens (\PhoneNumberCountry' {supportedPhoneNumberTypes} -> supportedPhoneNumberTypes) (\s@PhoneNumberCountry' {} a -> s {supportedPhoneNumberTypes = a} :: PhoneNumberCountry) Prelude.. Lens.mapping Lens.coerced

-- | The phone number country code. Format: ISO 3166-1 alpha-2.
phoneNumberCountry_countryCode :: Lens.Lens' PhoneNumberCountry (Prelude.Maybe Prelude.Text)
phoneNumberCountry_countryCode = Lens.lens (\PhoneNumberCountry' {countryCode} -> countryCode) (\s@PhoneNumberCountry' {} a -> s {countryCode = a} :: PhoneNumberCountry)

instance Core.FromJSON PhoneNumberCountry where
  parseJSON =
    Core.withObject
      "PhoneNumberCountry"
      ( \x ->
          PhoneNumberCountry'
            Prelude.<$> ( x Core..:? "SupportedPhoneNumberTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CountryCode")
      )

instance Prelude.Hashable PhoneNumberCountry where
  hashWithSalt _salt PhoneNumberCountry' {..} =
    _salt
      `Prelude.hashWithSalt` supportedPhoneNumberTypes
      `Prelude.hashWithSalt` countryCode

instance Prelude.NFData PhoneNumberCountry where
  rnf PhoneNumberCountry' {..} =
    Prelude.rnf supportedPhoneNumberTypes
      `Prelude.seq` Prelude.rnf countryCode
