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
-- Module      : Amazonka.ChimeSdkVoice.Types.PhoneNumberCountry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.PhoneNumberCountry where

import Amazonka.ChimeSdkVoice.Types.PhoneNumberType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newPhoneNumberCountry' smart constructor.
data PhoneNumberCountry = PhoneNumberCountry'
  { countryCode :: Prelude.Maybe Prelude.Text,
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
-- 'countryCode', 'phoneNumberCountry_countryCode' - Undocumented member.
--
-- 'supportedPhoneNumberTypes', 'phoneNumberCountry_supportedPhoneNumberTypes' - Undocumented member.
newPhoneNumberCountry ::
  PhoneNumberCountry
newPhoneNumberCountry =
  PhoneNumberCountry'
    { countryCode = Prelude.Nothing,
      supportedPhoneNumberTypes = Prelude.Nothing
    }

-- | Undocumented member.
phoneNumberCountry_countryCode :: Lens.Lens' PhoneNumberCountry (Prelude.Maybe Prelude.Text)
phoneNumberCountry_countryCode = Lens.lens (\PhoneNumberCountry' {countryCode} -> countryCode) (\s@PhoneNumberCountry' {} a -> s {countryCode = a} :: PhoneNumberCountry)

-- | Undocumented member.
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
    Prelude.rnf countryCode `Prelude.seq`
      Prelude.rnf supportedPhoneNumberTypes
