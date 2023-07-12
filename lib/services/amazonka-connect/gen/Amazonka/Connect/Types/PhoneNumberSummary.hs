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
-- Module      : Amazonka.Connect.Types.PhoneNumberSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.PhoneNumberSummary where

import Amazonka.Connect.Types.PhoneNumberCountryCode
import Amazonka.Connect.Types.PhoneNumberType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a phone number for a contact center.
--
-- /See:/ 'newPhoneNumberSummary' smart constructor.
data PhoneNumberSummary = PhoneNumberSummary'
  { -- | The Amazon Resource Name (ARN) of the phone number.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the phone number.
    id :: Prelude.Maybe Prelude.Text,
    -- | The phone number.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The ISO country code.
    phoneNumberCountryCode :: Prelude.Maybe PhoneNumberCountryCode,
    -- | The type of phone number.
    phoneNumberType :: Prelude.Maybe PhoneNumberType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'phoneNumberSummary_arn' - The Amazon Resource Name (ARN) of the phone number.
--
-- 'id', 'phoneNumberSummary_id' - The identifier of the phone number.
--
-- 'phoneNumber', 'phoneNumberSummary_phoneNumber' - The phone number.
--
-- 'phoneNumberCountryCode', 'phoneNumberSummary_phoneNumberCountryCode' - The ISO country code.
--
-- 'phoneNumberType', 'phoneNumberSummary_phoneNumberType' - The type of phone number.
newPhoneNumberSummary ::
  PhoneNumberSummary
newPhoneNumberSummary =
  PhoneNumberSummary'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      phoneNumberCountryCode = Prelude.Nothing,
      phoneNumberType = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the phone number.
phoneNumberSummary_arn :: Lens.Lens' PhoneNumberSummary (Prelude.Maybe Prelude.Text)
phoneNumberSummary_arn = Lens.lens (\PhoneNumberSummary' {arn} -> arn) (\s@PhoneNumberSummary' {} a -> s {arn = a} :: PhoneNumberSummary)

-- | The identifier of the phone number.
phoneNumberSummary_id :: Lens.Lens' PhoneNumberSummary (Prelude.Maybe Prelude.Text)
phoneNumberSummary_id = Lens.lens (\PhoneNumberSummary' {id} -> id) (\s@PhoneNumberSummary' {} a -> s {id = a} :: PhoneNumberSummary)

-- | The phone number.
phoneNumberSummary_phoneNumber :: Lens.Lens' PhoneNumberSummary (Prelude.Maybe Prelude.Text)
phoneNumberSummary_phoneNumber = Lens.lens (\PhoneNumberSummary' {phoneNumber} -> phoneNumber) (\s@PhoneNumberSummary' {} a -> s {phoneNumber = a} :: PhoneNumberSummary)

-- | The ISO country code.
phoneNumberSummary_phoneNumberCountryCode :: Lens.Lens' PhoneNumberSummary (Prelude.Maybe PhoneNumberCountryCode)
phoneNumberSummary_phoneNumberCountryCode = Lens.lens (\PhoneNumberSummary' {phoneNumberCountryCode} -> phoneNumberCountryCode) (\s@PhoneNumberSummary' {} a -> s {phoneNumberCountryCode = a} :: PhoneNumberSummary)

-- | The type of phone number.
phoneNumberSummary_phoneNumberType :: Lens.Lens' PhoneNumberSummary (Prelude.Maybe PhoneNumberType)
phoneNumberSummary_phoneNumberType = Lens.lens (\PhoneNumberSummary' {phoneNumberType} -> phoneNumberType) (\s@PhoneNumberSummary' {} a -> s {phoneNumberType = a} :: PhoneNumberSummary)

instance Data.FromJSON PhoneNumberSummary where
  parseJSON =
    Data.withObject
      "PhoneNumberSummary"
      ( \x ->
          PhoneNumberSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "PhoneNumber")
            Prelude.<*> (x Data..:? "PhoneNumberCountryCode")
            Prelude.<*> (x Data..:? "PhoneNumberType")
      )

instance Prelude.Hashable PhoneNumberSummary where
  hashWithSalt _salt PhoneNumberSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` phoneNumberCountryCode
      `Prelude.hashWithSalt` phoneNumberType

instance Prelude.NFData PhoneNumberSummary where
  rnf PhoneNumberSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf phoneNumberCountryCode
      `Prelude.seq` Prelude.rnf phoneNumberType
