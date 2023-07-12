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
-- Module      : Amazonka.SNS.Types.PhoneNumberInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Types.PhoneNumberInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SNS.Types.NumberCapability
import Amazonka.SNS.Types.RouteType

-- | A list of phone numbers and their metadata.
--
-- /See:/ 'newPhoneNumberInformation' smart constructor.
data PhoneNumberInformation = PhoneNumberInformation'
  { -- | The date and time when the phone number was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The two-character code for the country or region, in ISO 3166-1 alpha-2
    -- format.
    iso2CountryCode :: Prelude.Maybe Prelude.Text,
    -- | The capabilities of each phone number.
    numberCapabilities :: Prelude.Maybe [NumberCapability],
    -- | The phone number.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The list of supported routes.
    routeType :: Prelude.Maybe RouteType,
    -- | The status of the phone number.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'phoneNumberInformation_createdAt' - The date and time when the phone number was created.
--
-- 'iso2CountryCode', 'phoneNumberInformation_iso2CountryCode' - The two-character code for the country or region, in ISO 3166-1 alpha-2
-- format.
--
-- 'numberCapabilities', 'phoneNumberInformation_numberCapabilities' - The capabilities of each phone number.
--
-- 'phoneNumber', 'phoneNumberInformation_phoneNumber' - The phone number.
--
-- 'routeType', 'phoneNumberInformation_routeType' - The list of supported routes.
--
-- 'status', 'phoneNumberInformation_status' - The status of the phone number.
newPhoneNumberInformation ::
  PhoneNumberInformation
newPhoneNumberInformation =
  PhoneNumberInformation'
    { createdAt =
        Prelude.Nothing,
      iso2CountryCode = Prelude.Nothing,
      numberCapabilities = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      routeType = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date and time when the phone number was created.
phoneNumberInformation_createdAt :: Lens.Lens' PhoneNumberInformation (Prelude.Maybe Prelude.UTCTime)
phoneNumberInformation_createdAt = Lens.lens (\PhoneNumberInformation' {createdAt} -> createdAt) (\s@PhoneNumberInformation' {} a -> s {createdAt = a} :: PhoneNumberInformation) Prelude.. Lens.mapping Data._Time

-- | The two-character code for the country or region, in ISO 3166-1 alpha-2
-- format.
phoneNumberInformation_iso2CountryCode :: Lens.Lens' PhoneNumberInformation (Prelude.Maybe Prelude.Text)
phoneNumberInformation_iso2CountryCode = Lens.lens (\PhoneNumberInformation' {iso2CountryCode} -> iso2CountryCode) (\s@PhoneNumberInformation' {} a -> s {iso2CountryCode = a} :: PhoneNumberInformation)

-- | The capabilities of each phone number.
phoneNumberInformation_numberCapabilities :: Lens.Lens' PhoneNumberInformation (Prelude.Maybe [NumberCapability])
phoneNumberInformation_numberCapabilities = Lens.lens (\PhoneNumberInformation' {numberCapabilities} -> numberCapabilities) (\s@PhoneNumberInformation' {} a -> s {numberCapabilities = a} :: PhoneNumberInformation) Prelude.. Lens.mapping Lens.coerced

-- | The phone number.
phoneNumberInformation_phoneNumber :: Lens.Lens' PhoneNumberInformation (Prelude.Maybe Prelude.Text)
phoneNumberInformation_phoneNumber = Lens.lens (\PhoneNumberInformation' {phoneNumber} -> phoneNumber) (\s@PhoneNumberInformation' {} a -> s {phoneNumber = a} :: PhoneNumberInformation)

-- | The list of supported routes.
phoneNumberInformation_routeType :: Lens.Lens' PhoneNumberInformation (Prelude.Maybe RouteType)
phoneNumberInformation_routeType = Lens.lens (\PhoneNumberInformation' {routeType} -> routeType) (\s@PhoneNumberInformation' {} a -> s {routeType = a} :: PhoneNumberInformation)

-- | The status of the phone number.
phoneNumberInformation_status :: Lens.Lens' PhoneNumberInformation (Prelude.Maybe Prelude.Text)
phoneNumberInformation_status = Lens.lens (\PhoneNumberInformation' {status} -> status) (\s@PhoneNumberInformation' {} a -> s {status = a} :: PhoneNumberInformation)

instance Data.FromXML PhoneNumberInformation where
  parseXML x =
    PhoneNumberInformation'
      Prelude.<$> (x Data..@? "CreatedAt")
      Prelude.<*> (x Data..@? "Iso2CountryCode")
      Prelude.<*> ( x
                      Data..@? "NumberCapabilities"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "PhoneNumber")
      Prelude.<*> (x Data..@? "RouteType")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable PhoneNumberInformation where
  hashWithSalt _salt PhoneNumberInformation' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` iso2CountryCode
      `Prelude.hashWithSalt` numberCapabilities
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` routeType
      `Prelude.hashWithSalt` status

instance Prelude.NFData PhoneNumberInformation where
  rnf PhoneNumberInformation' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf iso2CountryCode
      `Prelude.seq` Prelude.rnf numberCapabilities
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf routeType
      `Prelude.seq` Prelude.rnf status
