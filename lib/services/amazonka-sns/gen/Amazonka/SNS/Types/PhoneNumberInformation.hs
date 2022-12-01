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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Types.PhoneNumberInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SNS.Types.NumberCapability
import Amazonka.SNS.Types.RouteType

-- | A list of phone numbers and their metadata.
--
-- /See:/ 'newPhoneNumberInformation' smart constructor.
data PhoneNumberInformation = PhoneNumberInformation'
  { -- | The list of supported routes.
    routeType :: Prelude.Maybe RouteType,
    -- | The status of the phone number.
    status :: Prelude.Maybe Prelude.Text,
    -- | The two-character code for the country or region, in ISO 3166-1 alpha-2
    -- format.
    iso2CountryCode :: Prelude.Maybe Prelude.Text,
    -- | The capabilities of each phone number.
    numberCapabilities :: Prelude.Maybe [NumberCapability],
    -- | The phone number.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the phone number was created.
    createdAt :: Prelude.Maybe Core.ISO8601
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
-- 'routeType', 'phoneNumberInformation_routeType' - The list of supported routes.
--
-- 'status', 'phoneNumberInformation_status' - The status of the phone number.
--
-- 'iso2CountryCode', 'phoneNumberInformation_iso2CountryCode' - The two-character code for the country or region, in ISO 3166-1 alpha-2
-- format.
--
-- 'numberCapabilities', 'phoneNumberInformation_numberCapabilities' - The capabilities of each phone number.
--
-- 'phoneNumber', 'phoneNumberInformation_phoneNumber' - The phone number.
--
-- 'createdAt', 'phoneNumberInformation_createdAt' - The date and time when the phone number was created.
newPhoneNumberInformation ::
  PhoneNumberInformation
newPhoneNumberInformation =
  PhoneNumberInformation'
    { routeType =
        Prelude.Nothing,
      status = Prelude.Nothing,
      iso2CountryCode = Prelude.Nothing,
      numberCapabilities = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The list of supported routes.
phoneNumberInformation_routeType :: Lens.Lens' PhoneNumberInformation (Prelude.Maybe RouteType)
phoneNumberInformation_routeType = Lens.lens (\PhoneNumberInformation' {routeType} -> routeType) (\s@PhoneNumberInformation' {} a -> s {routeType = a} :: PhoneNumberInformation)

-- | The status of the phone number.
phoneNumberInformation_status :: Lens.Lens' PhoneNumberInformation (Prelude.Maybe Prelude.Text)
phoneNumberInformation_status = Lens.lens (\PhoneNumberInformation' {status} -> status) (\s@PhoneNumberInformation' {} a -> s {status = a} :: PhoneNumberInformation)

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

-- | The date and time when the phone number was created.
phoneNumberInformation_createdAt :: Lens.Lens' PhoneNumberInformation (Prelude.Maybe Prelude.UTCTime)
phoneNumberInformation_createdAt = Lens.lens (\PhoneNumberInformation' {createdAt} -> createdAt) (\s@PhoneNumberInformation' {} a -> s {createdAt = a} :: PhoneNumberInformation) Prelude.. Lens.mapping Core._Time

instance Core.FromXML PhoneNumberInformation where
  parseXML x =
    PhoneNumberInformation'
      Prelude.<$> (x Core..@? "RouteType")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "Iso2CountryCode")
      Prelude.<*> ( x Core..@? "NumberCapabilities"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "PhoneNumber")
      Prelude.<*> (x Core..@? "CreatedAt")

instance Prelude.Hashable PhoneNumberInformation where
  hashWithSalt _salt PhoneNumberInformation' {..} =
    _salt `Prelude.hashWithSalt` routeType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` iso2CountryCode
      `Prelude.hashWithSalt` numberCapabilities
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData PhoneNumberInformation where
  rnf PhoneNumberInformation' {..} =
    Prelude.rnf routeType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf iso2CountryCode
      `Prelude.seq` Prelude.rnf numberCapabilities
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf createdAt
