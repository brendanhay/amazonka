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
-- Module      : Amazonka.Chime.Types.PhoneNumber
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.PhoneNumber where

import Amazonka.Chime.Types.CallingNameStatus
import Amazonka.Chime.Types.PhoneNumberAssociation
import Amazonka.Chime.Types.PhoneNumberCapabilities
import Amazonka.Chime.Types.PhoneNumberProductType
import Amazonka.Chime.Types.PhoneNumberStatus
import Amazonka.Chime.Types.PhoneNumberType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A phone number used for Amazon Chime Business Calling or an Amazon Chime
-- Voice Connector.
--
-- /See:/ 'newPhoneNumber' smart constructor.
data PhoneNumber = PhoneNumber'
  { -- | The phone number associations.
    associations :: Prelude.Maybe [PhoneNumberAssociation],
    -- | The outbound calling name associated with the phone number.
    callingName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The outbound calling name status.
    callingNameStatus :: Prelude.Maybe CallingNameStatus,
    -- | The phone number capabilities.
    capabilities :: Prelude.Maybe PhoneNumberCapabilities,
    -- | The phone number country. Format: ISO 3166-1 alpha-2.
    country :: Prelude.Maybe Prelude.Text,
    -- | The phone number creation timestamp, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The deleted phone number timestamp, in ISO 8601 format.
    deletionTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The phone number, in E.164 format.
    e164PhoneNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The phone number ID.
    phoneNumberId :: Prelude.Maybe Prelude.Text,
    -- | The phone number product type.
    productType :: Prelude.Maybe PhoneNumberProductType,
    -- | The phone number status.
    status :: Prelude.Maybe PhoneNumberStatus,
    -- | The phone number type.
    type' :: Prelude.Maybe PhoneNumberType,
    -- | The updated phone number timestamp, in ISO 8601 format.
    updatedTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associations', 'phoneNumber_associations' - The phone number associations.
--
-- 'callingName', 'phoneNumber_callingName' - The outbound calling name associated with the phone number.
--
-- 'callingNameStatus', 'phoneNumber_callingNameStatus' - The outbound calling name status.
--
-- 'capabilities', 'phoneNumber_capabilities' - The phone number capabilities.
--
-- 'country', 'phoneNumber_country' - The phone number country. Format: ISO 3166-1 alpha-2.
--
-- 'createdTimestamp', 'phoneNumber_createdTimestamp' - The phone number creation timestamp, in ISO 8601 format.
--
-- 'deletionTimestamp', 'phoneNumber_deletionTimestamp' - The deleted phone number timestamp, in ISO 8601 format.
--
-- 'e164PhoneNumber', 'phoneNumber_e164PhoneNumber' - The phone number, in E.164 format.
--
-- 'phoneNumberId', 'phoneNumber_phoneNumberId' - The phone number ID.
--
-- 'productType', 'phoneNumber_productType' - The phone number product type.
--
-- 'status', 'phoneNumber_status' - The phone number status.
--
-- 'type'', 'phoneNumber_type' - The phone number type.
--
-- 'updatedTimestamp', 'phoneNumber_updatedTimestamp' - The updated phone number timestamp, in ISO 8601 format.
newPhoneNumber ::
  PhoneNumber
newPhoneNumber =
  PhoneNumber'
    { associations = Prelude.Nothing,
      callingName = Prelude.Nothing,
      callingNameStatus = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      country = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      deletionTimestamp = Prelude.Nothing,
      e164PhoneNumber = Prelude.Nothing,
      phoneNumberId = Prelude.Nothing,
      productType = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing
    }

-- | The phone number associations.
phoneNumber_associations :: Lens.Lens' PhoneNumber (Prelude.Maybe [PhoneNumberAssociation])
phoneNumber_associations = Lens.lens (\PhoneNumber' {associations} -> associations) (\s@PhoneNumber' {} a -> s {associations = a} :: PhoneNumber) Prelude.. Lens.mapping Lens.coerced

-- | The outbound calling name associated with the phone number.
phoneNumber_callingName :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.Text)
phoneNumber_callingName = Lens.lens (\PhoneNumber' {callingName} -> callingName) (\s@PhoneNumber' {} a -> s {callingName = a} :: PhoneNumber) Prelude.. Lens.mapping Data._Sensitive

-- | The outbound calling name status.
phoneNumber_callingNameStatus :: Lens.Lens' PhoneNumber (Prelude.Maybe CallingNameStatus)
phoneNumber_callingNameStatus = Lens.lens (\PhoneNumber' {callingNameStatus} -> callingNameStatus) (\s@PhoneNumber' {} a -> s {callingNameStatus = a} :: PhoneNumber)

-- | The phone number capabilities.
phoneNumber_capabilities :: Lens.Lens' PhoneNumber (Prelude.Maybe PhoneNumberCapabilities)
phoneNumber_capabilities = Lens.lens (\PhoneNumber' {capabilities} -> capabilities) (\s@PhoneNumber' {} a -> s {capabilities = a} :: PhoneNumber)

-- | The phone number country. Format: ISO 3166-1 alpha-2.
phoneNumber_country :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.Text)
phoneNumber_country = Lens.lens (\PhoneNumber' {country} -> country) (\s@PhoneNumber' {} a -> s {country = a} :: PhoneNumber)

-- | The phone number creation timestamp, in ISO 8601 format.
phoneNumber_createdTimestamp :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.UTCTime)
phoneNumber_createdTimestamp = Lens.lens (\PhoneNumber' {createdTimestamp} -> createdTimestamp) (\s@PhoneNumber' {} a -> s {createdTimestamp = a} :: PhoneNumber) Prelude.. Lens.mapping Data._Time

-- | The deleted phone number timestamp, in ISO 8601 format.
phoneNumber_deletionTimestamp :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.UTCTime)
phoneNumber_deletionTimestamp = Lens.lens (\PhoneNumber' {deletionTimestamp} -> deletionTimestamp) (\s@PhoneNumber' {} a -> s {deletionTimestamp = a} :: PhoneNumber) Prelude.. Lens.mapping Data._Time

-- | The phone number, in E.164 format.
phoneNumber_e164PhoneNumber :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.Text)
phoneNumber_e164PhoneNumber = Lens.lens (\PhoneNumber' {e164PhoneNumber} -> e164PhoneNumber) (\s@PhoneNumber' {} a -> s {e164PhoneNumber = a} :: PhoneNumber) Prelude.. Lens.mapping Data._Sensitive

-- | The phone number ID.
phoneNumber_phoneNumberId :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.Text)
phoneNumber_phoneNumberId = Lens.lens (\PhoneNumber' {phoneNumberId} -> phoneNumberId) (\s@PhoneNumber' {} a -> s {phoneNumberId = a} :: PhoneNumber)

-- | The phone number product type.
phoneNumber_productType :: Lens.Lens' PhoneNumber (Prelude.Maybe PhoneNumberProductType)
phoneNumber_productType = Lens.lens (\PhoneNumber' {productType} -> productType) (\s@PhoneNumber' {} a -> s {productType = a} :: PhoneNumber)

-- | The phone number status.
phoneNumber_status :: Lens.Lens' PhoneNumber (Prelude.Maybe PhoneNumberStatus)
phoneNumber_status = Lens.lens (\PhoneNumber' {status} -> status) (\s@PhoneNumber' {} a -> s {status = a} :: PhoneNumber)

-- | The phone number type.
phoneNumber_type :: Lens.Lens' PhoneNumber (Prelude.Maybe PhoneNumberType)
phoneNumber_type = Lens.lens (\PhoneNumber' {type'} -> type') (\s@PhoneNumber' {} a -> s {type' = a} :: PhoneNumber)

-- | The updated phone number timestamp, in ISO 8601 format.
phoneNumber_updatedTimestamp :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.UTCTime)
phoneNumber_updatedTimestamp = Lens.lens (\PhoneNumber' {updatedTimestamp} -> updatedTimestamp) (\s@PhoneNumber' {} a -> s {updatedTimestamp = a} :: PhoneNumber) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON PhoneNumber where
  parseJSON =
    Data.withObject
      "PhoneNumber"
      ( \x ->
          PhoneNumber'
            Prelude.<$> (x Data..:? "Associations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CallingName")
            Prelude.<*> (x Data..:? "CallingNameStatus")
            Prelude.<*> (x Data..:? "Capabilities")
            Prelude.<*> (x Data..:? "Country")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "DeletionTimestamp")
            Prelude.<*> (x Data..:? "E164PhoneNumber")
            Prelude.<*> (x Data..:? "PhoneNumberId")
            Prelude.<*> (x Data..:? "ProductType")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
      )

instance Prelude.Hashable PhoneNumber where
  hashWithSalt _salt PhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` associations
      `Prelude.hashWithSalt` callingName
      `Prelude.hashWithSalt` callingNameStatus
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` deletionTimestamp
      `Prelude.hashWithSalt` e164PhoneNumber
      `Prelude.hashWithSalt` phoneNumberId
      `Prelude.hashWithSalt` productType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` updatedTimestamp

instance Prelude.NFData PhoneNumber where
  rnf PhoneNumber' {..} =
    Prelude.rnf associations
      `Prelude.seq` Prelude.rnf callingName
      `Prelude.seq` Prelude.rnf callingNameStatus
      `Prelude.seq` Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf deletionTimestamp
      `Prelude.seq` Prelude.rnf e164PhoneNumber
      `Prelude.seq` Prelude.rnf phoneNumberId
      `Prelude.seq` Prelude.rnf productType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf updatedTimestamp
