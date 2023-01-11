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
-- Module      : Amazonka.ChimeSdkVoice.Types.PhoneNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.PhoneNumber where

import Amazonka.ChimeSdkVoice.Types.CallingNameStatus
import Amazonka.ChimeSdkVoice.Types.PhoneNumberAssociation
import Amazonka.ChimeSdkVoice.Types.PhoneNumberCapabilities
import Amazonka.ChimeSdkVoice.Types.PhoneNumberProductType
import Amazonka.ChimeSdkVoice.Types.PhoneNumberStatus
import Amazonka.ChimeSdkVoice.Types.PhoneNumberType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newPhoneNumber' smart constructor.
data PhoneNumber = PhoneNumber'
  { associations :: Prelude.Maybe [PhoneNumberAssociation],
    callingName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    callingNameStatus :: Prelude.Maybe CallingNameStatus,
    capabilities :: Prelude.Maybe PhoneNumberCapabilities,
    country :: Prelude.Maybe Prelude.Text,
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    deletionTimestamp :: Prelude.Maybe Data.ISO8601,
    e164PhoneNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    orderId :: Prelude.Maybe Prelude.Text,
    phoneNumberId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    productType :: Prelude.Maybe PhoneNumberProductType,
    status :: Prelude.Maybe PhoneNumberStatus,
    type' :: Prelude.Maybe PhoneNumberType,
    updatedTimestamp :: Prelude.Maybe Data.ISO8601
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
-- 'associations', 'phoneNumber_associations' - Undocumented member.
--
-- 'callingName', 'phoneNumber_callingName' - Undocumented member.
--
-- 'callingNameStatus', 'phoneNumber_callingNameStatus' - Undocumented member.
--
-- 'capabilities', 'phoneNumber_capabilities' - Undocumented member.
--
-- 'country', 'phoneNumber_country' - Undocumented member.
--
-- 'createdTimestamp', 'phoneNumber_createdTimestamp' - Undocumented member.
--
-- 'deletionTimestamp', 'phoneNumber_deletionTimestamp' - Undocumented member.
--
-- 'e164PhoneNumber', 'phoneNumber_e164PhoneNumber' - Undocumented member.
--
-- 'orderId', 'phoneNumber_orderId' - Undocumented member.
--
-- 'phoneNumberId', 'phoneNumber_phoneNumberId' - Undocumented member.
--
-- 'productType', 'phoneNumber_productType' - Undocumented member.
--
-- 'status', 'phoneNumber_status' - Undocumented member.
--
-- 'type'', 'phoneNumber_type' - Undocumented member.
--
-- 'updatedTimestamp', 'phoneNumber_updatedTimestamp' - Undocumented member.
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
      orderId = Prelude.Nothing,
      phoneNumberId = Prelude.Nothing,
      productType = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing
    }

-- | Undocumented member.
phoneNumber_associations :: Lens.Lens' PhoneNumber (Prelude.Maybe [PhoneNumberAssociation])
phoneNumber_associations = Lens.lens (\PhoneNumber' {associations} -> associations) (\s@PhoneNumber' {} a -> s {associations = a} :: PhoneNumber) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
phoneNumber_callingName :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.Text)
phoneNumber_callingName = Lens.lens (\PhoneNumber' {callingName} -> callingName) (\s@PhoneNumber' {} a -> s {callingName = a} :: PhoneNumber) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
phoneNumber_callingNameStatus :: Lens.Lens' PhoneNumber (Prelude.Maybe CallingNameStatus)
phoneNumber_callingNameStatus = Lens.lens (\PhoneNumber' {callingNameStatus} -> callingNameStatus) (\s@PhoneNumber' {} a -> s {callingNameStatus = a} :: PhoneNumber)

-- | Undocumented member.
phoneNumber_capabilities :: Lens.Lens' PhoneNumber (Prelude.Maybe PhoneNumberCapabilities)
phoneNumber_capabilities = Lens.lens (\PhoneNumber' {capabilities} -> capabilities) (\s@PhoneNumber' {} a -> s {capabilities = a} :: PhoneNumber)

-- | Undocumented member.
phoneNumber_country :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.Text)
phoneNumber_country = Lens.lens (\PhoneNumber' {country} -> country) (\s@PhoneNumber' {} a -> s {country = a} :: PhoneNumber)

-- | Undocumented member.
phoneNumber_createdTimestamp :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.UTCTime)
phoneNumber_createdTimestamp = Lens.lens (\PhoneNumber' {createdTimestamp} -> createdTimestamp) (\s@PhoneNumber' {} a -> s {createdTimestamp = a} :: PhoneNumber) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
phoneNumber_deletionTimestamp :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.UTCTime)
phoneNumber_deletionTimestamp = Lens.lens (\PhoneNumber' {deletionTimestamp} -> deletionTimestamp) (\s@PhoneNumber' {} a -> s {deletionTimestamp = a} :: PhoneNumber) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
phoneNumber_e164PhoneNumber :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.Text)
phoneNumber_e164PhoneNumber = Lens.lens (\PhoneNumber' {e164PhoneNumber} -> e164PhoneNumber) (\s@PhoneNumber' {} a -> s {e164PhoneNumber = a} :: PhoneNumber) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
phoneNumber_orderId :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.Text)
phoneNumber_orderId = Lens.lens (\PhoneNumber' {orderId} -> orderId) (\s@PhoneNumber' {} a -> s {orderId = a} :: PhoneNumber)

-- | Undocumented member.
phoneNumber_phoneNumberId :: Lens.Lens' PhoneNumber (Prelude.Maybe Prelude.Text)
phoneNumber_phoneNumberId = Lens.lens (\PhoneNumber' {phoneNumberId} -> phoneNumberId) (\s@PhoneNumber' {} a -> s {phoneNumberId = a} :: PhoneNumber) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
phoneNumber_productType :: Lens.Lens' PhoneNumber (Prelude.Maybe PhoneNumberProductType)
phoneNumber_productType = Lens.lens (\PhoneNumber' {productType} -> productType) (\s@PhoneNumber' {} a -> s {productType = a} :: PhoneNumber)

-- | Undocumented member.
phoneNumber_status :: Lens.Lens' PhoneNumber (Prelude.Maybe PhoneNumberStatus)
phoneNumber_status = Lens.lens (\PhoneNumber' {status} -> status) (\s@PhoneNumber' {} a -> s {status = a} :: PhoneNumber)

-- | Undocumented member.
phoneNumber_type :: Lens.Lens' PhoneNumber (Prelude.Maybe PhoneNumberType)
phoneNumber_type = Lens.lens (\PhoneNumber' {type'} -> type') (\s@PhoneNumber' {} a -> s {type' = a} :: PhoneNumber)

-- | Undocumented member.
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
            Prelude.<*> (x Data..:? "OrderId")
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
      `Prelude.hashWithSalt` orderId
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
      `Prelude.seq` Prelude.rnf orderId
      `Prelude.seq` Prelude.rnf phoneNumberId
      `Prelude.seq` Prelude.rnf productType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf updatedTimestamp
