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
-- Module      : Amazonka.Chime.Types.PhoneNumberOrder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.PhoneNumberOrder where

import Amazonka.Chime.Types.OrderedPhoneNumber
import Amazonka.Chime.Types.PhoneNumberOrderStatus
import Amazonka.Chime.Types.PhoneNumberProductType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a phone number order created for Amazon Chime.
--
-- /See:/ 'newPhoneNumberOrder' smart constructor.
data PhoneNumberOrder = PhoneNumberOrder'
  { -- | The phone number order creation time stamp, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The ordered phone number details, such as the phone number in E.164
    -- format and the phone number status.
    orderedPhoneNumbers :: Prelude.Maybe [OrderedPhoneNumber],
    -- | The phone number order ID.
    phoneNumberOrderId :: Prelude.Maybe Prelude.Text,
    -- | The phone number order product type.
    productType :: Prelude.Maybe PhoneNumberProductType,
    -- | The status of the phone number order.
    status :: Prelude.Maybe PhoneNumberOrderStatus,
    -- | The updated phone number order time stamp, in ISO 8601 format.
    updatedTimestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberOrder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'phoneNumberOrder_createdTimestamp' - The phone number order creation time stamp, in ISO 8601 format.
--
-- 'orderedPhoneNumbers', 'phoneNumberOrder_orderedPhoneNumbers' - The ordered phone number details, such as the phone number in E.164
-- format and the phone number status.
--
-- 'phoneNumberOrderId', 'phoneNumberOrder_phoneNumberOrderId' - The phone number order ID.
--
-- 'productType', 'phoneNumberOrder_productType' - The phone number order product type.
--
-- 'status', 'phoneNumberOrder_status' - The status of the phone number order.
--
-- 'updatedTimestamp', 'phoneNumberOrder_updatedTimestamp' - The updated phone number order time stamp, in ISO 8601 format.
newPhoneNumberOrder ::
  PhoneNumberOrder
newPhoneNumberOrder =
  PhoneNumberOrder'
    { createdTimestamp =
        Prelude.Nothing,
      orderedPhoneNumbers = Prelude.Nothing,
      phoneNumberOrderId = Prelude.Nothing,
      productType = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing
    }

-- | The phone number order creation time stamp, in ISO 8601 format.
phoneNumberOrder_createdTimestamp :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe Prelude.UTCTime)
phoneNumberOrder_createdTimestamp = Lens.lens (\PhoneNumberOrder' {createdTimestamp} -> createdTimestamp) (\s@PhoneNumberOrder' {} a -> s {createdTimestamp = a} :: PhoneNumberOrder) Prelude.. Lens.mapping Data._Time

-- | The ordered phone number details, such as the phone number in E.164
-- format and the phone number status.
phoneNumberOrder_orderedPhoneNumbers :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe [OrderedPhoneNumber])
phoneNumberOrder_orderedPhoneNumbers = Lens.lens (\PhoneNumberOrder' {orderedPhoneNumbers} -> orderedPhoneNumbers) (\s@PhoneNumberOrder' {} a -> s {orderedPhoneNumbers = a} :: PhoneNumberOrder) Prelude.. Lens.mapping Lens.coerced

-- | The phone number order ID.
phoneNumberOrder_phoneNumberOrderId :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe Prelude.Text)
phoneNumberOrder_phoneNumberOrderId = Lens.lens (\PhoneNumberOrder' {phoneNumberOrderId} -> phoneNumberOrderId) (\s@PhoneNumberOrder' {} a -> s {phoneNumberOrderId = a} :: PhoneNumberOrder)

-- | The phone number order product type.
phoneNumberOrder_productType :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe PhoneNumberProductType)
phoneNumberOrder_productType = Lens.lens (\PhoneNumberOrder' {productType} -> productType) (\s@PhoneNumberOrder' {} a -> s {productType = a} :: PhoneNumberOrder)

-- | The status of the phone number order.
phoneNumberOrder_status :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe PhoneNumberOrderStatus)
phoneNumberOrder_status = Lens.lens (\PhoneNumberOrder' {status} -> status) (\s@PhoneNumberOrder' {} a -> s {status = a} :: PhoneNumberOrder)

-- | The updated phone number order time stamp, in ISO 8601 format.
phoneNumberOrder_updatedTimestamp :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe Prelude.UTCTime)
phoneNumberOrder_updatedTimestamp = Lens.lens (\PhoneNumberOrder' {updatedTimestamp} -> updatedTimestamp) (\s@PhoneNumberOrder' {} a -> s {updatedTimestamp = a} :: PhoneNumberOrder) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON PhoneNumberOrder where
  parseJSON =
    Data.withObject
      "PhoneNumberOrder"
      ( \x ->
          PhoneNumberOrder'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> ( x Data..:? "OrderedPhoneNumbers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PhoneNumberOrderId")
            Prelude.<*> (x Data..:? "ProductType")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
      )

instance Prelude.Hashable PhoneNumberOrder where
  hashWithSalt _salt PhoneNumberOrder' {..} =
    _salt `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` orderedPhoneNumbers
      `Prelude.hashWithSalt` phoneNumberOrderId
      `Prelude.hashWithSalt` productType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updatedTimestamp

instance Prelude.NFData PhoneNumberOrder where
  rnf PhoneNumberOrder' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf orderedPhoneNumbers
      `Prelude.seq` Prelude.rnf phoneNumberOrderId
      `Prelude.seq` Prelude.rnf productType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedTimestamp
