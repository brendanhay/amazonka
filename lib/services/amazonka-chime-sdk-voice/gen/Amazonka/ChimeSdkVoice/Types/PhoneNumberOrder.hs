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
-- Module      : Amazonka.ChimeSdkVoice.Types.PhoneNumberOrder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.PhoneNumberOrder where

import Amazonka.ChimeSdkVoice.Types.OrderedPhoneNumber
import Amazonka.ChimeSdkVoice.Types.PhoneNumberOrderStatus
import Amazonka.ChimeSdkVoice.Types.PhoneNumberOrderType
import Amazonka.ChimeSdkVoice.Types.PhoneNumberProductType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newPhoneNumberOrder' smart constructor.
data PhoneNumberOrder = PhoneNumberOrder'
  { createdTimestamp :: Prelude.Maybe Data.ISO8601,
    orderType :: Prelude.Maybe PhoneNumberOrderType,
    orderedPhoneNumbers :: Prelude.Maybe [OrderedPhoneNumber],
    phoneNumberOrderId :: Prelude.Maybe Prelude.Text,
    productType :: Prelude.Maybe PhoneNumberProductType,
    status :: Prelude.Maybe PhoneNumberOrderStatus,
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
-- 'createdTimestamp', 'phoneNumberOrder_createdTimestamp' - Undocumented member.
--
-- 'orderType', 'phoneNumberOrder_orderType' - Undocumented member.
--
-- 'orderedPhoneNumbers', 'phoneNumberOrder_orderedPhoneNumbers' - Undocumented member.
--
-- 'phoneNumberOrderId', 'phoneNumberOrder_phoneNumberOrderId' - Undocumented member.
--
-- 'productType', 'phoneNumberOrder_productType' - Undocumented member.
--
-- 'status', 'phoneNumberOrder_status' - Undocumented member.
--
-- 'updatedTimestamp', 'phoneNumberOrder_updatedTimestamp' - Undocumented member.
newPhoneNumberOrder ::
  PhoneNumberOrder
newPhoneNumberOrder =
  PhoneNumberOrder'
    { createdTimestamp =
        Prelude.Nothing,
      orderType = Prelude.Nothing,
      orderedPhoneNumbers = Prelude.Nothing,
      phoneNumberOrderId = Prelude.Nothing,
      productType = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing
    }

-- | Undocumented member.
phoneNumberOrder_createdTimestamp :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe Prelude.UTCTime)
phoneNumberOrder_createdTimestamp = Lens.lens (\PhoneNumberOrder' {createdTimestamp} -> createdTimestamp) (\s@PhoneNumberOrder' {} a -> s {createdTimestamp = a} :: PhoneNumberOrder) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
phoneNumberOrder_orderType :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe PhoneNumberOrderType)
phoneNumberOrder_orderType = Lens.lens (\PhoneNumberOrder' {orderType} -> orderType) (\s@PhoneNumberOrder' {} a -> s {orderType = a} :: PhoneNumberOrder)

-- | Undocumented member.
phoneNumberOrder_orderedPhoneNumbers :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe [OrderedPhoneNumber])
phoneNumberOrder_orderedPhoneNumbers = Lens.lens (\PhoneNumberOrder' {orderedPhoneNumbers} -> orderedPhoneNumbers) (\s@PhoneNumberOrder' {} a -> s {orderedPhoneNumbers = a} :: PhoneNumberOrder) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
phoneNumberOrder_phoneNumberOrderId :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe Prelude.Text)
phoneNumberOrder_phoneNumberOrderId = Lens.lens (\PhoneNumberOrder' {phoneNumberOrderId} -> phoneNumberOrderId) (\s@PhoneNumberOrder' {} a -> s {phoneNumberOrderId = a} :: PhoneNumberOrder)

-- | Undocumented member.
phoneNumberOrder_productType :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe PhoneNumberProductType)
phoneNumberOrder_productType = Lens.lens (\PhoneNumberOrder' {productType} -> productType) (\s@PhoneNumberOrder' {} a -> s {productType = a} :: PhoneNumberOrder)

-- | Undocumented member.
phoneNumberOrder_status :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe PhoneNumberOrderStatus)
phoneNumberOrder_status = Lens.lens (\PhoneNumberOrder' {status} -> status) (\s@PhoneNumberOrder' {} a -> s {status = a} :: PhoneNumberOrder)

-- | Undocumented member.
phoneNumberOrder_updatedTimestamp :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe Prelude.UTCTime)
phoneNumberOrder_updatedTimestamp = Lens.lens (\PhoneNumberOrder' {updatedTimestamp} -> updatedTimestamp) (\s@PhoneNumberOrder' {} a -> s {updatedTimestamp = a} :: PhoneNumberOrder) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON PhoneNumberOrder where
  parseJSON =
    Data.withObject
      "PhoneNumberOrder"
      ( \x ->
          PhoneNumberOrder'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "OrderType")
            Prelude.<*> ( x
                            Data..:? "OrderedPhoneNumbers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PhoneNumberOrderId")
            Prelude.<*> (x Data..:? "ProductType")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
      )

instance Prelude.Hashable PhoneNumberOrder where
  hashWithSalt _salt PhoneNumberOrder' {..} =
    _salt
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` orderType
      `Prelude.hashWithSalt` orderedPhoneNumbers
      `Prelude.hashWithSalt` phoneNumberOrderId
      `Prelude.hashWithSalt` productType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updatedTimestamp

instance Prelude.NFData PhoneNumberOrder where
  rnf PhoneNumberOrder' {..} =
    Prelude.rnf createdTimestamp `Prelude.seq`
      Prelude.rnf orderType `Prelude.seq`
        Prelude.rnf orderedPhoneNumbers `Prelude.seq`
          Prelude.rnf phoneNumberOrderId `Prelude.seq`
            Prelude.rnf productType `Prelude.seq`
              Prelude.rnf status `Prelude.seq`
                Prelude.rnf updatedTimestamp
