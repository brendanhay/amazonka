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
-- Module      : Amazonka.Chime.Types.UpdatePhoneNumberRequestItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.UpdatePhoneNumberRequestItem where

import Amazonka.Chime.Types.PhoneNumberProductType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The phone number ID, product type, or calling name fields to update,
-- used with the BatchUpdatePhoneNumber and UpdatePhoneNumber actions.
--
-- /See:/ 'newUpdatePhoneNumberRequestItem' smart constructor.
data UpdatePhoneNumberRequestItem = UpdatePhoneNumberRequestItem'
  { -- | The product type to update.
    productType :: Prelude.Maybe PhoneNumberProductType,
    -- | The outbound calling name to update.
    callingName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The phone number ID to update.
    phoneNumberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePhoneNumberRequestItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productType', 'updatePhoneNumberRequestItem_productType' - The product type to update.
--
-- 'callingName', 'updatePhoneNumberRequestItem_callingName' - The outbound calling name to update.
--
-- 'phoneNumberId', 'updatePhoneNumberRequestItem_phoneNumberId' - The phone number ID to update.
newUpdatePhoneNumberRequestItem ::
  -- | 'phoneNumberId'
  Prelude.Text ->
  UpdatePhoneNumberRequestItem
newUpdatePhoneNumberRequestItem pPhoneNumberId_ =
  UpdatePhoneNumberRequestItem'
    { productType =
        Prelude.Nothing,
      callingName = Prelude.Nothing,
      phoneNumberId = pPhoneNumberId_
    }

-- | The product type to update.
updatePhoneNumberRequestItem_productType :: Lens.Lens' UpdatePhoneNumberRequestItem (Prelude.Maybe PhoneNumberProductType)
updatePhoneNumberRequestItem_productType = Lens.lens (\UpdatePhoneNumberRequestItem' {productType} -> productType) (\s@UpdatePhoneNumberRequestItem' {} a -> s {productType = a} :: UpdatePhoneNumberRequestItem)

-- | The outbound calling name to update.
updatePhoneNumberRequestItem_callingName :: Lens.Lens' UpdatePhoneNumberRequestItem (Prelude.Maybe Prelude.Text)
updatePhoneNumberRequestItem_callingName = Lens.lens (\UpdatePhoneNumberRequestItem' {callingName} -> callingName) (\s@UpdatePhoneNumberRequestItem' {} a -> s {callingName = a} :: UpdatePhoneNumberRequestItem) Prelude.. Lens.mapping Core._Sensitive

-- | The phone number ID to update.
updatePhoneNumberRequestItem_phoneNumberId :: Lens.Lens' UpdatePhoneNumberRequestItem Prelude.Text
updatePhoneNumberRequestItem_phoneNumberId = Lens.lens (\UpdatePhoneNumberRequestItem' {phoneNumberId} -> phoneNumberId) (\s@UpdatePhoneNumberRequestItem' {} a -> s {phoneNumberId = a} :: UpdatePhoneNumberRequestItem)

instance
  Prelude.Hashable
    UpdatePhoneNumberRequestItem
  where
  hashWithSalt _salt UpdatePhoneNumberRequestItem' {..} =
    _salt `Prelude.hashWithSalt` productType
      `Prelude.hashWithSalt` callingName
      `Prelude.hashWithSalt` phoneNumberId

instance Prelude.NFData UpdatePhoneNumberRequestItem where
  rnf UpdatePhoneNumberRequestItem' {..} =
    Prelude.rnf productType
      `Prelude.seq` Prelude.rnf callingName
      `Prelude.seq` Prelude.rnf phoneNumberId

instance Core.ToJSON UpdatePhoneNumberRequestItem where
  toJSON UpdatePhoneNumberRequestItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProductType" Core..=) Prelude.<$> productType,
            ("CallingName" Core..=) Prelude.<$> callingName,
            Prelude.Just
              ("PhoneNumberId" Core..= phoneNumberId)
          ]
      )
