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
-- Module      : Amazonka.VerifiedPermissions.Types.PolicyStoreItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.PolicyStoreItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a policy store.
--
-- This data type is used as a response parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_ListPolicyStores.html ListPolicyStores>
-- operation.
--
-- /See:/ 'newPolicyStoreItem' smart constructor.
data PolicyStoreItem = PolicyStoreItem'
  { -- | The unique identifier of the policy store.
    policyStoreId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the policy store.
    arn :: Prelude.Text,
    -- | The date and time the policy was created.
    createdDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyStoreItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStoreId', 'policyStoreItem_policyStoreId' - The unique identifier of the policy store.
--
-- 'arn', 'policyStoreItem_arn' - The Amazon Resource Name (ARN) of the policy store.
--
-- 'createdDate', 'policyStoreItem_createdDate' - The date and time the policy was created.
newPolicyStoreItem ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  PolicyStoreItem
newPolicyStoreItem
  pPolicyStoreId_
  pArn_
  pCreatedDate_ =
    PolicyStoreItem'
      { policyStoreId = pPolicyStoreId_,
        arn = pArn_,
        createdDate = Data._Time Lens.# pCreatedDate_
      }

-- | The unique identifier of the policy store.
policyStoreItem_policyStoreId :: Lens.Lens' PolicyStoreItem Prelude.Text
policyStoreItem_policyStoreId = Lens.lens (\PolicyStoreItem' {policyStoreId} -> policyStoreId) (\s@PolicyStoreItem' {} a -> s {policyStoreId = a} :: PolicyStoreItem)

-- | The Amazon Resource Name (ARN) of the policy store.
policyStoreItem_arn :: Lens.Lens' PolicyStoreItem Prelude.Text
policyStoreItem_arn = Lens.lens (\PolicyStoreItem' {arn} -> arn) (\s@PolicyStoreItem' {} a -> s {arn = a} :: PolicyStoreItem)

-- | The date and time the policy was created.
policyStoreItem_createdDate :: Lens.Lens' PolicyStoreItem Prelude.UTCTime
policyStoreItem_createdDate = Lens.lens (\PolicyStoreItem' {createdDate} -> createdDate) (\s@PolicyStoreItem' {} a -> s {createdDate = a} :: PolicyStoreItem) Prelude.. Data._Time

instance Data.FromJSON PolicyStoreItem where
  parseJSON =
    Data.withObject
      "PolicyStoreItem"
      ( \x ->
          PolicyStoreItem'
            Prelude.<$> (x Data..: "policyStoreId")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdDate")
      )

instance Prelude.Hashable PolicyStoreItem where
  hashWithSalt _salt PolicyStoreItem' {..} =
    _salt
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdDate

instance Prelude.NFData PolicyStoreItem where
  rnf PolicyStoreItem' {..} =
    Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdDate
