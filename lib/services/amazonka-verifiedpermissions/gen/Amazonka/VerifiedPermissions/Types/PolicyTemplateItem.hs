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
-- Module      : Amazonka.VerifiedPermissions.Types.PolicyTemplateItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.PolicyTemplateItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a policy template
--
-- This data type is used as a response parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_ListPolicyTemplates.html ListPolicyTemplates>
-- operation.
--
-- /See:/ 'newPolicyTemplateItem' smart constructor.
data PolicyTemplateItem = PolicyTemplateItem'
  { -- | The description attached to the policy template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the policy store that contains the template.
    policyStoreId :: Prelude.Text,
    -- | The unique identifier of the policy template.
    policyTemplateId :: Prelude.Text,
    -- | The date and time that the policy template was created.
    createdDate :: Data.ISO8601,
    -- | The date and time that the policy template was most recently updated.
    lastUpdatedDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyTemplateItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'policyTemplateItem_description' - The description attached to the policy template.
--
-- 'policyStoreId', 'policyTemplateItem_policyStoreId' - The unique identifier of the policy store that contains the template.
--
-- 'policyTemplateId', 'policyTemplateItem_policyTemplateId' - The unique identifier of the policy template.
--
-- 'createdDate', 'policyTemplateItem_createdDate' - The date and time that the policy template was created.
--
-- 'lastUpdatedDate', 'policyTemplateItem_lastUpdatedDate' - The date and time that the policy template was most recently updated.
newPolicyTemplateItem ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'policyTemplateId'
  Prelude.Text ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  PolicyTemplateItem
newPolicyTemplateItem
  pPolicyStoreId_
  pPolicyTemplateId_
  pCreatedDate_
  pLastUpdatedDate_ =
    PolicyTemplateItem'
      { description = Prelude.Nothing,
        policyStoreId = pPolicyStoreId_,
        policyTemplateId = pPolicyTemplateId_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_
      }

-- | The description attached to the policy template.
policyTemplateItem_description :: Lens.Lens' PolicyTemplateItem (Prelude.Maybe Prelude.Text)
policyTemplateItem_description = Lens.lens (\PolicyTemplateItem' {description} -> description) (\s@PolicyTemplateItem' {} a -> s {description = a} :: PolicyTemplateItem)

-- | The unique identifier of the policy store that contains the template.
policyTemplateItem_policyStoreId :: Lens.Lens' PolicyTemplateItem Prelude.Text
policyTemplateItem_policyStoreId = Lens.lens (\PolicyTemplateItem' {policyStoreId} -> policyStoreId) (\s@PolicyTemplateItem' {} a -> s {policyStoreId = a} :: PolicyTemplateItem)

-- | The unique identifier of the policy template.
policyTemplateItem_policyTemplateId :: Lens.Lens' PolicyTemplateItem Prelude.Text
policyTemplateItem_policyTemplateId = Lens.lens (\PolicyTemplateItem' {policyTemplateId} -> policyTemplateId) (\s@PolicyTemplateItem' {} a -> s {policyTemplateId = a} :: PolicyTemplateItem)

-- | The date and time that the policy template was created.
policyTemplateItem_createdDate :: Lens.Lens' PolicyTemplateItem Prelude.UTCTime
policyTemplateItem_createdDate = Lens.lens (\PolicyTemplateItem' {createdDate} -> createdDate) (\s@PolicyTemplateItem' {} a -> s {createdDate = a} :: PolicyTemplateItem) Prelude.. Data._Time

-- | The date and time that the policy template was most recently updated.
policyTemplateItem_lastUpdatedDate :: Lens.Lens' PolicyTemplateItem Prelude.UTCTime
policyTemplateItem_lastUpdatedDate = Lens.lens (\PolicyTemplateItem' {lastUpdatedDate} -> lastUpdatedDate) (\s@PolicyTemplateItem' {} a -> s {lastUpdatedDate = a} :: PolicyTemplateItem) Prelude.. Data._Time

instance Data.FromJSON PolicyTemplateItem where
  parseJSON =
    Data.withObject
      "PolicyTemplateItem"
      ( \x ->
          PolicyTemplateItem'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..: "policyStoreId")
            Prelude.<*> (x Data..: "policyTemplateId")
            Prelude.<*> (x Data..: "createdDate")
            Prelude.<*> (x Data..: "lastUpdatedDate")
      )

instance Prelude.Hashable PolicyTemplateItem where
  hashWithSalt _salt PolicyTemplateItem' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` policyTemplateId
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` lastUpdatedDate

instance Prelude.NFData PolicyTemplateItem where
  rnf PolicyTemplateItem' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyTemplateId
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
