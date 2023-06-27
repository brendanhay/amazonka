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
-- Module      : Amazonka.VerifiedPermissions.Types.PolicyItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.PolicyItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.EntityIdentifier
import Amazonka.VerifiedPermissions.Types.PolicyDefinitionItem
import Amazonka.VerifiedPermissions.Types.PolicyType

-- | Contains information about a policy.
--
-- This data type is used as a response parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_ListPolicies.html ListPolicies>
-- operation.
--
-- /See:/ 'newPolicyItem' smart constructor.
data PolicyItem = PolicyItem'
  { -- | The principal associated with the policy.
    principal :: Prelude.Maybe EntityIdentifier,
    -- | The resource associated with the policy.
    resource :: Prelude.Maybe EntityIdentifier,
    -- | The identifier of the PolicyStore where the policy you want information
    -- about is stored.
    policyStoreId :: Prelude.Text,
    -- | The identifier of the policy you want information about.
    policyId :: Prelude.Text,
    -- | The type of the policy. This is one of the following values:
    --
    -- -   @static@
    --
    -- -   @templateLinked@
    policyType :: PolicyType,
    -- | The policy definition of an item in the list of policies returned.
    definition :: PolicyDefinitionItem,
    -- | The date and time the policy was created.
    createdDate :: Data.ISO8601,
    -- | The date and time the policy was most recently updated.
    lastUpdatedDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'policyItem_principal' - The principal associated with the policy.
--
-- 'resource', 'policyItem_resource' - The resource associated with the policy.
--
-- 'policyStoreId', 'policyItem_policyStoreId' - The identifier of the PolicyStore where the policy you want information
-- about is stored.
--
-- 'policyId', 'policyItem_policyId' - The identifier of the policy you want information about.
--
-- 'policyType', 'policyItem_policyType' - The type of the policy. This is one of the following values:
--
-- -   @static@
--
-- -   @templateLinked@
--
-- 'definition', 'policyItem_definition' - The policy definition of an item in the list of policies returned.
--
-- 'createdDate', 'policyItem_createdDate' - The date and time the policy was created.
--
-- 'lastUpdatedDate', 'policyItem_lastUpdatedDate' - The date and time the policy was most recently updated.
newPolicyItem ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'policyId'
  Prelude.Text ->
  -- | 'policyType'
  PolicyType ->
  -- | 'definition'
  PolicyDefinitionItem ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  PolicyItem
newPolicyItem
  pPolicyStoreId_
  pPolicyId_
  pPolicyType_
  pDefinition_
  pCreatedDate_
  pLastUpdatedDate_ =
    PolicyItem'
      { principal = Prelude.Nothing,
        resource = Prelude.Nothing,
        policyStoreId = pPolicyStoreId_,
        policyId = pPolicyId_,
        policyType = pPolicyType_,
        definition = pDefinition_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_
      }

-- | The principal associated with the policy.
policyItem_principal :: Lens.Lens' PolicyItem (Prelude.Maybe EntityIdentifier)
policyItem_principal = Lens.lens (\PolicyItem' {principal} -> principal) (\s@PolicyItem' {} a -> s {principal = a} :: PolicyItem)

-- | The resource associated with the policy.
policyItem_resource :: Lens.Lens' PolicyItem (Prelude.Maybe EntityIdentifier)
policyItem_resource = Lens.lens (\PolicyItem' {resource} -> resource) (\s@PolicyItem' {} a -> s {resource = a} :: PolicyItem)

-- | The identifier of the PolicyStore where the policy you want information
-- about is stored.
policyItem_policyStoreId :: Lens.Lens' PolicyItem Prelude.Text
policyItem_policyStoreId = Lens.lens (\PolicyItem' {policyStoreId} -> policyStoreId) (\s@PolicyItem' {} a -> s {policyStoreId = a} :: PolicyItem)

-- | The identifier of the policy you want information about.
policyItem_policyId :: Lens.Lens' PolicyItem Prelude.Text
policyItem_policyId = Lens.lens (\PolicyItem' {policyId} -> policyId) (\s@PolicyItem' {} a -> s {policyId = a} :: PolicyItem)

-- | The type of the policy. This is one of the following values:
--
-- -   @static@
--
-- -   @templateLinked@
policyItem_policyType :: Lens.Lens' PolicyItem PolicyType
policyItem_policyType = Lens.lens (\PolicyItem' {policyType} -> policyType) (\s@PolicyItem' {} a -> s {policyType = a} :: PolicyItem)

-- | The policy definition of an item in the list of policies returned.
policyItem_definition :: Lens.Lens' PolicyItem PolicyDefinitionItem
policyItem_definition = Lens.lens (\PolicyItem' {definition} -> definition) (\s@PolicyItem' {} a -> s {definition = a} :: PolicyItem)

-- | The date and time the policy was created.
policyItem_createdDate :: Lens.Lens' PolicyItem Prelude.UTCTime
policyItem_createdDate = Lens.lens (\PolicyItem' {createdDate} -> createdDate) (\s@PolicyItem' {} a -> s {createdDate = a} :: PolicyItem) Prelude.. Data._Time

-- | The date and time the policy was most recently updated.
policyItem_lastUpdatedDate :: Lens.Lens' PolicyItem Prelude.UTCTime
policyItem_lastUpdatedDate = Lens.lens (\PolicyItem' {lastUpdatedDate} -> lastUpdatedDate) (\s@PolicyItem' {} a -> s {lastUpdatedDate = a} :: PolicyItem) Prelude.. Data._Time

instance Data.FromJSON PolicyItem where
  parseJSON =
    Data.withObject
      "PolicyItem"
      ( \x ->
          PolicyItem'
            Prelude.<$> (x Data..:? "principal")
            Prelude.<*> (x Data..:? "resource")
            Prelude.<*> (x Data..: "policyStoreId")
            Prelude.<*> (x Data..: "policyId")
            Prelude.<*> (x Data..: "policyType")
            Prelude.<*> (x Data..: "definition")
            Prelude.<*> (x Data..: "createdDate")
            Prelude.<*> (x Data..: "lastUpdatedDate")
      )

instance Prelude.Hashable PolicyItem where
  hashWithSalt _salt PolicyItem' {..} =
    _salt
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` policyType
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` lastUpdatedDate

instance Prelude.NFData PolicyItem where
  rnf PolicyItem' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
