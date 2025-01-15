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
-- Module      : Amazonka.DynamoDB.Types.UpdateReplicationGroupMemberAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.UpdateReplicationGroupMemberAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ProvisionedThroughputOverride
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndex
import Amazonka.DynamoDB.Types.TableClass
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents a replica to be modified.
--
-- /See:/ 'newUpdateReplicationGroupMemberAction' smart constructor.
data UpdateReplicationGroupMemberAction = UpdateReplicationGroupMemberAction'
  { -- | Replica-specific global secondary index settings.
    globalSecondaryIndexes :: Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndex),
    -- | The KMS key of the replica that should be used for KMS encryption. To
    -- specify a key, use its key ID, Amazon Resource Name (ARN), alias name,
    -- or alias ARN. Note that you should only provide this parameter if the
    -- key is different from the default DynamoDB KMS key
    -- @alias\/aws\/dynamodb@.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | Replica-specific provisioned throughput. If not specified, uses the
    -- source table\'s provisioned throughput settings.
    provisionedThroughputOverride :: Prelude.Maybe ProvisionedThroughputOverride,
    -- | Replica-specific table class. If not specified, uses the source table\'s
    -- table class.
    tableClassOverride :: Prelude.Maybe TableClass,
    -- | The Region where the replica exists.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReplicationGroupMemberAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalSecondaryIndexes', 'updateReplicationGroupMemberAction_globalSecondaryIndexes' - Replica-specific global secondary index settings.
--
-- 'kmsMasterKeyId', 'updateReplicationGroupMemberAction_kmsMasterKeyId' - The KMS key of the replica that should be used for KMS encryption. To
-- specify a key, use its key ID, Amazon Resource Name (ARN), alias name,
-- or alias ARN. Note that you should only provide this parameter if the
-- key is different from the default DynamoDB KMS key
-- @alias\/aws\/dynamodb@.
--
-- 'provisionedThroughputOverride', 'updateReplicationGroupMemberAction_provisionedThroughputOverride' - Replica-specific provisioned throughput. If not specified, uses the
-- source table\'s provisioned throughput settings.
--
-- 'tableClassOverride', 'updateReplicationGroupMemberAction_tableClassOverride' - Replica-specific table class. If not specified, uses the source table\'s
-- table class.
--
-- 'regionName', 'updateReplicationGroupMemberAction_regionName' - The Region where the replica exists.
newUpdateReplicationGroupMemberAction ::
  -- | 'regionName'
  Prelude.Text ->
  UpdateReplicationGroupMemberAction
newUpdateReplicationGroupMemberAction pRegionName_ =
  UpdateReplicationGroupMemberAction'
    { globalSecondaryIndexes =
        Prelude.Nothing,
      kmsMasterKeyId = Prelude.Nothing,
      provisionedThroughputOverride =
        Prelude.Nothing,
      tableClassOverride = Prelude.Nothing,
      regionName = pRegionName_
    }

-- | Replica-specific global secondary index settings.
updateReplicationGroupMemberAction_globalSecondaryIndexes :: Lens.Lens' UpdateReplicationGroupMemberAction (Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndex))
updateReplicationGroupMemberAction_globalSecondaryIndexes = Lens.lens (\UpdateReplicationGroupMemberAction' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@UpdateReplicationGroupMemberAction' {} a -> s {globalSecondaryIndexes = a} :: UpdateReplicationGroupMemberAction) Prelude.. Lens.mapping Lens.coerced

-- | The KMS key of the replica that should be used for KMS encryption. To
-- specify a key, use its key ID, Amazon Resource Name (ARN), alias name,
-- or alias ARN. Note that you should only provide this parameter if the
-- key is different from the default DynamoDB KMS key
-- @alias\/aws\/dynamodb@.
updateReplicationGroupMemberAction_kmsMasterKeyId :: Lens.Lens' UpdateReplicationGroupMemberAction (Prelude.Maybe Prelude.Text)
updateReplicationGroupMemberAction_kmsMasterKeyId = Lens.lens (\UpdateReplicationGroupMemberAction' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@UpdateReplicationGroupMemberAction' {} a -> s {kmsMasterKeyId = a} :: UpdateReplicationGroupMemberAction)

-- | Replica-specific provisioned throughput. If not specified, uses the
-- source table\'s provisioned throughput settings.
updateReplicationGroupMemberAction_provisionedThroughputOverride :: Lens.Lens' UpdateReplicationGroupMemberAction (Prelude.Maybe ProvisionedThroughputOverride)
updateReplicationGroupMemberAction_provisionedThroughputOverride = Lens.lens (\UpdateReplicationGroupMemberAction' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@UpdateReplicationGroupMemberAction' {} a -> s {provisionedThroughputOverride = a} :: UpdateReplicationGroupMemberAction)

-- | Replica-specific table class. If not specified, uses the source table\'s
-- table class.
updateReplicationGroupMemberAction_tableClassOverride :: Lens.Lens' UpdateReplicationGroupMemberAction (Prelude.Maybe TableClass)
updateReplicationGroupMemberAction_tableClassOverride = Lens.lens (\UpdateReplicationGroupMemberAction' {tableClassOverride} -> tableClassOverride) (\s@UpdateReplicationGroupMemberAction' {} a -> s {tableClassOverride = a} :: UpdateReplicationGroupMemberAction)

-- | The Region where the replica exists.
updateReplicationGroupMemberAction_regionName :: Lens.Lens' UpdateReplicationGroupMemberAction Prelude.Text
updateReplicationGroupMemberAction_regionName = Lens.lens (\UpdateReplicationGroupMemberAction' {regionName} -> regionName) (\s@UpdateReplicationGroupMemberAction' {} a -> s {regionName = a} :: UpdateReplicationGroupMemberAction)

instance
  Prelude.Hashable
    UpdateReplicationGroupMemberAction
  where
  hashWithSalt
    _salt
    UpdateReplicationGroupMemberAction' {..} =
      _salt
        `Prelude.hashWithSalt` globalSecondaryIndexes
        `Prelude.hashWithSalt` kmsMasterKeyId
        `Prelude.hashWithSalt` provisionedThroughputOverride
        `Prelude.hashWithSalt` tableClassOverride
        `Prelude.hashWithSalt` regionName

instance
  Prelude.NFData
    UpdateReplicationGroupMemberAction
  where
  rnf UpdateReplicationGroupMemberAction' {..} =
    Prelude.rnf globalSecondaryIndexes `Prelude.seq`
      Prelude.rnf kmsMasterKeyId `Prelude.seq`
        Prelude.rnf provisionedThroughputOverride `Prelude.seq`
          Prelude.rnf tableClassOverride `Prelude.seq`
            Prelude.rnf regionName

instance
  Data.ToJSON
    UpdateReplicationGroupMemberAction
  where
  toJSON UpdateReplicationGroupMemberAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GlobalSecondaryIndexes" Data..=)
              Prelude.<$> globalSecondaryIndexes,
            ("KMSMasterKeyId" Data..=)
              Prelude.<$> kmsMasterKeyId,
            ("ProvisionedThroughputOverride" Data..=)
              Prelude.<$> provisionedThroughputOverride,
            ("TableClassOverride" Data..=)
              Prelude.<$> tableClassOverride,
            Prelude.Just ("RegionName" Data..= regionName)
          ]
      )
