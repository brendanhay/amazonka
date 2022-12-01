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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.UpdateReplicationGroupMemberAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ProvisionedThroughputOverride
import Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndex
import Amazonka.DynamoDB.Types.TableClass
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents a replica to be modified.
--
-- /See:/ 'newUpdateReplicationGroupMemberAction' smart constructor.
data UpdateReplicationGroupMemberAction = UpdateReplicationGroupMemberAction'
  { -- | The KMS key of the replica that should be used for KMS encryption. To
    -- specify a key, use its key ID, Amazon Resource Name (ARN), alias name,
    -- or alias ARN. Note that you should only provide this parameter if the
    -- key is different from the default DynamoDB KMS key
    -- @alias\/aws\/dynamodb@.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | Replica-specific table class. If not specified, uses the source table\'s
    -- table class.
    tableClassOverride :: Prelude.Maybe TableClass,
    -- | Replica-specific provisioned throughput. If not specified, uses the
    -- source table\'s provisioned throughput settings.
    provisionedThroughputOverride :: Prelude.Maybe ProvisionedThroughputOverride,
    -- | Replica-specific global secondary index settings.
    globalSecondaryIndexes :: Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndex),
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
-- 'kmsMasterKeyId', 'updateReplicationGroupMemberAction_kmsMasterKeyId' - The KMS key of the replica that should be used for KMS encryption. To
-- specify a key, use its key ID, Amazon Resource Name (ARN), alias name,
-- or alias ARN. Note that you should only provide this parameter if the
-- key is different from the default DynamoDB KMS key
-- @alias\/aws\/dynamodb@.
--
-- 'tableClassOverride', 'updateReplicationGroupMemberAction_tableClassOverride' - Replica-specific table class. If not specified, uses the source table\'s
-- table class.
--
-- 'provisionedThroughputOverride', 'updateReplicationGroupMemberAction_provisionedThroughputOverride' - Replica-specific provisioned throughput. If not specified, uses the
-- source table\'s provisioned throughput settings.
--
-- 'globalSecondaryIndexes', 'updateReplicationGroupMemberAction_globalSecondaryIndexes' - Replica-specific global secondary index settings.
--
-- 'regionName', 'updateReplicationGroupMemberAction_regionName' - The Region where the replica exists.
newUpdateReplicationGroupMemberAction ::
  -- | 'regionName'
  Prelude.Text ->
  UpdateReplicationGroupMemberAction
newUpdateReplicationGroupMemberAction pRegionName_ =
  UpdateReplicationGroupMemberAction'
    { kmsMasterKeyId =
        Prelude.Nothing,
      tableClassOverride = Prelude.Nothing,
      provisionedThroughputOverride =
        Prelude.Nothing,
      globalSecondaryIndexes =
        Prelude.Nothing,
      regionName = pRegionName_
    }

-- | The KMS key of the replica that should be used for KMS encryption. To
-- specify a key, use its key ID, Amazon Resource Name (ARN), alias name,
-- or alias ARN. Note that you should only provide this parameter if the
-- key is different from the default DynamoDB KMS key
-- @alias\/aws\/dynamodb@.
updateReplicationGroupMemberAction_kmsMasterKeyId :: Lens.Lens' UpdateReplicationGroupMemberAction (Prelude.Maybe Prelude.Text)
updateReplicationGroupMemberAction_kmsMasterKeyId = Lens.lens (\UpdateReplicationGroupMemberAction' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@UpdateReplicationGroupMemberAction' {} a -> s {kmsMasterKeyId = a} :: UpdateReplicationGroupMemberAction)

-- | Replica-specific table class. If not specified, uses the source table\'s
-- table class.
updateReplicationGroupMemberAction_tableClassOverride :: Lens.Lens' UpdateReplicationGroupMemberAction (Prelude.Maybe TableClass)
updateReplicationGroupMemberAction_tableClassOverride = Lens.lens (\UpdateReplicationGroupMemberAction' {tableClassOverride} -> tableClassOverride) (\s@UpdateReplicationGroupMemberAction' {} a -> s {tableClassOverride = a} :: UpdateReplicationGroupMemberAction)

-- | Replica-specific provisioned throughput. If not specified, uses the
-- source table\'s provisioned throughput settings.
updateReplicationGroupMemberAction_provisionedThroughputOverride :: Lens.Lens' UpdateReplicationGroupMemberAction (Prelude.Maybe ProvisionedThroughputOverride)
updateReplicationGroupMemberAction_provisionedThroughputOverride = Lens.lens (\UpdateReplicationGroupMemberAction' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@UpdateReplicationGroupMemberAction' {} a -> s {provisionedThroughputOverride = a} :: UpdateReplicationGroupMemberAction)

-- | Replica-specific global secondary index settings.
updateReplicationGroupMemberAction_globalSecondaryIndexes :: Lens.Lens' UpdateReplicationGroupMemberAction (Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndex))
updateReplicationGroupMemberAction_globalSecondaryIndexes = Lens.lens (\UpdateReplicationGroupMemberAction' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@UpdateReplicationGroupMemberAction' {} a -> s {globalSecondaryIndexes = a} :: UpdateReplicationGroupMemberAction) Prelude.. Lens.mapping Lens.coerced

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
      _salt `Prelude.hashWithSalt` kmsMasterKeyId
        `Prelude.hashWithSalt` tableClassOverride
        `Prelude.hashWithSalt` provisionedThroughputOverride
        `Prelude.hashWithSalt` globalSecondaryIndexes
        `Prelude.hashWithSalt` regionName

instance
  Prelude.NFData
    UpdateReplicationGroupMemberAction
  where
  rnf UpdateReplicationGroupMemberAction' {..} =
    Prelude.rnf kmsMasterKeyId
      `Prelude.seq` Prelude.rnf tableClassOverride
      `Prelude.seq` Prelude.rnf provisionedThroughputOverride
      `Prelude.seq` Prelude.rnf globalSecondaryIndexes
      `Prelude.seq` Prelude.rnf regionName

instance
  Core.ToJSON
    UpdateReplicationGroupMemberAction
  where
  toJSON UpdateReplicationGroupMemberAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KMSMasterKeyId" Core..=)
              Prelude.<$> kmsMasterKeyId,
            ("TableClassOverride" Core..=)
              Prelude.<$> tableClassOverride,
            ("ProvisionedThroughputOverride" Core..=)
              Prelude.<$> provisionedThroughputOverride,
            ("GlobalSecondaryIndexes" Core..=)
              Prelude.<$> globalSecondaryIndexes,
            Prelude.Just ("RegionName" Core..= regionName)
          ]
      )
