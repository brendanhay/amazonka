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
-- Module      : Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a replica to be modified.
--
-- /See:/ 'newUpdateReplicationGroupMemberAction' smart constructor.
data UpdateReplicationGroupMemberAction = UpdateReplicationGroupMemberAction'
  { -- | Replica-specific global secondary index settings.
    globalSecondaryIndexes :: Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndex),
    -- | Replica-specific provisioned throughput. If not specified, uses the
    -- source table\'s provisioned throughput settings.
    provisionedThroughputOverride :: Prelude.Maybe ProvisionedThroughputOverride,
    -- | The AWS KMS customer master key (CMK) of the replica that should be used
    -- for AWS KMS encryption. To specify a CMK, use its key ID, Amazon
    -- Resource Name (ARN), alias name, or alias ARN. Note that you should only
    -- provide this parameter if the key is different from the default DynamoDB
    -- KMS master key alias\/aws\/dynamodb.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
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
-- 'provisionedThroughputOverride', 'updateReplicationGroupMemberAction_provisionedThroughputOverride' - Replica-specific provisioned throughput. If not specified, uses the
-- source table\'s provisioned throughput settings.
--
-- 'kmsMasterKeyId', 'updateReplicationGroupMemberAction_kmsMasterKeyId' - The AWS KMS customer master key (CMK) of the replica that should be used
-- for AWS KMS encryption. To specify a CMK, use its key ID, Amazon
-- Resource Name (ARN), alias name, or alias ARN. Note that you should only
-- provide this parameter if the key is different from the default DynamoDB
-- KMS master key alias\/aws\/dynamodb.
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
      provisionedThroughputOverride =
        Prelude.Nothing,
      kmsMasterKeyId = Prelude.Nothing,
      regionName = pRegionName_
    }

-- | Replica-specific global secondary index settings.
updateReplicationGroupMemberAction_globalSecondaryIndexes :: Lens.Lens' UpdateReplicationGroupMemberAction (Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndex))
updateReplicationGroupMemberAction_globalSecondaryIndexes = Lens.lens (\UpdateReplicationGroupMemberAction' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@UpdateReplicationGroupMemberAction' {} a -> s {globalSecondaryIndexes = a} :: UpdateReplicationGroupMemberAction) Prelude.. Lens.mapping Lens._Coerce

-- | Replica-specific provisioned throughput. If not specified, uses the
-- source table\'s provisioned throughput settings.
updateReplicationGroupMemberAction_provisionedThroughputOverride :: Lens.Lens' UpdateReplicationGroupMemberAction (Prelude.Maybe ProvisionedThroughputOverride)
updateReplicationGroupMemberAction_provisionedThroughputOverride = Lens.lens (\UpdateReplicationGroupMemberAction' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@UpdateReplicationGroupMemberAction' {} a -> s {provisionedThroughputOverride = a} :: UpdateReplicationGroupMemberAction)

-- | The AWS KMS customer master key (CMK) of the replica that should be used
-- for AWS KMS encryption. To specify a CMK, use its key ID, Amazon
-- Resource Name (ARN), alias name, or alias ARN. Note that you should only
-- provide this parameter if the key is different from the default DynamoDB
-- KMS master key alias\/aws\/dynamodb.
updateReplicationGroupMemberAction_kmsMasterKeyId :: Lens.Lens' UpdateReplicationGroupMemberAction (Prelude.Maybe Prelude.Text)
updateReplicationGroupMemberAction_kmsMasterKeyId = Lens.lens (\UpdateReplicationGroupMemberAction' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@UpdateReplicationGroupMemberAction' {} a -> s {kmsMasterKeyId = a} :: UpdateReplicationGroupMemberAction)

-- | The Region where the replica exists.
updateReplicationGroupMemberAction_regionName :: Lens.Lens' UpdateReplicationGroupMemberAction Prelude.Text
updateReplicationGroupMemberAction_regionName = Lens.lens (\UpdateReplicationGroupMemberAction' {regionName} -> regionName) (\s@UpdateReplicationGroupMemberAction' {} a -> s {regionName = a} :: UpdateReplicationGroupMemberAction)

instance
  Prelude.Hashable
    UpdateReplicationGroupMemberAction

instance
  Prelude.NFData
    UpdateReplicationGroupMemberAction

instance
  Core.ToJSON
    UpdateReplicationGroupMemberAction
  where
  toJSON UpdateReplicationGroupMemberAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GlobalSecondaryIndexes" Core..=)
              Prelude.<$> globalSecondaryIndexes,
            ("ProvisionedThroughputOverride" Core..=)
              Prelude.<$> provisionedThroughputOverride,
            ("KMSMasterKeyId" Core..=)
              Prelude.<$> kmsMasterKeyId,
            Prelude.Just ("RegionName" Core..= regionName)
          ]
      )
