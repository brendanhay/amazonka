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
-- Module      : Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a replica to be created.
--
-- /See:/ 'newCreateReplicationGroupMemberAction' smart constructor.
data CreateReplicationGroupMemberAction = CreateReplicationGroupMemberAction'
  { -- | The AWS KMS customer master key (CMK) that should be used for AWS KMS
    -- encryption in the new replica. To specify a CMK, use its key ID, Amazon
    -- Resource Name (ARN), alias name, or alias ARN. Note that you should only
    -- provide this parameter if the key is different from the default DynamoDB
    -- KMS master key alias\/aws\/dynamodb.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text,
    -- | Replica-specific provisioned throughput. If not specified, uses the
    -- source table\'s provisioned throughput settings.
    provisionedThroughputOverride :: Prelude.Maybe ProvisionedThroughputOverride,
    -- | Replica-specific global secondary index settings.
    globalSecondaryIndexes :: Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndex),
    -- | The Region where the new replica will be created.
    regionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplicationGroupMemberAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsMasterKeyId', 'createReplicationGroupMemberAction_kmsMasterKeyId' - The AWS KMS customer master key (CMK) that should be used for AWS KMS
-- encryption in the new replica. To specify a CMK, use its key ID, Amazon
-- Resource Name (ARN), alias name, or alias ARN. Note that you should only
-- provide this parameter if the key is different from the default DynamoDB
-- KMS master key alias\/aws\/dynamodb.
--
-- 'provisionedThroughputOverride', 'createReplicationGroupMemberAction_provisionedThroughputOverride' - Replica-specific provisioned throughput. If not specified, uses the
-- source table\'s provisioned throughput settings.
--
-- 'globalSecondaryIndexes', 'createReplicationGroupMemberAction_globalSecondaryIndexes' - Replica-specific global secondary index settings.
--
-- 'regionName', 'createReplicationGroupMemberAction_regionName' - The Region where the new replica will be created.
newCreateReplicationGroupMemberAction ::
  -- | 'regionName'
  Prelude.Text ->
  CreateReplicationGroupMemberAction
newCreateReplicationGroupMemberAction pRegionName_ =
  CreateReplicationGroupMemberAction'
    { kmsMasterKeyId =
        Prelude.Nothing,
      provisionedThroughputOverride =
        Prelude.Nothing,
      globalSecondaryIndexes =
        Prelude.Nothing,
      regionName = pRegionName_
    }

-- | The AWS KMS customer master key (CMK) that should be used for AWS KMS
-- encryption in the new replica. To specify a CMK, use its key ID, Amazon
-- Resource Name (ARN), alias name, or alias ARN. Note that you should only
-- provide this parameter if the key is different from the default DynamoDB
-- KMS master key alias\/aws\/dynamodb.
createReplicationGroupMemberAction_kmsMasterKeyId :: Lens.Lens' CreateReplicationGroupMemberAction (Prelude.Maybe Prelude.Text)
createReplicationGroupMemberAction_kmsMasterKeyId = Lens.lens (\CreateReplicationGroupMemberAction' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@CreateReplicationGroupMemberAction' {} a -> s {kmsMasterKeyId = a} :: CreateReplicationGroupMemberAction)

-- | Replica-specific provisioned throughput. If not specified, uses the
-- source table\'s provisioned throughput settings.
createReplicationGroupMemberAction_provisionedThroughputOverride :: Lens.Lens' CreateReplicationGroupMemberAction (Prelude.Maybe ProvisionedThroughputOverride)
createReplicationGroupMemberAction_provisionedThroughputOverride = Lens.lens (\CreateReplicationGroupMemberAction' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@CreateReplicationGroupMemberAction' {} a -> s {provisionedThroughputOverride = a} :: CreateReplicationGroupMemberAction)

-- | Replica-specific global secondary index settings.
createReplicationGroupMemberAction_globalSecondaryIndexes :: Lens.Lens' CreateReplicationGroupMemberAction (Prelude.Maybe (Prelude.NonEmpty ReplicaGlobalSecondaryIndex))
createReplicationGroupMemberAction_globalSecondaryIndexes = Lens.lens (\CreateReplicationGroupMemberAction' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@CreateReplicationGroupMemberAction' {} a -> s {globalSecondaryIndexes = a} :: CreateReplicationGroupMemberAction) Prelude.. Lens.mapping Lens.coerced

-- | The Region where the new replica will be created.
createReplicationGroupMemberAction_regionName :: Lens.Lens' CreateReplicationGroupMemberAction Prelude.Text
createReplicationGroupMemberAction_regionName = Lens.lens (\CreateReplicationGroupMemberAction' {regionName} -> regionName) (\s@CreateReplicationGroupMemberAction' {} a -> s {regionName = a} :: CreateReplicationGroupMemberAction)

instance
  Prelude.Hashable
    CreateReplicationGroupMemberAction

instance
  Prelude.NFData
    CreateReplicationGroupMemberAction

instance
  Core.ToJSON
    CreateReplicationGroupMemberAction
  where
  toJSON CreateReplicationGroupMemberAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KMSMasterKeyId" Core..=)
              Prelude.<$> kmsMasterKeyId,
            ("ProvisionedThroughputOverride" Core..=)
              Prelude.<$> provisionedThroughputOverride,
            ("GlobalSecondaryIndexes" Core..=)
              Prelude.<$> globalSecondaryIndexes,
            Prelude.Just ("RegionName" Core..= regionName)
          ]
      )
