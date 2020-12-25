{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction
  ( UpdateReplicationGroupMemberAction (..),

    -- * Smart constructor
    mkUpdateReplicationGroupMemberAction,

    -- * Lenses
    urgmaRegionName,
    urgmaGlobalSecondaryIndexes,
    urgmaKMSMasterKeyId,
    urgmaProvisionedThroughputOverride,
  )
where

import qualified Network.AWS.DynamoDB.Types.KMSMasterKeyId as Types
import qualified Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride as Types
import qualified Network.AWS.DynamoDB.Types.RegionName as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a replica to be modified.
--
-- /See:/ 'mkUpdateReplicationGroupMemberAction' smart constructor.
data UpdateReplicationGroupMemberAction = UpdateReplicationGroupMemberAction'
  { -- | The Region where the replica exists.
    regionName :: Types.RegionName,
    -- | Replica-specific global secondary index settings.
    globalSecondaryIndexes :: Core.Maybe (Core.NonEmpty Types.ReplicaGlobalSecondaryIndex),
    -- | The AWS KMS customer master key (CMK) of the replica that should be used for AWS KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS master key alias/aws/dynamodb.
    kMSMasterKeyId :: Core.Maybe Types.KMSMasterKeyId,
    -- | Replica-specific provisioned throughput. If not specified, uses the source table's provisioned throughput settings.
    provisionedThroughputOverride :: Core.Maybe Types.ProvisionedThroughputOverride
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateReplicationGroupMemberAction' value with any optional fields omitted.
mkUpdateReplicationGroupMemberAction ::
  -- | 'regionName'
  Types.RegionName ->
  UpdateReplicationGroupMemberAction
mkUpdateReplicationGroupMemberAction regionName =
  UpdateReplicationGroupMemberAction'
    { regionName,
      globalSecondaryIndexes = Core.Nothing,
      kMSMasterKeyId = Core.Nothing,
      provisionedThroughputOverride = Core.Nothing
    }

-- | The Region where the replica exists.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgmaRegionName :: Lens.Lens' UpdateReplicationGroupMemberAction Types.RegionName
urgmaRegionName = Lens.field @"regionName"
{-# DEPRECATED urgmaRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | Replica-specific global secondary index settings.
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgmaGlobalSecondaryIndexes :: Lens.Lens' UpdateReplicationGroupMemberAction (Core.Maybe (Core.NonEmpty Types.ReplicaGlobalSecondaryIndex))
urgmaGlobalSecondaryIndexes = Lens.field @"globalSecondaryIndexes"
{-# DEPRECATED urgmaGlobalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead." #-}

-- | The AWS KMS customer master key (CMK) of the replica that should be used for AWS KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS master key alias/aws/dynamodb.
--
-- /Note:/ Consider using 'kMSMasterKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgmaKMSMasterKeyId :: Lens.Lens' UpdateReplicationGroupMemberAction (Core.Maybe Types.KMSMasterKeyId)
urgmaKMSMasterKeyId = Lens.field @"kMSMasterKeyId"
{-# DEPRECATED urgmaKMSMasterKeyId "Use generic-lens or generic-optics with 'kMSMasterKeyId' instead." #-}

-- | Replica-specific provisioned throughput. If not specified, uses the source table's provisioned throughput settings.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urgmaProvisionedThroughputOverride :: Lens.Lens' UpdateReplicationGroupMemberAction (Core.Maybe Types.ProvisionedThroughputOverride)
urgmaProvisionedThroughputOverride = Lens.field @"provisionedThroughputOverride"
{-# DEPRECATED urgmaProvisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead." #-}

instance Core.FromJSON UpdateReplicationGroupMemberAction where
  toJSON UpdateReplicationGroupMemberAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RegionName" Core..= regionName),
            ("GlobalSecondaryIndexes" Core..=) Core.<$> globalSecondaryIndexes,
            ("KMSMasterKeyId" Core..=) Core.<$> kMSMasterKeyId,
            ("ProvisionedThroughputOverride" Core..=)
              Core.<$> provisionedThroughputOverride
          ]
      )
