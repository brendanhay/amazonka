{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction
  ( CreateReplicationGroupMemberAction (..)
  -- * Smart constructor
  , mkCreateReplicationGroupMemberAction
  -- * Lenses
  , crgmaRegionName
  , crgmaGlobalSecondaryIndexes
  , crgmaKMSMasterKeyId
  , crgmaProvisionedThroughputOverride
  ) where

import qualified Network.AWS.DynamoDB.Types.KMSMasterKeyId as Types
import qualified Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride as Types
import qualified Network.AWS.DynamoDB.Types.RegionName as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a replica to be created.
--
-- /See:/ 'mkCreateReplicationGroupMemberAction' smart constructor.
data CreateReplicationGroupMemberAction = CreateReplicationGroupMemberAction'
  { regionName :: Types.RegionName
    -- ^ The Region where the new replica will be created.
  , globalSecondaryIndexes :: Core.Maybe (Core.NonEmpty Types.ReplicaGlobalSecondaryIndex)
    -- ^ Replica-specific global secondary index settings.
  , kMSMasterKeyId :: Core.Maybe Types.KMSMasterKeyId
    -- ^ The AWS KMS customer master key (CMK) that should be used for AWS KMS encryption in the new replica. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS master key alias/aws/dynamodb.
  , provisionedThroughputOverride :: Core.Maybe Types.ProvisionedThroughputOverride
    -- ^ Replica-specific provisioned throughput. If not specified, uses the source table's provisioned throughput settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReplicationGroupMemberAction' value with any optional fields omitted.
mkCreateReplicationGroupMemberAction
    :: Types.RegionName -- ^ 'regionName'
    -> CreateReplicationGroupMemberAction
mkCreateReplicationGroupMemberAction regionName
  = CreateReplicationGroupMemberAction'{regionName,
                                        globalSecondaryIndexes = Core.Nothing,
                                        kMSMasterKeyId = Core.Nothing,
                                        provisionedThroughputOverride = Core.Nothing}

-- | The Region where the new replica will be created.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgmaRegionName :: Lens.Lens' CreateReplicationGroupMemberAction Types.RegionName
crgmaRegionName = Lens.field @"regionName"
{-# INLINEABLE crgmaRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

-- | Replica-specific global secondary index settings.
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgmaGlobalSecondaryIndexes :: Lens.Lens' CreateReplicationGroupMemberAction (Core.Maybe (Core.NonEmpty Types.ReplicaGlobalSecondaryIndex))
crgmaGlobalSecondaryIndexes = Lens.field @"globalSecondaryIndexes"
{-# INLINEABLE crgmaGlobalSecondaryIndexes #-}
{-# DEPRECATED globalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead"  #-}

-- | The AWS KMS customer master key (CMK) that should be used for AWS KMS encryption in the new replica. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB KMS master key alias/aws/dynamodb.
--
-- /Note:/ Consider using 'kMSMasterKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgmaKMSMasterKeyId :: Lens.Lens' CreateReplicationGroupMemberAction (Core.Maybe Types.KMSMasterKeyId)
crgmaKMSMasterKeyId = Lens.field @"kMSMasterKeyId"
{-# INLINEABLE crgmaKMSMasterKeyId #-}
{-# DEPRECATED kMSMasterKeyId "Use generic-lens or generic-optics with 'kMSMasterKeyId' instead"  #-}

-- | Replica-specific provisioned throughput. If not specified, uses the source table's provisioned throughput settings.
--
-- /Note:/ Consider using 'provisionedThroughputOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgmaProvisionedThroughputOverride :: Lens.Lens' CreateReplicationGroupMemberAction (Core.Maybe Types.ProvisionedThroughputOverride)
crgmaProvisionedThroughputOverride = Lens.field @"provisionedThroughputOverride"
{-# INLINEABLE crgmaProvisionedThroughputOverride #-}
{-# DEPRECATED provisionedThroughputOverride "Use generic-lens or generic-optics with 'provisionedThroughputOverride' instead"  #-}

instance Core.FromJSON CreateReplicationGroupMemberAction where
        toJSON CreateReplicationGroupMemberAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RegionName" Core..= regionName),
                  ("GlobalSecondaryIndexes" Core..=) Core.<$> globalSecondaryIndexes,
                  ("KMSMasterKeyId" Core..=) Core.<$> kMSMasterKeyId,
                  ("ProvisionedThroughputOverride" Core..=) Core.<$>
                    provisionedThroughputOverride])
