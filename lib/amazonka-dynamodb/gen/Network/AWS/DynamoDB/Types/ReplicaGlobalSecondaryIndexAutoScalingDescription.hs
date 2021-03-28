{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
  ( ReplicaGlobalSecondaryIndexAutoScalingDescription (..)
  -- * Smart constructor
  , mkReplicaGlobalSecondaryIndexAutoScalingDescription
  -- * Lenses
  , rgsiasdIndexName
  , rgsiasdIndexStatus
  , rgsiasdProvisionedReadCapacityAutoScalingSettings
  , rgsiasdProvisionedWriteCapacityAutoScalingSettings
  ) where

import qualified Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription as Types
import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.DynamoDB.Types.IndexStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the auto scaling configuration for a replica global secondary index.
--
-- /See:/ 'mkReplicaGlobalSecondaryIndexAutoScalingDescription' smart constructor.
data ReplicaGlobalSecondaryIndexAutoScalingDescription = ReplicaGlobalSecondaryIndexAutoScalingDescription'
  { indexName :: Core.Maybe Types.IndexName
    -- ^ The name of the global secondary index.
  , indexStatus :: Core.Maybe Types.IndexStatus
    -- ^ The current state of the replica global secondary index:
--
--
--     * @CREATING@ - The index is being created.
--
--
--     * @UPDATING@ - The index is being updated.
--
--
--     * @DELETING@ - The index is being deleted.
--
--
--     * @ACTIVE@ - The index is ready for use.
--
--
  , provisionedReadCapacityAutoScalingSettings :: Core.Maybe Types.AutoScalingSettingsDescription
  , provisionedWriteCapacityAutoScalingSettings :: Core.Maybe Types.AutoScalingSettingsDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicaGlobalSecondaryIndexAutoScalingDescription' value with any optional fields omitted.
mkReplicaGlobalSecondaryIndexAutoScalingDescription
    :: ReplicaGlobalSecondaryIndexAutoScalingDescription
mkReplicaGlobalSecondaryIndexAutoScalingDescription
  = ReplicaGlobalSecondaryIndexAutoScalingDescription'{indexName =
                                                         Core.Nothing,
                                                       indexStatus = Core.Nothing,
                                                       provisionedReadCapacityAutoScalingSettings =
                                                         Core.Nothing,
                                                       provisionedWriteCapacityAutoScalingSettings =
                                                         Core.Nothing}

-- | The name of the global secondary index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiasdIndexName :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Core.Maybe Types.IndexName)
rgsiasdIndexName = Lens.field @"indexName"
{-# INLINEABLE rgsiasdIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | The current state of the replica global secondary index:
--
--
--     * @CREATING@ - The index is being created.
--
--
--     * @UPDATING@ - The index is being updated.
--
--
--     * @DELETING@ - The index is being deleted.
--
--
--     * @ACTIVE@ - The index is ready for use.
--
--
--
-- /Note:/ Consider using 'indexStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiasdIndexStatus :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Core.Maybe Types.IndexStatus)
rgsiasdIndexStatus = Lens.field @"indexStatus"
{-# INLINEABLE rgsiasdIndexStatus #-}
{-# DEPRECATED indexStatus "Use generic-lens or generic-optics with 'indexStatus' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'provisionedReadCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiasdProvisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Core.Maybe Types.AutoScalingSettingsDescription)
rgsiasdProvisionedReadCapacityAutoScalingSettings = Lens.field @"provisionedReadCapacityAutoScalingSettings"
{-# INLINEABLE rgsiasdProvisionedReadCapacityAutoScalingSettings #-}
{-# DEPRECATED provisionedReadCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'provisionedReadCapacityAutoScalingSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'provisionedWriteCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsiasdProvisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingDescription (Core.Maybe Types.AutoScalingSettingsDescription)
rgsiasdProvisionedWriteCapacityAutoScalingSettings = Lens.field @"provisionedWriteCapacityAutoScalingSettings"
{-# INLINEABLE rgsiasdProvisionedWriteCapacityAutoScalingSettings #-}
{-# DEPRECATED provisionedWriteCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'provisionedWriteCapacityAutoScalingSettings' instead"  #-}

instance Core.FromJSON
           ReplicaGlobalSecondaryIndexAutoScalingDescription
         where
        parseJSON
          = Core.withObject
              "ReplicaGlobalSecondaryIndexAutoScalingDescription"
              Core.$
              \ x ->
                ReplicaGlobalSecondaryIndexAutoScalingDescription' Core.<$>
                  (x Core..:? "IndexName") Core.<*> x Core..:? "IndexStatus" Core.<*>
                    x Core..:? "ProvisionedReadCapacityAutoScalingSettings"
                    Core.<*> x Core..:? "ProvisionedWriteCapacityAutoScalingSettings"
