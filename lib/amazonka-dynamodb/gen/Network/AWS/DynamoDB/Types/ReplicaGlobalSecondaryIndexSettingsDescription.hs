{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
  ( ReplicaGlobalSecondaryIndexSettingsDescription (..)
  -- * Smart constructor
  , mkReplicaGlobalSecondaryIndexSettingsDescription
  -- * Lenses
  , rgsisdIndexName
  , rgsisdIndexStatus
  , rgsisdProvisionedReadCapacityAutoScalingSettings
  , rgsisdProvisionedReadCapacityUnits
  , rgsisdProvisionedWriteCapacityAutoScalingSettings
  , rgsisdProvisionedWriteCapacityUnits
  ) where

import qualified Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription as Types
import qualified Network.AWS.DynamoDB.Types.IndexName as Types
import qualified Network.AWS.DynamoDB.Types.IndexStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a global secondary index.
--
-- /See:/ 'mkReplicaGlobalSecondaryIndexSettingsDescription' smart constructor.
data ReplicaGlobalSecondaryIndexSettingsDescription = ReplicaGlobalSecondaryIndexSettingsDescription'
  { indexName :: Types.IndexName
    -- ^ The name of the global secondary index. The name must be unique among all other indexes on this table.
  , indexStatus :: Core.Maybe Types.IndexStatus
    -- ^ The current status of the global secondary index:
--
--
--     * @CREATING@ - The global secondary index is being created.
--
--
--     * @UPDATING@ - The global secondary index is being updated.
--
--
--     * @DELETING@ - The global secondary index is being deleted.
--
--
--     * @ACTIVE@ - The global secondary index is ready for use.
--
--
  , provisionedReadCapacityAutoScalingSettings :: Core.Maybe Types.AutoScalingSettingsDescription
    -- ^ Auto scaling settings for a global secondary index replica's read capacity units.
  , provisionedReadCapacityUnits :: Core.Maybe Core.Natural
    -- ^ The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
  , provisionedWriteCapacityAutoScalingSettings :: Core.Maybe Types.AutoScalingSettingsDescription
    -- ^ Auto scaling settings for a global secondary index replica's write capacity units.
  , provisionedWriteCapacityUnits :: Core.Maybe Core.Natural
    -- ^ The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicaGlobalSecondaryIndexSettingsDescription' value with any optional fields omitted.
mkReplicaGlobalSecondaryIndexSettingsDescription
    :: Types.IndexName -- ^ 'indexName'
    -> ReplicaGlobalSecondaryIndexSettingsDescription
mkReplicaGlobalSecondaryIndexSettingsDescription indexName
  = ReplicaGlobalSecondaryIndexSettingsDescription'{indexName,
                                                    indexStatus = Core.Nothing,
                                                    provisionedReadCapacityAutoScalingSettings =
                                                      Core.Nothing,
                                                    provisionedReadCapacityUnits = Core.Nothing,
                                                    provisionedWriteCapacityAutoScalingSettings =
                                                      Core.Nothing,
                                                    provisionedWriteCapacityUnits = Core.Nothing}

-- | The name of the global secondary index. The name must be unique among all other indexes on this table.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisdIndexName :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription Types.IndexName
rgsisdIndexName = Lens.field @"indexName"
{-# INLINEABLE rgsisdIndexName #-}
{-# DEPRECATED indexName "Use generic-lens or generic-optics with 'indexName' instead"  #-}

-- | The current status of the global secondary index:
--
--
--     * @CREATING@ - The global secondary index is being created.
--
--
--     * @UPDATING@ - The global secondary index is being updated.
--
--
--     * @DELETING@ - The global secondary index is being deleted.
--
--
--     * @ACTIVE@ - The global secondary index is ready for use.
--
--
--
-- /Note:/ Consider using 'indexStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisdIndexStatus :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Core.Maybe Types.IndexStatus)
rgsisdIndexStatus = Lens.field @"indexStatus"
{-# INLINEABLE rgsisdIndexStatus #-}
{-# DEPRECATED indexStatus "Use generic-lens or generic-optics with 'indexStatus' instead"  #-}

-- | Auto scaling settings for a global secondary index replica's read capacity units.
--
-- /Note:/ Consider using 'provisionedReadCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisdProvisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Core.Maybe Types.AutoScalingSettingsDescription)
rgsisdProvisionedReadCapacityAutoScalingSettings = Lens.field @"provisionedReadCapacityAutoScalingSettings"
{-# INLINEABLE rgsisdProvisionedReadCapacityAutoScalingSettings #-}
{-# DEPRECATED provisionedReadCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'provisionedReadCapacityAutoScalingSettings' instead"  #-}

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- /Note:/ Consider using 'provisionedReadCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisdProvisionedReadCapacityUnits :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Core.Maybe Core.Natural)
rgsisdProvisionedReadCapacityUnits = Lens.field @"provisionedReadCapacityUnits"
{-# INLINEABLE rgsisdProvisionedReadCapacityUnits #-}
{-# DEPRECATED provisionedReadCapacityUnits "Use generic-lens or generic-optics with 'provisionedReadCapacityUnits' instead"  #-}

-- | Auto scaling settings for a global secondary index replica's write capacity units.
--
-- /Note:/ Consider using 'provisionedWriteCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisdProvisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Core.Maybe Types.AutoScalingSettingsDescription)
rgsisdProvisionedWriteCapacityAutoScalingSettings = Lens.field @"provisionedWriteCapacityAutoScalingSettings"
{-# INLINEABLE rgsisdProvisionedWriteCapacityAutoScalingSettings #-}
{-# DEPRECATED provisionedWriteCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'provisionedWriteCapacityAutoScalingSettings' instead"  #-}

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ .
--
-- /Note:/ Consider using 'provisionedWriteCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsisdProvisionedWriteCapacityUnits :: Lens.Lens' ReplicaGlobalSecondaryIndexSettingsDescription (Core.Maybe Core.Natural)
rgsisdProvisionedWriteCapacityUnits = Lens.field @"provisionedWriteCapacityUnits"
{-# INLINEABLE rgsisdProvisionedWriteCapacityUnits #-}
{-# DEPRECATED provisionedWriteCapacityUnits "Use generic-lens or generic-optics with 'provisionedWriteCapacityUnits' instead"  #-}

instance Core.FromJSON
           ReplicaGlobalSecondaryIndexSettingsDescription
         where
        parseJSON
          = Core.withObject "ReplicaGlobalSecondaryIndexSettingsDescription"
              Core.$
              \ x ->
                ReplicaGlobalSecondaryIndexSettingsDescription' Core.<$>
                  (x Core..: "IndexName") Core.<*> x Core..:? "IndexStatus" Core.<*>
                    x Core..:? "ProvisionedReadCapacityAutoScalingSettings"
                    Core.<*> x Core..:? "ProvisionedReadCapacityUnits"
                    Core.<*> x Core..:? "ProvisionedWriteCapacityAutoScalingSettings"
                    Core.<*> x Core..:? "ProvisionedWriteCapacityUnits"
