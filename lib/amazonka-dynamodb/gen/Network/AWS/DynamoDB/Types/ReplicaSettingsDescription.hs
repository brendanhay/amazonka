{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaSettingsDescription
  ( ReplicaSettingsDescription (..),

    -- * Smart constructor
    mkReplicaSettingsDescription,

    -- * Lenses
    rsdRegionName,
    rsdReplicaBillingModeSummary,
    rsdReplicaGlobalSecondaryIndexSettings,
    rsdReplicaProvisionedReadCapacityAutoScalingSettings,
    rsdReplicaProvisionedReadCapacityUnits,
    rsdReplicaProvisionedWriteCapacityAutoScalingSettings,
    rsdReplicaProvisionedWriteCapacityUnits,
    rsdReplicaStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription as Types
import qualified Network.AWS.DynamoDB.Types.BillingModeSummary as Types
import qualified Network.AWS.DynamoDB.Types.RegionName as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a replica.
--
-- /See:/ 'mkReplicaSettingsDescription' smart constructor.
data ReplicaSettingsDescription = ReplicaSettingsDescription'
  { -- | The Region name of the replica.
    regionName :: Types.RegionName,
    -- | The read/write capacity mode of the replica.
    replicaBillingModeSummary :: Core.Maybe Types.BillingModeSummary,
    -- | Replica global secondary index settings for the global table.
    replicaGlobalSecondaryIndexSettings :: Core.Maybe [Types.ReplicaGlobalSecondaryIndexSettingsDescription],
    -- | Auto scaling settings for a global table replica's read capacity units.
    replicaProvisionedReadCapacityAutoScalingSettings :: Core.Maybe Types.AutoScalingSettingsDescription,
    -- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
    replicaProvisionedReadCapacityUnits :: Core.Maybe Core.Natural,
    -- | Auto scaling settings for a global table replica's write capacity units.
    replicaProvisionedWriteCapacityAutoScalingSettings :: Core.Maybe Types.AutoScalingSettingsDescription,
    -- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
    replicaProvisionedWriteCapacityUnits :: Core.Maybe Core.Natural,
    -- | The current state of the Region:
    --
    --
    --     * @CREATING@ - The Region is being created.
    --
    --
    --     * @UPDATING@ - The Region is being updated.
    --
    --
    --     * @DELETING@ - The Region is being deleted.
    --
    --
    --     * @ACTIVE@ - The Region is ready for use.
    replicaStatus :: Core.Maybe Types.ReplicaStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReplicaSettingsDescription' value with any optional fields omitted.
mkReplicaSettingsDescription ::
  -- | 'regionName'
  Types.RegionName ->
  ReplicaSettingsDescription
mkReplicaSettingsDescription regionName =
  ReplicaSettingsDescription'
    { regionName,
      replicaBillingModeSummary = Core.Nothing,
      replicaGlobalSecondaryIndexSettings = Core.Nothing,
      replicaProvisionedReadCapacityAutoScalingSettings = Core.Nothing,
      replicaProvisionedReadCapacityUnits = Core.Nothing,
      replicaProvisionedWriteCapacityAutoScalingSettings = Core.Nothing,
      replicaProvisionedWriteCapacityUnits = Core.Nothing,
      replicaStatus = Core.Nothing
    }

-- | The Region name of the replica.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdRegionName :: Lens.Lens' ReplicaSettingsDescription Types.RegionName
rsdRegionName = Lens.field @"regionName"
{-# DEPRECATED rsdRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | The read/write capacity mode of the replica.
--
-- /Note:/ Consider using 'replicaBillingModeSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdReplicaBillingModeSummary :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe Types.BillingModeSummary)
rsdReplicaBillingModeSummary = Lens.field @"replicaBillingModeSummary"
{-# DEPRECATED rsdReplicaBillingModeSummary "Use generic-lens or generic-optics with 'replicaBillingModeSummary' instead." #-}

-- | Replica global secondary index settings for the global table.
--
-- /Note:/ Consider using 'replicaGlobalSecondaryIndexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdReplicaGlobalSecondaryIndexSettings :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe [Types.ReplicaGlobalSecondaryIndexSettingsDescription])
rsdReplicaGlobalSecondaryIndexSettings = Lens.field @"replicaGlobalSecondaryIndexSettings"
{-# DEPRECATED rsdReplicaGlobalSecondaryIndexSettings "Use generic-lens or generic-optics with 'replicaGlobalSecondaryIndexSettings' instead." #-}

-- | Auto scaling settings for a global table replica's read capacity units.
--
-- /Note:/ Consider using 'replicaProvisionedReadCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdReplicaProvisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe Types.AutoScalingSettingsDescription)
rsdReplicaProvisionedReadCapacityAutoScalingSettings = Lens.field @"replicaProvisionedReadCapacityAutoScalingSettings"
{-# DEPRECATED rsdReplicaProvisionedReadCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'replicaProvisionedReadCapacityAutoScalingSettings' instead." #-}

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'replicaProvisionedReadCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdReplicaProvisionedReadCapacityUnits :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe Core.Natural)
rsdReplicaProvisionedReadCapacityUnits = Lens.field @"replicaProvisionedReadCapacityUnits"
{-# DEPRECATED rsdReplicaProvisionedReadCapacityUnits "Use generic-lens or generic-optics with 'replicaProvisionedReadCapacityUnits' instead." #-}

-- | Auto scaling settings for a global table replica's write capacity units.
--
-- /Note:/ Consider using 'replicaProvisionedWriteCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdReplicaProvisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe Types.AutoScalingSettingsDescription)
rsdReplicaProvisionedWriteCapacityAutoScalingSettings = Lens.field @"replicaProvisionedWriteCapacityAutoScalingSettings"
{-# DEPRECATED rsdReplicaProvisionedWriteCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'replicaProvisionedWriteCapacityAutoScalingSettings' instead." #-}

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'replicaProvisionedWriteCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdReplicaProvisionedWriteCapacityUnits :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe Core.Natural)
rsdReplicaProvisionedWriteCapacityUnits = Lens.field @"replicaProvisionedWriteCapacityUnits"
{-# DEPRECATED rsdReplicaProvisionedWriteCapacityUnits "Use generic-lens or generic-optics with 'replicaProvisionedWriteCapacityUnits' instead." #-}

-- | The current state of the Region:
--
--
--     * @CREATING@ - The Region is being created.
--
--
--     * @UPDATING@ - The Region is being updated.
--
--
--     * @DELETING@ - The Region is being deleted.
--
--
--     * @ACTIVE@ - The Region is ready for use.
--
--
--
-- /Note:/ Consider using 'replicaStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdReplicaStatus :: Lens.Lens' ReplicaSettingsDescription (Core.Maybe Types.ReplicaStatus)
rsdReplicaStatus = Lens.field @"replicaStatus"
{-# DEPRECATED rsdReplicaStatus "Use generic-lens or generic-optics with 'replicaStatus' instead." #-}

instance Core.FromJSON ReplicaSettingsDescription where
  parseJSON =
    Core.withObject "ReplicaSettingsDescription" Core.$
      \x ->
        ReplicaSettingsDescription'
          Core.<$> (x Core..: "RegionName")
          Core.<*> (x Core..:? "ReplicaBillingModeSummary")
          Core.<*> (x Core..:? "ReplicaGlobalSecondaryIndexSettings")
          Core.<*> (x Core..:? "ReplicaProvisionedReadCapacityAutoScalingSettings")
          Core.<*> (x Core..:? "ReplicaProvisionedReadCapacityUnits")
          Core.<*> (x Core..:? "ReplicaProvisionedWriteCapacityAutoScalingSettings")
          Core.<*> (x Core..:? "ReplicaProvisionedWriteCapacityUnits")
          Core.<*> (x Core..:? "ReplicaStatus")
