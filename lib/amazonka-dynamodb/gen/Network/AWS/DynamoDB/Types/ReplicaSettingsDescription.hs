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
    rsdReplicaStatus,
    rsdReplicaProvisionedReadCapacityUnits,
    rsdReplicaProvisionedWriteCapacityUnits,
    rsdReplicaBillingModeSummary,
    rsdReplicaGlobalSecondaryIndexSettings,
    rsdReplicaProvisionedWriteCapacityAutoScalingSettings,
    rsdReplicaProvisionedReadCapacityAutoScalingSettings,
    rsdRegionName,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.BillingModeSummary
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
import Network.AWS.DynamoDB.Types.ReplicaStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the properties of a replica.
--
-- /See:/ 'mkReplicaSettingsDescription' smart constructor.
data ReplicaSettingsDescription = ReplicaSettingsDescription'
  { replicaStatus ::
      Lude.Maybe ReplicaStatus,
    replicaProvisionedReadCapacityUnits ::
      Lude.Maybe Lude.Natural,
    replicaProvisionedWriteCapacityUnits ::
      Lude.Maybe Lude.Natural,
    replicaBillingModeSummary ::
      Lude.Maybe BillingModeSummary,
    replicaGlobalSecondaryIndexSettings ::
      Lude.Maybe
        [ReplicaGlobalSecondaryIndexSettingsDescription],
    replicaProvisionedWriteCapacityAutoScalingSettings ::
      Lude.Maybe
        AutoScalingSettingsDescription,
    replicaProvisionedReadCapacityAutoScalingSettings ::
      Lude.Maybe
        AutoScalingSettingsDescription,
    regionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicaSettingsDescription' with the minimum fields required to make a request.
--
-- * 'regionName' - The Region name of the replica.
-- * 'replicaBillingModeSummary' - The read/write capacity mode of the replica.
-- * 'replicaGlobalSecondaryIndexSettings' - Replica global secondary index settings for the global table.
-- * 'replicaProvisionedReadCapacityAutoScalingSettings' - Auto scaling settings for a global table replica's read capacity units.
-- * 'replicaProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
-- * 'replicaProvisionedWriteCapacityAutoScalingSettings' - Auto scaling settings for a global table replica's write capacity units.
-- * 'replicaProvisionedWriteCapacityUnits' - The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
-- * 'replicaStatus' - The current state of the Region:
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
mkReplicaSettingsDescription ::
  -- | 'regionName'
  Lude.Text ->
  ReplicaSettingsDescription
mkReplicaSettingsDescription pRegionName_ =
  ReplicaSettingsDescription'
    { replicaStatus = Lude.Nothing,
      replicaProvisionedReadCapacityUnits = Lude.Nothing,
      replicaProvisionedWriteCapacityUnits = Lude.Nothing,
      replicaBillingModeSummary = Lude.Nothing,
      replicaGlobalSecondaryIndexSettings = Lude.Nothing,
      replicaProvisionedWriteCapacityAutoScalingSettings = Lude.Nothing,
      replicaProvisionedReadCapacityAutoScalingSettings = Lude.Nothing,
      regionName = pRegionName_
    }

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
rsdReplicaStatus :: Lens.Lens' ReplicaSettingsDescription (Lude.Maybe ReplicaStatus)
rsdReplicaStatus = Lens.lens (replicaStatus :: ReplicaSettingsDescription -> Lude.Maybe ReplicaStatus) (\s a -> s {replicaStatus = a} :: ReplicaSettingsDescription)
{-# DEPRECATED rsdReplicaStatus "Use generic-lens or generic-optics with 'replicaStatus' instead." #-}

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'replicaProvisionedReadCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdReplicaProvisionedReadCapacityUnits :: Lens.Lens' ReplicaSettingsDescription (Lude.Maybe Lude.Natural)
rsdReplicaProvisionedReadCapacityUnits = Lens.lens (replicaProvisionedReadCapacityUnits :: ReplicaSettingsDescription -> Lude.Maybe Lude.Natural) (\s a -> s {replicaProvisionedReadCapacityUnits = a} :: ReplicaSettingsDescription)
{-# DEPRECATED rsdReplicaProvisionedReadCapacityUnits "Use generic-lens or generic-optics with 'replicaProvisionedReadCapacityUnits' instead." #-}

-- | The maximum number of writes consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'replicaProvisionedWriteCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdReplicaProvisionedWriteCapacityUnits :: Lens.Lens' ReplicaSettingsDescription (Lude.Maybe Lude.Natural)
rsdReplicaProvisionedWriteCapacityUnits = Lens.lens (replicaProvisionedWriteCapacityUnits :: ReplicaSettingsDescription -> Lude.Maybe Lude.Natural) (\s a -> s {replicaProvisionedWriteCapacityUnits = a} :: ReplicaSettingsDescription)
{-# DEPRECATED rsdReplicaProvisionedWriteCapacityUnits "Use generic-lens or generic-optics with 'replicaProvisionedWriteCapacityUnits' instead." #-}

-- | The read/write capacity mode of the replica.
--
-- /Note:/ Consider using 'replicaBillingModeSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdReplicaBillingModeSummary :: Lens.Lens' ReplicaSettingsDescription (Lude.Maybe BillingModeSummary)
rsdReplicaBillingModeSummary = Lens.lens (replicaBillingModeSummary :: ReplicaSettingsDescription -> Lude.Maybe BillingModeSummary) (\s a -> s {replicaBillingModeSummary = a} :: ReplicaSettingsDescription)
{-# DEPRECATED rsdReplicaBillingModeSummary "Use generic-lens or generic-optics with 'replicaBillingModeSummary' instead." #-}

-- | Replica global secondary index settings for the global table.
--
-- /Note:/ Consider using 'replicaGlobalSecondaryIndexSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdReplicaGlobalSecondaryIndexSettings :: Lens.Lens' ReplicaSettingsDescription (Lude.Maybe [ReplicaGlobalSecondaryIndexSettingsDescription])
rsdReplicaGlobalSecondaryIndexSettings = Lens.lens (replicaGlobalSecondaryIndexSettings :: ReplicaSettingsDescription -> Lude.Maybe [ReplicaGlobalSecondaryIndexSettingsDescription]) (\s a -> s {replicaGlobalSecondaryIndexSettings = a} :: ReplicaSettingsDescription)
{-# DEPRECATED rsdReplicaGlobalSecondaryIndexSettings "Use generic-lens or generic-optics with 'replicaGlobalSecondaryIndexSettings' instead." #-}

-- | Auto scaling settings for a global table replica's write capacity units.
--
-- /Note:/ Consider using 'replicaProvisionedWriteCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdReplicaProvisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaSettingsDescription (Lude.Maybe AutoScalingSettingsDescription)
rsdReplicaProvisionedWriteCapacityAutoScalingSettings = Lens.lens (replicaProvisionedWriteCapacityAutoScalingSettings :: ReplicaSettingsDescription -> Lude.Maybe AutoScalingSettingsDescription) (\s a -> s {replicaProvisionedWriteCapacityAutoScalingSettings = a} :: ReplicaSettingsDescription)
{-# DEPRECATED rsdReplicaProvisionedWriteCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'replicaProvisionedWriteCapacityAutoScalingSettings' instead." #-}

-- | Auto scaling settings for a global table replica's read capacity units.
--
-- /Note:/ Consider using 'replicaProvisionedReadCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdReplicaProvisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaSettingsDescription (Lude.Maybe AutoScalingSettingsDescription)
rsdReplicaProvisionedReadCapacityAutoScalingSettings = Lens.lens (replicaProvisionedReadCapacityAutoScalingSettings :: ReplicaSettingsDescription -> Lude.Maybe AutoScalingSettingsDescription) (\s a -> s {replicaProvisionedReadCapacityAutoScalingSettings = a} :: ReplicaSettingsDescription)
{-# DEPRECATED rsdReplicaProvisionedReadCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'replicaProvisionedReadCapacityAutoScalingSettings' instead." #-}

-- | The Region name of the replica.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsdRegionName :: Lens.Lens' ReplicaSettingsDescription Lude.Text
rsdRegionName = Lens.lens (regionName :: ReplicaSettingsDescription -> Lude.Text) (\s a -> s {regionName = a} :: ReplicaSettingsDescription)
{-# DEPRECATED rsdRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Lude.FromJSON ReplicaSettingsDescription where
  parseJSON =
    Lude.withObject
      "ReplicaSettingsDescription"
      ( \x ->
          ReplicaSettingsDescription'
            Lude.<$> (x Lude..:? "ReplicaStatus")
            Lude.<*> (x Lude..:? "ReplicaProvisionedReadCapacityUnits")
            Lude.<*> (x Lude..:? "ReplicaProvisionedWriteCapacityUnits")
            Lude.<*> (x Lude..:? "ReplicaBillingModeSummary")
            Lude.<*> ( x Lude..:? "ReplicaGlobalSecondaryIndexSettings"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..:? "ReplicaProvisionedWriteCapacityAutoScalingSettings")
            Lude.<*> (x Lude..:? "ReplicaProvisionedReadCapacityAutoScalingSettings")
            Lude.<*> (x Lude..: "RegionName")
      )
