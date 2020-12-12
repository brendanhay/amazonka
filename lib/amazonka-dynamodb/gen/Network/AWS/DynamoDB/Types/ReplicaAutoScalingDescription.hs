{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription
  ( ReplicaAutoScalingDescription (..),

    -- * Smart constructor
    mkReplicaAutoScalingDescription,

    -- * Lenses
    rasdReplicaStatus,
    rasdRegionName,
    rasdGlobalSecondaryIndexes,
    rasdReplicaProvisionedWriteCapacityAutoScalingSettings,
    rasdReplicaProvisionedReadCapacityAutoScalingSettings,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
import Network.AWS.DynamoDB.Types.ReplicaStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the auto scaling settings of the replica.
--
-- /See:/ 'mkReplicaAutoScalingDescription' smart constructor.
data ReplicaAutoScalingDescription = ReplicaAutoScalingDescription'
  { replicaStatus ::
      Lude.Maybe ReplicaStatus,
    regionName ::
      Lude.Maybe Lude.Text,
    globalSecondaryIndexes ::
      Lude.Maybe
        [ReplicaGlobalSecondaryIndexAutoScalingDescription],
    replicaProvisionedWriteCapacityAutoScalingSettings ::
      Lude.Maybe
        AutoScalingSettingsDescription,
    replicaProvisionedReadCapacityAutoScalingSettings ::
      Lude.Maybe
        AutoScalingSettingsDescription
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicaAutoScalingDescription' with the minimum fields required to make a request.
--
-- * 'globalSecondaryIndexes' - Replica-specific global secondary index auto scaling settings.
-- * 'regionName' - The Region where the replica exists.
-- * 'replicaProvisionedReadCapacityAutoScalingSettings' - Undocumented field.
-- * 'replicaProvisionedWriteCapacityAutoScalingSettings' - Undocumented field.
-- * 'replicaStatus' - The current state of the replica:
--
--
--     * @CREATING@ - The replica is being created.
--
--
--     * @UPDATING@ - The replica is being updated.
--
--
--     * @DELETING@ - The replica is being deleted.
--
--
--     * @ACTIVE@ - The replica is ready for use.
mkReplicaAutoScalingDescription ::
  ReplicaAutoScalingDescription
mkReplicaAutoScalingDescription =
  ReplicaAutoScalingDescription'
    { replicaStatus = Lude.Nothing,
      regionName = Lude.Nothing,
      globalSecondaryIndexes = Lude.Nothing,
      replicaProvisionedWriteCapacityAutoScalingSettings =
        Lude.Nothing,
      replicaProvisionedReadCapacityAutoScalingSettings = Lude.Nothing
    }

-- | The current state of the replica:
--
--
--     * @CREATING@ - The replica is being created.
--
--
--     * @UPDATING@ - The replica is being updated.
--
--
--     * @DELETING@ - The replica is being deleted.
--
--
--     * @ACTIVE@ - The replica is ready for use.
--
--
--
-- /Note:/ Consider using 'replicaStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasdReplicaStatus :: Lens.Lens' ReplicaAutoScalingDescription (Lude.Maybe ReplicaStatus)
rasdReplicaStatus = Lens.lens (replicaStatus :: ReplicaAutoScalingDescription -> Lude.Maybe ReplicaStatus) (\s a -> s {replicaStatus = a} :: ReplicaAutoScalingDescription)
{-# DEPRECATED rasdReplicaStatus "Use generic-lens or generic-optics with 'replicaStatus' instead." #-}

-- | The Region where the replica exists.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasdRegionName :: Lens.Lens' ReplicaAutoScalingDescription (Lude.Maybe Lude.Text)
rasdRegionName = Lens.lens (regionName :: ReplicaAutoScalingDescription -> Lude.Maybe Lude.Text) (\s a -> s {regionName = a} :: ReplicaAutoScalingDescription)
{-# DEPRECATED rasdRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | Replica-specific global secondary index auto scaling settings.
--
-- /Note:/ Consider using 'globalSecondaryIndexes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasdGlobalSecondaryIndexes :: Lens.Lens' ReplicaAutoScalingDescription (Lude.Maybe [ReplicaGlobalSecondaryIndexAutoScalingDescription])
rasdGlobalSecondaryIndexes = Lens.lens (globalSecondaryIndexes :: ReplicaAutoScalingDescription -> Lude.Maybe [ReplicaGlobalSecondaryIndexAutoScalingDescription]) (\s a -> s {globalSecondaryIndexes = a} :: ReplicaAutoScalingDescription)
{-# DEPRECATED rasdGlobalSecondaryIndexes "Use generic-lens or generic-optics with 'globalSecondaryIndexes' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicaProvisionedWriteCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasdReplicaProvisionedWriteCapacityAutoScalingSettings :: Lens.Lens' ReplicaAutoScalingDescription (Lude.Maybe AutoScalingSettingsDescription)
rasdReplicaProvisionedWriteCapacityAutoScalingSettings = Lens.lens (replicaProvisionedWriteCapacityAutoScalingSettings :: ReplicaAutoScalingDescription -> Lude.Maybe AutoScalingSettingsDescription) (\s a -> s {replicaProvisionedWriteCapacityAutoScalingSettings = a} :: ReplicaAutoScalingDescription)
{-# DEPRECATED rasdReplicaProvisionedWriteCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'replicaProvisionedWriteCapacityAutoScalingSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicaProvisionedReadCapacityAutoScalingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasdReplicaProvisionedReadCapacityAutoScalingSettings :: Lens.Lens' ReplicaAutoScalingDescription (Lude.Maybe AutoScalingSettingsDescription)
rasdReplicaProvisionedReadCapacityAutoScalingSettings = Lens.lens (replicaProvisionedReadCapacityAutoScalingSettings :: ReplicaAutoScalingDescription -> Lude.Maybe AutoScalingSettingsDescription) (\s a -> s {replicaProvisionedReadCapacityAutoScalingSettings = a} :: ReplicaAutoScalingDescription)
{-# DEPRECATED rasdReplicaProvisionedReadCapacityAutoScalingSettings "Use generic-lens or generic-optics with 'replicaProvisionedReadCapacityAutoScalingSettings' instead." #-}

instance Lude.FromJSON ReplicaAutoScalingDescription where
  parseJSON =
    Lude.withObject
      "ReplicaAutoScalingDescription"
      ( \x ->
          ReplicaAutoScalingDescription'
            Lude.<$> (x Lude..:? "ReplicaStatus")
            Lude.<*> (x Lude..:? "RegionName")
            Lude.<*> (x Lude..:? "GlobalSecondaryIndexes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ReplicaProvisionedWriteCapacityAutoScalingSettings")
            Lude.<*> (x Lude..:? "ReplicaProvisionedReadCapacityAutoScalingSettings")
      )
