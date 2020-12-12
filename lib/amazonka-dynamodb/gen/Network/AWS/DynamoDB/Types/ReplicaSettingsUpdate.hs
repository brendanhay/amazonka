{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaSettingsUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaSettingsUpdate
  ( ReplicaSettingsUpdate (..),

    -- * Smart constructor
    mkReplicaSettingsUpdate,

    -- * Lenses
    rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate,
    rsuReplicaProvisionedReadCapacityUnits,
    rsuReplicaGlobalSecondaryIndexSettingsUpdate,
    rsuRegionName,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the settings for a global table in a Region that will be modified.
--
-- /See:/ 'mkReplicaSettingsUpdate' smart constructor.
data ReplicaSettingsUpdate = ReplicaSettingsUpdate'
  { replicaProvisionedReadCapacityAutoScalingSettingsUpdate ::
      Lude.Maybe AutoScalingSettingsUpdate,
    replicaProvisionedReadCapacityUnits ::
      Lude.Maybe Lude.Natural,
    replicaGlobalSecondaryIndexSettingsUpdate ::
      Lude.Maybe
        ( Lude.NonEmpty
            ReplicaGlobalSecondaryIndexSettingsUpdate
        ),
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

-- | Creates a value of 'ReplicaSettingsUpdate' with the minimum fields required to make a request.
--
-- * 'regionName' - The Region of the replica to be added.
-- * 'replicaGlobalSecondaryIndexSettingsUpdate' - Represents the settings of a global secondary index for a global table that will be modified.
-- * 'replicaProvisionedReadCapacityAutoScalingSettingsUpdate' - Auto scaling settings for managing a global table replica's read capacity units.
-- * 'replicaProvisionedReadCapacityUnits' - The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
mkReplicaSettingsUpdate ::
  -- | 'regionName'
  Lude.Text ->
  ReplicaSettingsUpdate
mkReplicaSettingsUpdate pRegionName_ =
  ReplicaSettingsUpdate'
    { replicaProvisionedReadCapacityAutoScalingSettingsUpdate =
        Lude.Nothing,
      replicaProvisionedReadCapacityUnits = Lude.Nothing,
      replicaGlobalSecondaryIndexSettingsUpdate = Lude.Nothing,
      regionName = pRegionName_
    }

-- | Auto scaling settings for managing a global table replica's read capacity units.
--
-- /Note:/ Consider using 'replicaProvisionedReadCapacityAutoScalingSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate :: Lens.Lens' ReplicaSettingsUpdate (Lude.Maybe AutoScalingSettingsUpdate)
rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate = Lens.lens (replicaProvisionedReadCapacityAutoScalingSettingsUpdate :: ReplicaSettingsUpdate -> Lude.Maybe AutoScalingSettingsUpdate) (\s a -> s {replicaProvisionedReadCapacityAutoScalingSettingsUpdate = a} :: ReplicaSettingsUpdate)
{-# DEPRECATED rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate "Use generic-lens or generic-optics with 'replicaProvisionedReadCapacityAutoScalingSettingsUpdate' instead." #-}

-- | The maximum number of strongly consistent reads consumed per second before DynamoDB returns a @ThrottlingException@ . For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/WorkingWithTables.html#ProvisionedThroughput Specifying Read and Write Requirements> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'replicaProvisionedReadCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsuReplicaProvisionedReadCapacityUnits :: Lens.Lens' ReplicaSettingsUpdate (Lude.Maybe Lude.Natural)
rsuReplicaProvisionedReadCapacityUnits = Lens.lens (replicaProvisionedReadCapacityUnits :: ReplicaSettingsUpdate -> Lude.Maybe Lude.Natural) (\s a -> s {replicaProvisionedReadCapacityUnits = a} :: ReplicaSettingsUpdate)
{-# DEPRECATED rsuReplicaProvisionedReadCapacityUnits "Use generic-lens or generic-optics with 'replicaProvisionedReadCapacityUnits' instead." #-}

-- | Represents the settings of a global secondary index for a global table that will be modified.
--
-- /Note:/ Consider using 'replicaGlobalSecondaryIndexSettingsUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsuReplicaGlobalSecondaryIndexSettingsUpdate :: Lens.Lens' ReplicaSettingsUpdate (Lude.Maybe (Lude.NonEmpty ReplicaGlobalSecondaryIndexSettingsUpdate))
rsuReplicaGlobalSecondaryIndexSettingsUpdate = Lens.lens (replicaGlobalSecondaryIndexSettingsUpdate :: ReplicaSettingsUpdate -> Lude.Maybe (Lude.NonEmpty ReplicaGlobalSecondaryIndexSettingsUpdate)) (\s a -> s {replicaGlobalSecondaryIndexSettingsUpdate = a} :: ReplicaSettingsUpdate)
{-# DEPRECATED rsuReplicaGlobalSecondaryIndexSettingsUpdate "Use generic-lens or generic-optics with 'replicaGlobalSecondaryIndexSettingsUpdate' instead." #-}

-- | The Region of the replica to be added.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsuRegionName :: Lens.Lens' ReplicaSettingsUpdate Lude.Text
rsuRegionName = Lens.lens (regionName :: ReplicaSettingsUpdate -> Lude.Text) (\s a -> s {regionName = a} :: ReplicaSettingsUpdate)
{-# DEPRECATED rsuRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Lude.ToJSON ReplicaSettingsUpdate where
  toJSON ReplicaSettingsUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ( "ReplicaProvisionedReadCapacityAutoScalingSettingsUpdate"
                Lude..=
            )
              Lude.<$> replicaProvisionedReadCapacityAutoScalingSettingsUpdate,
            ("ReplicaProvisionedReadCapacityUnits" Lude..=)
              Lude.<$> replicaProvisionedReadCapacityUnits,
            ("ReplicaGlobalSecondaryIndexSettingsUpdate" Lude..=)
              Lude.<$> replicaGlobalSecondaryIndexSettingsUpdate,
            Lude.Just ("RegionName" Lude..= regionName)
          ]
      )
