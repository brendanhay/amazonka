{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaAutoScalingUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReplicaAutoScalingUpdate
  ( ReplicaAutoScalingUpdate (..),

    -- * Smart constructor
    mkReplicaAutoScalingUpdate,

    -- * Lenses
    rasuRegionName,
    rasuReplicaProvisionedReadCapacityAutoScalingUpdate,
    rasuReplicaGlobalSecondaryIndexUpdates,
  )
where

import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the auto scaling settings of a replica that will be modified.
--
-- /See:/ 'mkReplicaAutoScalingUpdate' smart constructor.
data ReplicaAutoScalingUpdate = ReplicaAutoScalingUpdate'
  { -- | The Region where the replica exists.
    regionName :: Lude.Text,
    replicaProvisionedReadCapacityAutoScalingUpdate :: Lude.Maybe AutoScalingSettingsUpdate,
    -- | Represents the auto scaling settings of global secondary indexes that will be modified.
    replicaGlobalSecondaryIndexUpdates :: Lude.Maybe [ReplicaGlobalSecondaryIndexAutoScalingUpdate]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicaAutoScalingUpdate' with the minimum fields required to make a request.
--
-- * 'regionName' - The Region where the replica exists.
-- * 'replicaProvisionedReadCapacityAutoScalingUpdate' -
-- * 'replicaGlobalSecondaryIndexUpdates' - Represents the auto scaling settings of global secondary indexes that will be modified.
mkReplicaAutoScalingUpdate ::
  -- | 'regionName'
  Lude.Text ->
  ReplicaAutoScalingUpdate
mkReplicaAutoScalingUpdate pRegionName_ =
  ReplicaAutoScalingUpdate'
    { regionName = pRegionName_,
      replicaProvisionedReadCapacityAutoScalingUpdate = Lude.Nothing,
      replicaGlobalSecondaryIndexUpdates = Lude.Nothing
    }

-- | The Region where the replica exists.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasuRegionName :: Lens.Lens' ReplicaAutoScalingUpdate Lude.Text
rasuRegionName = Lens.lens (regionName :: ReplicaAutoScalingUpdate -> Lude.Text) (\s a -> s {regionName = a} :: ReplicaAutoScalingUpdate)
{-# DEPRECATED rasuRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicaProvisionedReadCapacityAutoScalingUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasuReplicaProvisionedReadCapacityAutoScalingUpdate :: Lens.Lens' ReplicaAutoScalingUpdate (Lude.Maybe AutoScalingSettingsUpdate)
rasuReplicaProvisionedReadCapacityAutoScalingUpdate = Lens.lens (replicaProvisionedReadCapacityAutoScalingUpdate :: ReplicaAutoScalingUpdate -> Lude.Maybe AutoScalingSettingsUpdate) (\s a -> s {replicaProvisionedReadCapacityAutoScalingUpdate = a} :: ReplicaAutoScalingUpdate)
{-# DEPRECATED rasuReplicaProvisionedReadCapacityAutoScalingUpdate "Use generic-lens or generic-optics with 'replicaProvisionedReadCapacityAutoScalingUpdate' instead." #-}

-- | Represents the auto scaling settings of global secondary indexes that will be modified.
--
-- /Note:/ Consider using 'replicaGlobalSecondaryIndexUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rasuReplicaGlobalSecondaryIndexUpdates :: Lens.Lens' ReplicaAutoScalingUpdate (Lude.Maybe [ReplicaGlobalSecondaryIndexAutoScalingUpdate])
rasuReplicaGlobalSecondaryIndexUpdates = Lens.lens (replicaGlobalSecondaryIndexUpdates :: ReplicaAutoScalingUpdate -> Lude.Maybe [ReplicaGlobalSecondaryIndexAutoScalingUpdate]) (\s a -> s {replicaGlobalSecondaryIndexUpdates = a} :: ReplicaAutoScalingUpdate)
{-# DEPRECATED rasuReplicaGlobalSecondaryIndexUpdates "Use generic-lens or generic-optics with 'replicaGlobalSecondaryIndexUpdates' instead." #-}

instance Lude.ToJSON ReplicaAutoScalingUpdate where
  toJSON ReplicaAutoScalingUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RegionName" Lude..= regionName),
            ("ReplicaProvisionedReadCapacityAutoScalingUpdate" Lude..=)
              Lude.<$> replicaProvisionedReadCapacityAutoScalingUpdate,
            ("ReplicaGlobalSecondaryIndexUpdates" Lude..=)
              Lude.<$> replicaGlobalSecondaryIndexUpdates
          ]
      )
