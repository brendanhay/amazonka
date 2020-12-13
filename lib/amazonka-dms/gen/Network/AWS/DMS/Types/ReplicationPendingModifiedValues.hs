{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationPendingModifiedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationPendingModifiedValues
  ( ReplicationPendingModifiedValues (..),

    -- * Smart constructor
    mkReplicationPendingModifiedValues,

    -- * Lenses
    rpmvEngineVersion,
    rpmvMultiAZ,
    rpmvAllocatedStorage,
    rpmvReplicationInstanceClass,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the values of pending modifications to a replication instance. This data type is an object of the <https://docs.aws.amazon.com/dms/latest/APIReference/API_ReplicationInstance.html @ReplicationInstance@ > user-defined data type.
--
-- /See:/ 'mkReplicationPendingModifiedValues' smart constructor.
data ReplicationPendingModifiedValues = ReplicationPendingModifiedValues'
  { -- | The engine version number of the replication instance.
    engineVersion :: Lude.Maybe Lude.Text,
    -- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
    multiAZ :: Lude.Maybe Lude.Bool,
    -- | The amount of storage (in gigabytes) that is allocated for the replication instance.
    allocatedStorage :: Lude.Maybe Lude.Int,
    -- | The compute and memory capacity of the replication instance as defined for the specified replication instance class.
    --
    -- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
    replicationInstanceClass :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationPendingModifiedValues' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The engine version number of the replication instance.
-- * 'multiAZ' - Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
-- * 'allocatedStorage' - The amount of storage (in gigabytes) that is allocated for the replication instance.
-- * 'replicationInstanceClass' - The compute and memory capacity of the replication instance as defined for the specified replication instance class.
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
mkReplicationPendingModifiedValues ::
  ReplicationPendingModifiedValues
mkReplicationPendingModifiedValues =
  ReplicationPendingModifiedValues'
    { engineVersion = Lude.Nothing,
      multiAZ = Lude.Nothing,
      allocatedStorage = Lude.Nothing,
      replicationInstanceClass = Lude.Nothing
    }

-- | The engine version number of the replication instance.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmvEngineVersion :: Lens.Lens' ReplicationPendingModifiedValues (Lude.Maybe Lude.Text)
rpmvEngineVersion = Lens.lens (engineVersion :: ReplicationPendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: ReplicationPendingModifiedValues)
{-# DEPRECATED rpmvEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ .
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmvMultiAZ :: Lens.Lens' ReplicationPendingModifiedValues (Lude.Maybe Lude.Bool)
rpmvMultiAZ = Lens.lens (multiAZ :: ReplicationPendingModifiedValues -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: ReplicationPendingModifiedValues)
{-# DEPRECATED rpmvMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The amount of storage (in gigabytes) that is allocated for the replication instance.
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmvAllocatedStorage :: Lens.Lens' ReplicationPendingModifiedValues (Lude.Maybe Lude.Int)
rpmvAllocatedStorage = Lens.lens (allocatedStorage :: ReplicationPendingModifiedValues -> Lude.Maybe Lude.Int) (\s a -> s {allocatedStorage = a} :: ReplicationPendingModifiedValues)
{-# DEPRECATED rpmvAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | The compute and memory capacity of the replication instance as defined for the specified replication instance class.
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
--
-- /Note:/ Consider using 'replicationInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmvReplicationInstanceClass :: Lens.Lens' ReplicationPendingModifiedValues (Lude.Maybe Lude.Text)
rpmvReplicationInstanceClass = Lens.lens (replicationInstanceClass :: ReplicationPendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceClass = a} :: ReplicationPendingModifiedValues)
{-# DEPRECATED rpmvReplicationInstanceClass "Use generic-lens or generic-optics with 'replicationInstanceClass' instead." #-}

instance Lude.FromJSON ReplicationPendingModifiedValues where
  parseJSON =
    Lude.withObject
      "ReplicationPendingModifiedValues"
      ( \x ->
          ReplicationPendingModifiedValues'
            Lude.<$> (x Lude..:? "EngineVersion")
            Lude.<*> (x Lude..:? "MultiAZ")
            Lude.<*> (x Lude..:? "AllocatedStorage")
            Lude.<*> (x Lude..:? "ReplicationInstanceClass")
      )
