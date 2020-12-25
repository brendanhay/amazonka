{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.OrderableReplicationInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.OrderableReplicationInstance
  ( OrderableReplicationInstance (..),

    -- * Smart constructor
    mkOrderableReplicationInstance,

    -- * Lenses
    oriAvailabilityZones,
    oriDefaultAllocatedStorage,
    oriEngineVersion,
    oriIncludedAllocatedStorage,
    oriMaxAllocatedStorage,
    oriMinAllocatedStorage,
    oriReleaseStatus,
    oriReplicationInstanceClass,
    oriStorageType,
  )
where

import qualified Network.AWS.DMS.Types.ReleaseStatusValues as Types
import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | In response to the @DescribeOrderableReplicationInstances@ operation, this object describes an available replication instance. This description includes the replication instance's type, engine version, and allocated storage.
--
-- /See:/ 'mkOrderableReplicationInstance' smart constructor.
data OrderableReplicationInstance = OrderableReplicationInstance'
  { -- | List of Availability Zones for this replication instance.
    availabilityZones :: Core.Maybe [Types.String],
    -- | The default amount of storage (in gigabytes) that is allocated for the replication instance.
    defaultAllocatedStorage :: Core.Maybe Core.Int,
    -- | The version of the replication engine.
    engineVersion :: Core.Maybe Types.String,
    -- | The amount of storage (in gigabytes) that is allocated for the replication instance.
    includedAllocatedStorage :: Core.Maybe Core.Int,
    -- | The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
    maxAllocatedStorage :: Core.Maybe Core.Int,
    -- | The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
    minAllocatedStorage :: Core.Maybe Core.Int,
    -- | The value returned when the specified @EngineVersion@ of the replication instance is in Beta or test mode. This indicates some features might not work as expected.
    releaseStatus :: Core.Maybe Types.ReleaseStatusValues,
    -- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ .
    --
    -- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
    replicationInstanceClass :: Core.Maybe Types.String,
    -- | The type of storage used by the replication instance.
    storageType :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrderableReplicationInstance' value with any optional fields omitted.
mkOrderableReplicationInstance ::
  OrderableReplicationInstance
mkOrderableReplicationInstance =
  OrderableReplicationInstance'
    { availabilityZones = Core.Nothing,
      defaultAllocatedStorage = Core.Nothing,
      engineVersion = Core.Nothing,
      includedAllocatedStorage = Core.Nothing,
      maxAllocatedStorage = Core.Nothing,
      minAllocatedStorage = Core.Nothing,
      releaseStatus = Core.Nothing,
      replicationInstanceClass = Core.Nothing,
      storageType = Core.Nothing
    }

-- | List of Availability Zones for this replication instance.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oriAvailabilityZones :: Lens.Lens' OrderableReplicationInstance (Core.Maybe [Types.String])
oriAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED oriAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The default amount of storage (in gigabytes) that is allocated for the replication instance.
--
-- /Note:/ Consider using 'defaultAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oriDefaultAllocatedStorage :: Lens.Lens' OrderableReplicationInstance (Core.Maybe Core.Int)
oriDefaultAllocatedStorage = Lens.field @"defaultAllocatedStorage"
{-# DEPRECATED oriDefaultAllocatedStorage "Use generic-lens or generic-optics with 'defaultAllocatedStorage' instead." #-}

-- | The version of the replication engine.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oriEngineVersion :: Lens.Lens' OrderableReplicationInstance (Core.Maybe Types.String)
oriEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED oriEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The amount of storage (in gigabytes) that is allocated for the replication instance.
--
-- /Note:/ Consider using 'includedAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oriIncludedAllocatedStorage :: Lens.Lens' OrderableReplicationInstance (Core.Maybe Core.Int)
oriIncludedAllocatedStorage = Lens.field @"includedAllocatedStorage"
{-# DEPRECATED oriIncludedAllocatedStorage "Use generic-lens or generic-optics with 'includedAllocatedStorage' instead." #-}

-- | The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
--
-- /Note:/ Consider using 'maxAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oriMaxAllocatedStorage :: Lens.Lens' OrderableReplicationInstance (Core.Maybe Core.Int)
oriMaxAllocatedStorage = Lens.field @"maxAllocatedStorage"
{-# DEPRECATED oriMaxAllocatedStorage "Use generic-lens or generic-optics with 'maxAllocatedStorage' instead." #-}

-- | The minimum amount of storage (in gigabytes) that can be allocated for the replication instance.
--
-- /Note:/ Consider using 'minAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oriMinAllocatedStorage :: Lens.Lens' OrderableReplicationInstance (Core.Maybe Core.Int)
oriMinAllocatedStorage = Lens.field @"minAllocatedStorage"
{-# DEPRECATED oriMinAllocatedStorage "Use generic-lens or generic-optics with 'minAllocatedStorage' instead." #-}

-- | The value returned when the specified @EngineVersion@ of the replication instance is in Beta or test mode. This indicates some features might not work as expected.
--
-- /Note:/ Consider using 'releaseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oriReleaseStatus :: Lens.Lens' OrderableReplicationInstance (Core.Maybe Types.ReleaseStatusValues)
oriReleaseStatus = Lens.field @"releaseStatus"
{-# DEPRECATED oriReleaseStatus "Use generic-lens or generic-optics with 'releaseStatus' instead." #-}

-- | The compute and memory capacity of the replication instance as defined for the specified replication instance class. For example to specify the instance class dms.c4.large, set this parameter to @"dms.c4.large"@ .
--
-- For more information on the settings and capacities for the available replication instance classes, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_ReplicationInstance.html#CHAP_ReplicationInstance.InDepth Selecting the right AWS DMS replication instance for your migration> .
--
-- /Note:/ Consider using 'replicationInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oriReplicationInstanceClass :: Lens.Lens' OrderableReplicationInstance (Core.Maybe Types.String)
oriReplicationInstanceClass = Lens.field @"replicationInstanceClass"
{-# DEPRECATED oriReplicationInstanceClass "Use generic-lens or generic-optics with 'replicationInstanceClass' instead." #-}

-- | The type of storage used by the replication instance.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oriStorageType :: Lens.Lens' OrderableReplicationInstance (Core.Maybe Types.String)
oriStorageType = Lens.field @"storageType"
{-# DEPRECATED oriStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Core.FromJSON OrderableReplicationInstance where
  parseJSON =
    Core.withObject "OrderableReplicationInstance" Core.$
      \x ->
        OrderableReplicationInstance'
          Core.<$> (x Core..:? "AvailabilityZones")
          Core.<*> (x Core..:? "DefaultAllocatedStorage")
          Core.<*> (x Core..:? "EngineVersion")
          Core.<*> (x Core..:? "IncludedAllocatedStorage")
          Core.<*> (x Core..:? "MaxAllocatedStorage")
          Core.<*> (x Core..:? "MinAllocatedStorage")
          Core.<*> (x Core..:? "ReleaseStatus")
          Core.<*> (x Core..:? "ReplicationInstanceClass")
          Core.<*> (x Core..:? "StorageType")
