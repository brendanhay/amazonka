{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpus
  ( ElasticGpus (..),

    -- * Smart constructor
    mkElasticGpus,

    -- * Lenses
    egAvailabilityZone,
    egElasticGpuHealth,
    egElasticGpuId,
    egElasticGpuState,
    egElasticGpuType,
    egInstanceId,
    egTags,
  )
where

import qualified Network.AWS.EC2.Types.ElasticGpuHealth as Types
import qualified Network.AWS.EC2.Types.ElasticGpuState as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Elastic Graphics accelerator.
--
-- /See:/ 'mkElasticGpus' smart constructor.
data ElasticGpus = ElasticGpus'
  { -- | The Availability Zone in the which the Elastic Graphics accelerator resides.
    availabilityZone :: Core.Maybe Types.String,
    -- | The status of the Elastic Graphics accelerator.
    elasticGpuHealth :: Core.Maybe Types.ElasticGpuHealth,
    -- | The ID of the Elastic Graphics accelerator.
    elasticGpuId :: Core.Maybe Types.String,
    -- | The state of the Elastic Graphics accelerator.
    elasticGpuState :: Core.Maybe Types.ElasticGpuState,
    -- | The type of Elastic Graphics accelerator.
    elasticGpuType :: Core.Maybe Types.String,
    -- | The ID of the instance to which the Elastic Graphics accelerator is attached.
    instanceId :: Core.Maybe Types.String,
    -- | The tags assigned to the Elastic Graphics accelerator.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticGpus' value with any optional fields omitted.
mkElasticGpus ::
  ElasticGpus
mkElasticGpus =
  ElasticGpus'
    { availabilityZone = Core.Nothing,
      elasticGpuHealth = Core.Nothing,
      elasticGpuId = Core.Nothing,
      elasticGpuState = Core.Nothing,
      elasticGpuType = Core.Nothing,
      instanceId = Core.Nothing,
      tags = Core.Nothing
    }

-- | The Availability Zone in the which the Elastic Graphics accelerator resides.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egAvailabilityZone :: Lens.Lens' ElasticGpus (Core.Maybe Types.String)
egAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED egAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The status of the Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'elasticGpuHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egElasticGpuHealth :: Lens.Lens' ElasticGpus (Core.Maybe Types.ElasticGpuHealth)
egElasticGpuHealth = Lens.field @"elasticGpuHealth"
{-# DEPRECATED egElasticGpuHealth "Use generic-lens or generic-optics with 'elasticGpuHealth' instead." #-}

-- | The ID of the Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'elasticGpuId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egElasticGpuId :: Lens.Lens' ElasticGpus (Core.Maybe Types.String)
egElasticGpuId = Lens.field @"elasticGpuId"
{-# DEPRECATED egElasticGpuId "Use generic-lens or generic-optics with 'elasticGpuId' instead." #-}

-- | The state of the Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'elasticGpuState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egElasticGpuState :: Lens.Lens' ElasticGpus (Core.Maybe Types.ElasticGpuState)
egElasticGpuState = Lens.field @"elasticGpuState"
{-# DEPRECATED egElasticGpuState "Use generic-lens or generic-optics with 'elasticGpuState' instead." #-}

-- | The type of Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'elasticGpuType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egElasticGpuType :: Lens.Lens' ElasticGpus (Core.Maybe Types.String)
egElasticGpuType = Lens.field @"elasticGpuType"
{-# DEPRECATED egElasticGpuType "Use generic-lens or generic-optics with 'elasticGpuType' instead." #-}

-- | The ID of the instance to which the Elastic Graphics accelerator is attached.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egInstanceId :: Lens.Lens' ElasticGpus (Core.Maybe Types.String)
egInstanceId = Lens.field @"instanceId"
{-# DEPRECATED egInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The tags assigned to the Elastic Graphics accelerator.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egTags :: Lens.Lens' ElasticGpus (Core.Maybe [Types.Tag])
egTags = Lens.field @"tags"
{-# DEPRECATED egTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML ElasticGpus where
  parseXML x =
    ElasticGpus'
      Core.<$> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "elasticGpuHealth")
      Core.<*> (x Core..@? "elasticGpuId")
      Core.<*> (x Core..@? "elasticGpuState")
      Core.<*> (x Core..@? "elasticGpuType")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
