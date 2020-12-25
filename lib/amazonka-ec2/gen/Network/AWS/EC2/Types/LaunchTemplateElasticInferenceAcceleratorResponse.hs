{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAcceleratorResponse
  ( LaunchTemplateElasticInferenceAcceleratorResponse (..),

    -- * Smart constructor
    mkLaunchTemplateElasticInferenceAcceleratorResponse,

    -- * Lenses
    lteiarCount,
    lteiarType,
  )
where

import qualified Network.AWS.EC2.Types.Type as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an elastic inference accelerator.
--
-- /See:/ 'mkLaunchTemplateElasticInferenceAcceleratorResponse' smart constructor.
data LaunchTemplateElasticInferenceAcceleratorResponse = LaunchTemplateElasticInferenceAcceleratorResponse'
  { -- | The number of elastic inference accelerators to attach to the instance.
    --
    -- Default: 1
    count :: Core.Maybe Core.Int,
    -- | The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge.
    type' :: Core.Maybe Types.Type
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateElasticInferenceAcceleratorResponse' value with any optional fields omitted.
mkLaunchTemplateElasticInferenceAcceleratorResponse ::
  LaunchTemplateElasticInferenceAcceleratorResponse
mkLaunchTemplateElasticInferenceAcceleratorResponse =
  LaunchTemplateElasticInferenceAcceleratorResponse'
    { count =
        Core.Nothing,
      type' = Core.Nothing
    }

-- | The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lteiarCount :: Lens.Lens' LaunchTemplateElasticInferenceAcceleratorResponse (Core.Maybe Core.Int)
lteiarCount = Lens.field @"count"
{-# DEPRECATED lteiarCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lteiarType :: Lens.Lens' LaunchTemplateElasticInferenceAcceleratorResponse (Core.Maybe Types.Type)
lteiarType = Lens.field @"type'"
{-# DEPRECATED lteiarType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance
  Core.FromXML
    LaunchTemplateElasticInferenceAcceleratorResponse
  where
  parseXML x =
    LaunchTemplateElasticInferenceAcceleratorResponse'
      Core.<$> (x Core..@? "count") Core.<*> (x Core..@? "type")
