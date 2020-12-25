{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
  ( LaunchTemplateElasticInferenceAccelerator (..),

    -- * Smart constructor
    mkLaunchTemplateElasticInferenceAccelerator,

    -- * Lenses
    lteiaType,
    lteiaCount,
  )
where

import qualified Network.AWS.EC2.Types.Type as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an elastic inference accelerator.
--
-- /See:/ 'mkLaunchTemplateElasticInferenceAccelerator' smart constructor.
data LaunchTemplateElasticInferenceAccelerator = LaunchTemplateElasticInferenceAccelerator'
  { -- | The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge.
    type' :: Types.Type,
    -- | The number of elastic inference accelerators to attach to the instance.
    --
    -- Default: 1
    count :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateElasticInferenceAccelerator' value with any optional fields omitted.
mkLaunchTemplateElasticInferenceAccelerator ::
  -- | 'type\''
  Types.Type ->
  LaunchTemplateElasticInferenceAccelerator
mkLaunchTemplateElasticInferenceAccelerator type' =
  LaunchTemplateElasticInferenceAccelerator'
    { type',
      count = Core.Nothing
    }

-- | The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lteiaType :: Lens.Lens' LaunchTemplateElasticInferenceAccelerator Types.Type
lteiaType = Lens.field @"type'"
{-# DEPRECATED lteiaType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The number of elastic inference accelerators to attach to the instance.
--
-- Default: 1
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lteiaCount :: Lens.Lens' LaunchTemplateElasticInferenceAccelerator (Core.Maybe Core.Natural)
lteiaCount = Lens.field @"count"
{-# DEPRECATED lteiaCount "Use generic-lens or generic-optics with 'count' instead." #-}
