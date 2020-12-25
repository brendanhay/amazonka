{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ResourceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ResourceConfiguration
  ( ResourceConfiguration (..),

    -- * Smart constructor
    mkResourceConfiguration,

    -- * Lenses
    rcComputeType,
    rcVolumeSizeInGB,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.ComputeType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration of the resource used to execute the @containerAction@ .
--
-- /See:/ 'mkResourceConfiguration' smart constructor.
data ResourceConfiguration = ResourceConfiguration'
  { -- | The type of the compute resource used to execute the @containerAction@ . Possible values are: @ACU_1@ (vCPU=4, memory=16 GiB) or @ACU_2@ (vCPU=8, memory=32 GiB).
    computeType :: Types.ComputeType,
    -- | The size, in GB, of the persistent storage available to the resource instance used to execute the @containerAction@ (min: 1, max: 50).
    volumeSizeInGB :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceConfiguration' value with any optional fields omitted.
mkResourceConfiguration ::
  -- | 'computeType'
  Types.ComputeType ->
  -- | 'volumeSizeInGB'
  Core.Natural ->
  ResourceConfiguration
mkResourceConfiguration computeType volumeSizeInGB =
  ResourceConfiguration' {computeType, volumeSizeInGB}

-- | The type of the compute resource used to execute the @containerAction@ . Possible values are: @ACU_1@ (vCPU=4, memory=16 GiB) or @ACU_2@ (vCPU=8, memory=32 GiB).
--
-- /Note:/ Consider using 'computeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcComputeType :: Lens.Lens' ResourceConfiguration Types.ComputeType
rcComputeType = Lens.field @"computeType"
{-# DEPRECATED rcComputeType "Use generic-lens or generic-optics with 'computeType' instead." #-}

-- | The size, in GB, of the persistent storage available to the resource instance used to execute the @containerAction@ (min: 1, max: 50).
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcVolumeSizeInGB :: Lens.Lens' ResourceConfiguration Core.Natural
rcVolumeSizeInGB = Lens.field @"volumeSizeInGB"
{-# DEPRECATED rcVolumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead." #-}

instance Core.FromJSON ResourceConfiguration where
  toJSON ResourceConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("computeType" Core..= computeType),
            Core.Just ("volumeSizeInGB" Core..= volumeSizeInGB)
          ]
      )

instance Core.FromJSON ResourceConfiguration where
  parseJSON =
    Core.withObject "ResourceConfiguration" Core.$
      \x ->
        ResourceConfiguration'
          Core.<$> (x Core..: "computeType") Core.<*> (x Core..: "volumeSizeInGB")
