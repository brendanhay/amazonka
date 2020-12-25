{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateConfig
  ( FleetLaunchTemplateConfig (..),

    -- * Smart constructor
    mkFleetLaunchTemplateConfig,

    -- * Lenses
    fltcLaunchTemplateSpecification,
    fltcOverrides,
  )
where

import qualified Network.AWS.EC2.Types.FleetLaunchTemplateOverrides as Types
import qualified Network.AWS.EC2.Types.FleetLaunchTemplateSpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch template and overrides.
--
-- /See:/ 'mkFleetLaunchTemplateConfig' smart constructor.
data FleetLaunchTemplateConfig = FleetLaunchTemplateConfig'
  { -- | The launch template.
    launchTemplateSpecification :: Core.Maybe Types.FleetLaunchTemplateSpecification,
    -- | Any parameters that you specify override the same parameters in the launch template.
    overrides :: Core.Maybe [Types.FleetLaunchTemplateOverrides]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FleetLaunchTemplateConfig' value with any optional fields omitted.
mkFleetLaunchTemplateConfig ::
  FleetLaunchTemplateConfig
mkFleetLaunchTemplateConfig =
  FleetLaunchTemplateConfig'
    { launchTemplateSpecification =
        Core.Nothing,
      overrides = Core.Nothing
    }

-- | The launch template.
--
-- /Note:/ Consider using 'launchTemplateSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltcLaunchTemplateSpecification :: Lens.Lens' FleetLaunchTemplateConfig (Core.Maybe Types.FleetLaunchTemplateSpecification)
fltcLaunchTemplateSpecification = Lens.field @"launchTemplateSpecification"
{-# DEPRECATED fltcLaunchTemplateSpecification "Use generic-lens or generic-optics with 'launchTemplateSpecification' instead." #-}

-- | Any parameters that you specify override the same parameters in the launch template.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltcOverrides :: Lens.Lens' FleetLaunchTemplateConfig (Core.Maybe [Types.FleetLaunchTemplateOverrides])
fltcOverrides = Lens.field @"overrides"
{-# DEPRECATED fltcOverrides "Use generic-lens or generic-optics with 'overrides' instead." #-}

instance Core.FromXML FleetLaunchTemplateConfig where
  parseXML x =
    FleetLaunchTemplateConfig'
      Core.<$> (x Core..@? "launchTemplateSpecification")
      Core.<*> (x Core..@? "overrides" Core..<@> Core.parseXMLList "item")
