{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
  ( LaunchTemplateAndOverridesResponse (..),

    -- * Smart constructor
    mkLaunchTemplateAndOverridesResponse,

    -- * Lenses
    ltaorLaunchTemplateSpecification,
    ltaorOverrides,
  )
where

import qualified Network.AWS.EC2.Types.FleetLaunchTemplateOverrides as Types
import qualified Network.AWS.EC2.Types.FleetLaunchTemplateSpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch template and overrides.
--
-- /See:/ 'mkLaunchTemplateAndOverridesResponse' smart constructor.
data LaunchTemplateAndOverridesResponse = LaunchTemplateAndOverridesResponse'
  { -- | The launch template.
    launchTemplateSpecification :: Core.Maybe Types.FleetLaunchTemplateSpecification,
    -- | Any parameters that you specify override the same parameters in the launch template.
    overrides :: Core.Maybe Types.FleetLaunchTemplateOverrides
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateAndOverridesResponse' value with any optional fields omitted.
mkLaunchTemplateAndOverridesResponse ::
  LaunchTemplateAndOverridesResponse
mkLaunchTemplateAndOverridesResponse =
  LaunchTemplateAndOverridesResponse'
    { launchTemplateSpecification =
        Core.Nothing,
      overrides = Core.Nothing
    }

-- | The launch template.
--
-- /Note:/ Consider using 'launchTemplateSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltaorLaunchTemplateSpecification :: Lens.Lens' LaunchTemplateAndOverridesResponse (Core.Maybe Types.FleetLaunchTemplateSpecification)
ltaorLaunchTemplateSpecification = Lens.field @"launchTemplateSpecification"
{-# DEPRECATED ltaorLaunchTemplateSpecification "Use generic-lens or generic-optics with 'launchTemplateSpecification' instead." #-}

-- | Any parameters that you specify override the same parameters in the launch template.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltaorOverrides :: Lens.Lens' LaunchTemplateAndOverridesResponse (Core.Maybe Types.FleetLaunchTemplateOverrides)
ltaorOverrides = Lens.field @"overrides"
{-# DEPRECATED ltaorOverrides "Use generic-lens or generic-optics with 'overrides' instead." #-}

instance Core.FromXML LaunchTemplateAndOverridesResponse where
  parseXML x =
    LaunchTemplateAndOverridesResponse'
      Core.<$> (x Core..@? "launchTemplateSpecification")
      Core.<*> (x Core..@? "overrides")
