{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateConfig
  ( LaunchTemplateConfig (..),

    -- * Smart constructor
    mkLaunchTemplateConfig,

    -- * Lenses
    ltcLaunchTemplateSpecification,
    ltcOverrides,
  )
where

import qualified Network.AWS.EC2.Types.FleetLaunchTemplateSpecification as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateOverrides as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch template and overrides.
--
-- /See:/ 'mkLaunchTemplateConfig' smart constructor.
data LaunchTemplateConfig = LaunchTemplateConfig'
  { -- | The launch template.
    launchTemplateSpecification :: Core.Maybe Types.FleetLaunchTemplateSpecification,
    -- | Any parameters that you specify override the same parameters in the launch template.
    overrides :: Core.Maybe [Types.LaunchTemplateOverrides]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateConfig' value with any optional fields omitted.
mkLaunchTemplateConfig ::
  LaunchTemplateConfig
mkLaunchTemplateConfig =
  LaunchTemplateConfig'
    { launchTemplateSpecification = Core.Nothing,
      overrides = Core.Nothing
    }

-- | The launch template.
--
-- /Note:/ Consider using 'launchTemplateSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcLaunchTemplateSpecification :: Lens.Lens' LaunchTemplateConfig (Core.Maybe Types.FleetLaunchTemplateSpecification)
ltcLaunchTemplateSpecification = Lens.field @"launchTemplateSpecification"
{-# DEPRECATED ltcLaunchTemplateSpecification "Use generic-lens or generic-optics with 'launchTemplateSpecification' instead." #-}

-- | Any parameters that you specify override the same parameters in the launch template.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcOverrides :: Lens.Lens' LaunchTemplateConfig (Core.Maybe [Types.LaunchTemplateOverrides])
ltcOverrides = Lens.field @"overrides"
{-# DEPRECATED ltcOverrides "Use generic-lens or generic-optics with 'overrides' instead." #-}

instance Core.FromXML LaunchTemplateConfig where
  parseXML x =
    LaunchTemplateConfig'
      Core.<$> (x Core..@? "launchTemplateSpecification")
      Core.<*> (x Core..@? "overrides" Core..<@> Core.parseXMLList "item")
