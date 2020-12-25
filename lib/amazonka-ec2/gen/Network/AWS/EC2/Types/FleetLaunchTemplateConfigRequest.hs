{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateConfigRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateConfigRequest
  ( FleetLaunchTemplateConfigRequest (..),

    -- * Smart constructor
    mkFleetLaunchTemplateConfigRequest,

    -- * Lenses
    fltcrLaunchTemplateSpecification,
    fltcrOverrides,
  )
where

import qualified Network.AWS.EC2.Types.FleetLaunchTemplateOverridesRequest as Types
import qualified Network.AWS.EC2.Types.FleetLaunchTemplateSpecificationRequest as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch template and overrides.
--
-- /See:/ 'mkFleetLaunchTemplateConfigRequest' smart constructor.
data FleetLaunchTemplateConfigRequest = FleetLaunchTemplateConfigRequest'
  { -- | The launch template to use. You must specify either the launch template ID or launch template name in the request.
    launchTemplateSpecification :: Core.Maybe Types.FleetLaunchTemplateSpecificationRequest,
    -- | Any parameters that you specify override the same parameters in the launch template.
    overrides :: Core.Maybe [Types.FleetLaunchTemplateOverridesRequest]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FleetLaunchTemplateConfigRequest' value with any optional fields omitted.
mkFleetLaunchTemplateConfigRequest ::
  FleetLaunchTemplateConfigRequest
mkFleetLaunchTemplateConfigRequest =
  FleetLaunchTemplateConfigRequest'
    { launchTemplateSpecification =
        Core.Nothing,
      overrides = Core.Nothing
    }

-- | The launch template to use. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltcrLaunchTemplateSpecification :: Lens.Lens' FleetLaunchTemplateConfigRequest (Core.Maybe Types.FleetLaunchTemplateSpecificationRequest)
fltcrLaunchTemplateSpecification = Lens.field @"launchTemplateSpecification"
{-# DEPRECATED fltcrLaunchTemplateSpecification "Use generic-lens or generic-optics with 'launchTemplateSpecification' instead." #-}

-- | Any parameters that you specify override the same parameters in the launch template.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltcrOverrides :: Lens.Lens' FleetLaunchTemplateConfigRequest (Core.Maybe [Types.FleetLaunchTemplateOverridesRequest])
fltcrOverrides = Lens.field @"overrides"
{-# DEPRECATED fltcrOverrides "Use generic-lens or generic-optics with 'overrides' instead." #-}
