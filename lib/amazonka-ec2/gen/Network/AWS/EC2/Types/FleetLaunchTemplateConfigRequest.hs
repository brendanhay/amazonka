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
    fltcrOverrides,
    fltcrLaunchTemplateSpecification,
  )
where

import Network.AWS.EC2.Types.FleetLaunchTemplateOverridesRequest
import Network.AWS.EC2.Types.FleetLaunchTemplateSpecificationRequest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a launch template and overrides.
--
-- /See:/ 'mkFleetLaunchTemplateConfigRequest' smart constructor.
data FleetLaunchTemplateConfigRequest = FleetLaunchTemplateConfigRequest'
  { overrides ::
      Lude.Maybe
        [FleetLaunchTemplateOverridesRequest],
    launchTemplateSpecification ::
      Lude.Maybe
        FleetLaunchTemplateSpecificationRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FleetLaunchTemplateConfigRequest' with the minimum fields required to make a request.
--
-- * 'launchTemplateSpecification' - The launch template to use. You must specify either the launch template ID or launch template name in the request.
-- * 'overrides' - Any parameters that you specify override the same parameters in the launch template.
mkFleetLaunchTemplateConfigRequest ::
  FleetLaunchTemplateConfigRequest
mkFleetLaunchTemplateConfigRequest =
  FleetLaunchTemplateConfigRequest'
    { overrides = Lude.Nothing,
      launchTemplateSpecification = Lude.Nothing
    }

-- | Any parameters that you specify override the same parameters in the launch template.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltcrOverrides :: Lens.Lens' FleetLaunchTemplateConfigRequest (Lude.Maybe [FleetLaunchTemplateOverridesRequest])
fltcrOverrides = Lens.lens (overrides :: FleetLaunchTemplateConfigRequest -> Lude.Maybe [FleetLaunchTemplateOverridesRequest]) (\s a -> s {overrides = a} :: FleetLaunchTemplateConfigRequest)
{-# DEPRECATED fltcrOverrides "Use generic-lens or generic-optics with 'overrides' instead." #-}

-- | The launch template to use. You must specify either the launch template ID or launch template name in the request.
--
-- /Note:/ Consider using 'launchTemplateSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltcrLaunchTemplateSpecification :: Lens.Lens' FleetLaunchTemplateConfigRequest (Lude.Maybe FleetLaunchTemplateSpecificationRequest)
fltcrLaunchTemplateSpecification = Lens.lens (launchTemplateSpecification :: FleetLaunchTemplateConfigRequest -> Lude.Maybe FleetLaunchTemplateSpecificationRequest) (\s a -> s {launchTemplateSpecification = a} :: FleetLaunchTemplateConfigRequest)
{-# DEPRECATED fltcrLaunchTemplateSpecification "Use generic-lens or generic-optics with 'launchTemplateSpecification' instead." #-}

instance Lude.ToQuery FleetLaunchTemplateConfigRequest where
  toQuery FleetLaunchTemplateConfigRequest' {..} =
    Lude.mconcat
      [ Lude.toQuery (Lude.toQueryList "Overrides" Lude.<$> overrides),
        "LaunchTemplateSpecification" Lude.=: launchTemplateSpecification
      ]
