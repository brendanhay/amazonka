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
    ltaoOverrides,
    ltaoLaunchTemplateSpecification,
  )
where

import Network.AWS.EC2.Types.FleetLaunchTemplateOverrides
import Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a launch template and overrides.
--
-- /See:/ 'mkLaunchTemplateAndOverridesResponse' smart constructor.
data LaunchTemplateAndOverridesResponse = LaunchTemplateAndOverridesResponse'
  { overrides ::
      Lude.Maybe
        FleetLaunchTemplateOverrides,
    launchTemplateSpecification ::
      Lude.Maybe
        FleetLaunchTemplateSpecification
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateAndOverridesResponse' with the minimum fields required to make a request.
--
-- * 'launchTemplateSpecification' - The launch template.
-- * 'overrides' - Any parameters that you specify override the same parameters in the launch template.
mkLaunchTemplateAndOverridesResponse ::
  LaunchTemplateAndOverridesResponse
mkLaunchTemplateAndOverridesResponse =
  LaunchTemplateAndOverridesResponse'
    { overrides = Lude.Nothing,
      launchTemplateSpecification = Lude.Nothing
    }

-- | Any parameters that you specify override the same parameters in the launch template.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltaoOverrides :: Lens.Lens' LaunchTemplateAndOverridesResponse (Lude.Maybe FleetLaunchTemplateOverrides)
ltaoOverrides = Lens.lens (overrides :: LaunchTemplateAndOverridesResponse -> Lude.Maybe FleetLaunchTemplateOverrides) (\s a -> s {overrides = a} :: LaunchTemplateAndOverridesResponse)
{-# DEPRECATED ltaoOverrides "Use generic-lens or generic-optics with 'overrides' instead." #-}

-- | The launch template.
--
-- /Note:/ Consider using 'launchTemplateSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltaoLaunchTemplateSpecification :: Lens.Lens' LaunchTemplateAndOverridesResponse (Lude.Maybe FleetLaunchTemplateSpecification)
ltaoLaunchTemplateSpecification = Lens.lens (launchTemplateSpecification :: LaunchTemplateAndOverridesResponse -> Lude.Maybe FleetLaunchTemplateSpecification) (\s a -> s {launchTemplateSpecification = a} :: LaunchTemplateAndOverridesResponse)
{-# DEPRECATED ltaoLaunchTemplateSpecification "Use generic-lens or generic-optics with 'launchTemplateSpecification' instead." #-}

instance Lude.FromXML LaunchTemplateAndOverridesResponse where
  parseXML x =
    LaunchTemplateAndOverridesResponse'
      Lude.<$> (x Lude..@? "overrides")
      Lude.<*> (x Lude..@? "launchTemplateSpecification")
