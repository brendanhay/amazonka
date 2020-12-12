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
    fltcOverrides,
    fltcLaunchTemplateSpecification,
  )
where

import Network.AWS.EC2.Types.FleetLaunchTemplateOverrides
import Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a launch template and overrides.
--
-- /See:/ 'mkFleetLaunchTemplateConfig' smart constructor.
data FleetLaunchTemplateConfig = FleetLaunchTemplateConfig'
  { overrides ::
      Lude.Maybe
        [FleetLaunchTemplateOverrides],
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

-- | Creates a value of 'FleetLaunchTemplateConfig' with the minimum fields required to make a request.
--
-- * 'launchTemplateSpecification' - The launch template.
-- * 'overrides' - Any parameters that you specify override the same parameters in the launch template.
mkFleetLaunchTemplateConfig ::
  FleetLaunchTemplateConfig
mkFleetLaunchTemplateConfig =
  FleetLaunchTemplateConfig'
    { overrides = Lude.Nothing,
      launchTemplateSpecification = Lude.Nothing
    }

-- | Any parameters that you specify override the same parameters in the launch template.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltcOverrides :: Lens.Lens' FleetLaunchTemplateConfig (Lude.Maybe [FleetLaunchTemplateOverrides])
fltcOverrides = Lens.lens (overrides :: FleetLaunchTemplateConfig -> Lude.Maybe [FleetLaunchTemplateOverrides]) (\s a -> s {overrides = a} :: FleetLaunchTemplateConfig)
{-# DEPRECATED fltcOverrides "Use generic-lens or generic-optics with 'overrides' instead." #-}

-- | The launch template.
--
-- /Note:/ Consider using 'launchTemplateSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltcLaunchTemplateSpecification :: Lens.Lens' FleetLaunchTemplateConfig (Lude.Maybe FleetLaunchTemplateSpecification)
fltcLaunchTemplateSpecification = Lens.lens (launchTemplateSpecification :: FleetLaunchTemplateConfig -> Lude.Maybe FleetLaunchTemplateSpecification) (\s a -> s {launchTemplateSpecification = a} :: FleetLaunchTemplateConfig)
{-# DEPRECATED fltcLaunchTemplateSpecification "Use generic-lens or generic-optics with 'launchTemplateSpecification' instead." #-}

instance Lude.FromXML FleetLaunchTemplateConfig where
  parseXML x =
    FleetLaunchTemplateConfig'
      Lude.<$> ( x Lude..@? "overrides" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "launchTemplateSpecification")
