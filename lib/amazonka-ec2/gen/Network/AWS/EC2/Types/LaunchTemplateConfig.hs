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
    ltcOverrides,
    ltcLaunchTemplateSpecification,
  )
where

import Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
import Network.AWS.EC2.Types.LaunchTemplateOverrides
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a launch template and overrides.
--
-- /See:/ 'mkLaunchTemplateConfig' smart constructor.
data LaunchTemplateConfig = LaunchTemplateConfig'
  { -- | Any parameters that you specify override the same parameters in the launch template.
    overrides :: Lude.Maybe [LaunchTemplateOverrides],
    -- | The launch template.
    launchTemplateSpecification :: Lude.Maybe FleetLaunchTemplateSpecification
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateConfig' with the minimum fields required to make a request.
--
-- * 'overrides' - Any parameters that you specify override the same parameters in the launch template.
-- * 'launchTemplateSpecification' - The launch template.
mkLaunchTemplateConfig ::
  LaunchTemplateConfig
mkLaunchTemplateConfig =
  LaunchTemplateConfig'
    { overrides = Lude.Nothing,
      launchTemplateSpecification = Lude.Nothing
    }

-- | Any parameters that you specify override the same parameters in the launch template.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcOverrides :: Lens.Lens' LaunchTemplateConfig (Lude.Maybe [LaunchTemplateOverrides])
ltcOverrides = Lens.lens (overrides :: LaunchTemplateConfig -> Lude.Maybe [LaunchTemplateOverrides]) (\s a -> s {overrides = a} :: LaunchTemplateConfig)
{-# DEPRECATED ltcOverrides "Use generic-lens or generic-optics with 'overrides' instead." #-}

-- | The launch template.
--
-- /Note:/ Consider using 'launchTemplateSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcLaunchTemplateSpecification :: Lens.Lens' LaunchTemplateConfig (Lude.Maybe FleetLaunchTemplateSpecification)
ltcLaunchTemplateSpecification = Lens.lens (launchTemplateSpecification :: LaunchTemplateConfig -> Lude.Maybe FleetLaunchTemplateSpecification) (\s a -> s {launchTemplateSpecification = a} :: LaunchTemplateConfig)
{-# DEPRECATED ltcLaunchTemplateSpecification "Use generic-lens or generic-optics with 'launchTemplateSpecification' instead." #-}

instance Lude.FromXML LaunchTemplateConfig where
  parseXML x =
    LaunchTemplateConfig'
      Lude.<$> ( x Lude..@? "overrides" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "launchTemplateSpecification")

instance Lude.ToQuery LaunchTemplateConfig where
  toQuery LaunchTemplateConfig' {..} =
    Lude.mconcat
      [ Lude.toQuery (Lude.toQueryList "Overrides" Lude.<$> overrides),
        "LaunchTemplateSpecification" Lude.=: launchTemplateSpecification
      ]
