-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LaunchTemplate
  ( LaunchTemplate (..),

    -- * Smart constructor
    mkLaunchTemplate,

    -- * Lenses
    ltOverrides,
    ltLaunchTemplateSpecification,
  )
where

import Network.AWS.AutoScaling.Types.LaunchTemplateOverrides
import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a launch template and overrides.
--
-- You specify these parameters as part of a mixed instances policy.
-- When you update the launch template or overrides, existing Amazon EC2 instances continue to run. When scale out occurs, Amazon EC2 Auto Scaling launches instances to match the new settings. When scale in occurs, Amazon EC2 Auto Scaling terminates instances according to the group's termination policies.
--
-- /See:/ 'mkLaunchTemplate' smart constructor.
data LaunchTemplate = LaunchTemplate'
  { overrides ::
      Lude.Maybe [LaunchTemplateOverrides],
    launchTemplateSpecification ::
      Lude.Maybe LaunchTemplateSpecification
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplate' with the minimum fields required to make a request.
--
-- * 'launchTemplateSpecification' - The launch template to use.
-- * 'overrides' - Any parameters that you specify override the same parameters in the launch template. If not provided, Amazon EC2 Auto Scaling uses the instance type specified in the launch template when it launches an instance.
mkLaunchTemplate ::
  LaunchTemplate
mkLaunchTemplate =
  LaunchTemplate'
    { overrides = Lude.Nothing,
      launchTemplateSpecification = Lude.Nothing
    }

-- | Any parameters that you specify override the same parameters in the launch template. If not provided, Amazon EC2 Auto Scaling uses the instance type specified in the launch template when it launches an instance.
--
-- /Note:/ Consider using 'overrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltOverrides :: Lens.Lens' LaunchTemplate (Lude.Maybe [LaunchTemplateOverrides])
ltOverrides = Lens.lens (overrides :: LaunchTemplate -> Lude.Maybe [LaunchTemplateOverrides]) (\s a -> s {overrides = a} :: LaunchTemplate)
{-# DEPRECATED ltOverrides "Use generic-lens or generic-optics with 'overrides' instead." #-}

-- | The launch template to use.
--
-- /Note:/ Consider using 'launchTemplateSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLaunchTemplateSpecification :: Lens.Lens' LaunchTemplate (Lude.Maybe LaunchTemplateSpecification)
ltLaunchTemplateSpecification = Lens.lens (launchTemplateSpecification :: LaunchTemplate -> Lude.Maybe LaunchTemplateSpecification) (\s a -> s {launchTemplateSpecification = a} :: LaunchTemplate)
{-# DEPRECATED ltLaunchTemplateSpecification "Use generic-lens or generic-optics with 'launchTemplateSpecification' instead." #-}

instance Lude.FromXML LaunchTemplate where
  parseXML x =
    LaunchTemplate'
      Lude.<$> ( x Lude..@? "Overrides" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "LaunchTemplateSpecification")

instance Lude.ToQuery LaunchTemplate where
  toQuery LaunchTemplate' {..} =
    Lude.mconcat
      [ "Overrides"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> overrides),
        "LaunchTemplateSpecification" Lude.=: launchTemplateSpecification
      ]
