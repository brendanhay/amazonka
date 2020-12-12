{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.LaunchTemplateSpecification
  ( LaunchTemplateSpecification (..),

    -- * Smart constructor
    mkLaunchTemplateSpecification,

    -- * Lenses
    ltsLaunchTemplateName,
    ltsLaunchTemplateId,
    ltsVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | __This data type is used with the Amazon GameLift FleetIQ and game server groups.__
--
-- An EC2 launch template that contains configuration settings and game server code to be deployed to all instances in a game server group. The launch template is specified when creating a new game server group with 'CreateGameServerGroup' .
--
-- /See:/ 'mkLaunchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { launchTemplateName ::
      Lude.Maybe Lude.Text,
    launchTemplateId ::
      Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateSpecification' with the minimum fields required to make a request.
--
-- * 'launchTemplateId' - A unique identifier for an existing EC2 launch template.
-- * 'launchTemplateName' - A readable identifier for an existing EC2 launch template.
-- * 'version' - The version of the EC2 launch template to use. If no version is specified, the default version will be used. With Amazon EC2, you can specify a default version for a launch template. If none is set, the default is the first version created.
mkLaunchTemplateSpecification ::
  LaunchTemplateSpecification
mkLaunchTemplateSpecification =
  LaunchTemplateSpecification'
    { launchTemplateName = Lude.Nothing,
      launchTemplateId = Lude.Nothing,
      version = Lude.Nothing
    }

-- | A readable identifier for an existing EC2 launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsLaunchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Lude.Maybe Lude.Text)
ltsLaunchTemplateName = Lens.lens (launchTemplateName :: LaunchTemplateSpecification -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateName = a} :: LaunchTemplateSpecification)
{-# DEPRECATED ltsLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | A unique identifier for an existing EC2 launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsLaunchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Lude.Maybe Lude.Text)
ltsLaunchTemplateId = Lens.lens (launchTemplateId :: LaunchTemplateSpecification -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateId = a} :: LaunchTemplateSpecification)
{-# DEPRECATED ltsLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The version of the EC2 launch template to use. If no version is specified, the default version will be used. With Amazon EC2, you can specify a default version for a launch template. If none is set, the default is the first version created.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsVersion :: Lens.Lens' LaunchTemplateSpecification (Lude.Maybe Lude.Text)
ltsVersion = Lens.lens (version :: LaunchTemplateSpecification -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: LaunchTemplateSpecification)
{-# DEPRECATED ltsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.ToJSON LaunchTemplateSpecification where
  toJSON LaunchTemplateSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LaunchTemplateName" Lude..=) Lude.<$> launchTemplateName,
            ("LaunchTemplateId" Lude..=) Lude.<$> launchTemplateId,
            ("Version" Lude..=) Lude.<$> version
          ]
      )
