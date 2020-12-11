-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateSpecification
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

-- | The launch template to use. You must specify either the launch template ID or launch template name in the request, but not both.
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
-- * 'launchTemplateId' - The ID of the launch template.
-- * 'launchTemplateName' - The name of the launch template.
-- * 'version' - The version number of the launch template.
--
-- Default: The default version for the launch template.
mkLaunchTemplateSpecification ::
  LaunchTemplateSpecification
mkLaunchTemplateSpecification =
  LaunchTemplateSpecification'
    { launchTemplateName = Lude.Nothing,
      launchTemplateId = Lude.Nothing,
      version = Lude.Nothing
    }

-- | The name of the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsLaunchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Lude.Maybe Lude.Text)
ltsLaunchTemplateName = Lens.lens (launchTemplateName :: LaunchTemplateSpecification -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateName = a} :: LaunchTemplateSpecification)
{-# DEPRECATED ltsLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsLaunchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Lude.Maybe Lude.Text)
ltsLaunchTemplateId = Lens.lens (launchTemplateId :: LaunchTemplateSpecification -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateId = a} :: LaunchTemplateSpecification)
{-# DEPRECATED ltsLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The version number of the launch template.
--
-- Default: The default version for the launch template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsVersion :: Lens.Lens' LaunchTemplateSpecification (Lude.Maybe Lude.Text)
ltsVersion = Lens.lens (version :: LaunchTemplateSpecification -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: LaunchTemplateSpecification)
{-# DEPRECATED ltsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.ToQuery LaunchTemplateSpecification where
  toQuery LaunchTemplateSpecification' {..} =
    Lude.mconcat
      [ "LaunchTemplateName" Lude.=: launchTemplateName,
        "LaunchTemplateId" Lude.=: launchTemplateId,
        "Version" Lude.=: version
      ]
