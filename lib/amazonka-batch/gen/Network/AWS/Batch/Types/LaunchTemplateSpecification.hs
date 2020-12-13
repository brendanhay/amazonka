{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.LaunchTemplateSpecification
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

-- | An object representing a launch template associated with a compute resource. You must specify either the launch template ID or launch template name in the request, but not both.
--
-- /See:/ 'mkLaunchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { -- | The name of the launch template.
    launchTemplateName :: Lude.Maybe Lude.Text,
    -- | The ID of the launch template.
    launchTemplateId :: Lude.Maybe Lude.Text,
    -- | The version number of the launch template, @> Latest@ , or @> Default@ .
    --
    -- If the value is @> Latest@ , the latest version of the launch template is used. If the value is @> Default@ , the default version of the launch template is used.
    -- Default: @> Default@ .
    version :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateSpecification' with the minimum fields required to make a request.
--
-- * 'launchTemplateName' - The name of the launch template.
-- * 'launchTemplateId' - The ID of the launch template.
-- * 'version' - The version number of the launch template, @> Latest@ , or @> Default@ .
--
-- If the value is @> Latest@ , the latest version of the launch template is used. If the value is @> Default@ , the default version of the launch template is used.
-- Default: @> Default@ .
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

-- | The version number of the launch template, @> Latest@ , or @> Default@ .
--
-- If the value is @> Latest@ , the latest version of the launch template is used. If the value is @> Default@ , the default version of the launch template is used.
-- Default: @> Default@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsVersion :: Lens.Lens' LaunchTemplateSpecification (Lude.Maybe Lude.Text)
ltsVersion = Lens.lens (version :: LaunchTemplateSpecification -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: LaunchTemplateSpecification)
{-# DEPRECATED ltsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON LaunchTemplateSpecification where
  parseJSON =
    Lude.withObject
      "LaunchTemplateSpecification"
      ( \x ->
          LaunchTemplateSpecification'
            Lude.<$> (x Lude..:? "launchTemplateName")
            Lude.<*> (x Lude..:? "launchTemplateId")
            Lude.<*> (x Lude..:? "version")
      )

instance Lude.ToJSON LaunchTemplateSpecification where
  toJSON LaunchTemplateSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("launchTemplateName" Lude..=) Lude.<$> launchTemplateName,
            ("launchTemplateId" Lude..=) Lude.<$> launchTemplateId,
            ("version" Lude..=) Lude.<$> version
          ]
      )
