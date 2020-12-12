{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
  ( FleetLaunchTemplateSpecification (..),

    -- * Smart constructor
    mkFleetLaunchTemplateSpecification,

    -- * Lenses
    fltsLaunchTemplateName,
    fltsLaunchTemplateId,
    fltsVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Amazon EC2 launch template and the launch template version that can be used by a Spot Fleet request to configure Amazon EC2 instances. For information about launch templates, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launching an instance from a launch template> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /See:/ 'mkFleetLaunchTemplateSpecification' smart constructor.
data FleetLaunchTemplateSpecification = FleetLaunchTemplateSpecification'
  { launchTemplateName ::
      Lude.Maybe Lude.Text,
    launchTemplateId ::
      Lude.Maybe Lude.Text,
    version ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FleetLaunchTemplateSpecification' with the minimum fields required to make a request.
--
-- * 'launchTemplateId' - The ID of the launch template. If you specify the template ID, you can't specify the template name.
-- * 'launchTemplateName' - The name of the launch template. If you specify the template name, you can't specify the template ID.
-- * 'version' - The launch template version number, @> Latest@ , or @> Default@ . You must specify a value, otherwise the request fails.
--
-- If the value is @> Latest@ , Amazon EC2 uses the latest version of the launch template.
-- If the value is @> Default@ , Amazon EC2 uses the default version of the launch template.
mkFleetLaunchTemplateSpecification ::
  FleetLaunchTemplateSpecification
mkFleetLaunchTemplateSpecification =
  FleetLaunchTemplateSpecification'
    { launchTemplateName =
        Lude.Nothing,
      launchTemplateId = Lude.Nothing,
      version = Lude.Nothing
    }

-- | The name of the launch template. If you specify the template name, you can't specify the template ID.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltsLaunchTemplateName :: Lens.Lens' FleetLaunchTemplateSpecification (Lude.Maybe Lude.Text)
fltsLaunchTemplateName = Lens.lens (launchTemplateName :: FleetLaunchTemplateSpecification -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateName = a} :: FleetLaunchTemplateSpecification)
{-# DEPRECATED fltsLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The ID of the launch template. If you specify the template ID, you can't specify the template name.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltsLaunchTemplateId :: Lens.Lens' FleetLaunchTemplateSpecification (Lude.Maybe Lude.Text)
fltsLaunchTemplateId = Lens.lens (launchTemplateId :: FleetLaunchTemplateSpecification -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateId = a} :: FleetLaunchTemplateSpecification)
{-# DEPRECATED fltsLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The launch template version number, @> Latest@ , or @> Default@ . You must specify a value, otherwise the request fails.
--
-- If the value is @> Latest@ , Amazon EC2 uses the latest version of the launch template.
-- If the value is @> Default@ , Amazon EC2 uses the default version of the launch template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltsVersion :: Lens.Lens' FleetLaunchTemplateSpecification (Lude.Maybe Lude.Text)
fltsVersion = Lens.lens (version :: FleetLaunchTemplateSpecification -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: FleetLaunchTemplateSpecification)
{-# DEPRECATED fltsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromXML FleetLaunchTemplateSpecification where
  parseXML x =
    FleetLaunchTemplateSpecification'
      Lude.<$> (x Lude..@? "launchTemplateName")
      Lude.<*> (x Lude..@? "launchTemplateId")
      Lude.<*> (x Lude..@? "version")

instance Lude.ToQuery FleetLaunchTemplateSpecification where
  toQuery FleetLaunchTemplateSpecification' {..} =
    Lude.mconcat
      [ "LaunchTemplateName" Lude.=: launchTemplateName,
        "LaunchTemplateId" Lude.=: launchTemplateId,
        "Version" Lude.=: version
      ]
