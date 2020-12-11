-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
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

-- | Describes the Amazon EC2 launch template and the launch template version that can be used by an Auto Scaling group to configure Amazon EC2 instances.
--
-- The launch template that is specified must be configured for use with an Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a launch template for an Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
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
-- * 'launchTemplateId' - The ID of the launch template. To get the template ID, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates> API operation. New launch templates can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate> API.
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a @LaunchTemplateName@ .
-- * 'launchTemplateName' - The name of the launch template. To get the template name, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates> API operation. New launch templates can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate> API.
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a @LaunchTemplateName@ .
-- * 'version' - The version number, @> Latest@ , or @> Default@ . To get the version number, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplateVersions.html DescribeLaunchTemplateVersions> API operation. New launch template versions can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplateVersion.html CreateLaunchTemplateVersion> API. If the value is @> Latest@ , Amazon EC2 Auto Scaling selects the latest version of the launch template when launching instances. If the value is @> Default@ , Amazon EC2 Auto Scaling selects the default version of the launch template when launching instances. The default value is @> Default@ .
mkLaunchTemplateSpecification ::
  LaunchTemplateSpecification
mkLaunchTemplateSpecification =
  LaunchTemplateSpecification'
    { launchTemplateName = Lude.Nothing,
      launchTemplateId = Lude.Nothing,
      version = Lude.Nothing
    }

-- | The name of the launch template. To get the template name, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates> API operation. New launch templates can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate> API.
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a @LaunchTemplateName@ .
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsLaunchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Lude.Maybe Lude.Text)
ltsLaunchTemplateName = Lens.lens (launchTemplateName :: LaunchTemplateSpecification -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateName = a} :: LaunchTemplateSpecification)
{-# DEPRECATED ltsLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The ID of the launch template. To get the template ID, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates> API operation. New launch templates can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate> API.
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a @LaunchTemplateName@ .
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsLaunchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Lude.Maybe Lude.Text)
ltsLaunchTemplateId = Lens.lens (launchTemplateId :: LaunchTemplateSpecification -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateId = a} :: LaunchTemplateSpecification)
{-# DEPRECATED ltsLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The version number, @> Latest@ , or @> Default@ . To get the version number, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplateVersions.html DescribeLaunchTemplateVersions> API operation. New launch template versions can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplateVersion.html CreateLaunchTemplateVersion> API. If the value is @> Latest@ , Amazon EC2 Auto Scaling selects the latest version of the launch template when launching instances. If the value is @> Default@ , Amazon EC2 Auto Scaling selects the default version of the launch template when launching instances. The default value is @> Default@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsVersion :: Lens.Lens' LaunchTemplateSpecification (Lude.Maybe Lude.Text)
ltsVersion = Lens.lens (version :: LaunchTemplateSpecification -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: LaunchTemplateSpecification)
{-# DEPRECATED ltsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromXML LaunchTemplateSpecification where
  parseXML x =
    LaunchTemplateSpecification'
      Lude.<$> (x Lude..@? "LaunchTemplateName")
      Lude.<*> (x Lude..@? "LaunchTemplateId")
      Lude.<*> (x Lude..@? "Version")

instance Lude.ToQuery LaunchTemplateSpecification where
  toQuery LaunchTemplateSpecification' {..} =
    Lude.mconcat
      [ "LaunchTemplateName" Lude.=: launchTemplateName,
        "LaunchTemplateId" Lude.=: launchTemplateId,
        "Version" Lude.=: version
      ]
