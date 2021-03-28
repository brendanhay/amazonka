{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
  ( LaunchTemplateSpecification (..)
  -- * Smart constructor
  , mkLaunchTemplateSpecification
  -- * Lenses
  , ltsLaunchTemplateId
  , ltsLaunchTemplateName
  , ltsVersion
  ) where

import qualified Network.AWS.AutoScaling.Types.LaunchTemplateName as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Amazon EC2 launch template and the launch template version that can be used by an Auto Scaling group to configure Amazon EC2 instances.
--
-- The launch template that is specified must be configured for use with an Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-template.html Creating a launch template for an Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /See:/ 'mkLaunchTemplateSpecification' smart constructor.
data LaunchTemplateSpecification = LaunchTemplateSpecification'
  { launchTemplateId :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The ID of the launch template. To get the template ID, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates> API operation. New launch templates can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate> API. 
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a @LaunchTemplateName@ .
  , launchTemplateName :: Core.Maybe Types.LaunchTemplateName
    -- ^ The name of the launch template. To get the template name, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates> API operation. New launch templates can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate> API. 
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a @LaunchTemplateName@ .
  , version :: Core.Maybe Types.XmlStringMaxLen255
    -- ^ The version number, @> Latest@ , or @> Default@ . To get the version number, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplateVersions.html DescribeLaunchTemplateVersions> API operation. New launch template versions can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplateVersion.html CreateLaunchTemplateVersion> API. If the value is @> Latest@ , Amazon EC2 Auto Scaling selects the latest version of the launch template when launching instances. If the value is @> Default@ , Amazon EC2 Auto Scaling selects the default version of the launch template when launching instances. The default value is @> Default@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateSpecification' value with any optional fields omitted.
mkLaunchTemplateSpecification
    :: LaunchTemplateSpecification
mkLaunchTemplateSpecification
  = LaunchTemplateSpecification'{launchTemplateId = Core.Nothing,
                                 launchTemplateName = Core.Nothing, version = Core.Nothing}

-- | The ID of the launch template. To get the template ID, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates> API operation. New launch templates can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate> API. 
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a @LaunchTemplateName@ .
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsLaunchTemplateId :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Types.XmlStringMaxLen255)
ltsLaunchTemplateId = Lens.field @"launchTemplateId"
{-# INLINEABLE ltsLaunchTemplateId #-}
{-# DEPRECATED launchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead"  #-}

-- | The name of the launch template. To get the template name, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplates.html DescribeLaunchTemplates> API operation. New launch templates can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplate.html CreateLaunchTemplate> API. 
--
-- Conditional: You must specify either a @LaunchTemplateId@ or a @LaunchTemplateName@ .
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsLaunchTemplateName :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Types.LaunchTemplateName)
ltsLaunchTemplateName = Lens.field @"launchTemplateName"
{-# INLINEABLE ltsLaunchTemplateName #-}
{-# DEPRECATED launchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead"  #-}

-- | The version number, @> Latest@ , or @> Default@ . To get the version number, use the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeLaunchTemplateVersions.html DescribeLaunchTemplateVersions> API operation. New launch template versions can be created using the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateLaunchTemplateVersion.html CreateLaunchTemplateVersion> API. If the value is @> Latest@ , Amazon EC2 Auto Scaling selects the latest version of the launch template when launching instances. If the value is @> Default@ , Amazon EC2 Auto Scaling selects the default version of the launch template when launching instances. The default value is @> Default@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsVersion :: Lens.Lens' LaunchTemplateSpecification (Core.Maybe Types.XmlStringMaxLen255)
ltsVersion = Lens.field @"version"
{-# INLINEABLE ltsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery LaunchTemplateSpecification where
        toQuery LaunchTemplateSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateId")
              launchTemplateId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateName")
                launchTemplateName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Version") version

instance Core.FromXML LaunchTemplateSpecification where
        parseXML x
          = LaunchTemplateSpecification' Core.<$>
              (x Core..@? "LaunchTemplateId") Core.<*>
                x Core..@? "LaunchTemplateName"
                Core.<*> x Core..@? "Version"
