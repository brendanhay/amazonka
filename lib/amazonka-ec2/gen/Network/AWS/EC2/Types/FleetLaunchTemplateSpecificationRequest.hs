{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetLaunchTemplateSpecificationRequest
  ( FleetLaunchTemplateSpecificationRequest (..),

    -- * Smart constructor
    mkFleetLaunchTemplateSpecificationRequest,

    -- * Lenses
    fltsrLaunchTemplateId,
    fltsrLaunchTemplateName,
    fltsrVersion,
  )
where

import qualified Network.AWS.EC2.Types.LaunchTemplateId as Types
import qualified Network.AWS.EC2.Types.LaunchTemplateName as Types
import qualified Network.AWS.EC2.Types.Version as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Amazon EC2 launch template and the launch template version that can be used by an EC2 Fleet to configure Amazon EC2 instances. For information about launch templates, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launching an instance from a launch template> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkFleetLaunchTemplateSpecificationRequest' smart constructor.
data FleetLaunchTemplateSpecificationRequest = FleetLaunchTemplateSpecificationRequest'
  { -- | The ID of the launch template. If you specify the template ID, you can't specify the template name.
    launchTemplateId :: Core.Maybe Types.LaunchTemplateId,
    -- | The name of the launch template. If you specify the template name, you can't specify the template ID.
    launchTemplateName :: Core.Maybe Types.LaunchTemplateName,
    -- | The launch template version number, @> Latest@ , or @> Default@ . You must specify a value, otherwise the request fails.
    --
    -- If the value is @> Latest@ , Amazon EC2 uses the latest version of the launch template.
    -- If the value is @> Default@ , Amazon EC2 uses the default version of the launch template.
    version :: Core.Maybe Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FleetLaunchTemplateSpecificationRequest' value with any optional fields omitted.
mkFleetLaunchTemplateSpecificationRequest ::
  FleetLaunchTemplateSpecificationRequest
mkFleetLaunchTemplateSpecificationRequest =
  FleetLaunchTemplateSpecificationRequest'
    { launchTemplateId =
        Core.Nothing,
      launchTemplateName = Core.Nothing,
      version = Core.Nothing
    }

-- | The ID of the launch template. If you specify the template ID, you can't specify the template name.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltsrLaunchTemplateId :: Lens.Lens' FleetLaunchTemplateSpecificationRequest (Core.Maybe Types.LaunchTemplateId)
fltsrLaunchTemplateId = Lens.field @"launchTemplateId"
{-# DEPRECATED fltsrLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The name of the launch template. If you specify the template name, you can't specify the template ID.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltsrLaunchTemplateName :: Lens.Lens' FleetLaunchTemplateSpecificationRequest (Core.Maybe Types.LaunchTemplateName)
fltsrLaunchTemplateName = Lens.field @"launchTemplateName"
{-# DEPRECATED fltsrLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The launch template version number, @> Latest@ , or @> Default@ . You must specify a value, otherwise the request fails.
--
-- If the value is @> Latest@ , Amazon EC2 uses the latest version of the launch template.
-- If the value is @> Default@ , Amazon EC2 uses the default version of the launch template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltsrVersion :: Lens.Lens' FleetLaunchTemplateSpecificationRequest (Core.Maybe Types.Version)
fltsrVersion = Lens.field @"version"
{-# DEPRECATED fltsrVersion "Use generic-lens or generic-optics with 'version' instead." #-}
