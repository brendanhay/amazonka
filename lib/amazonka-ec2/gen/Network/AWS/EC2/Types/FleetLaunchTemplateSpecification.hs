{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FleetLaunchTemplateSpecification
  ( FleetLaunchTemplateSpecification (..)
  -- * Smart constructor
  , mkFleetLaunchTemplateSpecification
  -- * Lenses
  , fltsLaunchTemplateId
  , fltsLaunchTemplateName
  , fltsVersion
  ) where

import qualified Network.AWS.EC2.Types.LaunchTemplateName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Amazon EC2 launch template and the launch template version that can be used by a Spot Fleet request to configure Amazon EC2 instances. For information about launch templates, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launching an instance from a launch template> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /See:/ 'mkFleetLaunchTemplateSpecification' smart constructor.
data FleetLaunchTemplateSpecification = FleetLaunchTemplateSpecification'
  { launchTemplateId :: Core.Maybe Core.Text
    -- ^ The ID of the launch template. If you specify the template ID, you can't specify the template name.
  , launchTemplateName :: Core.Maybe Types.LaunchTemplateName
    -- ^ The name of the launch template. If you specify the template name, you can't specify the template ID.
  , version :: Core.Maybe Core.Text
    -- ^ The launch template version number, @> Latest@ , or @> Default@ . You must specify a value, otherwise the request fails.
--
-- If the value is @> Latest@ , Amazon EC2 uses the latest version of the launch template.
-- If the value is @> Default@ , Amazon EC2 uses the default version of the launch template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FleetLaunchTemplateSpecification' value with any optional fields omitted.
mkFleetLaunchTemplateSpecification
    :: FleetLaunchTemplateSpecification
mkFleetLaunchTemplateSpecification
  = FleetLaunchTemplateSpecification'{launchTemplateId =
                                        Core.Nothing,
                                      launchTemplateName = Core.Nothing, version = Core.Nothing}

-- | The ID of the launch template. If you specify the template ID, you can't specify the template name.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltsLaunchTemplateId :: Lens.Lens' FleetLaunchTemplateSpecification (Core.Maybe Core.Text)
fltsLaunchTemplateId = Lens.field @"launchTemplateId"
{-# INLINEABLE fltsLaunchTemplateId #-}
{-# DEPRECATED launchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead"  #-}

-- | The name of the launch template. If you specify the template name, you can't specify the template ID.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltsLaunchTemplateName :: Lens.Lens' FleetLaunchTemplateSpecification (Core.Maybe Types.LaunchTemplateName)
fltsLaunchTemplateName = Lens.field @"launchTemplateName"
{-# INLINEABLE fltsLaunchTemplateName #-}
{-# DEPRECATED launchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead"  #-}

-- | The launch template version number, @> Latest@ , or @> Default@ . You must specify a value, otherwise the request fails.
--
-- If the value is @> Latest@ , Amazon EC2 uses the latest version of the launch template.
-- If the value is @> Default@ , Amazon EC2 uses the default version of the launch template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fltsVersion :: Lens.Lens' FleetLaunchTemplateSpecification (Core.Maybe Core.Text)
fltsVersion = Lens.field @"version"
{-# INLINEABLE fltsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery FleetLaunchTemplateSpecification where
        toQuery FleetLaunchTemplateSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateId")
              launchTemplateId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LaunchTemplateName")
                launchTemplateName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Version") version

instance Core.FromXML FleetLaunchTemplateSpecification where
        parseXML x
          = FleetLaunchTemplateSpecification' Core.<$>
              (x Core..@? "launchTemplateId") Core.<*>
                x Core..@? "launchTemplateName"
                Core.<*> x Core..@? "version"
