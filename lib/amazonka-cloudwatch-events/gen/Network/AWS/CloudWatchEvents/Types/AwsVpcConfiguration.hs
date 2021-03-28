{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.AwsVpcConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.AwsVpcConfiguration
  ( AwsVpcConfiguration (..)
  -- * Smart constructor
  , mkAwsVpcConfiguration
  -- * Lenses
  , avcSubnets
  , avcAssignPublicIp
  , avcSecurityGroups
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.AssignPublicIp as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This structure specifies the VPC subnets and security groups for the task, and whether a public IP address is to be used. This structure is relevant only for ECS tasks that use the @awsvpc@ network mode.
--
-- /See:/ 'mkAwsVpcConfiguration' smart constructor.
data AwsVpcConfiguration = AwsVpcConfiguration'
  { subnets :: [Core.Text]
    -- ^ Specifies the subnets associated with the task. These subnets must all be in the same VPC. You can specify as many as 16 subnets.
  , assignPublicIp :: Core.Maybe Types.AssignPublicIp
    -- ^ Specifies whether the task's elastic network interface receives a public IP address. You can specify @ENABLED@ only when @LaunchType@ in @EcsParameters@ is set to @FARGATE@ .
  , securityGroups :: Core.Maybe [Core.Text]
    -- ^ Specifies the security groups associated with the task. These security groups must all be in the same VPC. You can specify as many as five security groups. If you do not specify a security group, the default security group for the VPC is used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AwsVpcConfiguration' value with any optional fields omitted.
mkAwsVpcConfiguration
    :: AwsVpcConfiguration
mkAwsVpcConfiguration
  = AwsVpcConfiguration'{subnets = Core.mempty,
                         assignPublicIp = Core.Nothing, securityGroups = Core.Nothing}

-- | Specifies the subnets associated with the task. These subnets must all be in the same VPC. You can specify as many as 16 subnets.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcSubnets :: Lens.Lens' AwsVpcConfiguration [Core.Text]
avcSubnets = Lens.field @"subnets"
{-# INLINEABLE avcSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

-- | Specifies whether the task's elastic network interface receives a public IP address. You can specify @ENABLED@ only when @LaunchType@ in @EcsParameters@ is set to @FARGATE@ .
--
-- /Note:/ Consider using 'assignPublicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcAssignPublicIp :: Lens.Lens' AwsVpcConfiguration (Core.Maybe Types.AssignPublicIp)
avcAssignPublicIp = Lens.field @"assignPublicIp"
{-# INLINEABLE avcAssignPublicIp #-}
{-# DEPRECATED assignPublicIp "Use generic-lens or generic-optics with 'assignPublicIp' instead"  #-}

-- | Specifies the security groups associated with the task. These security groups must all be in the same VPC. You can specify as many as five security groups. If you do not specify a security group, the default security group for the VPC is used.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcSecurityGroups :: Lens.Lens' AwsVpcConfiguration (Core.Maybe [Core.Text])
avcSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE avcSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

instance Core.FromJSON AwsVpcConfiguration where
        toJSON AwsVpcConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Subnets" Core..= subnets),
                  ("AssignPublicIp" Core..=) Core.<$> assignPublicIp,
                  ("SecurityGroups" Core..=) Core.<$> securityGroups])

instance Core.FromJSON AwsVpcConfiguration where
        parseJSON
          = Core.withObject "AwsVpcConfiguration" Core.$
              \ x ->
                AwsVpcConfiguration' Core.<$>
                  (x Core..:? "Subnets" Core..!= Core.mempty) Core.<*>
                    x Core..:? "AssignPublicIp"
                    Core.<*> x Core..:? "SecurityGroups"
