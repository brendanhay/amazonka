{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AwsVpcConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.AwsVpcConfiguration
  ( AwsVpcConfiguration (..)
  -- * Smart constructor
  , mkAwsVpcConfiguration
  -- * Lenses
  , avcSubnets
  , avcAssignPublicIp
  , avcSecurityGroups
  ) where

import qualified Network.AWS.ECS.Types.AssignPublicIp as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the networking details for a task or service.
--
-- /See:/ 'mkAwsVpcConfiguration' smart constructor.
data AwsVpcConfiguration = AwsVpcConfiguration'
  { subnets :: [Core.Text]
    -- ^ The IDs of the subnets associated with the task or service. There is a limit of 16 subnets that can be specified per @AwsVpcConfiguration@ .
  , assignPublicIp :: Core.Maybe Types.AssignPublicIp
    -- ^ Whether the task's elastic network interface receives a public IP address. The default value is @DISABLED@ .
  , securityGroups :: Core.Maybe [Core.Text]
    -- ^ The IDs of the security groups associated with the task or service. If you do not specify a security group, the default security group for the VPC is used. There is a limit of 5 security groups that can be specified per @AwsVpcConfiguration@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AwsVpcConfiguration' value with any optional fields omitted.
mkAwsVpcConfiguration
    :: AwsVpcConfiguration
mkAwsVpcConfiguration
  = AwsVpcConfiguration'{subnets = Core.mempty,
                         assignPublicIp = Core.Nothing, securityGroups = Core.Nothing}

-- | The IDs of the subnets associated with the task or service. There is a limit of 16 subnets that can be specified per @AwsVpcConfiguration@ .
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcSubnets :: Lens.Lens' AwsVpcConfiguration [Core.Text]
avcSubnets = Lens.field @"subnets"
{-# INLINEABLE avcSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

-- | Whether the task's elastic network interface receives a public IP address. The default value is @DISABLED@ .
--
-- /Note:/ Consider using 'assignPublicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcAssignPublicIp :: Lens.Lens' AwsVpcConfiguration (Core.Maybe Types.AssignPublicIp)
avcAssignPublicIp = Lens.field @"assignPublicIp"
{-# INLINEABLE avcAssignPublicIp #-}
{-# DEPRECATED assignPublicIp "Use generic-lens or generic-optics with 'assignPublicIp' instead"  #-}

-- | The IDs of the security groups associated with the task or service. If you do not specify a security group, the default security group for the VPC is used. There is a limit of 5 security groups that can be specified per @AwsVpcConfiguration@ .
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
                 [Core.Just ("subnets" Core..= subnets),
                  ("assignPublicIp" Core..=) Core.<$> assignPublicIp,
                  ("securityGroups" Core..=) Core.<$> securityGroups])

instance Core.FromJSON AwsVpcConfiguration where
        parseJSON
          = Core.withObject "AwsVpcConfiguration" Core.$
              \ x ->
                AwsVpcConfiguration' Core.<$>
                  (x Core..:? "subnets" Core..!= Core.mempty) Core.<*>
                    x Core..:? "assignPublicIp"
                    Core.<*> x Core..:? "securityGroups"
