{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.VpcConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.VpcConfig
  ( VpcConfig (..)
  -- * Smart constructor
  , mkVpcConfig
  -- * Lenses
  , vcSecurityGroupIds
  , vcSubnetIds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes VPC configuration information for fleets and image builders.
--
-- /See:/ 'mkVpcConfig' smart constructor.
data VpcConfig = VpcConfig'
  { securityGroupIds :: Core.Maybe [Core.Text]
    -- ^ The identifiers of the security groups for the fleet or image builder.
  , subnetIds :: Core.Maybe [Core.Text]
    -- ^ The identifiers of the subnets to which a network interface is attached from the fleet instance or image builder instance. Fleet instances use one or more subnets. Image builder instances use one subnet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcConfig' value with any optional fields omitted.
mkVpcConfig
    :: VpcConfig
mkVpcConfig
  = VpcConfig'{securityGroupIds = Core.Nothing,
               subnetIds = Core.Nothing}

-- | The identifiers of the security groups for the fleet or image builder.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSecurityGroupIds :: Lens.Lens' VpcConfig (Core.Maybe [Core.Text])
vcSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE vcSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The identifiers of the subnets to which a network interface is attached from the fleet instance or image builder instance. Fleet instances use one or more subnets. Image builder instances use one subnet.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSubnetIds :: Lens.Lens' VpcConfig (Core.Maybe [Core.Text])
vcSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE vcSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

instance Core.FromJSON VpcConfig where
        toJSON VpcConfig{..}
          = Core.object
              (Core.catMaybes
                 [("SecurityGroupIds" Core..=) Core.<$> securityGroupIds,
                  ("SubnetIds" Core..=) Core.<$> subnetIds])

instance Core.FromJSON VpcConfig where
        parseJSON
          = Core.withObject "VpcConfig" Core.$
              \ x ->
                VpcConfig' Core.<$>
                  (x Core..:? "SecurityGroupIds") Core.<*> x Core..:? "SubnetIds"
