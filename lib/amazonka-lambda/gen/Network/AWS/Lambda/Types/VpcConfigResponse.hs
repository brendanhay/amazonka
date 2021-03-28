{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.VpcConfigResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.VpcConfigResponse
  ( VpcConfigResponse (..)
  -- * Smart constructor
  , mkVpcConfigResponse
  -- * Lenses
  , vcrSecurityGroupIds
  , vcrSubnetIds
  , vcrVpcId
  ) where

import qualified Network.AWS.Lambda.Types.SecurityGroupId as Types
import qualified Network.AWS.Lambda.Types.SubnetId as Types
import qualified Network.AWS.Lambda.Types.VpcId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The VPC security groups and subnets that are attached to a Lambda function.
--
-- /See:/ 'mkVpcConfigResponse' smart constructor.
data VpcConfigResponse = VpcConfigResponse'
  { securityGroupIds :: Core.Maybe [Types.SecurityGroupId]
    -- ^ A list of VPC security groups IDs.
  , subnetIds :: Core.Maybe [Types.SubnetId]
    -- ^ A list of VPC subnet IDs.
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The ID of the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcConfigResponse' value with any optional fields omitted.
mkVpcConfigResponse
    :: VpcConfigResponse
mkVpcConfigResponse
  = VpcConfigResponse'{securityGroupIds = Core.Nothing,
                       subnetIds = Core.Nothing, vpcId = Core.Nothing}

-- | A list of VPC security groups IDs.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrSecurityGroupIds :: Lens.Lens' VpcConfigResponse (Core.Maybe [Types.SecurityGroupId])
vcrSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE vcrSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | A list of VPC subnet IDs.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrSubnetIds :: Lens.Lens' VpcConfigResponse (Core.Maybe [Types.SubnetId])
vcrSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE vcrSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcrVpcId :: Lens.Lens' VpcConfigResponse (Core.Maybe Types.VpcId)
vcrVpcId = Lens.field @"vpcId"
{-# INLINEABLE vcrVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromJSON VpcConfigResponse where
        parseJSON
          = Core.withObject "VpcConfigResponse" Core.$
              \ x ->
                VpcConfigResponse' Core.<$>
                  (x Core..:? "SecurityGroupIds") Core.<*> x Core..:? "SubnetIds"
                    Core.<*> x Core..:? "VpcId"
