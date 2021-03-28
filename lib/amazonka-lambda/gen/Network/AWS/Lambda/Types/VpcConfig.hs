{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.VpcConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.VpcConfig
  ( VpcConfig (..)
  -- * Smart constructor
  , mkVpcConfig
  -- * Lenses
  , vcSecurityGroupIds
  , vcSubnetIds
  ) where

import qualified Network.AWS.Lambda.Types.SecurityGroupId as Types
import qualified Network.AWS.Lambda.Types.SubnetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The VPC security groups and subnets that are attached to a Lambda function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings> .
--
-- /See:/ 'mkVpcConfig' smart constructor.
data VpcConfig = VpcConfig'
  { securityGroupIds :: Core.Maybe [Types.SecurityGroupId]
    -- ^ A list of VPC security groups IDs.
  , subnetIds :: Core.Maybe [Types.SubnetId]
    -- ^ A list of VPC subnet IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcConfig' value with any optional fields omitted.
mkVpcConfig
    :: VpcConfig
mkVpcConfig
  = VpcConfig'{securityGroupIds = Core.Nothing,
               subnetIds = Core.Nothing}

-- | A list of VPC security groups IDs.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSecurityGroupIds :: Lens.Lens' VpcConfig (Core.Maybe [Types.SecurityGroupId])
vcSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE vcSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | A list of VPC subnet IDs.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSubnetIds :: Lens.Lens' VpcConfig (Core.Maybe [Types.SubnetId])
vcSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE vcSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

instance Core.FromJSON VpcConfig where
        toJSON VpcConfig{..}
          = Core.object
              (Core.catMaybes
                 [("SecurityGroupIds" Core..=) Core.<$> securityGroupIds,
                  ("SubnetIds" Core..=) Core.<$> subnetIds])
