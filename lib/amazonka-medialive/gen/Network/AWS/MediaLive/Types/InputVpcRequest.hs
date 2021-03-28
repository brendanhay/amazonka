{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputVpcRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputVpcRequest
  ( InputVpcRequest (..)
  -- * Smart constructor
  , mkInputVpcRequest
  -- * Lenses
  , ivrSubnetIds
  , ivrSecurityGroupIds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for a private VPC Input.
--
-- When this property is specified, the input destination addresses will be created in a VPC rather than with public Internet addresses.
-- This property requires setting the roleArn property on Input creation.
-- Not compatible with the inputSecurityGroups property.
--
-- /See:/ 'mkInputVpcRequest' smart constructor.
data InputVpcRequest = InputVpcRequest'
  { subnetIds :: [Core.Text]
    -- ^ A list of 2 VPC subnet IDs from the same VPC.
--
-- Subnet IDs must be mapped to two unique availability zones (AZ).
  , securityGroupIds :: Core.Maybe [Core.Text]
    -- ^ A list of up to 5 EC2 VPC security group IDs to attach to the Input VPC network interfaces.
--
-- Requires subnetIds. If none are specified then the VPC default security group will be used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputVpcRequest' value with any optional fields omitted.
mkInputVpcRequest
    :: InputVpcRequest
mkInputVpcRequest
  = InputVpcRequest'{subnetIds = Core.mempty,
                     securityGroupIds = Core.Nothing}

-- | A list of 2 VPC subnet IDs from the same VPC.
--
-- Subnet IDs must be mapped to two unique availability zones (AZ).
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivrSubnetIds :: Lens.Lens' InputVpcRequest [Core.Text]
ivrSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE ivrSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | A list of up to 5 EC2 VPC security group IDs to attach to the Input VPC network interfaces.
--
-- Requires subnetIds. If none are specified then the VPC default security group will be used.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivrSecurityGroupIds :: Lens.Lens' InputVpcRequest (Core.Maybe [Core.Text])
ivrSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE ivrSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

instance Core.FromJSON InputVpcRequest where
        toJSON InputVpcRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("subnetIds" Core..= subnetIds),
                  ("securityGroupIds" Core..=) Core.<$> securityGroupIds])
