-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputVPCRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputVPCRequest
  ( InputVPCRequest (..),

    -- * Smart constructor
    mkInputVPCRequest,

    -- * Lenses
    ivrSecurityGroupIds,
    ivrSubnetIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for a private VPC Input.
--
-- When this property is specified, the input destination addresses will be created in a VPC rather than with public Internet addresses.
-- This property requires setting the roleArn property on Input creation.
-- Not compatible with the inputSecurityGroups property.
--
-- /See:/ 'mkInputVPCRequest' smart constructor.
data InputVPCRequest = InputVPCRequest'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    subnetIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputVPCRequest' with the minimum fields required to make a request.
--
-- * 'securityGroupIds' - A list of up to 5 EC2 VPC security group IDs to attach to the Input VPC network interfaces.
--
-- Requires subnetIds. If none are specified then the VPC default security group will be used.
-- * 'subnetIds' - A list of 2 VPC subnet IDs from the same VPC.
--
-- Subnet IDs must be mapped to two unique availability zones (AZ).
mkInputVPCRequest ::
  InputVPCRequest
mkInputVPCRequest =
  InputVPCRequest'
    { securityGroupIds = Lude.Nothing,
      subnetIds = Lude.mempty
    }

-- | A list of up to 5 EC2 VPC security group IDs to attach to the Input VPC network interfaces.
--
-- Requires subnetIds. If none are specified then the VPC default security group will be used.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivrSecurityGroupIds :: Lens.Lens' InputVPCRequest (Lude.Maybe [Lude.Text])
ivrSecurityGroupIds = Lens.lens (securityGroupIds :: InputVPCRequest -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: InputVPCRequest)
{-# DEPRECATED ivrSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | A list of 2 VPC subnet IDs from the same VPC.
--
-- Subnet IDs must be mapped to two unique availability zones (AZ).
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivrSubnetIds :: Lens.Lens' InputVPCRequest [Lude.Text]
ivrSubnetIds = Lens.lens (subnetIds :: InputVPCRequest -> [Lude.Text]) (\s a -> s {subnetIds = a} :: InputVPCRequest)
{-# DEPRECATED ivrSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

instance Lude.ToJSON InputVPCRequest where
  toJSON InputVPCRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("securityGroupIds" Lude..=) Lude.<$> securityGroupIds,
            Lude.Just ("subnetIds" Lude..= subnetIds)
          ]
      )
