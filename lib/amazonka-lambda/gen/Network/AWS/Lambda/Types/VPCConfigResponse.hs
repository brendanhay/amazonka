{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.VPCConfigResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.VPCConfigResponse
  ( VPCConfigResponse (..),

    -- * Smart constructor
    mkVPCConfigResponse,

    -- * Lenses
    vpccSecurityGroupIds,
    vpccSubnetIds,
    vpccVPCId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The VPC security groups and subnets that are attached to a Lambda function.
--
-- /See:/ 'mkVPCConfigResponse' smart constructor.
data VPCConfigResponse = VPCConfigResponse'
  { -- | A list of VPC security groups IDs.
    securityGroupIds :: Lude.Maybe [Lude.Text],
    -- | A list of VPC subnet IDs.
    subnetIds :: Lude.Maybe [Lude.Text],
    -- | The ID of the VPC.
    vpcId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCConfigResponse' with the minimum fields required to make a request.
--
-- * 'securityGroupIds' - A list of VPC security groups IDs.
-- * 'subnetIds' - A list of VPC subnet IDs.
-- * 'vpcId' - The ID of the VPC.
mkVPCConfigResponse ::
  VPCConfigResponse
mkVPCConfigResponse =
  VPCConfigResponse'
    { securityGroupIds = Lude.Nothing,
      subnetIds = Lude.Nothing,
      vpcId = Lude.Nothing
    }

-- | A list of VPC security groups IDs.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpccSecurityGroupIds :: Lens.Lens' VPCConfigResponse (Lude.Maybe [Lude.Text])
vpccSecurityGroupIds = Lens.lens (securityGroupIds :: VPCConfigResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: VPCConfigResponse)
{-# DEPRECATED vpccSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | A list of VPC subnet IDs.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpccSubnetIds :: Lens.Lens' VPCConfigResponse (Lude.Maybe [Lude.Text])
vpccSubnetIds = Lens.lens (subnetIds :: VPCConfigResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: VPCConfigResponse)
{-# DEPRECATED vpccSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpccVPCId :: Lens.Lens' VPCConfigResponse (Lude.Maybe Lude.Text)
vpccVPCId = Lens.lens (vpcId :: VPCConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: VPCConfigResponse)
{-# DEPRECATED vpccVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.FromJSON VPCConfigResponse where
  parseJSON =
    Lude.withObject
      "VPCConfigResponse"
      ( \x ->
          VPCConfigResponse'
            Lude.<$> (x Lude..:? "SecurityGroupIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SubnetIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "VpcId")
      )
