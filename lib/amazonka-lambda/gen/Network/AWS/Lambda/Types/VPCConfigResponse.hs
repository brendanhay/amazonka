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
    vcSecurityGroupIds,
    vcSubnetIds,
    vcVPCId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The VPC security groups and subnets that are attached to a Lambda function.
--
-- /See:/ 'mkVPCConfigResponse' smart constructor.
data VPCConfigResponse = VPCConfigResponse'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    subnetIds :: Lude.Maybe [Lude.Text],
    vpcId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
vcSecurityGroupIds :: Lens.Lens' VPCConfigResponse (Lude.Maybe [Lude.Text])
vcSecurityGroupIds = Lens.lens (securityGroupIds :: VPCConfigResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: VPCConfigResponse)
{-# DEPRECATED vcSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | A list of VPC subnet IDs.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSubnetIds :: Lens.Lens' VPCConfigResponse (Lude.Maybe [Lude.Text])
vcSubnetIds = Lens.lens (subnetIds :: VPCConfigResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: VPCConfigResponse)
{-# DEPRECATED vcSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcVPCId :: Lens.Lens' VPCConfigResponse (Lude.Maybe Lude.Text)
vcVPCId = Lens.lens (vpcId :: VPCConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: VPCConfigResponse)
{-# DEPRECATED vcVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

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
