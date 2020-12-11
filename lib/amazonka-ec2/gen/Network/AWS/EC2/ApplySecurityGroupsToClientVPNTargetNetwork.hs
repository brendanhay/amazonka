{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ApplySecurityGroupsToClientVPNTargetNetwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a security group to the association between the target network and the Client VPN endpoint. This action replaces the existing security groups with the specified security groups.
module Network.AWS.EC2.ApplySecurityGroupsToClientVPNTargetNetwork
  ( -- * Creating a request
    ApplySecurityGroupsToClientVPNTargetNetwork (..),
    mkApplySecurityGroupsToClientVPNTargetNetwork,

    -- ** Request lenses
    asgtcvtnDryRun,
    asgtcvtnClientVPNEndpointId,
    asgtcvtnVPCId,
    asgtcvtnSecurityGroupIds,

    -- * Destructuring the response
    ApplySecurityGroupsToClientVPNTargetNetworkResponse (..),
    mkApplySecurityGroupsToClientVPNTargetNetworkResponse,

    -- ** Response lenses
    asgtcvtnrsSecurityGroupIds,
    asgtcvtnrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkApplySecurityGroupsToClientVPNTargetNetwork' smart constructor.
data ApplySecurityGroupsToClientVPNTargetNetwork = ApplySecurityGroupsToClientVPNTargetNetwork'
  { dryRun ::
      Lude.Maybe
        Lude.Bool,
    clientVPNEndpointId ::
      Lude.Text,
    vpcId ::
      Lude.Text,
    securityGroupIds ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplySecurityGroupsToClientVPNTargetNetwork' with the minimum fields required to make a request.
--
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'securityGroupIds' - The IDs of the security groups to apply to the associated target network. Up to 5 security groups can be applied to an associated target network.
-- * 'vpcId' - The ID of the VPC in which the associated target network is located.
mkApplySecurityGroupsToClientVPNTargetNetwork ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  ApplySecurityGroupsToClientVPNTargetNetwork
mkApplySecurityGroupsToClientVPNTargetNetwork
  pClientVPNEndpointId_
  pVPCId_ =
    ApplySecurityGroupsToClientVPNTargetNetwork'
      { dryRun =
          Lude.Nothing,
        clientVPNEndpointId = pClientVPNEndpointId_,
        vpcId = pVPCId_,
        securityGroupIds = Lude.mempty
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtcvtnDryRun :: Lens.Lens' ApplySecurityGroupsToClientVPNTargetNetwork (Lude.Maybe Lude.Bool)
asgtcvtnDryRun = Lens.lens (dryRun :: ApplySecurityGroupsToClientVPNTargetNetwork -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ApplySecurityGroupsToClientVPNTargetNetwork)
{-# DEPRECATED asgtcvtnDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtcvtnClientVPNEndpointId :: Lens.Lens' ApplySecurityGroupsToClientVPNTargetNetwork Lude.Text
asgtcvtnClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: ApplySecurityGroupsToClientVPNTargetNetwork -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: ApplySecurityGroupsToClientVPNTargetNetwork)
{-# DEPRECATED asgtcvtnClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | The ID of the VPC in which the associated target network is located.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtcvtnVPCId :: Lens.Lens' ApplySecurityGroupsToClientVPNTargetNetwork Lude.Text
asgtcvtnVPCId = Lens.lens (vpcId :: ApplySecurityGroupsToClientVPNTargetNetwork -> Lude.Text) (\s a -> s {vpcId = a} :: ApplySecurityGroupsToClientVPNTargetNetwork)
{-# DEPRECATED asgtcvtnVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The IDs of the security groups to apply to the associated target network. Up to 5 security groups can be applied to an associated target network.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtcvtnSecurityGroupIds :: Lens.Lens' ApplySecurityGroupsToClientVPNTargetNetwork [Lude.Text]
asgtcvtnSecurityGroupIds = Lens.lens (securityGroupIds :: ApplySecurityGroupsToClientVPNTargetNetwork -> [Lude.Text]) (\s a -> s {securityGroupIds = a} :: ApplySecurityGroupsToClientVPNTargetNetwork)
{-# DEPRECATED asgtcvtnSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

instance
  Lude.AWSRequest
    ApplySecurityGroupsToClientVPNTargetNetwork
  where
  type
    Rs ApplySecurityGroupsToClientVPNTargetNetwork =
      ApplySecurityGroupsToClientVPNTargetNetworkResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ApplySecurityGroupsToClientVPNTargetNetworkResponse'
            Lude.<$> ( x Lude..@? "securityGroupIds" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ApplySecurityGroupsToClientVPNTargetNetwork where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ApplySecurityGroupsToClientVPNTargetNetwork where
  toPath = Lude.const "/"

instance Lude.ToQuery ApplySecurityGroupsToClientVPNTargetNetwork where
  toQuery ApplySecurityGroupsToClientVPNTargetNetwork' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ApplySecurityGroupsToClientVpnTargetNetwork" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId,
        "VpcId" Lude.=: vpcId,
        Lude.toQueryList "SecurityGroupId" securityGroupIds
      ]

-- | /See:/ 'mkApplySecurityGroupsToClientVPNTargetNetworkResponse' smart constructor.
data ApplySecurityGroupsToClientVPNTargetNetworkResponse = ApplySecurityGroupsToClientVPNTargetNetworkResponse'
  { securityGroupIds ::
      Lude.Maybe
        [Lude.Text],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'ApplySecurityGroupsToClientVPNTargetNetworkResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'securityGroupIds' - The IDs of the applied security groups.
mkApplySecurityGroupsToClientVPNTargetNetworkResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ApplySecurityGroupsToClientVPNTargetNetworkResponse
mkApplySecurityGroupsToClientVPNTargetNetworkResponse
  pResponseStatus_ =
    ApplySecurityGroupsToClientVPNTargetNetworkResponse'
      { securityGroupIds =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The IDs of the applied security groups.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtcvtnrsSecurityGroupIds :: Lens.Lens' ApplySecurityGroupsToClientVPNTargetNetworkResponse (Lude.Maybe [Lude.Text])
asgtcvtnrsSecurityGroupIds = Lens.lens (securityGroupIds :: ApplySecurityGroupsToClientVPNTargetNetworkResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: ApplySecurityGroupsToClientVPNTargetNetworkResponse)
{-# DEPRECATED asgtcvtnrsSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtcvtnrsResponseStatus :: Lens.Lens' ApplySecurityGroupsToClientVPNTargetNetworkResponse Lude.Int
asgtcvtnrsResponseStatus = Lens.lens (responseStatus :: ApplySecurityGroupsToClientVPNTargetNetworkResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ApplySecurityGroupsToClientVPNTargetNetworkResponse)
{-# DEPRECATED asgtcvtnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
