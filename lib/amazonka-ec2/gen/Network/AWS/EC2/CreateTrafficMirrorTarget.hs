{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTrafficMirrorTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a target for your Traffic Mirror session.
--
-- A Traffic Mirror target is the destination for mirrored traffic. The Traffic Mirror source and the Traffic Mirror target (monitoring appliances) can be in the same VPC, or in different VPCs connected via VPC peering or a transit gateway.
-- A Traffic Mirror target can be a network interface, or a Network Load Balancer.
-- To use the target in a Traffic Mirror session, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTrafficMirrorSession.htm CreateTrafficMirrorSession> .
module Network.AWS.EC2.CreateTrafficMirrorTarget
  ( -- * Creating a request
    CreateTrafficMirrorTarget (..),
    mkCreateTrafficMirrorTarget,

    -- ** Request lenses
    ctmtClientToken,
    ctmtNetworkInterfaceId,
    ctmtNetworkLoadBalancerARN,
    ctmtTagSpecifications,
    ctmtDescription,
    ctmtDryRun,

    -- * Destructuring the response
    CreateTrafficMirrorTargetResponse (..),
    mkCreateTrafficMirrorTargetResponse,

    -- ** Response lenses
    ctmtrsClientToken,
    ctmtrsTrafficMirrorTarget,
    ctmtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTrafficMirrorTarget' smart constructor.
data CreateTrafficMirrorTarget = CreateTrafficMirrorTarget'
  { clientToken ::
      Lude.Maybe Lude.Text,
    networkInterfaceId ::
      Lude.Maybe Lude.Text,
    networkLoadBalancerARN ::
      Lude.Maybe Lude.Text,
    tagSpecifications ::
      Lude.Maybe [TagSpecification],
    description :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrafficMirrorTarget' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'description' - The description of the Traffic Mirror target.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'networkInterfaceId' - The network interface ID that is associated with the target.
-- * 'networkLoadBalancerARN' - The Amazon Resource Name (ARN) of the Network Load Balancer that is associated with the target.
-- * 'tagSpecifications' - The tags to assign to the Traffic Mirror target.
mkCreateTrafficMirrorTarget ::
  CreateTrafficMirrorTarget
mkCreateTrafficMirrorTarget =
  CreateTrafficMirrorTarget'
    { clientToken = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      networkLoadBalancerARN = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtClientToken :: Lens.Lens' CreateTrafficMirrorTarget (Lude.Maybe Lude.Text)
ctmtClientToken = Lens.lens (clientToken :: CreateTrafficMirrorTarget -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateTrafficMirrorTarget)
{-# DEPRECATED ctmtClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The network interface ID that is associated with the target.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtNetworkInterfaceId :: Lens.Lens' CreateTrafficMirrorTarget (Lude.Maybe Lude.Text)
ctmtNetworkInterfaceId = Lens.lens (networkInterfaceId :: CreateTrafficMirrorTarget -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: CreateTrafficMirrorTarget)
{-# DEPRECATED ctmtNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Network Load Balancer that is associated with the target.
--
-- /Note:/ Consider using 'networkLoadBalancerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtNetworkLoadBalancerARN :: Lens.Lens' CreateTrafficMirrorTarget (Lude.Maybe Lude.Text)
ctmtNetworkLoadBalancerARN = Lens.lens (networkLoadBalancerARN :: CreateTrafficMirrorTarget -> Lude.Maybe Lude.Text) (\s a -> s {networkLoadBalancerARN = a} :: CreateTrafficMirrorTarget)
{-# DEPRECATED ctmtNetworkLoadBalancerARN "Use generic-lens or generic-optics with 'networkLoadBalancerARN' instead." #-}

-- | The tags to assign to the Traffic Mirror target.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtTagSpecifications :: Lens.Lens' CreateTrafficMirrorTarget (Lude.Maybe [TagSpecification])
ctmtTagSpecifications = Lens.lens (tagSpecifications :: CreateTrafficMirrorTarget -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateTrafficMirrorTarget)
{-# DEPRECATED ctmtTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The description of the Traffic Mirror target.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtDescription :: Lens.Lens' CreateTrafficMirrorTarget (Lude.Maybe Lude.Text)
ctmtDescription = Lens.lens (description :: CreateTrafficMirrorTarget -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateTrafficMirrorTarget)
{-# DEPRECATED ctmtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtDryRun :: Lens.Lens' CreateTrafficMirrorTarget (Lude.Maybe Lude.Bool)
ctmtDryRun = Lens.lens (dryRun :: CreateTrafficMirrorTarget -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateTrafficMirrorTarget)
{-# DEPRECATED ctmtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateTrafficMirrorTarget where
  type
    Rs CreateTrafficMirrorTarget =
      CreateTrafficMirrorTargetResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateTrafficMirrorTargetResponse'
            Lude.<$> (x Lude..@? "clientToken")
            Lude.<*> (x Lude..@? "trafficMirrorTarget")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTrafficMirrorTarget where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTrafficMirrorTarget where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTrafficMirrorTarget where
  toQuery CreateTrafficMirrorTarget' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateTrafficMirrorTarget" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "NetworkInterfaceId" Lude.=: networkInterfaceId,
        "NetworkLoadBalancerArn" Lude.=: networkLoadBalancerARN,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateTrafficMirrorTargetResponse' smart constructor.
data CreateTrafficMirrorTargetResponse = CreateTrafficMirrorTargetResponse'
  { clientToken ::
      Lude.Maybe Lude.Text,
    trafficMirrorTarget ::
      Lude.Maybe
        TrafficMirrorTarget,
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrafficMirrorTargetResponse' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'responseStatus' - The response status code.
-- * 'trafficMirrorTarget' - Information about the Traffic Mirror target.
mkCreateTrafficMirrorTargetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTrafficMirrorTargetResponse
mkCreateTrafficMirrorTargetResponse pResponseStatus_ =
  CreateTrafficMirrorTargetResponse'
    { clientToken = Lude.Nothing,
      trafficMirrorTarget = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtrsClientToken :: Lens.Lens' CreateTrafficMirrorTargetResponse (Lude.Maybe Lude.Text)
ctmtrsClientToken = Lens.lens (clientToken :: CreateTrafficMirrorTargetResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateTrafficMirrorTargetResponse)
{-# DEPRECATED ctmtrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Information about the Traffic Mirror target.
--
-- /Note:/ Consider using 'trafficMirrorTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtrsTrafficMirrorTarget :: Lens.Lens' CreateTrafficMirrorTargetResponse (Lude.Maybe TrafficMirrorTarget)
ctmtrsTrafficMirrorTarget = Lens.lens (trafficMirrorTarget :: CreateTrafficMirrorTargetResponse -> Lude.Maybe TrafficMirrorTarget) (\s a -> s {trafficMirrorTarget = a} :: CreateTrafficMirrorTargetResponse)
{-# DEPRECATED ctmtrsTrafficMirrorTarget "Use generic-lens or generic-optics with 'trafficMirrorTarget' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmtrsResponseStatus :: Lens.Lens' CreateTrafficMirrorTargetResponse Lude.Int
ctmtrsResponseStatus = Lens.lens (responseStatus :: CreateTrafficMirrorTargetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTrafficMirrorTargetResponse)
{-# DEPRECATED ctmtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
