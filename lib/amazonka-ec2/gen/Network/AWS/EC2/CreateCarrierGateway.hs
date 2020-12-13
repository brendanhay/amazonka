{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateCarrierGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a carrier gateway. For more information about carrier gateways, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#wavelength-carrier-gateway Carrier gateways> in the /AWS Wavelength Developer Guide/ .
module Network.AWS.EC2.CreateCarrierGateway
  ( -- * Creating a request
    CreateCarrierGateway (..),
    mkCreateCarrierGateway,

    -- ** Request lenses
    ccgClientToken,
    ccgVPCId,
    ccgTagSpecifications,
    ccgDryRun,

    -- * Destructuring the response
    CreateCarrierGatewayResponse (..),
    mkCreateCarrierGatewayResponse,

    -- ** Response lenses
    ccgfrsCarrierGateway,
    ccgfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateCarrierGateway' smart constructor.
data CreateCarrierGateway = CreateCarrierGateway'
  { -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Lude.Maybe Lude.Text,
    -- | The ID of the VPC to associate with the carrier gateway.
    vpcId :: Lude.Text,
    -- | The tags to associate with the carrier gateway.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCarrierGateway' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'vpcId' - The ID of the VPC to associate with the carrier gateway.
-- * 'tagSpecifications' - The tags to associate with the carrier gateway.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateCarrierGateway ::
  -- | 'vpcId'
  Lude.Text ->
  CreateCarrierGateway
mkCreateCarrierGateway pVPCId_ =
  CreateCarrierGateway'
    { clientToken = Lude.Nothing,
      vpcId = pVPCId_,
      tagSpecifications = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgClientToken :: Lens.Lens' CreateCarrierGateway (Lude.Maybe Lude.Text)
ccgClientToken = Lens.lens (clientToken :: CreateCarrierGateway -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateCarrierGateway)
{-# DEPRECATED ccgClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ID of the VPC to associate with the carrier gateway.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgVPCId :: Lens.Lens' CreateCarrierGateway Lude.Text
ccgVPCId = Lens.lens (vpcId :: CreateCarrierGateway -> Lude.Text) (\s a -> s {vpcId = a} :: CreateCarrierGateway)
{-# DEPRECATED ccgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The tags to associate with the carrier gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgTagSpecifications :: Lens.Lens' CreateCarrierGateway (Lude.Maybe [TagSpecification])
ccgTagSpecifications = Lens.lens (tagSpecifications :: CreateCarrierGateway -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateCarrierGateway)
{-# DEPRECATED ccgTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgDryRun :: Lens.Lens' CreateCarrierGateway (Lude.Maybe Lude.Bool)
ccgDryRun = Lens.lens (dryRun :: CreateCarrierGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateCarrierGateway)
{-# DEPRECATED ccgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateCarrierGateway where
  type Rs CreateCarrierGateway = CreateCarrierGatewayResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateCarrierGatewayResponse'
            Lude.<$> (x Lude..@? "carrierGateway")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCarrierGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateCarrierGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCarrierGateway where
  toQuery CreateCarrierGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateCarrierGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        "VpcId" Lude.=: vpcId,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateCarrierGatewayResponse' smart constructor.
data CreateCarrierGatewayResponse = CreateCarrierGatewayResponse'
  { -- | Information about the carrier gateway.
    carrierGateway :: Lude.Maybe CarrierGateway,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCarrierGatewayResponse' with the minimum fields required to make a request.
--
-- * 'carrierGateway' - Information about the carrier gateway.
-- * 'responseStatus' - The response status code.
mkCreateCarrierGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCarrierGatewayResponse
mkCreateCarrierGatewayResponse pResponseStatus_ =
  CreateCarrierGatewayResponse'
    { carrierGateway = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the carrier gateway.
--
-- /Note:/ Consider using 'carrierGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgfrsCarrierGateway :: Lens.Lens' CreateCarrierGatewayResponse (Lude.Maybe CarrierGateway)
ccgfrsCarrierGateway = Lens.lens (carrierGateway :: CreateCarrierGatewayResponse -> Lude.Maybe CarrierGateway) (\s a -> s {carrierGateway = a} :: CreateCarrierGatewayResponse)
{-# DEPRECATED ccgfrsCarrierGateway "Use generic-lens or generic-optics with 'carrierGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgfrsResponseStatus :: Lens.Lens' CreateCarrierGatewayResponse Lude.Int
ccgfrsResponseStatus = Lens.lens (responseStatus :: CreateCarrierGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCarrierGatewayResponse)
{-# DEPRECATED ccgfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
