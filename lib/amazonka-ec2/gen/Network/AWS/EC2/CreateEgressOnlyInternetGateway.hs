{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateEgressOnlyInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [IPv6 only] Creates an egress-only internet gateway for your VPC. An egress-only internet gateway is used to enable outbound communication over IPv6 from instances in your VPC to the internet, and prevents hosts outside of your VPC from initiating an IPv6 connection with your instance.
module Network.AWS.EC2.CreateEgressOnlyInternetGateway
  ( -- * Creating a request
    CreateEgressOnlyInternetGateway (..),
    mkCreateEgressOnlyInternetGateway,

    -- ** Request lenses
    ceoigClientToken,
    ceoigTagSpecifications,
    ceoigDryRun,
    ceoigVPCId,

    -- * Destructuring the response
    CreateEgressOnlyInternetGatewayResponse (..),
    mkCreateEgressOnlyInternetGatewayResponse,

    -- ** Response lenses
    ceoigrsClientToken,
    ceoigrsEgressOnlyInternetGateway,
    ceoigrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateEgressOnlyInternetGateway' smart constructor.
data CreateEgressOnlyInternetGateway = CreateEgressOnlyInternetGateway'
  { clientToken ::
      Lude.Maybe Lude.Text,
    tagSpecifications ::
      Lude.Maybe
        [TagSpecification],
    dryRun ::
      Lude.Maybe Lude.Bool,
    vpcId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEgressOnlyInternetGateway' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'tagSpecifications' - The tags to assign to the egress-only internet gateway.
-- * 'vpcId' - The ID of the VPC for which to create the egress-only internet gateway.
mkCreateEgressOnlyInternetGateway ::
  -- | 'vpcId'
  Lude.Text ->
  CreateEgressOnlyInternetGateway
mkCreateEgressOnlyInternetGateway pVPCId_ =
  CreateEgressOnlyInternetGateway'
    { clientToken = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      dryRun = Lude.Nothing,
      vpcId = pVPCId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigClientToken :: Lens.Lens' CreateEgressOnlyInternetGateway (Lude.Maybe Lude.Text)
ceoigClientToken = Lens.lens (clientToken :: CreateEgressOnlyInternetGateway -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateEgressOnlyInternetGateway)
{-# DEPRECATED ceoigClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The tags to assign to the egress-only internet gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigTagSpecifications :: Lens.Lens' CreateEgressOnlyInternetGateway (Lude.Maybe [TagSpecification])
ceoigTagSpecifications = Lens.lens (tagSpecifications :: CreateEgressOnlyInternetGateway -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateEgressOnlyInternetGateway)
{-# DEPRECATED ceoigTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigDryRun :: Lens.Lens' CreateEgressOnlyInternetGateway (Lude.Maybe Lude.Bool)
ceoigDryRun = Lens.lens (dryRun :: CreateEgressOnlyInternetGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateEgressOnlyInternetGateway)
{-# DEPRECATED ceoigDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the VPC for which to create the egress-only internet gateway.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigVPCId :: Lens.Lens' CreateEgressOnlyInternetGateway Lude.Text
ceoigVPCId = Lens.lens (vpcId :: CreateEgressOnlyInternetGateway -> Lude.Text) (\s a -> s {vpcId = a} :: CreateEgressOnlyInternetGateway)
{-# DEPRECATED ceoigVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.AWSRequest CreateEgressOnlyInternetGateway where
  type
    Rs CreateEgressOnlyInternetGateway =
      CreateEgressOnlyInternetGatewayResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateEgressOnlyInternetGatewayResponse'
            Lude.<$> (x Lude..@? "clientToken")
            Lude.<*> (x Lude..@? "egressOnlyInternetGateway")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateEgressOnlyInternetGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateEgressOnlyInternetGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateEgressOnlyInternetGateway where
  toQuery CreateEgressOnlyInternetGateway' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateEgressOnlyInternetGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DryRun" Lude.=: dryRun,
        "VpcId" Lude.=: vpcId
      ]

-- | /See:/ 'mkCreateEgressOnlyInternetGatewayResponse' smart constructor.
data CreateEgressOnlyInternetGatewayResponse = CreateEgressOnlyInternetGatewayResponse'
  { clientToken ::
      Lude.Maybe
        Lude.Text,
    egressOnlyInternetGateway ::
      Lude.Maybe
        EgressOnlyInternetGateway,
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

-- | Creates a value of 'CreateEgressOnlyInternetGatewayResponse' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
-- * 'egressOnlyInternetGateway' - Information about the egress-only internet gateway.
-- * 'responseStatus' - The response status code.
mkCreateEgressOnlyInternetGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateEgressOnlyInternetGatewayResponse
mkCreateEgressOnlyInternetGatewayResponse pResponseStatus_ =
  CreateEgressOnlyInternetGatewayResponse'
    { clientToken =
        Lude.Nothing,
      egressOnlyInternetGateway = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigrsClientToken :: Lens.Lens' CreateEgressOnlyInternetGatewayResponse (Lude.Maybe Lude.Text)
ceoigrsClientToken = Lens.lens (clientToken :: CreateEgressOnlyInternetGatewayResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateEgressOnlyInternetGatewayResponse)
{-# DEPRECATED ceoigrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Information about the egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigrsEgressOnlyInternetGateway :: Lens.Lens' CreateEgressOnlyInternetGatewayResponse (Lude.Maybe EgressOnlyInternetGateway)
ceoigrsEgressOnlyInternetGateway = Lens.lens (egressOnlyInternetGateway :: CreateEgressOnlyInternetGatewayResponse -> Lude.Maybe EgressOnlyInternetGateway) (\s a -> s {egressOnlyInternetGateway = a} :: CreateEgressOnlyInternetGatewayResponse)
{-# DEPRECATED ceoigrsEgressOnlyInternetGateway "Use generic-lens or generic-optics with 'egressOnlyInternetGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigrsResponseStatus :: Lens.Lens' CreateEgressOnlyInternetGatewayResponse Lude.Int
ceoigrsResponseStatus = Lens.lens (responseStatus :: CreateEgressOnlyInternetGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateEgressOnlyInternetGatewayResponse)
{-# DEPRECATED ceoigrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
