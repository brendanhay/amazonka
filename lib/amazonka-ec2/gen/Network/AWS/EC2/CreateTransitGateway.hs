{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a transit gateway.
--
-- You can use a transit gateway to interconnect your virtual private clouds (VPC) and on-premises networks. After the transit gateway enters the @available@ state, you can attach your VPCs and VPN connections to the transit gateway.
-- To attach your VPCs, use 'CreateTransitGatewayVpcAttachment' .
-- To attach a VPN connection, use 'CreateCustomerGateway' to create a customer gateway and specify the ID of the customer gateway and the ID of the transit gateway in a call to 'CreateVpnConnection' .
-- When you create a transit gateway, we create a default transit gateway route table and use it as the default association route table and the default propagation route table. You can use 'CreateTransitGatewayRouteTable' to create additional transit gateway route tables. If you disable automatic route propagation, we do not create a default transit gateway route table. You can use 'EnableTransitGatewayRouteTablePropagation' to propagate routes from a resource attachment to a transit gateway route table. If you disable automatic associations, you can use 'AssociateTransitGatewayRouteTable' to associate a resource attachment with a transit gateway route table.
module Network.AWS.EC2.CreateTransitGateway
  ( -- * Creating a request
    CreateTransitGateway (..),
    mkCreateTransitGateway,

    -- ** Request lenses
    ctgTagSpecifications,
    ctgOptions,
    ctgDescription,
    ctgDryRun,

    -- * Destructuring the response
    CreateTransitGatewayResponse (..),
    mkCreateTransitGatewayResponse,

    -- ** Response lenses
    ctgrsTransitGateway,
    ctgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTransitGateway' smart constructor.
data CreateTransitGateway = CreateTransitGateway'
  { tagSpecifications ::
      Lude.Maybe [TagSpecification],
    options ::
      Lude.Maybe TransitGatewayRequestOptions,
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

-- | Creates a value of 'CreateTransitGateway' with the minimum fields required to make a request.
--
-- * 'description' - A description of the transit gateway.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'options' - The transit gateway options.
-- * 'tagSpecifications' - The tags to apply to the transit gateway.
mkCreateTransitGateway ::
  CreateTransitGateway
mkCreateTransitGateway =
  CreateTransitGateway'
    { tagSpecifications = Lude.Nothing,
      options = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The tags to apply to the transit gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgTagSpecifications :: Lens.Lens' CreateTransitGateway (Lude.Maybe [TagSpecification])
ctgTagSpecifications = Lens.lens (tagSpecifications :: CreateTransitGateway -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateTransitGateway)
{-# DEPRECATED ctgTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The transit gateway options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgOptions :: Lens.Lens' CreateTransitGateway (Lude.Maybe TransitGatewayRequestOptions)
ctgOptions = Lens.lens (options :: CreateTransitGateway -> Lude.Maybe TransitGatewayRequestOptions) (\s a -> s {options = a} :: CreateTransitGateway)
{-# DEPRECATED ctgOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | A description of the transit gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgDescription :: Lens.Lens' CreateTransitGateway (Lude.Maybe Lude.Text)
ctgDescription = Lens.lens (description :: CreateTransitGateway -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateTransitGateway)
{-# DEPRECATED ctgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgDryRun :: Lens.Lens' CreateTransitGateway (Lude.Maybe Lude.Bool)
ctgDryRun = Lens.lens (dryRun :: CreateTransitGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateTransitGateway)
{-# DEPRECATED ctgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateTransitGateway where
  type Rs CreateTransitGateway = CreateTransitGatewayResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateTransitGatewayResponse'
            Lude.<$> (x Lude..@? "transitGateway")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTransitGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTransitGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTransitGateway where
  toQuery CreateTransitGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateTransitGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "Options" Lude.=: options,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateTransitGatewayResponse' smart constructor.
data CreateTransitGatewayResponse = CreateTransitGatewayResponse'
  { transitGateway ::
      Lude.Maybe TransitGateway,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitGatewayResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transitGateway' - Information about the transit gateway.
mkCreateTransitGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTransitGatewayResponse
mkCreateTransitGatewayResponse pResponseStatus_ =
  CreateTransitGatewayResponse'
    { transitGateway = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the transit gateway.
--
-- /Note:/ Consider using 'transitGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrsTransitGateway :: Lens.Lens' CreateTransitGatewayResponse (Lude.Maybe TransitGateway)
ctgrsTransitGateway = Lens.lens (transitGateway :: CreateTransitGatewayResponse -> Lude.Maybe TransitGateway) (\s a -> s {transitGateway = a} :: CreateTransitGatewayResponse)
{-# DEPRECATED ctgrsTransitGateway "Use generic-lens or generic-optics with 'transitGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrsResponseStatus :: Lens.Lens' CreateTransitGatewayResponse Lude.Int
ctgrsResponseStatus = Lens.lens (responseStatus :: CreateTransitGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTransitGatewayResponse)
{-# DEPRECATED ctgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
