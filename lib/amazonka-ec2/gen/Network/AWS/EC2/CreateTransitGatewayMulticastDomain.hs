{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a multicast domain using the specified transit gateway.
--
-- The transit gateway must be in the available state before you create a domain. Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeTransitGateways.html DescribeTransitGateways> to see the state of transit gateway.
module Network.AWS.EC2.CreateTransitGatewayMulticastDomain
  ( -- * Creating a request
    CreateTransitGatewayMulticastDomain (..),
    mkCreateTransitGatewayMulticastDomain,

    -- ** Request lenses
    ctgmdTagSpecifications,
    ctgmdDryRun,
    ctgmdTransitGatewayId,

    -- * Destructuring the response
    CreateTransitGatewayMulticastDomainResponse (..),
    mkCreateTransitGatewayMulticastDomainResponse,

    -- ** Response lenses
    ctgmdrsTransitGatewayMulticastDomain,
    ctgmdrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTransitGatewayMulticastDomain' smart constructor.
data CreateTransitGatewayMulticastDomain = CreateTransitGatewayMulticastDomain'
  { tagSpecifications ::
      Lude.Maybe
        [TagSpecification],
    dryRun ::
      Lude.Maybe
        Lude.Bool,
    transitGatewayId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTransitGatewayMulticastDomain' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'tagSpecifications' - The tags for the transit gateway multicast domain.
-- * 'transitGatewayId' - The ID of the transit gateway.
mkCreateTransitGatewayMulticastDomain ::
  -- | 'transitGatewayId'
  Lude.Text ->
  CreateTransitGatewayMulticastDomain
mkCreateTransitGatewayMulticastDomain pTransitGatewayId_ =
  CreateTransitGatewayMulticastDomain'
    { tagSpecifications =
        Lude.Nothing,
      dryRun = Lude.Nothing,
      transitGatewayId = pTransitGatewayId_
    }

-- | The tags for the transit gateway multicast domain.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgmdTagSpecifications :: Lens.Lens' CreateTransitGatewayMulticastDomain (Lude.Maybe [TagSpecification])
ctgmdTagSpecifications = Lens.lens (tagSpecifications :: CreateTransitGatewayMulticastDomain -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateTransitGatewayMulticastDomain)
{-# DEPRECATED ctgmdTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgmdDryRun :: Lens.Lens' CreateTransitGatewayMulticastDomain (Lude.Maybe Lude.Bool)
ctgmdDryRun = Lens.lens (dryRun :: CreateTransitGatewayMulticastDomain -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateTransitGatewayMulticastDomain)
{-# DEPRECATED ctgmdDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgmdTransitGatewayId :: Lens.Lens' CreateTransitGatewayMulticastDomain Lude.Text
ctgmdTransitGatewayId = Lens.lens (transitGatewayId :: CreateTransitGatewayMulticastDomain -> Lude.Text) (\s a -> s {transitGatewayId = a} :: CreateTransitGatewayMulticastDomain)
{-# DEPRECATED ctgmdTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

instance Lude.AWSRequest CreateTransitGatewayMulticastDomain where
  type
    Rs CreateTransitGatewayMulticastDomain =
      CreateTransitGatewayMulticastDomainResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateTransitGatewayMulticastDomainResponse'
            Lude.<$> (x Lude..@? "transitGatewayMulticastDomain")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTransitGatewayMulticastDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTransitGatewayMulticastDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTransitGatewayMulticastDomain where
  toQuery CreateTransitGatewayMulticastDomain' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateTransitGatewayMulticastDomain" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DryRun" Lude.=: dryRun,
        "TransitGatewayId" Lude.=: transitGatewayId
      ]

-- | /See:/ 'mkCreateTransitGatewayMulticastDomainResponse' smart constructor.
data CreateTransitGatewayMulticastDomainResponse = CreateTransitGatewayMulticastDomainResponse'
  { transitGatewayMulticastDomain ::
      Lude.Maybe
        TransitGatewayMulticastDomain,
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

-- | Creates a value of 'CreateTransitGatewayMulticastDomainResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayMulticastDomain' - Information about the transit gateway multicast domain.
mkCreateTransitGatewayMulticastDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTransitGatewayMulticastDomainResponse
mkCreateTransitGatewayMulticastDomainResponse pResponseStatus_ =
  CreateTransitGatewayMulticastDomainResponse'
    { transitGatewayMulticastDomain =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgmdrsTransitGatewayMulticastDomain :: Lens.Lens' CreateTransitGatewayMulticastDomainResponse (Lude.Maybe TransitGatewayMulticastDomain)
ctgmdrsTransitGatewayMulticastDomain = Lens.lens (transitGatewayMulticastDomain :: CreateTransitGatewayMulticastDomainResponse -> Lude.Maybe TransitGatewayMulticastDomain) (\s a -> s {transitGatewayMulticastDomain = a} :: CreateTransitGatewayMulticastDomainResponse)
{-# DEPRECATED ctgmdrsTransitGatewayMulticastDomain "Use generic-lens or generic-optics with 'transitGatewayMulticastDomain' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgmdrsResponseStatus :: Lens.Lens' CreateTransitGatewayMulticastDomainResponse Lude.Int
ctgmdrsResponseStatus = Lens.lens (responseStatus :: CreateTransitGatewayMulticastDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTransitGatewayMulticastDomainResponse)
{-# DEPRECATED ctgmdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
