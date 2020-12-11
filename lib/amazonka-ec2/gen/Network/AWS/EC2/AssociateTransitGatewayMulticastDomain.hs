{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified subnets and transit gateway attachments with the specified transit gateway multicast domain.
--
-- The transit gateway attachment must be in the available state before you can add a resource. Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeTransitGatewayAttachments.html DescribeTransitGatewayAttachments> to see the state of the attachment.
module Network.AWS.EC2.AssociateTransitGatewayMulticastDomain
  ( -- * Creating a request
    AssociateTransitGatewayMulticastDomain (..),
    mkAssociateTransitGatewayMulticastDomain,

    -- ** Request lenses
    atgmdSubnetIds,
    atgmdTransitGatewayMulticastDomainId,
    atgmdTransitGatewayAttachmentId,
    atgmdDryRun,

    -- * Destructuring the response
    AssociateTransitGatewayMulticastDomainResponse (..),
    mkAssociateTransitGatewayMulticastDomainResponse,

    -- ** Response lenses
    atgmdrsAssociations,
    atgmdrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateTransitGatewayMulticastDomain' smart constructor.
data AssociateTransitGatewayMulticastDomain = AssociateTransitGatewayMulticastDomain'
  { subnetIds ::
      Lude.Maybe
        [Lude.Text],
    transitGatewayMulticastDomainId ::
      Lude.Maybe
        Lude.Text,
    transitGatewayAttachmentId ::
      Lude.Maybe
        Lude.Text,
    dryRun ::
      Lude.Maybe
        Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateTransitGatewayMulticastDomain' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'subnetIds' - The IDs of the subnets to associate with the transit gateway multicast domain.
-- * 'transitGatewayAttachmentId' - The ID of the transit gateway attachment to associate with the transit gateway multicast domain.
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
mkAssociateTransitGatewayMulticastDomain ::
  AssociateTransitGatewayMulticastDomain
mkAssociateTransitGatewayMulticastDomain =
  AssociateTransitGatewayMulticastDomain'
    { subnetIds = Lude.Nothing,
      transitGatewayMulticastDomainId = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The IDs of the subnets to associate with the transit gateway multicast domain.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgmdSubnetIds :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Lude.Maybe [Lude.Text])
atgmdSubnetIds = Lens.lens (subnetIds :: AssociateTransitGatewayMulticastDomain -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: AssociateTransitGatewayMulticastDomain)
{-# DEPRECATED atgmdSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgmdTransitGatewayMulticastDomainId :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Lude.Maybe Lude.Text)
atgmdTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: AssociateTransitGatewayMulticastDomain -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: AssociateTransitGatewayMulticastDomain)
{-# DEPRECATED atgmdTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | The ID of the transit gateway attachment to associate with the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgmdTransitGatewayAttachmentId :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Lude.Maybe Lude.Text)
atgmdTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: AssociateTransitGatewayMulticastDomain -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: AssociateTransitGatewayMulticastDomain)
{-# DEPRECATED atgmdTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgmdDryRun :: Lens.Lens' AssociateTransitGatewayMulticastDomain (Lude.Maybe Lude.Bool)
atgmdDryRun = Lens.lens (dryRun :: AssociateTransitGatewayMulticastDomain -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AssociateTransitGatewayMulticastDomain)
{-# DEPRECATED atgmdDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AssociateTransitGatewayMulticastDomain where
  type
    Rs AssociateTransitGatewayMulticastDomain =
      AssociateTransitGatewayMulticastDomainResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AssociateTransitGatewayMulticastDomainResponse'
            Lude.<$> (x Lude..@? "associations") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateTransitGatewayMulticastDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssociateTransitGatewayMulticastDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateTransitGatewayMulticastDomain where
  toQuery AssociateTransitGatewayMulticastDomain' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AssociateTransitGatewayMulticastDomain" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "SubnetIds" Lude.<$> subnetIds),
        "TransitGatewayMulticastDomainId"
          Lude.=: transitGatewayMulticastDomainId,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkAssociateTransitGatewayMulticastDomainResponse' smart constructor.
data AssociateTransitGatewayMulticastDomainResponse = AssociateTransitGatewayMulticastDomainResponse'
  { associations ::
      Lude.Maybe
        TransitGatewayMulticastDomainAssociations,
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

-- | Creates a value of 'AssociateTransitGatewayMulticastDomainResponse' with the minimum fields required to make a request.
--
-- * 'associations' - Information about the transit gateway multicast domain associations.
-- * 'responseStatus' - The response status code.
mkAssociateTransitGatewayMulticastDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateTransitGatewayMulticastDomainResponse
mkAssociateTransitGatewayMulticastDomainResponse pResponseStatus_ =
  AssociateTransitGatewayMulticastDomainResponse'
    { associations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the transit gateway multicast domain associations.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgmdrsAssociations :: Lens.Lens' AssociateTransitGatewayMulticastDomainResponse (Lude.Maybe TransitGatewayMulticastDomainAssociations)
atgmdrsAssociations = Lens.lens (associations :: AssociateTransitGatewayMulticastDomainResponse -> Lude.Maybe TransitGatewayMulticastDomainAssociations) (\s a -> s {associations = a} :: AssociateTransitGatewayMulticastDomainResponse)
{-# DEPRECATED atgmdrsAssociations "Use generic-lens or generic-optics with 'associations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atgmdrsResponseStatus :: Lens.Lens' AssociateTransitGatewayMulticastDomainResponse Lude.Int
atgmdrsResponseStatus = Lens.lens (responseStatus :: AssociateTransitGatewayMulticastDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateTransitGatewayMulticastDomainResponse)
{-# DEPRECATED atgmdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
