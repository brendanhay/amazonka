{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified subnets from the transit gateway multicast domain.
module Network.AWS.EC2.DisassociateTransitGatewayMulticastDomain
  ( -- * Creating a request
    DisassociateTransitGatewayMulticastDomain (..),
    mkDisassociateTransitGatewayMulticastDomain,

    -- ** Request lenses
    dtgmdSubnetIds,
    dtgmdTransitGatewayMulticastDomainId,
    dtgmdTransitGatewayAttachmentId,
    dtgmdDryRun,

    -- * Destructuring the response
    DisassociateTransitGatewayMulticastDomainResponse (..),
    mkDisassociateTransitGatewayMulticastDomainResponse,

    -- ** Response lenses
    dtgmdrsAssociations,
    dtgmdrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateTransitGatewayMulticastDomain' smart constructor.
data DisassociateTransitGatewayMulticastDomain = DisassociateTransitGatewayMulticastDomain'
  { -- | The IDs of the subnets;
    subnetIds :: Lude.Maybe [Lude.Text],
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Lude.Maybe Lude.Text,
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateTransitGatewayMulticastDomain' with the minimum fields required to make a request.
--
-- * 'subnetIds' - The IDs of the subnets;
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDisassociateTransitGatewayMulticastDomain ::
  DisassociateTransitGatewayMulticastDomain
mkDisassociateTransitGatewayMulticastDomain =
  DisassociateTransitGatewayMulticastDomain'
    { subnetIds =
        Lude.Nothing,
      transitGatewayMulticastDomainId = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The IDs of the subnets;
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdSubnetIds :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Lude.Maybe [Lude.Text])
dtgmdSubnetIds = Lens.lens (subnetIds :: DisassociateTransitGatewayMulticastDomain -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: DisassociateTransitGatewayMulticastDomain)
{-# DEPRECATED dtgmdSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdTransitGatewayMulticastDomainId :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Lude.Maybe Lude.Text)
dtgmdTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: DisassociateTransitGatewayMulticastDomain -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: DisassociateTransitGatewayMulticastDomain)
{-# DEPRECATED dtgmdTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdTransitGatewayAttachmentId :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Lude.Maybe Lude.Text)
dtgmdTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: DisassociateTransitGatewayMulticastDomain -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: DisassociateTransitGatewayMulticastDomain)
{-# DEPRECATED dtgmdTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdDryRun :: Lens.Lens' DisassociateTransitGatewayMulticastDomain (Lude.Maybe Lude.Bool)
dtgmdDryRun = Lens.lens (dryRun :: DisassociateTransitGatewayMulticastDomain -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DisassociateTransitGatewayMulticastDomain)
{-# DEPRECATED dtgmdDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DisassociateTransitGatewayMulticastDomain where
  type
    Rs DisassociateTransitGatewayMulticastDomain =
      DisassociateTransitGatewayMulticastDomainResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DisassociateTransitGatewayMulticastDomainResponse'
            Lude.<$> (x Lude..@? "associations") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateTransitGatewayMulticastDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisassociateTransitGatewayMulticastDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateTransitGatewayMulticastDomain where
  toQuery DisassociateTransitGatewayMulticastDomain' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisassociateTransitGatewayMulticastDomain" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "SubnetIds" Lude.<$> subnetIds),
        "TransitGatewayMulticastDomainId"
          Lude.=: transitGatewayMulticastDomainId,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDisassociateTransitGatewayMulticastDomainResponse' smart constructor.
data DisassociateTransitGatewayMulticastDomainResponse = DisassociateTransitGatewayMulticastDomainResponse'
  { -- | Information about the association.
    associations :: Lude.Maybe TransitGatewayMulticastDomainAssociations,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateTransitGatewayMulticastDomainResponse' with the minimum fields required to make a request.
--
-- * 'associations' - Information about the association.
-- * 'responseStatus' - The response status code.
mkDisassociateTransitGatewayMulticastDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateTransitGatewayMulticastDomainResponse
mkDisassociateTransitGatewayMulticastDomainResponse
  pResponseStatus_ =
    DisassociateTransitGatewayMulticastDomainResponse'
      { associations =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about the association.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrsAssociations :: Lens.Lens' DisassociateTransitGatewayMulticastDomainResponse (Lude.Maybe TransitGatewayMulticastDomainAssociations)
dtgmdrsAssociations = Lens.lens (associations :: DisassociateTransitGatewayMulticastDomainResponse -> Lude.Maybe TransitGatewayMulticastDomainAssociations) (\s a -> s {associations = a} :: DisassociateTransitGatewayMulticastDomainResponse)
{-# DEPRECATED dtgmdrsAssociations "Use generic-lens or generic-optics with 'associations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdrsResponseStatus :: Lens.Lens' DisassociateTransitGatewayMulticastDomainResponse Lude.Int
dtgmdrsResponseStatus = Lens.lens (responseStatus :: DisassociateTransitGatewayMulticastDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateTransitGatewayMulticastDomainResponse)
{-# DEPRECATED dtgmdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
