{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway multicast domain.
module Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
  ( -- * Creating a request
    DeleteTransitGatewayMulticastDomain (..),
    mkDeleteTransitGatewayMulticastDomain,

    -- ** Request lenses
    dtgmdfTransitGatewayMulticastDomainId,
    dtgmdfDryRun,

    -- * Destructuring the response
    DeleteTransitGatewayMulticastDomainResponse (..),
    mkDeleteTransitGatewayMulticastDomainResponse,

    -- ** Response lenses
    dtgmdfrsTransitGatewayMulticastDomain,
    dtgmdfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTransitGatewayMulticastDomain' smart constructor.
data DeleteTransitGatewayMulticastDomain = DeleteTransitGatewayMulticastDomain'
  { -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTransitGatewayMulticastDomain' with the minimum fields required to make a request.
--
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteTransitGatewayMulticastDomain ::
  -- | 'transitGatewayMulticastDomainId'
  Lude.Text ->
  DeleteTransitGatewayMulticastDomain
mkDeleteTransitGatewayMulticastDomain
  pTransitGatewayMulticastDomainId_ =
    DeleteTransitGatewayMulticastDomain'
      { transitGatewayMulticastDomainId =
          pTransitGatewayMulticastDomainId_,
        dryRun = Lude.Nothing
      }

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdfTransitGatewayMulticastDomainId :: Lens.Lens' DeleteTransitGatewayMulticastDomain Lude.Text
dtgmdfTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: DeleteTransitGatewayMulticastDomain -> Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: DeleteTransitGatewayMulticastDomain)
{-# DEPRECATED dtgmdfTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdfDryRun :: Lens.Lens' DeleteTransitGatewayMulticastDomain (Lude.Maybe Lude.Bool)
dtgmdfDryRun = Lens.lens (dryRun :: DeleteTransitGatewayMulticastDomain -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteTransitGatewayMulticastDomain)
{-# DEPRECATED dtgmdfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteTransitGatewayMulticastDomain where
  type
    Rs DeleteTransitGatewayMulticastDomain =
      DeleteTransitGatewayMulticastDomainResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteTransitGatewayMulticastDomainResponse'
            Lude.<$> (x Lude..@? "transitGatewayMulticastDomain")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTransitGatewayMulticastDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTransitGatewayMulticastDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTransitGatewayMulticastDomain where
  toQuery DeleteTransitGatewayMulticastDomain' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteTransitGatewayMulticastDomain" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TransitGatewayMulticastDomainId"
          Lude.=: transitGatewayMulticastDomainId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteTransitGatewayMulticastDomainResponse' smart constructor.
data DeleteTransitGatewayMulticastDomainResponse = DeleteTransitGatewayMulticastDomainResponse'
  { -- | Information about the deleted transit gateway multicast domain.
    transitGatewayMulticastDomain :: Lude.Maybe TransitGatewayMulticastDomain,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTransitGatewayMulticastDomainResponse' with the minimum fields required to make a request.
--
-- * 'transitGatewayMulticastDomain' - Information about the deleted transit gateway multicast domain.
-- * 'responseStatus' - The response status code.
mkDeleteTransitGatewayMulticastDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTransitGatewayMulticastDomainResponse
mkDeleteTransitGatewayMulticastDomainResponse pResponseStatus_ =
  DeleteTransitGatewayMulticastDomainResponse'
    { transitGatewayMulticastDomain =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the deleted transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdfrsTransitGatewayMulticastDomain :: Lens.Lens' DeleteTransitGatewayMulticastDomainResponse (Lude.Maybe TransitGatewayMulticastDomain)
dtgmdfrsTransitGatewayMulticastDomain = Lens.lens (transitGatewayMulticastDomain :: DeleteTransitGatewayMulticastDomainResponse -> Lude.Maybe TransitGatewayMulticastDomain) (\s a -> s {transitGatewayMulticastDomain = a} :: DeleteTransitGatewayMulticastDomainResponse)
{-# DEPRECATED dtgmdfrsTransitGatewayMulticastDomain "Use generic-lens or generic-optics with 'transitGatewayMulticastDomain' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdfrsResponseStatus :: Lens.Lens' DeleteTransitGatewayMulticastDomainResponse Lude.Int
dtgmdfrsResponseStatus = Lens.lens (responseStatus :: DeleteTransitGatewayMulticastDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTransitGatewayMulticastDomainResponse)
{-# DEPRECATED dtgmdfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
