{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dtgmdDryRun,
    dtgmdTransitGatewayMulticastDomainId,

    -- * Destructuring the response
    DeleteTransitGatewayMulticastDomainResponse (..),
    mkDeleteTransitGatewayMulticastDomainResponse,

    -- ** Response lenses
    delrsTransitGatewayMulticastDomain,
    delrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTransitGatewayMulticastDomain' smart constructor.
data DeleteTransitGatewayMulticastDomain = DeleteTransitGatewayMulticastDomain'
  { dryRun ::
      Lude.Maybe
        Lude.Bool,
    transitGatewayMulticastDomainId ::
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

-- | Creates a value of 'DeleteTransitGatewayMulticastDomain' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
mkDeleteTransitGatewayMulticastDomain ::
  -- | 'transitGatewayMulticastDomainId'
  Lude.Text ->
  DeleteTransitGatewayMulticastDomain
mkDeleteTransitGatewayMulticastDomain
  pTransitGatewayMulticastDomainId_ =
    DeleteTransitGatewayMulticastDomain'
      { dryRun = Lude.Nothing,
        transitGatewayMulticastDomainId =
          pTransitGatewayMulticastDomainId_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdDryRun :: Lens.Lens' DeleteTransitGatewayMulticastDomain (Lude.Maybe Lude.Bool)
dtgmdDryRun = Lens.lens (dryRun :: DeleteTransitGatewayMulticastDomain -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteTransitGatewayMulticastDomain)
{-# DEPRECATED dtgmdDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdTransitGatewayMulticastDomainId :: Lens.Lens' DeleteTransitGatewayMulticastDomain Lude.Text
dtgmdTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: DeleteTransitGatewayMulticastDomain -> Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: DeleteTransitGatewayMulticastDomain)
{-# DEPRECATED dtgmdTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

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
        "DryRun" Lude.=: dryRun,
        "TransitGatewayMulticastDomainId"
          Lude.=: transitGatewayMulticastDomainId
      ]

-- | /See:/ 'mkDeleteTransitGatewayMulticastDomainResponse' smart constructor.
data DeleteTransitGatewayMulticastDomainResponse = DeleteTransitGatewayMulticastDomainResponse'
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

-- | Creates a value of 'DeleteTransitGatewayMulticastDomainResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayMulticastDomain' - Information about the deleted transit gateway multicast domain.
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
delrsTransitGatewayMulticastDomain :: Lens.Lens' DeleteTransitGatewayMulticastDomainResponse (Lude.Maybe TransitGatewayMulticastDomain)
delrsTransitGatewayMulticastDomain = Lens.lens (transitGatewayMulticastDomain :: DeleteTransitGatewayMulticastDomainResponse -> Lude.Maybe TransitGatewayMulticastDomain) (\s a -> s {transitGatewayMulticastDomain = a} :: DeleteTransitGatewayMulticastDomainResponse)
{-# DEPRECATED delrsTransitGatewayMulticastDomain "Use generic-lens or generic-optics with 'transitGatewayMulticastDomain' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteTransitGatewayMulticastDomainResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteTransitGatewayMulticastDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTransitGatewayMulticastDomainResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
