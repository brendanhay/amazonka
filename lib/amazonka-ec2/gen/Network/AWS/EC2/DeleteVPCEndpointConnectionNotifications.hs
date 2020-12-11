{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVPCEndpointConnectionNotifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more VPC endpoint connection notifications.
module Network.AWS.EC2.DeleteVPCEndpointConnectionNotifications
  ( -- * Creating a request
    DeleteVPCEndpointConnectionNotifications (..),
    mkDeleteVPCEndpointConnectionNotifications,

    -- ** Request lenses
    dvecnDryRun,
    dvecnConnectionNotificationIds,

    -- * Destructuring the response
    DeleteVPCEndpointConnectionNotificationsResponse (..),
    mkDeleteVPCEndpointConnectionNotificationsResponse,

    -- ** Response lenses
    dvecnrsUnsuccessful,
    dvecnrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteVPCEndpointConnectionNotifications' smart constructor.
data DeleteVPCEndpointConnectionNotifications = DeleteVPCEndpointConnectionNotifications'
  { dryRun ::
      Lude.Maybe
        Lude.Bool,
    connectionNotificationIds ::
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

-- | Creates a value of 'DeleteVPCEndpointConnectionNotifications' with the minimum fields required to make a request.
--
-- * 'connectionNotificationIds' - One or more notification IDs.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteVPCEndpointConnectionNotifications ::
  DeleteVPCEndpointConnectionNotifications
mkDeleteVPCEndpointConnectionNotifications =
  DeleteVPCEndpointConnectionNotifications'
    { dryRun = Lude.Nothing,
      connectionNotificationIds = Lude.mempty
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnDryRun :: Lens.Lens' DeleteVPCEndpointConnectionNotifications (Lude.Maybe Lude.Bool)
dvecnDryRun = Lens.lens (dryRun :: DeleteVPCEndpointConnectionNotifications -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteVPCEndpointConnectionNotifications)
{-# DEPRECATED dvecnDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more notification IDs.
--
-- /Note:/ Consider using 'connectionNotificationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnConnectionNotificationIds :: Lens.Lens' DeleteVPCEndpointConnectionNotifications [Lude.Text]
dvecnConnectionNotificationIds = Lens.lens (connectionNotificationIds :: DeleteVPCEndpointConnectionNotifications -> [Lude.Text]) (\s a -> s {connectionNotificationIds = a} :: DeleteVPCEndpointConnectionNotifications)
{-# DEPRECATED dvecnConnectionNotificationIds "Use generic-lens or generic-optics with 'connectionNotificationIds' instead." #-}

instance Lude.AWSRequest DeleteVPCEndpointConnectionNotifications where
  type
    Rs DeleteVPCEndpointConnectionNotifications =
      DeleteVPCEndpointConnectionNotificationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteVPCEndpointConnectionNotificationsResponse'
            Lude.<$> ( x Lude..@? "unsuccessful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteVPCEndpointConnectionNotifications where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVPCEndpointConnectionNotifications where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVPCEndpointConnectionNotifications where
  toQuery DeleteVPCEndpointConnectionNotifications' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteVpcEndpointConnectionNotifications" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        Lude.toQueryList
          "ConnectionNotificationId"
          connectionNotificationIds
      ]

-- | /See:/ 'mkDeleteVPCEndpointConnectionNotificationsResponse' smart constructor.
data DeleteVPCEndpointConnectionNotificationsResponse = DeleteVPCEndpointConnectionNotificationsResponse'
  { unsuccessful ::
      Lude.Maybe
        [UnsuccessfulItem],
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

-- | Creates a value of 'DeleteVPCEndpointConnectionNotificationsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'unsuccessful' - Information about the notifications that could not be deleted successfully.
mkDeleteVPCEndpointConnectionNotificationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteVPCEndpointConnectionNotificationsResponse
mkDeleteVPCEndpointConnectionNotificationsResponse pResponseStatus_ =
  DeleteVPCEndpointConnectionNotificationsResponse'
    { unsuccessful =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the notifications that could not be deleted successfully.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnrsUnsuccessful :: Lens.Lens' DeleteVPCEndpointConnectionNotificationsResponse (Lude.Maybe [UnsuccessfulItem])
dvecnrsUnsuccessful = Lens.lens (unsuccessful :: DeleteVPCEndpointConnectionNotificationsResponse -> Lude.Maybe [UnsuccessfulItem]) (\s a -> s {unsuccessful = a} :: DeleteVPCEndpointConnectionNotificationsResponse)
{-# DEPRECATED dvecnrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnrsResponseStatus :: Lens.Lens' DeleteVPCEndpointConnectionNotificationsResponse Lude.Int
dvecnrsResponseStatus = Lens.lens (responseStatus :: DeleteVPCEndpointConnectionNotificationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteVPCEndpointConnectionNotificationsResponse)
{-# DEPRECATED dvecnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
