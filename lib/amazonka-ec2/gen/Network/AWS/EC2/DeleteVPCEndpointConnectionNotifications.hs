{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dvpcecnConnectionNotificationIds,
    dvpcecnDryRun,

    -- * Destructuring the response
    DeleteVPCEndpointConnectionNotificationsResponse (..),
    mkDeleteVPCEndpointConnectionNotificationsResponse,

    -- ** Response lenses
    dvpcecnrsUnsuccessful,
    dvpcecnrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteVPCEndpointConnectionNotifications' smart constructor.
data DeleteVPCEndpointConnectionNotifications = DeleteVPCEndpointConnectionNotifications'
  { -- | One or more notification IDs.
    connectionNotificationIds :: [Lude.Text],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCEndpointConnectionNotifications' with the minimum fields required to make a request.
--
-- * 'connectionNotificationIds' - One or more notification IDs.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteVPCEndpointConnectionNotifications ::
  DeleteVPCEndpointConnectionNotifications
mkDeleteVPCEndpointConnectionNotifications =
  DeleteVPCEndpointConnectionNotifications'
    { connectionNotificationIds =
        Lude.mempty,
      dryRun = Lude.Nothing
    }

-- | One or more notification IDs.
--
-- /Note:/ Consider using 'connectionNotificationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecnConnectionNotificationIds :: Lens.Lens' DeleteVPCEndpointConnectionNotifications [Lude.Text]
dvpcecnConnectionNotificationIds = Lens.lens (connectionNotificationIds :: DeleteVPCEndpointConnectionNotifications -> [Lude.Text]) (\s a -> s {connectionNotificationIds = a} :: DeleteVPCEndpointConnectionNotifications)
{-# DEPRECATED dvpcecnConnectionNotificationIds "Use generic-lens or generic-optics with 'connectionNotificationIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecnDryRun :: Lens.Lens' DeleteVPCEndpointConnectionNotifications (Lude.Maybe Lude.Bool)
dvpcecnDryRun = Lens.lens (dryRun :: DeleteVPCEndpointConnectionNotifications -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteVPCEndpointConnectionNotifications)
{-# DEPRECATED dvpcecnDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
        Lude.toQueryList
          "ConnectionNotificationId"
          connectionNotificationIds,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteVPCEndpointConnectionNotificationsResponse' smart constructor.
data DeleteVPCEndpointConnectionNotificationsResponse = DeleteVPCEndpointConnectionNotificationsResponse'
  { -- | Information about the notifications that could not be deleted successfully.
    unsuccessful :: Lude.Maybe [UnsuccessfulItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCEndpointConnectionNotificationsResponse' with the minimum fields required to make a request.
--
-- * 'unsuccessful' - Information about the notifications that could not be deleted successfully.
-- * 'responseStatus' - The response status code.
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
dvpcecnrsUnsuccessful :: Lens.Lens' DeleteVPCEndpointConnectionNotificationsResponse (Lude.Maybe [UnsuccessfulItem])
dvpcecnrsUnsuccessful = Lens.lens (unsuccessful :: DeleteVPCEndpointConnectionNotificationsResponse -> Lude.Maybe [UnsuccessfulItem]) (\s a -> s {unsuccessful = a} :: DeleteVPCEndpointConnectionNotificationsResponse)
{-# DEPRECATED dvpcecnrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcecnrsResponseStatus :: Lens.Lens' DeleteVPCEndpointConnectionNotificationsResponse Lude.Int
dvpcecnrsResponseStatus = Lens.lens (responseStatus :: DeleteVPCEndpointConnectionNotificationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteVPCEndpointConnectionNotificationsResponse)
{-# DEPRECATED dvpcecnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
