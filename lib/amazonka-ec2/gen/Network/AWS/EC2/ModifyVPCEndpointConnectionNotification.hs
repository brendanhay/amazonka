{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVPCEndpointConnectionNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a connection notification for VPC endpoint or VPC endpoint service. You can change the SNS topic for the notification, or the events for which to be notified.
module Network.AWS.EC2.ModifyVPCEndpointConnectionNotification
  ( -- * Creating a request
    ModifyVPCEndpointConnectionNotification (..),
    mkModifyVPCEndpointConnectionNotification,

    -- ** Request lenses
    mvecnConnectionEvents,
    mvecnConnectionNotificationId,
    mvecnConnectionNotificationARN,
    mvecnDryRun,

    -- * Destructuring the response
    ModifyVPCEndpointConnectionNotificationResponse (..),
    mkModifyVPCEndpointConnectionNotificationResponse,

    -- ** Response lenses
    mvecnrsReturnValue,
    mvecnrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyVPCEndpointConnectionNotification' smart constructor.
data ModifyVPCEndpointConnectionNotification = ModifyVPCEndpointConnectionNotification'
  { -- | One or more events for the endpoint. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
    connectionEvents :: Lude.Maybe [Lude.Text],
    -- | The ID of the notification.
    connectionNotificationId :: Lude.Text,
    -- | The ARN for the SNS topic for the notification.
    connectionNotificationARN :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPCEndpointConnectionNotification' with the minimum fields required to make a request.
--
-- * 'connectionEvents' - One or more events for the endpoint. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
-- * 'connectionNotificationId' - The ID of the notification.
-- * 'connectionNotificationARN' - The ARN for the SNS topic for the notification.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyVPCEndpointConnectionNotification ::
  -- | 'connectionNotificationId'
  Lude.Text ->
  ModifyVPCEndpointConnectionNotification
mkModifyVPCEndpointConnectionNotification
  pConnectionNotificationId_ =
    ModifyVPCEndpointConnectionNotification'
      { connectionEvents =
          Lude.Nothing,
        connectionNotificationId = pConnectionNotificationId_,
        connectionNotificationARN = Lude.Nothing,
        dryRun = Lude.Nothing
      }

-- | One or more events for the endpoint. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
--
-- /Note:/ Consider using 'connectionEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvecnConnectionEvents :: Lens.Lens' ModifyVPCEndpointConnectionNotification (Lude.Maybe [Lude.Text])
mvecnConnectionEvents = Lens.lens (connectionEvents :: ModifyVPCEndpointConnectionNotification -> Lude.Maybe [Lude.Text]) (\s a -> s {connectionEvents = a} :: ModifyVPCEndpointConnectionNotification)
{-# DEPRECATED mvecnConnectionEvents "Use generic-lens or generic-optics with 'connectionEvents' instead." #-}

-- | The ID of the notification.
--
-- /Note:/ Consider using 'connectionNotificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvecnConnectionNotificationId :: Lens.Lens' ModifyVPCEndpointConnectionNotification Lude.Text
mvecnConnectionNotificationId = Lens.lens (connectionNotificationId :: ModifyVPCEndpointConnectionNotification -> Lude.Text) (\s a -> s {connectionNotificationId = a} :: ModifyVPCEndpointConnectionNotification)
{-# DEPRECATED mvecnConnectionNotificationId "Use generic-lens or generic-optics with 'connectionNotificationId' instead." #-}

-- | The ARN for the SNS topic for the notification.
--
-- /Note:/ Consider using 'connectionNotificationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvecnConnectionNotificationARN :: Lens.Lens' ModifyVPCEndpointConnectionNotification (Lude.Maybe Lude.Text)
mvecnConnectionNotificationARN = Lens.lens (connectionNotificationARN :: ModifyVPCEndpointConnectionNotification -> Lude.Maybe Lude.Text) (\s a -> s {connectionNotificationARN = a} :: ModifyVPCEndpointConnectionNotification)
{-# DEPRECATED mvecnConnectionNotificationARN "Use generic-lens or generic-optics with 'connectionNotificationARN' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvecnDryRun :: Lens.Lens' ModifyVPCEndpointConnectionNotification (Lude.Maybe Lude.Bool)
mvecnDryRun = Lens.lens (dryRun :: ModifyVPCEndpointConnectionNotification -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyVPCEndpointConnectionNotification)
{-# DEPRECATED mvecnDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyVPCEndpointConnectionNotification where
  type
    Rs ModifyVPCEndpointConnectionNotification =
      ModifyVPCEndpointConnectionNotificationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyVPCEndpointConnectionNotificationResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyVPCEndpointConnectionNotification where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyVPCEndpointConnectionNotification where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyVPCEndpointConnectionNotification where
  toQuery ModifyVPCEndpointConnectionNotification' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyVpcEndpointConnectionNotification" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "ConnectionEvents" Lude.<$> connectionEvents),
        "ConnectionNotificationId" Lude.=: connectionNotificationId,
        "ConnectionNotificationArn" Lude.=: connectionNotificationARN,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyVPCEndpointConnectionNotificationResponse' smart constructor.
data ModifyVPCEndpointConnectionNotificationResponse = ModifyVPCEndpointConnectionNotificationResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    returnValue :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPCEndpointConnectionNotificationResponse' with the minimum fields required to make a request.
--
-- * 'returnValue' - Returns @true@ if the request succeeds; otherwise, it returns an error.
-- * 'responseStatus' - The response status code.
mkModifyVPCEndpointConnectionNotificationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyVPCEndpointConnectionNotificationResponse
mkModifyVPCEndpointConnectionNotificationResponse pResponseStatus_ =
  ModifyVPCEndpointConnectionNotificationResponse'
    { returnValue =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'returnValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvecnrsReturnValue :: Lens.Lens' ModifyVPCEndpointConnectionNotificationResponse (Lude.Maybe Lude.Bool)
mvecnrsReturnValue = Lens.lens (returnValue :: ModifyVPCEndpointConnectionNotificationResponse -> Lude.Maybe Lude.Bool) (\s a -> s {returnValue = a} :: ModifyVPCEndpointConnectionNotificationResponse)
{-# DEPRECATED mvecnrsReturnValue "Use generic-lens or generic-optics with 'returnValue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvecnrsResponseStatus :: Lens.Lens' ModifyVPCEndpointConnectionNotificationResponse Lude.Int
mvecnrsResponseStatus = Lens.lens (responseStatus :: ModifyVPCEndpointConnectionNotificationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyVPCEndpointConnectionNotificationResponse)
{-# DEPRECATED mvecnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
