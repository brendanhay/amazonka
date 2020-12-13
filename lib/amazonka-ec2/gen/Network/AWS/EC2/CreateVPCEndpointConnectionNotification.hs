{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVPCEndpointConnectionNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection notification for a specified VPC endpoint or VPC endpoint service. A connection notification notifies you of specific endpoint events. You must create an SNS topic to receive notifications. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Create a Topic> in the /Amazon Simple Notification Service Developer Guide/ .
--
-- You can create a connection notification for interface endpoints only.
module Network.AWS.EC2.CreateVPCEndpointConnectionNotification
  ( -- * Creating a request
    CreateVPCEndpointConnectionNotification (..),
    mkCreateVPCEndpointConnectionNotification,

    -- ** Request lenses
    cvecnClientToken,
    cvecnConnectionEvents,
    cvecnServiceId,
    cvecnVPCEndpointId,
    cvecnConnectionNotificationARN,
    cvecnDryRun,

    -- * Destructuring the response
    CreateVPCEndpointConnectionNotificationResponse (..),
    mkCreateVPCEndpointConnectionNotificationResponse,

    -- ** Response lenses
    cvecnrsClientToken,
    cvecnrsConnectionNotification,
    cvecnrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateVPCEndpointConnectionNotification' smart constructor.
data CreateVPCEndpointConnectionNotification = CreateVPCEndpointConnectionNotification'
  { -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Lude.Maybe Lude.Text,
    -- | One or more endpoint events for which to receive notifications. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
    connectionEvents :: [Lude.Text],
    -- | The ID of the endpoint service.
    serviceId :: Lude.Maybe Lude.Text,
    -- | The ID of the endpoint.
    vpcEndpointId :: Lude.Maybe Lude.Text,
    -- | The ARN of the SNS topic for the notifications.
    connectionNotificationARN :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCEndpointConnectionNotification' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'connectionEvents' - One or more endpoint events for which to receive notifications. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
-- * 'serviceId' - The ID of the endpoint service.
-- * 'vpcEndpointId' - The ID of the endpoint.
-- * 'connectionNotificationARN' - The ARN of the SNS topic for the notifications.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateVPCEndpointConnectionNotification ::
  -- | 'connectionNotificationARN'
  Lude.Text ->
  CreateVPCEndpointConnectionNotification
mkCreateVPCEndpointConnectionNotification
  pConnectionNotificationARN_ =
    CreateVPCEndpointConnectionNotification'
      { clientToken =
          Lude.Nothing,
        connectionEvents = Lude.mempty,
        serviceId = Lude.Nothing,
        vpcEndpointId = Lude.Nothing,
        connectionNotificationARN =
          pConnectionNotificationARN_,
        dryRun = Lude.Nothing
      }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnClientToken :: Lens.Lens' CreateVPCEndpointConnectionNotification (Lude.Maybe Lude.Text)
cvecnClientToken = Lens.lens (clientToken :: CreateVPCEndpointConnectionNotification -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateVPCEndpointConnectionNotification)
{-# DEPRECATED cvecnClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | One or more endpoint events for which to receive notifications. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
--
-- /Note:/ Consider using 'connectionEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnConnectionEvents :: Lens.Lens' CreateVPCEndpointConnectionNotification [Lude.Text]
cvecnConnectionEvents = Lens.lens (connectionEvents :: CreateVPCEndpointConnectionNotification -> [Lude.Text]) (\s a -> s {connectionEvents = a} :: CreateVPCEndpointConnectionNotification)
{-# DEPRECATED cvecnConnectionEvents "Use generic-lens or generic-optics with 'connectionEvents' instead." #-}

-- | The ID of the endpoint service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnServiceId :: Lens.Lens' CreateVPCEndpointConnectionNotification (Lude.Maybe Lude.Text)
cvecnServiceId = Lens.lens (serviceId :: CreateVPCEndpointConnectionNotification -> Lude.Maybe Lude.Text) (\s a -> s {serviceId = a} :: CreateVPCEndpointConnectionNotification)
{-# DEPRECATED cvecnServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | The ID of the endpoint.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnVPCEndpointId :: Lens.Lens' CreateVPCEndpointConnectionNotification (Lude.Maybe Lude.Text)
cvecnVPCEndpointId = Lens.lens (vpcEndpointId :: CreateVPCEndpointConnectionNotification -> Lude.Maybe Lude.Text) (\s a -> s {vpcEndpointId = a} :: CreateVPCEndpointConnectionNotification)
{-# DEPRECATED cvecnVPCEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead." #-}

-- | The ARN of the SNS topic for the notifications.
--
-- /Note:/ Consider using 'connectionNotificationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnConnectionNotificationARN :: Lens.Lens' CreateVPCEndpointConnectionNotification Lude.Text
cvecnConnectionNotificationARN = Lens.lens (connectionNotificationARN :: CreateVPCEndpointConnectionNotification -> Lude.Text) (\s a -> s {connectionNotificationARN = a} :: CreateVPCEndpointConnectionNotification)
{-# DEPRECATED cvecnConnectionNotificationARN "Use generic-lens or generic-optics with 'connectionNotificationARN' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnDryRun :: Lens.Lens' CreateVPCEndpointConnectionNotification (Lude.Maybe Lude.Bool)
cvecnDryRun = Lens.lens (dryRun :: CreateVPCEndpointConnectionNotification -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateVPCEndpointConnectionNotification)
{-# DEPRECATED cvecnDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateVPCEndpointConnectionNotification where
  type
    Rs CreateVPCEndpointConnectionNotification =
      CreateVPCEndpointConnectionNotificationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateVPCEndpointConnectionNotificationResponse'
            Lude.<$> (x Lude..@? "clientToken")
            Lude.<*> (x Lude..@? "connectionNotification")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateVPCEndpointConnectionNotification where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateVPCEndpointConnectionNotification where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVPCEndpointConnectionNotification where
  toQuery CreateVPCEndpointConnectionNotification' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateVpcEndpointConnectionNotification" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        Lude.toQueryList "ConnectionEvents" connectionEvents,
        "ServiceId" Lude.=: serviceId,
        "VpcEndpointId" Lude.=: vpcEndpointId,
        "ConnectionNotificationArn" Lude.=: connectionNotificationARN,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateVPCEndpointConnectionNotificationResponse' smart constructor.
data CreateVPCEndpointConnectionNotificationResponse = CreateVPCEndpointConnectionNotificationResponse'
  { -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
    clientToken :: Lude.Maybe Lude.Text,
    -- | Information about the notification.
    connectionNotification :: Lude.Maybe ConnectionNotification,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCEndpointConnectionNotificationResponse' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
-- * 'connectionNotification' - Information about the notification.
-- * 'responseStatus' - The response status code.
mkCreateVPCEndpointConnectionNotificationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateVPCEndpointConnectionNotificationResponse
mkCreateVPCEndpointConnectionNotificationResponse pResponseStatus_ =
  CreateVPCEndpointConnectionNotificationResponse'
    { clientToken =
        Lude.Nothing,
      connectionNotification = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnrsClientToken :: Lens.Lens' CreateVPCEndpointConnectionNotificationResponse (Lude.Maybe Lude.Text)
cvecnrsClientToken = Lens.lens (clientToken :: CreateVPCEndpointConnectionNotificationResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateVPCEndpointConnectionNotificationResponse)
{-# DEPRECATED cvecnrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Information about the notification.
--
-- /Note:/ Consider using 'connectionNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnrsConnectionNotification :: Lens.Lens' CreateVPCEndpointConnectionNotificationResponse (Lude.Maybe ConnectionNotification)
cvecnrsConnectionNotification = Lens.lens (connectionNotification :: CreateVPCEndpointConnectionNotificationResponse -> Lude.Maybe ConnectionNotification) (\s a -> s {connectionNotification = a} :: CreateVPCEndpointConnectionNotificationResponse)
{-# DEPRECATED cvecnrsConnectionNotification "Use generic-lens or generic-optics with 'connectionNotification' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvecnrsResponseStatus :: Lens.Lens' CreateVPCEndpointConnectionNotificationResponse Lude.Int
cvecnrsResponseStatus = Lens.lens (responseStatus :: CreateVPCEndpointConnectionNotificationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVPCEndpointConnectionNotificationResponse)
{-# DEPRECATED cvecnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
