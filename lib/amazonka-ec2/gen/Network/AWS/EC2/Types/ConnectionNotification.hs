-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConnectionNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConnectionNotification
  ( ConnectionNotification (..),

    -- * Smart constructor
    mkConnectionNotification,

    -- * Lenses
    cnConnectionNotificationState,
    cnConnectionNotificationType,
    cnConnectionEvents,
    cnServiceId,
    cnVPCEndpointId,
    cnConnectionNotificationId,
    cnConnectionNotificationARN,
  )
where

import Network.AWS.EC2.Types.ConnectionNotificationState
import Network.AWS.EC2.Types.ConnectionNotificationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a connection notification for a VPC endpoint or VPC endpoint service.
--
-- /See:/ 'mkConnectionNotification' smart constructor.
data ConnectionNotification = ConnectionNotification'
  { connectionNotificationState ::
      Lude.Maybe ConnectionNotificationState,
    connectionNotificationType ::
      Lude.Maybe ConnectionNotificationType,
    connectionEvents :: Lude.Maybe [Lude.Text],
    serviceId :: Lude.Maybe Lude.Text,
    vpcEndpointId :: Lude.Maybe Lude.Text,
    connectionNotificationId ::
      Lude.Maybe Lude.Text,
    connectionNotificationARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectionNotification' with the minimum fields required to make a request.
--
-- * 'connectionEvents' - The events for the notification. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
-- * 'connectionNotificationARN' - The ARN of the SNS topic for the notification.
-- * 'connectionNotificationId' - The ID of the notification.
-- * 'connectionNotificationState' - The state of the notification.
-- * 'connectionNotificationType' - The type of notification.
-- * 'serviceId' - The ID of the endpoint service.
-- * 'vpcEndpointId' - The ID of the VPC endpoint.
mkConnectionNotification ::
  ConnectionNotification
mkConnectionNotification =
  ConnectionNotification'
    { connectionNotificationState =
        Lude.Nothing,
      connectionNotificationType = Lude.Nothing,
      connectionEvents = Lude.Nothing,
      serviceId = Lude.Nothing,
      vpcEndpointId = Lude.Nothing,
      connectionNotificationId = Lude.Nothing,
      connectionNotificationARN = Lude.Nothing
    }

-- | The state of the notification.
--
-- /Note:/ Consider using 'connectionNotificationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnConnectionNotificationState :: Lens.Lens' ConnectionNotification (Lude.Maybe ConnectionNotificationState)
cnConnectionNotificationState = Lens.lens (connectionNotificationState :: ConnectionNotification -> Lude.Maybe ConnectionNotificationState) (\s a -> s {connectionNotificationState = a} :: ConnectionNotification)
{-# DEPRECATED cnConnectionNotificationState "Use generic-lens or generic-optics with 'connectionNotificationState' instead." #-}

-- | The type of notification.
--
-- /Note:/ Consider using 'connectionNotificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnConnectionNotificationType :: Lens.Lens' ConnectionNotification (Lude.Maybe ConnectionNotificationType)
cnConnectionNotificationType = Lens.lens (connectionNotificationType :: ConnectionNotification -> Lude.Maybe ConnectionNotificationType) (\s a -> s {connectionNotificationType = a} :: ConnectionNotification)
{-# DEPRECATED cnConnectionNotificationType "Use generic-lens or generic-optics with 'connectionNotificationType' instead." #-}

-- | The events for the notification. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
--
-- /Note:/ Consider using 'connectionEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnConnectionEvents :: Lens.Lens' ConnectionNotification (Lude.Maybe [Lude.Text])
cnConnectionEvents = Lens.lens (connectionEvents :: ConnectionNotification -> Lude.Maybe [Lude.Text]) (\s a -> s {connectionEvents = a} :: ConnectionNotification)
{-# DEPRECATED cnConnectionEvents "Use generic-lens or generic-optics with 'connectionEvents' instead." #-}

-- | The ID of the endpoint service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnServiceId :: Lens.Lens' ConnectionNotification (Lude.Maybe Lude.Text)
cnServiceId = Lens.lens (serviceId :: ConnectionNotification -> Lude.Maybe Lude.Text) (\s a -> s {serviceId = a} :: ConnectionNotification)
{-# DEPRECATED cnServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

-- | The ID of the VPC endpoint.
--
-- /Note:/ Consider using 'vpcEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnVPCEndpointId :: Lens.Lens' ConnectionNotification (Lude.Maybe Lude.Text)
cnVPCEndpointId = Lens.lens (vpcEndpointId :: ConnectionNotification -> Lude.Maybe Lude.Text) (\s a -> s {vpcEndpointId = a} :: ConnectionNotification)
{-# DEPRECATED cnVPCEndpointId "Use generic-lens or generic-optics with 'vpcEndpointId' instead." #-}

-- | The ID of the notification.
--
-- /Note:/ Consider using 'connectionNotificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnConnectionNotificationId :: Lens.Lens' ConnectionNotification (Lude.Maybe Lude.Text)
cnConnectionNotificationId = Lens.lens (connectionNotificationId :: ConnectionNotification -> Lude.Maybe Lude.Text) (\s a -> s {connectionNotificationId = a} :: ConnectionNotification)
{-# DEPRECATED cnConnectionNotificationId "Use generic-lens or generic-optics with 'connectionNotificationId' instead." #-}

-- | The ARN of the SNS topic for the notification.
--
-- /Note:/ Consider using 'connectionNotificationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnConnectionNotificationARN :: Lens.Lens' ConnectionNotification (Lude.Maybe Lude.Text)
cnConnectionNotificationARN = Lens.lens (connectionNotificationARN :: ConnectionNotification -> Lude.Maybe Lude.Text) (\s a -> s {connectionNotificationARN = a} :: ConnectionNotification)
{-# DEPRECATED cnConnectionNotificationARN "Use generic-lens or generic-optics with 'connectionNotificationARN' instead." #-}

instance Lude.FromXML ConnectionNotification where
  parseXML x =
    ConnectionNotification'
      Lude.<$> (x Lude..@? "connectionNotificationState")
      Lude.<*> (x Lude..@? "connectionNotificationType")
      Lude.<*> ( x Lude..@? "connectionEvents" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "serviceId")
      Lude.<*> (x Lude..@? "vpcEndpointId")
      Lude.<*> (x Lude..@? "connectionNotificationId")
      Lude.<*> (x Lude..@? "connectionNotificationArn")
