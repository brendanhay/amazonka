-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.NotificationSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.NotificationSpecification
  ( NotificationSpecification (..),

    -- * Smart constructor
    mkNotificationSpecification,

    -- * Lenses
    nsDestination,
    nsTransport,
    nsVersion,
    nsEventTypes,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.EventType
import Network.AWS.MechanicalTurk.Types.NotificationTransport
import qualified Network.AWS.Prelude as Lude

-- | The NotificationSpecification data structure describes a HIT event notification for a HIT type.
--
-- /See:/ 'mkNotificationSpecification' smart constructor.
data NotificationSpecification = NotificationSpecification'
  { destination ::
      Lude.Text,
    transport :: NotificationTransport,
    version :: Lude.Text,
    eventTypes :: [EventType]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotificationSpecification' with the minimum fields required to make a request.
--
-- * 'destination' - The target for notification messages. The Destination’s format is determined by the specified Transport:
--
--
--     * When Transport is Email, the Destination is your email address.
--
--
--     * When Transport is SQS, the Destination is your queue URL.
--
--
--     * When Transport is SNS, the Destination is the ARN of your topic.
--
--
-- * 'eventTypes' - The list of events that should cause notifications to be sent. Valid Values: AssignmentAccepted | AssignmentAbandoned | AssignmentReturned | AssignmentSubmitted | AssignmentRejected | AssignmentApproved | HITCreated | HITExtended | HITDisposed | HITReviewable | HITExpired | Ping. The Ping event is only valid for the SendTestEventNotification operation.
-- * 'transport' - The method Amazon Mechanical Turk uses to send the notification. Valid Values: Email | SQS | SNS.
-- * 'version' - The version of the Notification API to use. Valid value is 2006-05-05.
mkNotificationSpecification ::
  -- | 'destination'
  Lude.Text ->
  -- | 'transport'
  NotificationTransport ->
  -- | 'version'
  Lude.Text ->
  NotificationSpecification
mkNotificationSpecification pDestination_ pTransport_ pVersion_ =
  NotificationSpecification'
    { destination = pDestination_,
      transport = pTransport_,
      version = pVersion_,
      eventTypes = Lude.mempty
    }

-- | The target for notification messages. The Destination’s format is determined by the specified Transport:
--
--
--     * When Transport is Email, the Destination is your email address.
--
--
--     * When Transport is SQS, the Destination is your queue URL.
--
--
--     * When Transport is SNS, the Destination is the ARN of your topic.
--
--
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsDestination :: Lens.Lens' NotificationSpecification Lude.Text
nsDestination = Lens.lens (destination :: NotificationSpecification -> Lude.Text) (\s a -> s {destination = a} :: NotificationSpecification)
{-# DEPRECATED nsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The method Amazon Mechanical Turk uses to send the notification. Valid Values: Email | SQS | SNS.
--
-- /Note:/ Consider using 'transport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsTransport :: Lens.Lens' NotificationSpecification NotificationTransport
nsTransport = Lens.lens (transport :: NotificationSpecification -> NotificationTransport) (\s a -> s {transport = a} :: NotificationSpecification)
{-# DEPRECATED nsTransport "Use generic-lens or generic-optics with 'transport' instead." #-}

-- | The version of the Notification API to use. Valid value is 2006-05-05.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsVersion :: Lens.Lens' NotificationSpecification Lude.Text
nsVersion = Lens.lens (version :: NotificationSpecification -> Lude.Text) (\s a -> s {version = a} :: NotificationSpecification)
{-# DEPRECATED nsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The list of events that should cause notifications to be sent. Valid Values: AssignmentAccepted | AssignmentAbandoned | AssignmentReturned | AssignmentSubmitted | AssignmentRejected | AssignmentApproved | HITCreated | HITExtended | HITDisposed | HITReviewable | HITExpired | Ping. The Ping event is only valid for the SendTestEventNotification operation.
--
-- /Note:/ Consider using 'eventTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsEventTypes :: Lens.Lens' NotificationSpecification [EventType]
nsEventTypes = Lens.lens (eventTypes :: NotificationSpecification -> [EventType]) (\s a -> s {eventTypes = a} :: NotificationSpecification)
{-# DEPRECATED nsEventTypes "Use generic-lens or generic-optics with 'eventTypes' instead." #-}

instance Lude.ToJSON NotificationSpecification where
  toJSON NotificationSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Destination" Lude..= destination),
            Lude.Just ("Transport" Lude..= transport),
            Lude.Just ("Version" Lude..= version),
            Lude.Just ("EventTypes" Lude..= eventTypes)
          ]
      )
