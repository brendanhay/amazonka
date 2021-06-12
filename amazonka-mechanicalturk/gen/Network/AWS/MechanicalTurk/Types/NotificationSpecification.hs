{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.NotificationSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.NotificationSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types.EventType
import Network.AWS.MechanicalTurk.Types.NotificationTransport

-- | The NotificationSpecification data structure describes a HIT event
-- notification for a HIT type.
--
-- /See:/ 'newNotificationSpecification' smart constructor.
data NotificationSpecification = NotificationSpecification'
  { -- | The target for notification messages. The Destination’s format is
    -- determined by the specified Transport:
    --
    -- -   When Transport is Email, the Destination is your email address.
    --
    -- -   When Transport is SQS, the Destination is your queue URL.
    --
    -- -   When Transport is SNS, the Destination is the ARN of your topic.
    destination :: Core.Text,
    -- | The method Amazon Mechanical Turk uses to send the notification. Valid
    -- Values: Email | SQS | SNS.
    transport :: NotificationTransport,
    -- | The version of the Notification API to use. Valid value is 2006-05-05.
    version :: Core.Text,
    -- | The list of events that should cause notifications to be sent. Valid
    -- Values: AssignmentAccepted | AssignmentAbandoned | AssignmentReturned |
    -- AssignmentSubmitted | AssignmentRejected | AssignmentApproved |
    -- HITCreated | HITExtended | HITDisposed | HITReviewable | HITExpired |
    -- Ping. The Ping event is only valid for the SendTestEventNotification
    -- operation.
    eventTypes :: [EventType]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NotificationSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'notificationSpecification_destination' - The target for notification messages. The Destination’s format is
-- determined by the specified Transport:
--
-- -   When Transport is Email, the Destination is your email address.
--
-- -   When Transport is SQS, the Destination is your queue URL.
--
-- -   When Transport is SNS, the Destination is the ARN of your topic.
--
-- 'transport', 'notificationSpecification_transport' - The method Amazon Mechanical Turk uses to send the notification. Valid
-- Values: Email | SQS | SNS.
--
-- 'version', 'notificationSpecification_version' - The version of the Notification API to use. Valid value is 2006-05-05.
--
-- 'eventTypes', 'notificationSpecification_eventTypes' - The list of events that should cause notifications to be sent. Valid
-- Values: AssignmentAccepted | AssignmentAbandoned | AssignmentReturned |
-- AssignmentSubmitted | AssignmentRejected | AssignmentApproved |
-- HITCreated | HITExtended | HITDisposed | HITReviewable | HITExpired |
-- Ping. The Ping event is only valid for the SendTestEventNotification
-- operation.
newNotificationSpecification ::
  -- | 'destination'
  Core.Text ->
  -- | 'transport'
  NotificationTransport ->
  -- | 'version'
  Core.Text ->
  NotificationSpecification
newNotificationSpecification
  pDestination_
  pTransport_
  pVersion_ =
    NotificationSpecification'
      { destination =
          pDestination_,
        transport = pTransport_,
        version = pVersion_,
        eventTypes = Core.mempty
      }

-- | The target for notification messages. The Destination’s format is
-- determined by the specified Transport:
--
-- -   When Transport is Email, the Destination is your email address.
--
-- -   When Transport is SQS, the Destination is your queue URL.
--
-- -   When Transport is SNS, the Destination is the ARN of your topic.
notificationSpecification_destination :: Lens.Lens' NotificationSpecification Core.Text
notificationSpecification_destination = Lens.lens (\NotificationSpecification' {destination} -> destination) (\s@NotificationSpecification' {} a -> s {destination = a} :: NotificationSpecification)

-- | The method Amazon Mechanical Turk uses to send the notification. Valid
-- Values: Email | SQS | SNS.
notificationSpecification_transport :: Lens.Lens' NotificationSpecification NotificationTransport
notificationSpecification_transport = Lens.lens (\NotificationSpecification' {transport} -> transport) (\s@NotificationSpecification' {} a -> s {transport = a} :: NotificationSpecification)

-- | The version of the Notification API to use. Valid value is 2006-05-05.
notificationSpecification_version :: Lens.Lens' NotificationSpecification Core.Text
notificationSpecification_version = Lens.lens (\NotificationSpecification' {version} -> version) (\s@NotificationSpecification' {} a -> s {version = a} :: NotificationSpecification)

-- | The list of events that should cause notifications to be sent. Valid
-- Values: AssignmentAccepted | AssignmentAbandoned | AssignmentReturned |
-- AssignmentSubmitted | AssignmentRejected | AssignmentApproved |
-- HITCreated | HITExtended | HITDisposed | HITReviewable | HITExpired |
-- Ping. The Ping event is only valid for the SendTestEventNotification
-- operation.
notificationSpecification_eventTypes :: Lens.Lens' NotificationSpecification [EventType]
notificationSpecification_eventTypes = Lens.lens (\NotificationSpecification' {eventTypes} -> eventTypes) (\s@NotificationSpecification' {} a -> s {eventTypes = a} :: NotificationSpecification) Core.. Lens._Coerce

instance Core.Hashable NotificationSpecification

instance Core.NFData NotificationSpecification

instance Core.ToJSON NotificationSpecification where
  toJSON NotificationSpecification' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Destination" Core..= destination),
            Core.Just ("Transport" Core..= transport),
            Core.Just ("Version" Core..= version),
            Core.Just ("EventTypes" Core..= eventTypes)
          ]
      )
