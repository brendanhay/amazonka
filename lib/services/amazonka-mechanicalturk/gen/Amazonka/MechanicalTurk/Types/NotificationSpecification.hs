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
-- Module      : Amazonka.MechanicalTurk.Types.NotificationSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.NotificationSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types.EventType
import Amazonka.MechanicalTurk.Types.NotificationTransport
import qualified Amazonka.Prelude as Prelude

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
    destination :: Prelude.Text,
    -- | The method Amazon Mechanical Turk uses to send the notification. Valid
    -- Values: Email | SQS | SNS.
    transport :: NotificationTransport,
    -- | The version of the Notification API to use. Valid value is 2006-05-05.
    version :: Prelude.Text,
    -- | The list of events that should cause notifications to be sent. Valid
    -- Values: AssignmentAccepted | AssignmentAbandoned | AssignmentReturned |
    -- AssignmentSubmitted | AssignmentRejected | AssignmentApproved |
    -- HITCreated | HITExtended | HITDisposed | HITReviewable | HITExpired |
    -- Ping. The Ping event is only valid for the SendTestEventNotification
    -- operation.
    eventTypes :: [EventType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'transport'
  NotificationTransport ->
  -- | 'version'
  Prelude.Text ->
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
        eventTypes = Prelude.mempty
      }

-- | The target for notification messages. The Destination’s format is
-- determined by the specified Transport:
--
-- -   When Transport is Email, the Destination is your email address.
--
-- -   When Transport is SQS, the Destination is your queue URL.
--
-- -   When Transport is SNS, the Destination is the ARN of your topic.
notificationSpecification_destination :: Lens.Lens' NotificationSpecification Prelude.Text
notificationSpecification_destination = Lens.lens (\NotificationSpecification' {destination} -> destination) (\s@NotificationSpecification' {} a -> s {destination = a} :: NotificationSpecification)

-- | The method Amazon Mechanical Turk uses to send the notification. Valid
-- Values: Email | SQS | SNS.
notificationSpecification_transport :: Lens.Lens' NotificationSpecification NotificationTransport
notificationSpecification_transport = Lens.lens (\NotificationSpecification' {transport} -> transport) (\s@NotificationSpecification' {} a -> s {transport = a} :: NotificationSpecification)

-- | The version of the Notification API to use. Valid value is 2006-05-05.
notificationSpecification_version :: Lens.Lens' NotificationSpecification Prelude.Text
notificationSpecification_version = Lens.lens (\NotificationSpecification' {version} -> version) (\s@NotificationSpecification' {} a -> s {version = a} :: NotificationSpecification)

-- | The list of events that should cause notifications to be sent. Valid
-- Values: AssignmentAccepted | AssignmentAbandoned | AssignmentReturned |
-- AssignmentSubmitted | AssignmentRejected | AssignmentApproved |
-- HITCreated | HITExtended | HITDisposed | HITReviewable | HITExpired |
-- Ping. The Ping event is only valid for the SendTestEventNotification
-- operation.
notificationSpecification_eventTypes :: Lens.Lens' NotificationSpecification [EventType]
notificationSpecification_eventTypes = Lens.lens (\NotificationSpecification' {eventTypes} -> eventTypes) (\s@NotificationSpecification' {} a -> s {eventTypes = a} :: NotificationSpecification) Prelude.. Lens.coerced

instance Prelude.Hashable NotificationSpecification where
  hashWithSalt _salt NotificationSpecification' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` transport
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` eventTypes

instance Prelude.NFData NotificationSpecification where
  rnf NotificationSpecification' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf transport
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf eventTypes

instance Data.ToJSON NotificationSpecification where
  toJSON NotificationSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Destination" Data..= destination),
            Prelude.Just ("Transport" Data..= transport),
            Prelude.Just ("Version" Data..= version),
            Prelude.Just ("EventTypes" Data..= eventTypes)
          ]
      )
