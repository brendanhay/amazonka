{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.NotificationSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.NotificationSpecification where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types.EventType
import Network.AWS.MechanicalTurk.Types.NotificationTransport
import Network.AWS.Prelude

-- | The NotificationSpecification data structure describes a HIT event notification for a HIT type.
--
--
--
-- /See:/ 'notificationSpecification' smart constructor.
data NotificationSpecification = NotificationSpecification'
  { _nsDestination ::
      !Text,
    _nsTransport :: !NotificationTransport,
    _nsVersion :: !Text,
    _nsEventTypes :: ![EventType]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotificationSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nsDestination' - The target for notification messages. The Destination’s format is determined by the specified Transport:      * When Transport is Email, the Destination is your email address.     * When Transport is SQS, the Destination is your queue URL.     * When Transport is SNS, the Destination is the ARN of your topic.
--
-- * 'nsTransport' - The method Amazon Mechanical Turk uses to send the notification. Valid Values: Email | SQS | SNS.
--
-- * 'nsVersion' - The version of the Notification API to use. Valid value is 2006-05-05.
--
-- * 'nsEventTypes' - The list of events that should cause notifications to be sent. Valid Values: AssignmentAccepted | AssignmentAbandoned | AssignmentReturned | AssignmentSubmitted | AssignmentRejected | AssignmentApproved | HITCreated | HITExtended | HITDisposed | HITReviewable | HITExpired | Ping. The Ping event is only valid for the SendTestEventNotification operation.
notificationSpecification ::
  -- | 'nsDestination'
  Text ->
  -- | 'nsTransport'
  NotificationTransport ->
  -- | 'nsVersion'
  Text ->
  NotificationSpecification
notificationSpecification pDestination_ pTransport_ pVersion_ =
  NotificationSpecification'
    { _nsDestination = pDestination_,
      _nsTransport = pTransport_,
      _nsVersion = pVersion_,
      _nsEventTypes = mempty
    }

-- | The target for notification messages. The Destination’s format is determined by the specified Transport:      * When Transport is Email, the Destination is your email address.     * When Transport is SQS, the Destination is your queue URL.     * When Transport is SNS, the Destination is the ARN of your topic.
nsDestination :: Lens' NotificationSpecification Text
nsDestination = lens _nsDestination (\s a -> s {_nsDestination = a})

-- | The method Amazon Mechanical Turk uses to send the notification. Valid Values: Email | SQS | SNS.
nsTransport :: Lens' NotificationSpecification NotificationTransport
nsTransport = lens _nsTransport (\s a -> s {_nsTransport = a})

-- | The version of the Notification API to use. Valid value is 2006-05-05.
nsVersion :: Lens' NotificationSpecification Text
nsVersion = lens _nsVersion (\s a -> s {_nsVersion = a})

-- | The list of events that should cause notifications to be sent. Valid Values: AssignmentAccepted | AssignmentAbandoned | AssignmentReturned | AssignmentSubmitted | AssignmentRejected | AssignmentApproved | HITCreated | HITExtended | HITDisposed | HITReviewable | HITExpired | Ping. The Ping event is only valid for the SendTestEventNotification operation.
nsEventTypes :: Lens' NotificationSpecification [EventType]
nsEventTypes = lens _nsEventTypes (\s a -> s {_nsEventTypes = a}) . _Coerce

instance Hashable NotificationSpecification

instance NFData NotificationSpecification

instance ToJSON NotificationSpecification where
  toJSON NotificationSpecification' {..} =
    object
      ( catMaybes
          [ Just ("Destination" .= _nsDestination),
            Just ("Transport" .= _nsTransport),
            Just ("Version" .= _nsVersion),
            Just ("EventTypes" .= _nsEventTypes)
          ]
      )
