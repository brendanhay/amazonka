{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.NotificationSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.NotificationSpecification
  ( NotificationSpecification (..)
  -- * Smart constructor
  , mkNotificationSpecification
  -- * Lenses
  , nsDestination
  , nsTransport
  , nsVersion
  , nsEventTypes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.EventType as Types
import qualified Network.AWS.MechanicalTurk.Types.NotificationTransport as Types
import qualified Network.AWS.Prelude as Core

-- | The NotificationSpecification data structure describes a HIT event notification for a HIT type.
--
-- /See:/ 'mkNotificationSpecification' smart constructor.
data NotificationSpecification = NotificationSpecification'
  { destination :: Core.Text
    -- ^ The target for notification messages. The Destination’s format is determined by the specified Transport: 
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
  , transport :: Types.NotificationTransport
    -- ^ The method Amazon Mechanical Turk uses to send the notification. Valid Values: Email | SQS | SNS. 
  , version :: Core.Text
    -- ^ The version of the Notification API to use. Valid value is 2006-05-05.
  , eventTypes :: [Types.EventType]
    -- ^ The list of events that should cause notifications to be sent. Valid Values: AssignmentAccepted | AssignmentAbandoned | AssignmentReturned | AssignmentSubmitted | AssignmentRejected | AssignmentApproved | HITCreated | HITExtended | HITDisposed | HITReviewable | HITExpired | Ping. The Ping event is only valid for the SendTestEventNotification operation. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotificationSpecification' value with any optional fields omitted.
mkNotificationSpecification
    :: Core.Text -- ^ 'destination'
    -> Types.NotificationTransport -- ^ 'transport'
    -> Core.Text -- ^ 'version'
    -> NotificationSpecification
mkNotificationSpecification destination transport version
  = NotificationSpecification'{destination, transport, version,
                               eventTypes = Core.mempty}

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
nsDestination :: Lens.Lens' NotificationSpecification Core.Text
nsDestination = Lens.field @"destination"
{-# INLINEABLE nsDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

-- | The method Amazon Mechanical Turk uses to send the notification. Valid Values: Email | SQS | SNS. 
--
-- /Note:/ Consider using 'transport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsTransport :: Lens.Lens' NotificationSpecification Types.NotificationTransport
nsTransport = Lens.field @"transport"
{-# INLINEABLE nsTransport #-}
{-# DEPRECATED transport "Use generic-lens or generic-optics with 'transport' instead"  #-}

-- | The version of the Notification API to use. Valid value is 2006-05-05.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsVersion :: Lens.Lens' NotificationSpecification Core.Text
nsVersion = Lens.field @"version"
{-# INLINEABLE nsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The list of events that should cause notifications to be sent. Valid Values: AssignmentAccepted | AssignmentAbandoned | AssignmentReturned | AssignmentSubmitted | AssignmentRejected | AssignmentApproved | HITCreated | HITExtended | HITDisposed | HITReviewable | HITExpired | Ping. The Ping event is only valid for the SendTestEventNotification operation. 
--
-- /Note:/ Consider using 'eventTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsEventTypes :: Lens.Lens' NotificationSpecification [Types.EventType]
nsEventTypes = Lens.field @"eventTypes"
{-# INLINEABLE nsEventTypes #-}
{-# DEPRECATED eventTypes "Use generic-lens or generic-optics with 'eventTypes' instead"  #-}

instance Core.FromJSON NotificationSpecification where
        toJSON NotificationSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Destination" Core..= destination),
                  Core.Just ("Transport" Core..= transport),
                  Core.Just ("Version" Core..= version),
                  Core.Just ("EventTypes" Core..= eventTypes)])
