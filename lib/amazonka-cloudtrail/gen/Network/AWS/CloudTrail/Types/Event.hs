{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.Event
  ( Event (..),

    -- * Smart constructor
    mkEvent,

    -- * Lenses
    eAccessKeyId,
    eCloudTrailEvent,
    eEventId,
    eEventName,
    eEventSource,
    eEventTime,
    eReadOnly,
    eResources,
    eUsername,
  )
where

import qualified Network.AWS.CloudTrail.Types.AccessKeyId as Types
import qualified Network.AWS.CloudTrail.Types.CloudTrailEvent as Types
import qualified Network.AWS.CloudTrail.Types.EventId as Types
import qualified Network.AWS.CloudTrail.Types.EventName as Types
import qualified Network.AWS.CloudTrail.Types.EventSource as Types
import qualified Network.AWS.CloudTrail.Types.ReadOnly as Types
import qualified Network.AWS.CloudTrail.Types.Resource as Types
import qualified Network.AWS.CloudTrail.Types.Username as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an event that was returned by a lookup request. The result includes a representation of a CloudTrail event.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { -- | The AWS access key ID that was used to sign the request. If the request was made with temporary security credentials, this is the access key ID of the temporary credentials.
    accessKeyId :: Core.Maybe Types.AccessKeyId,
    -- | A JSON string that contains a representation of the event returned.
    cloudTrailEvent :: Core.Maybe Types.CloudTrailEvent,
    -- | The CloudTrail ID of the event returned.
    eventId :: Core.Maybe Types.EventId,
    -- | The name of the event returned.
    eventName :: Core.Maybe Types.EventName,
    -- | The AWS service that the request was made to.
    eventSource :: Core.Maybe Types.EventSource,
    -- | The date and time of the event returned.
    eventTime :: Core.Maybe Core.NominalDiffTime,
    -- | Information about whether the event is a write event or a read event.
    readOnly :: Core.Maybe Types.ReadOnly,
    -- | A list of resources referenced by the event returned.
    resources :: Core.Maybe [Types.Resource],
    -- | A user name or role name of the requester that called the API in the event returned.
    username :: Core.Maybe Types.Username
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Event' value with any optional fields omitted.
mkEvent ::
  Event
mkEvent =
  Event'
    { accessKeyId = Core.Nothing,
      cloudTrailEvent = Core.Nothing,
      eventId = Core.Nothing,
      eventName = Core.Nothing,
      eventSource = Core.Nothing,
      eventTime = Core.Nothing,
      readOnly = Core.Nothing,
      resources = Core.Nothing,
      username = Core.Nothing
    }

-- | The AWS access key ID that was used to sign the request. If the request was made with temporary security credentials, this is the access key ID of the temporary credentials.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAccessKeyId :: Lens.Lens' Event (Core.Maybe Types.AccessKeyId)
eAccessKeyId = Lens.field @"accessKeyId"
{-# DEPRECATED eAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

-- | A JSON string that contains a representation of the event returned.
--
-- /Note:/ Consider using 'cloudTrailEvent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCloudTrailEvent :: Lens.Lens' Event (Core.Maybe Types.CloudTrailEvent)
eCloudTrailEvent = Lens.field @"cloudTrailEvent"
{-# DEPRECATED eCloudTrailEvent "Use generic-lens or generic-optics with 'cloudTrailEvent' instead." #-}

-- | The CloudTrail ID of the event returned.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventId :: Lens.Lens' Event (Core.Maybe Types.EventId)
eEventId = Lens.field @"eventId"
{-# DEPRECATED eEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

-- | The name of the event returned.
--
-- /Note:/ Consider using 'eventName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventName :: Lens.Lens' Event (Core.Maybe Types.EventName)
eEventName = Lens.field @"eventName"
{-# DEPRECATED eEventName "Use generic-lens or generic-optics with 'eventName' instead." #-}

-- | The AWS service that the request was made to.
--
-- /Note:/ Consider using 'eventSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventSource :: Lens.Lens' Event (Core.Maybe Types.EventSource)
eEventSource = Lens.field @"eventSource"
{-# DEPRECATED eEventSource "Use generic-lens or generic-optics with 'eventSource' instead." #-}

-- | The date and time of the event returned.
--
-- /Note:/ Consider using 'eventTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventTime :: Lens.Lens' Event (Core.Maybe Core.NominalDiffTime)
eEventTime = Lens.field @"eventTime"
{-# DEPRECATED eEventTime "Use generic-lens or generic-optics with 'eventTime' instead." #-}

-- | Information about whether the event is a write event or a read event.
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eReadOnly :: Lens.Lens' Event (Core.Maybe Types.ReadOnly)
eReadOnly = Lens.field @"readOnly"
{-# DEPRECATED eReadOnly "Use generic-lens or generic-optics with 'readOnly' instead." #-}

-- | A list of resources referenced by the event returned.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eResources :: Lens.Lens' Event (Core.Maybe [Types.Resource])
eResources = Lens.field @"resources"
{-# DEPRECATED eResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | A user name or role name of the requester that called the API in the event returned.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eUsername :: Lens.Lens' Event (Core.Maybe Types.Username)
eUsername = Lens.field @"username"
{-# DEPRECATED eUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Core.FromJSON Event where
  parseJSON =
    Core.withObject "Event" Core.$
      \x ->
        Event'
          Core.<$> (x Core..:? "AccessKeyId")
          Core.<*> (x Core..:? "CloudTrailEvent")
          Core.<*> (x Core..:? "EventId")
          Core.<*> (x Core..:? "EventName")
          Core.<*> (x Core..:? "EventSource")
          Core.<*> (x Core..:? "EventTime")
          Core.<*> (x Core..:? "ReadOnly")
          Core.<*> (x Core..:? "Resources")
          Core.<*> (x Core..:? "Username")
