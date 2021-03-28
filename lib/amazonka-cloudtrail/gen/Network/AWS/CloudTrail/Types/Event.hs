{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.Event
  ( Event (..)
  -- * Smart constructor
  , mkEvent
  -- * Lenses
  , eAccessKeyId
  , eCloudTrailEvent
  , eEventId
  , eEventName
  , eEventSource
  , eEventTime
  , eReadOnly
  , eResources
  , eUsername
  ) where

import qualified Network.AWS.CloudTrail.Types.Resource as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an event that was returned by a lookup request. The result includes a representation of a CloudTrail event.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { accessKeyId :: Core.Maybe Core.Text
    -- ^ The AWS access key ID that was used to sign the request. If the request was made with temporary security credentials, this is the access key ID of the temporary credentials.
  , cloudTrailEvent :: Core.Maybe Core.Text
    -- ^ A JSON string that contains a representation of the event returned.
  , eventId :: Core.Maybe Core.Text
    -- ^ The CloudTrail ID of the event returned.
  , eventName :: Core.Maybe Core.Text
    -- ^ The name of the event returned.
  , eventSource :: Core.Maybe Core.Text
    -- ^ The AWS service that the request was made to.
  , eventTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time of the event returned.
  , readOnly :: Core.Maybe Core.Text
    -- ^ Information about whether the event is a write event or a read event. 
  , resources :: Core.Maybe [Types.Resource]
    -- ^ A list of resources referenced by the event returned.
  , username :: Core.Maybe Core.Text
    -- ^ A user name or role name of the requester that called the API in the event returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Event' value with any optional fields omitted.
mkEvent
    :: Event
mkEvent
  = Event'{accessKeyId = Core.Nothing,
           cloudTrailEvent = Core.Nothing, eventId = Core.Nothing,
           eventName = Core.Nothing, eventSource = Core.Nothing,
           eventTime = Core.Nothing, readOnly = Core.Nothing,
           resources = Core.Nothing, username = Core.Nothing}

-- | The AWS access key ID that was used to sign the request. If the request was made with temporary security credentials, this is the access key ID of the temporary credentials.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAccessKeyId :: Lens.Lens' Event (Core.Maybe Core.Text)
eAccessKeyId = Lens.field @"accessKeyId"
{-# INLINEABLE eAccessKeyId #-}
{-# DEPRECATED accessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead"  #-}

-- | A JSON string that contains a representation of the event returned.
--
-- /Note:/ Consider using 'cloudTrailEvent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCloudTrailEvent :: Lens.Lens' Event (Core.Maybe Core.Text)
eCloudTrailEvent = Lens.field @"cloudTrailEvent"
{-# INLINEABLE eCloudTrailEvent #-}
{-# DEPRECATED cloudTrailEvent "Use generic-lens or generic-optics with 'cloudTrailEvent' instead"  #-}

-- | The CloudTrail ID of the event returned.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventId :: Lens.Lens' Event (Core.Maybe Core.Text)
eEventId = Lens.field @"eventId"
{-# INLINEABLE eEventId #-}
{-# DEPRECATED eventId "Use generic-lens or generic-optics with 'eventId' instead"  #-}

-- | The name of the event returned.
--
-- /Note:/ Consider using 'eventName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventName :: Lens.Lens' Event (Core.Maybe Core.Text)
eEventName = Lens.field @"eventName"
{-# INLINEABLE eEventName #-}
{-# DEPRECATED eventName "Use generic-lens or generic-optics with 'eventName' instead"  #-}

-- | The AWS service that the request was made to.
--
-- /Note:/ Consider using 'eventSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventSource :: Lens.Lens' Event (Core.Maybe Core.Text)
eEventSource = Lens.field @"eventSource"
{-# INLINEABLE eEventSource #-}
{-# DEPRECATED eventSource "Use generic-lens or generic-optics with 'eventSource' instead"  #-}

-- | The date and time of the event returned.
--
-- /Note:/ Consider using 'eventTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventTime :: Lens.Lens' Event (Core.Maybe Core.NominalDiffTime)
eEventTime = Lens.field @"eventTime"
{-# INLINEABLE eEventTime #-}
{-# DEPRECATED eventTime "Use generic-lens or generic-optics with 'eventTime' instead"  #-}

-- | Information about whether the event is a write event or a read event. 
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eReadOnly :: Lens.Lens' Event (Core.Maybe Core.Text)
eReadOnly = Lens.field @"readOnly"
{-# INLINEABLE eReadOnly #-}
{-# DEPRECATED readOnly "Use generic-lens or generic-optics with 'readOnly' instead"  #-}

-- | A list of resources referenced by the event returned.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eResources :: Lens.Lens' Event (Core.Maybe [Types.Resource])
eResources = Lens.field @"resources"
{-# INLINEABLE eResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

-- | A user name or role name of the requester that called the API in the event returned.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eUsername :: Lens.Lens' Event (Core.Maybe Core.Text)
eUsername = Lens.field @"username"
{-# INLINEABLE eUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON Event where
        parseJSON
          = Core.withObject "Event" Core.$
              \ x ->
                Event' Core.<$>
                  (x Core..:? "AccessKeyId") Core.<*> x Core..:? "CloudTrailEvent"
                    Core.<*> x Core..:? "EventId"
                    Core.<*> x Core..:? "EventName"
                    Core.<*> x Core..:? "EventSource"
                    Core.<*> x Core..:? "EventTime"
                    Core.<*> x Core..:? "ReadOnly"
                    Core.<*> x Core..:? "Resources"
                    Core.<*> x Core..:? "Username"
