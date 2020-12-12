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
    eUsername,
    eResources,
    eEventTime,
    eCloudTrailEvent,
    eEventName,
    eReadOnly,
    eAccessKeyId,
    eEventSource,
    eEventId,
  )
where

import Network.AWS.CloudTrail.Types.Resource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an event that was returned by a lookup request. The result includes a representation of a CloudTrail event.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { username :: Lude.Maybe Lude.Text,
    resources :: Lude.Maybe [Resource],
    eventTime :: Lude.Maybe Lude.Timestamp,
    cloudTrailEvent :: Lude.Maybe Lude.Text,
    eventName :: Lude.Maybe Lude.Text,
    readOnly :: Lude.Maybe Lude.Text,
    accessKeyId :: Lude.Maybe Lude.Text,
    eventSource :: Lude.Maybe Lude.Text,
    eventId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- * 'accessKeyId' - The AWS access key ID that was used to sign the request. If the request was made with temporary security credentials, this is the access key ID of the temporary credentials.
-- * 'cloudTrailEvent' - A JSON string that contains a representation of the event returned.
-- * 'eventId' - The CloudTrail ID of the event returned.
-- * 'eventName' - The name of the event returned.
-- * 'eventSource' - The AWS service that the request was made to.
-- * 'eventTime' - The date and time of the event returned.
-- * 'readOnly' - Information about whether the event is a write event or a read event.
-- * 'resources' - A list of resources referenced by the event returned.
-- * 'username' - A user name or role name of the requester that called the API in the event returned.
mkEvent ::
  Event
mkEvent =
  Event'
    { username = Lude.Nothing,
      resources = Lude.Nothing,
      eventTime = Lude.Nothing,
      cloudTrailEvent = Lude.Nothing,
      eventName = Lude.Nothing,
      readOnly = Lude.Nothing,
      accessKeyId = Lude.Nothing,
      eventSource = Lude.Nothing,
      eventId = Lude.Nothing
    }

-- | A user name or role name of the requester that called the API in the event returned.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eUsername :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eUsername = Lens.lens (username :: Event -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: Event)
{-# DEPRECATED eUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | A list of resources referenced by the event returned.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eResources :: Lens.Lens' Event (Lude.Maybe [Resource])
eResources = Lens.lens (resources :: Event -> Lude.Maybe [Resource]) (\s a -> s {resources = a} :: Event)
{-# DEPRECATED eResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The date and time of the event returned.
--
-- /Note:/ Consider using 'eventTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventTime :: Lens.Lens' Event (Lude.Maybe Lude.Timestamp)
eEventTime = Lens.lens (eventTime :: Event -> Lude.Maybe Lude.Timestamp) (\s a -> s {eventTime = a} :: Event)
{-# DEPRECATED eEventTime "Use generic-lens or generic-optics with 'eventTime' instead." #-}

-- | A JSON string that contains a representation of the event returned.
--
-- /Note:/ Consider using 'cloudTrailEvent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCloudTrailEvent :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eCloudTrailEvent = Lens.lens (cloudTrailEvent :: Event -> Lude.Maybe Lude.Text) (\s a -> s {cloudTrailEvent = a} :: Event)
{-# DEPRECATED eCloudTrailEvent "Use generic-lens or generic-optics with 'cloudTrailEvent' instead." #-}

-- | The name of the event returned.
--
-- /Note:/ Consider using 'eventName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventName :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eEventName = Lens.lens (eventName :: Event -> Lude.Maybe Lude.Text) (\s a -> s {eventName = a} :: Event)
{-# DEPRECATED eEventName "Use generic-lens or generic-optics with 'eventName' instead." #-}

-- | Information about whether the event is a write event or a read event.
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eReadOnly :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eReadOnly = Lens.lens (readOnly :: Event -> Lude.Maybe Lude.Text) (\s a -> s {readOnly = a} :: Event)
{-# DEPRECATED eReadOnly "Use generic-lens or generic-optics with 'readOnly' instead." #-}

-- | The AWS access key ID that was used to sign the request. If the request was made with temporary security credentials, this is the access key ID of the temporary credentials.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAccessKeyId :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eAccessKeyId = Lens.lens (accessKeyId :: Event -> Lude.Maybe Lude.Text) (\s a -> s {accessKeyId = a} :: Event)
{-# DEPRECATED eAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

-- | The AWS service that the request was made to.
--
-- /Note:/ Consider using 'eventSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventSource :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eEventSource = Lens.lens (eventSource :: Event -> Lude.Maybe Lude.Text) (\s a -> s {eventSource = a} :: Event)
{-# DEPRECATED eEventSource "Use generic-lens or generic-optics with 'eventSource' instead." #-}

-- | The CloudTrail ID of the event returned.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventId :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eEventId = Lens.lens (eventId :: Event -> Lude.Maybe Lude.Text) (\s a -> s {eventId = a} :: Event)
{-# DEPRECATED eEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

instance Lude.FromJSON Event where
  parseJSON =
    Lude.withObject
      "Event"
      ( \x ->
          Event'
            Lude.<$> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "Resources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EventTime")
            Lude.<*> (x Lude..:? "CloudTrailEvent")
            Lude.<*> (x Lude..:? "EventName")
            Lude.<*> (x Lude..:? "ReadOnly")
            Lude.<*> (x Lude..:? "AccessKeyId")
            Lude.<*> (x Lude..:? "EventSource")
            Lude.<*> (x Lude..:? "EventId")
      )
