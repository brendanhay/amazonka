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
-- Module      : Network.AWS.CloudTrail.Types.Event
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.Event where

import Network.AWS.CloudTrail.Types.Resource
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about an event that was returned by a lookup
-- request. The result includes a representation of a CloudTrail event.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | A JSON string that contains a representation of the event returned.
    cloudTrailEvent :: Core.Maybe Core.Text,
    -- | The CloudTrail ID of the event returned.
    eventId :: Core.Maybe Core.Text,
    -- | The AWS service that the request was made to.
    eventSource :: Core.Maybe Core.Text,
    -- | Information about whether the event is a write event or a read event.
    readOnly :: Core.Maybe Core.Text,
    -- | The name of the event returned.
    eventName :: Core.Maybe Core.Text,
    -- | A list of resources referenced by the event returned.
    resources :: Core.Maybe [Resource],
    -- | The date and time of the event returned.
    eventTime :: Core.Maybe Core.POSIX,
    -- | The AWS access key ID that was used to sign the request. If the request
    -- was made with temporary security credentials, this is the access key ID
    -- of the temporary credentials.
    accessKeyId :: Core.Maybe Core.Text,
    -- | A user name or role name of the requester that called the API in the
    -- event returned.
    username :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudTrailEvent', 'event_cloudTrailEvent' - A JSON string that contains a representation of the event returned.
--
-- 'eventId', 'event_eventId' - The CloudTrail ID of the event returned.
--
-- 'eventSource', 'event_eventSource' - The AWS service that the request was made to.
--
-- 'readOnly', 'event_readOnly' - Information about whether the event is a write event or a read event.
--
-- 'eventName', 'event_eventName' - The name of the event returned.
--
-- 'resources', 'event_resources' - A list of resources referenced by the event returned.
--
-- 'eventTime', 'event_eventTime' - The date and time of the event returned.
--
-- 'accessKeyId', 'event_accessKeyId' - The AWS access key ID that was used to sign the request. If the request
-- was made with temporary security credentials, this is the access key ID
-- of the temporary credentials.
--
-- 'username', 'event_username' - A user name or role name of the requester that called the API in the
-- event returned.
newEvent ::
  Event
newEvent =
  Event'
    { cloudTrailEvent = Core.Nothing,
      eventId = Core.Nothing,
      eventSource = Core.Nothing,
      readOnly = Core.Nothing,
      eventName = Core.Nothing,
      resources = Core.Nothing,
      eventTime = Core.Nothing,
      accessKeyId = Core.Nothing,
      username = Core.Nothing
    }

-- | A JSON string that contains a representation of the event returned.
event_cloudTrailEvent :: Lens.Lens' Event (Core.Maybe Core.Text)
event_cloudTrailEvent = Lens.lens (\Event' {cloudTrailEvent} -> cloudTrailEvent) (\s@Event' {} a -> s {cloudTrailEvent = a} :: Event)

-- | The CloudTrail ID of the event returned.
event_eventId :: Lens.Lens' Event (Core.Maybe Core.Text)
event_eventId = Lens.lens (\Event' {eventId} -> eventId) (\s@Event' {} a -> s {eventId = a} :: Event)

-- | The AWS service that the request was made to.
event_eventSource :: Lens.Lens' Event (Core.Maybe Core.Text)
event_eventSource = Lens.lens (\Event' {eventSource} -> eventSource) (\s@Event' {} a -> s {eventSource = a} :: Event)

-- | Information about whether the event is a write event or a read event.
event_readOnly :: Lens.Lens' Event (Core.Maybe Core.Text)
event_readOnly = Lens.lens (\Event' {readOnly} -> readOnly) (\s@Event' {} a -> s {readOnly = a} :: Event)

-- | The name of the event returned.
event_eventName :: Lens.Lens' Event (Core.Maybe Core.Text)
event_eventName = Lens.lens (\Event' {eventName} -> eventName) (\s@Event' {} a -> s {eventName = a} :: Event)

-- | A list of resources referenced by the event returned.
event_resources :: Lens.Lens' Event (Core.Maybe [Resource])
event_resources = Lens.lens (\Event' {resources} -> resources) (\s@Event' {} a -> s {resources = a} :: Event) Core.. Lens.mapping Lens._Coerce

-- | The date and time of the event returned.
event_eventTime :: Lens.Lens' Event (Core.Maybe Core.UTCTime)
event_eventTime = Lens.lens (\Event' {eventTime} -> eventTime) (\s@Event' {} a -> s {eventTime = a} :: Event) Core.. Lens.mapping Core._Time

-- | The AWS access key ID that was used to sign the request. If the request
-- was made with temporary security credentials, this is the access key ID
-- of the temporary credentials.
event_accessKeyId :: Lens.Lens' Event (Core.Maybe Core.Text)
event_accessKeyId = Lens.lens (\Event' {accessKeyId} -> accessKeyId) (\s@Event' {} a -> s {accessKeyId = a} :: Event)

-- | A user name or role name of the requester that called the API in the
-- event returned.
event_username :: Lens.Lens' Event (Core.Maybe Core.Text)
event_username = Lens.lens (\Event' {username} -> username) (\s@Event' {} a -> s {username = a} :: Event)

instance Core.FromJSON Event where
  parseJSON =
    Core.withObject
      "Event"
      ( \x ->
          Event'
            Core.<$> (x Core..:? "CloudTrailEvent")
            Core.<*> (x Core..:? "EventId")
            Core.<*> (x Core..:? "EventSource")
            Core.<*> (x Core..:? "ReadOnly")
            Core.<*> (x Core..:? "EventName")
            Core.<*> (x Core..:? "Resources" Core..!= Core.mempty)
            Core.<*> (x Core..:? "EventTime")
            Core.<*> (x Core..:? "AccessKeyId")
            Core.<*> (x Core..:? "Username")
      )

instance Core.Hashable Event

instance Core.NFData Event
