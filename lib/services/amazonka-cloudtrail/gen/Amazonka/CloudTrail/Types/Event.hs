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
-- Module      : Amazonka.CloudTrail.Types.Event
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.Event where

import Amazonka.CloudTrail.Types.Resource
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an event that was returned by a lookup
-- request. The result includes a representation of a CloudTrail event.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | A user name or role name of the requester that called the API in the
    -- event returned.
    username :: Prelude.Maybe Prelude.Text,
    -- | Information about whether the event is a write event or a read event.
    readOnly :: Prelude.Maybe Prelude.Text,
    -- | The name of the event returned.
    eventName :: Prelude.Maybe Prelude.Text,
    -- | The CloudTrail ID of the event returned.
    eventId :: Prelude.Maybe Prelude.Text,
    -- | A list of resources referenced by the event returned.
    resources :: Prelude.Maybe [Resource],
    -- | The date and time of the event returned.
    eventTime :: Prelude.Maybe Core.POSIX,
    -- | A JSON string that contains a representation of the event returned.
    cloudTrailEvent :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services access key ID that was used to sign the request.
    -- If the request was made with temporary security credentials, this is the
    -- access key ID of the temporary credentials.
    accessKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services service to which the request was made.
    eventSource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'event_username' - A user name or role name of the requester that called the API in the
-- event returned.
--
-- 'readOnly', 'event_readOnly' - Information about whether the event is a write event or a read event.
--
-- 'eventName', 'event_eventName' - The name of the event returned.
--
-- 'eventId', 'event_eventId' - The CloudTrail ID of the event returned.
--
-- 'resources', 'event_resources' - A list of resources referenced by the event returned.
--
-- 'eventTime', 'event_eventTime' - The date and time of the event returned.
--
-- 'cloudTrailEvent', 'event_cloudTrailEvent' - A JSON string that contains a representation of the event returned.
--
-- 'accessKeyId', 'event_accessKeyId' - The Amazon Web Services access key ID that was used to sign the request.
-- If the request was made with temporary security credentials, this is the
-- access key ID of the temporary credentials.
--
-- 'eventSource', 'event_eventSource' - The Amazon Web Services service to which the request was made.
newEvent ::
  Event
newEvent =
  Event'
    { username = Prelude.Nothing,
      readOnly = Prelude.Nothing,
      eventName = Prelude.Nothing,
      eventId = Prelude.Nothing,
      resources = Prelude.Nothing,
      eventTime = Prelude.Nothing,
      cloudTrailEvent = Prelude.Nothing,
      accessKeyId = Prelude.Nothing,
      eventSource = Prelude.Nothing
    }

-- | A user name or role name of the requester that called the API in the
-- event returned.
event_username :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_username = Lens.lens (\Event' {username} -> username) (\s@Event' {} a -> s {username = a} :: Event)

-- | Information about whether the event is a write event or a read event.
event_readOnly :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_readOnly = Lens.lens (\Event' {readOnly} -> readOnly) (\s@Event' {} a -> s {readOnly = a} :: Event)

-- | The name of the event returned.
event_eventName :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventName = Lens.lens (\Event' {eventName} -> eventName) (\s@Event' {} a -> s {eventName = a} :: Event)

-- | The CloudTrail ID of the event returned.
event_eventId :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventId = Lens.lens (\Event' {eventId} -> eventId) (\s@Event' {} a -> s {eventId = a} :: Event)

-- | A list of resources referenced by the event returned.
event_resources :: Lens.Lens' Event (Prelude.Maybe [Resource])
event_resources = Lens.lens (\Event' {resources} -> resources) (\s@Event' {} a -> s {resources = a} :: Event) Prelude.. Lens.mapping Lens.coerced

-- | The date and time of the event returned.
event_eventTime :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_eventTime = Lens.lens (\Event' {eventTime} -> eventTime) (\s@Event' {} a -> s {eventTime = a} :: Event) Prelude.. Lens.mapping Core._Time

-- | A JSON string that contains a representation of the event returned.
event_cloudTrailEvent :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_cloudTrailEvent = Lens.lens (\Event' {cloudTrailEvent} -> cloudTrailEvent) (\s@Event' {} a -> s {cloudTrailEvent = a} :: Event)

-- | The Amazon Web Services access key ID that was used to sign the request.
-- If the request was made with temporary security credentials, this is the
-- access key ID of the temporary credentials.
event_accessKeyId :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_accessKeyId = Lens.lens (\Event' {accessKeyId} -> accessKeyId) (\s@Event' {} a -> s {accessKeyId = a} :: Event)

-- | The Amazon Web Services service to which the request was made.
event_eventSource :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventSource = Lens.lens (\Event' {eventSource} -> eventSource) (\s@Event' {} a -> s {eventSource = a} :: Event)

instance Core.FromJSON Event where
  parseJSON =
    Core.withObject
      "Event"
      ( \x ->
          Event'
            Prelude.<$> (x Core..:? "Username")
            Prelude.<*> (x Core..:? "ReadOnly")
            Prelude.<*> (x Core..:? "EventName")
            Prelude.<*> (x Core..:? "EventId")
            Prelude.<*> (x Core..:? "Resources" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "EventTime")
            Prelude.<*> (x Core..:? "CloudTrailEvent")
            Prelude.<*> (x Core..:? "AccessKeyId")
            Prelude.<*> (x Core..:? "EventSource")
      )

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` readOnly
      `Prelude.hashWithSalt` eventName
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` eventTime
      `Prelude.hashWithSalt` cloudTrailEvent
      `Prelude.hashWithSalt` accessKeyId
      `Prelude.hashWithSalt` eventSource

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf username
      `Prelude.seq` Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf eventName
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf eventTime
      `Prelude.seq` Prelude.rnf cloudTrailEvent
      `Prelude.seq` Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf eventSource
