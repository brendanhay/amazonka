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
-- Module      : Amazonka.CodeGuruProfiler.Types.Channel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.Channel where

import Amazonka.CodeGuruProfiler.Types.EventPublisher
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Notification medium for users to get alerted for events that occur in
-- application profile. We support SNS topic as a notification channel.
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { -- | Unique identifier for each @Channel@ in the notification configuration
    -- of a Profiling Group. A random UUID for channelId is used when adding a
    -- channel to the notification configuration if not specified in the
    -- request.
    id :: Prelude.Maybe Prelude.Text,
    -- | List of publishers for different type of events that may be detected in
    -- an application from the profile. Anomaly detection is the only event
    -- publisher in Profiler.
    eventPublishers :: Prelude.NonEmpty EventPublisher,
    -- | Unique arn of the resource to be used for notifications. We support a
    -- valid SNS topic arn as a channel uri.
    uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Channel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'channel_id' - Unique identifier for each @Channel@ in the notification configuration
-- of a Profiling Group. A random UUID for channelId is used when adding a
-- channel to the notification configuration if not specified in the
-- request.
--
-- 'eventPublishers', 'channel_eventPublishers' - List of publishers for different type of events that may be detected in
-- an application from the profile. Anomaly detection is the only event
-- publisher in Profiler.
--
-- 'uri', 'channel_uri' - Unique arn of the resource to be used for notifications. We support a
-- valid SNS topic arn as a channel uri.
newChannel ::
  -- | 'eventPublishers'
  Prelude.NonEmpty EventPublisher ->
  -- | 'uri'
  Prelude.Text ->
  Channel
newChannel pEventPublishers_ pUri_ =
  Channel'
    { id = Prelude.Nothing,
      eventPublishers =
        Lens.coerced Lens.# pEventPublishers_,
      uri = pUri_
    }

-- | Unique identifier for each @Channel@ in the notification configuration
-- of a Profiling Group. A random UUID for channelId is used when adding a
-- channel to the notification configuration if not specified in the
-- request.
channel_id :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_id = Lens.lens (\Channel' {id} -> id) (\s@Channel' {} a -> s {id = a} :: Channel)

-- | List of publishers for different type of events that may be detected in
-- an application from the profile. Anomaly detection is the only event
-- publisher in Profiler.
channel_eventPublishers :: Lens.Lens' Channel (Prelude.NonEmpty EventPublisher)
channel_eventPublishers = Lens.lens (\Channel' {eventPublishers} -> eventPublishers) (\s@Channel' {} a -> s {eventPublishers = a} :: Channel) Prelude.. Lens.coerced

-- | Unique arn of the resource to be used for notifications. We support a
-- valid SNS topic arn as a channel uri.
channel_uri :: Lens.Lens' Channel Prelude.Text
channel_uri = Lens.lens (\Channel' {uri} -> uri) (\s@Channel' {} a -> s {uri = a} :: Channel)

instance Core.FromJSON Channel where
  parseJSON =
    Core.withObject
      "Channel"
      ( \x ->
          Channel'
            Prelude.<$> (x Core..:? "id")
            Prelude.<*> (x Core..: "eventPublishers")
            Prelude.<*> (x Core..: "uri")
      )

instance Prelude.Hashable Channel where
  hashWithSalt _salt Channel' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` eventPublishers
      `Prelude.hashWithSalt` uri

instance Prelude.NFData Channel where
  rnf Channel' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf eventPublishers
      `Prelude.seq` Prelude.rnf uri

instance Core.ToJSON Channel where
  toJSON Channel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("id" Core..=) Prelude.<$> id,
            Prelude.Just
              ("eventPublishers" Core..= eventPublishers),
            Prelude.Just ("uri" Core..= uri)
          ]
      )
