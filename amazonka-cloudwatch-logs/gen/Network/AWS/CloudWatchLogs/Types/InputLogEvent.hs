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
-- Module      : Network.AWS.CloudWatchLogs.Types.InputLogEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.InputLogEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a log event, which is a record of activity that was recorded
-- by the application or resource being monitored.
--
-- /See:/ 'newInputLogEvent' smart constructor.
data InputLogEvent = InputLogEvent'
  { -- | The time the event occurred, expressed as the number of milliseconds
    -- after Jan 1, 1970 00:00:00 UTC.
    timestamp :: Core.Natural,
    -- | The raw event message.
    message :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputLogEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'inputLogEvent_timestamp' - The time the event occurred, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC.
--
-- 'message', 'inputLogEvent_message' - The raw event message.
newInputLogEvent ::
  -- | 'timestamp'
  Core.Natural ->
  -- | 'message'
  Core.Text ->
  InputLogEvent
newInputLogEvent pTimestamp_ pMessage_ =
  InputLogEvent'
    { timestamp = pTimestamp_,
      message = pMessage_
    }

-- | The time the event occurred, expressed as the number of milliseconds
-- after Jan 1, 1970 00:00:00 UTC.
inputLogEvent_timestamp :: Lens.Lens' InputLogEvent Core.Natural
inputLogEvent_timestamp = Lens.lens (\InputLogEvent' {timestamp} -> timestamp) (\s@InputLogEvent' {} a -> s {timestamp = a} :: InputLogEvent)

-- | The raw event message.
inputLogEvent_message :: Lens.Lens' InputLogEvent Core.Text
inputLogEvent_message = Lens.lens (\InputLogEvent' {message} -> message) (\s@InputLogEvent' {} a -> s {message = a} :: InputLogEvent)

instance Core.Hashable InputLogEvent

instance Core.NFData InputLogEvent

instance Core.ToJSON InputLogEvent where
  toJSON InputLogEvent' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("timestamp" Core..= timestamp),
            Core.Just ("message" Core..= message)
          ]
      )
