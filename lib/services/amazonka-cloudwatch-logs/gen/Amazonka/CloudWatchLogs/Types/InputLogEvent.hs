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
-- Module      : Amazonka.CloudWatchLogs.Types.InputLogEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.InputLogEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a log event, which is a record of activity that was recorded
-- by the application or resource being monitored.
--
-- /See:/ 'newInputLogEvent' smart constructor.
data InputLogEvent = InputLogEvent'
  { -- | The time the event occurred, expressed as the number of milliseconds
    -- after @Jan 1, 1970 00:00:00 UTC@.
    timestamp :: Prelude.Natural,
    -- | The raw event message. Each log event can be no larger than 256 KB.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputLogEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'inputLogEvent_timestamp' - The time the event occurred, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@.
--
-- 'message', 'inputLogEvent_message' - The raw event message. Each log event can be no larger than 256 KB.
newInputLogEvent ::
  -- | 'timestamp'
  Prelude.Natural ->
  -- | 'message'
  Prelude.Text ->
  InputLogEvent
newInputLogEvent pTimestamp_ pMessage_ =
  InputLogEvent'
    { timestamp = pTimestamp_,
      message = pMessage_
    }

-- | The time the event occurred, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@.
inputLogEvent_timestamp :: Lens.Lens' InputLogEvent Prelude.Natural
inputLogEvent_timestamp = Lens.lens (\InputLogEvent' {timestamp} -> timestamp) (\s@InputLogEvent' {} a -> s {timestamp = a} :: InputLogEvent)

-- | The raw event message. Each log event can be no larger than 256 KB.
inputLogEvent_message :: Lens.Lens' InputLogEvent Prelude.Text
inputLogEvent_message = Lens.lens (\InputLogEvent' {message} -> message) (\s@InputLogEvent' {} a -> s {message = a} :: InputLogEvent)

instance Prelude.Hashable InputLogEvent where
  hashWithSalt _salt InputLogEvent' {..} =
    _salt
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` message

instance Prelude.NFData InputLogEvent where
  rnf InputLogEvent' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf message

instance Data.ToJSON InputLogEvent where
  toJSON InputLogEvent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("timestamp" Data..= timestamp),
            Prelude.Just ("message" Data..= message)
          ]
      )
