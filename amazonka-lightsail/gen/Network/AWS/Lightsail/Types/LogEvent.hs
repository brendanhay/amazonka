{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lightsail.Types.LogEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LogEvent where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a database log event.
--
-- /See:/ 'newLogEvent' smart constructor.
data LogEvent = LogEvent'
  { -- | The message of the database log event.
    message :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the database log event was created.
    createdAt :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LogEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'logEvent_message' - The message of the database log event.
--
-- 'createdAt', 'logEvent_createdAt' - The timestamp when the database log event was created.
newLogEvent ::
  LogEvent
newLogEvent =
  LogEvent'
    { message = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The message of the database log event.
logEvent_message :: Lens.Lens' LogEvent (Prelude.Maybe Prelude.Text)
logEvent_message = Lens.lens (\LogEvent' {message} -> message) (\s@LogEvent' {} a -> s {message = a} :: LogEvent)

-- | The timestamp when the database log event was created.
logEvent_createdAt :: Lens.Lens' LogEvent (Prelude.Maybe Prelude.UTCTime)
logEvent_createdAt = Lens.lens (\LogEvent' {createdAt} -> createdAt) (\s@LogEvent' {} a -> s {createdAt = a} :: LogEvent) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON LogEvent where
  parseJSON =
    Prelude.withObject
      "LogEvent"
      ( \x ->
          LogEvent'
            Prelude.<$> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "createdAt")
      )

instance Prelude.Hashable LogEvent

instance Prelude.NFData LogEvent
