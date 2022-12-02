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
-- Module      : Amazonka.Lightsail.Types.LogEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LogEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a database log event.
--
-- /See:/ 'newLogEvent' smart constructor.
data LogEvent = LogEvent'
  { -- | The message of the database log event.
    message :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the database log event was created.
    createdAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
logEvent_createdAt = Lens.lens (\LogEvent' {createdAt} -> createdAt) (\s@LogEvent' {} a -> s {createdAt = a} :: LogEvent) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON LogEvent where
  parseJSON =
    Data.withObject
      "LogEvent"
      ( \x ->
          LogEvent'
            Prelude.<$> (x Data..:? "message")
            Prelude.<*> (x Data..:? "createdAt")
      )

instance Prelude.Hashable LogEvent where
  hashWithSalt _salt LogEvent' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData LogEvent where
  rnf LogEvent' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf createdAt
