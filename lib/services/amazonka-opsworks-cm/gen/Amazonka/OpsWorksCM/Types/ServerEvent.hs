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
-- Module      : Amazonka.OpsWorksCM.Types.ServerEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorksCM.Types.ServerEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An event that is related to the server, such as the start of maintenance
-- or backup.
--
-- /See:/ 'newServerEvent' smart constructor.
data ServerEvent = ServerEvent'
  { -- | The Amazon S3 URL of the event\'s log file.
    logUrl :: Prelude.Maybe Prelude.Text,
    -- | The name of the server on or for which the event occurred.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | The time when the event occurred.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | A human-readable informational or status message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logUrl', 'serverEvent_logUrl' - The Amazon S3 URL of the event\'s log file.
--
-- 'serverName', 'serverEvent_serverName' - The name of the server on or for which the event occurred.
--
-- 'createdAt', 'serverEvent_createdAt' - The time when the event occurred.
--
-- 'message', 'serverEvent_message' - A human-readable informational or status message.
newServerEvent ::
  ServerEvent
newServerEvent =
  ServerEvent'
    { logUrl = Prelude.Nothing,
      serverName = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The Amazon S3 URL of the event\'s log file.
serverEvent_logUrl :: Lens.Lens' ServerEvent (Prelude.Maybe Prelude.Text)
serverEvent_logUrl = Lens.lens (\ServerEvent' {logUrl} -> logUrl) (\s@ServerEvent' {} a -> s {logUrl = a} :: ServerEvent)

-- | The name of the server on or for which the event occurred.
serverEvent_serverName :: Lens.Lens' ServerEvent (Prelude.Maybe Prelude.Text)
serverEvent_serverName = Lens.lens (\ServerEvent' {serverName} -> serverName) (\s@ServerEvent' {} a -> s {serverName = a} :: ServerEvent)

-- | The time when the event occurred.
serverEvent_createdAt :: Lens.Lens' ServerEvent (Prelude.Maybe Prelude.UTCTime)
serverEvent_createdAt = Lens.lens (\ServerEvent' {createdAt} -> createdAt) (\s@ServerEvent' {} a -> s {createdAt = a} :: ServerEvent) Prelude.. Lens.mapping Core._Time

-- | A human-readable informational or status message.
serverEvent_message :: Lens.Lens' ServerEvent (Prelude.Maybe Prelude.Text)
serverEvent_message = Lens.lens (\ServerEvent' {message} -> message) (\s@ServerEvent' {} a -> s {message = a} :: ServerEvent)

instance Core.FromJSON ServerEvent where
  parseJSON =
    Core.withObject
      "ServerEvent"
      ( \x ->
          ServerEvent'
            Prelude.<$> (x Core..:? "LogUrl")
            Prelude.<*> (x Core..:? "ServerName")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "Message")
      )

instance Prelude.Hashable ServerEvent where
  hashWithSalt _salt ServerEvent' {..} =
    _salt `Prelude.hashWithSalt` logUrl
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` message

instance Prelude.NFData ServerEvent where
  rnf ServerEvent' {..} =
    Prelude.rnf logUrl
      `Prelude.seq` Prelude.rnf serverName
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf message
