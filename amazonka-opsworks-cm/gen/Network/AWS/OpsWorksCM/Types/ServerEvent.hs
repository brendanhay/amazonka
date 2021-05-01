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
-- Module      : Network.AWS.OpsWorksCM.Types.ServerEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.ServerEvent where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An event that is related to the server, such as the start of maintenance
-- or backup.
--
-- /See:/ 'newServerEvent' smart constructor.
data ServerEvent = ServerEvent'
  { -- | The Amazon S3 URL of the event\'s log file.
    logUrl :: Prelude.Maybe Prelude.Text,
    -- | A human-readable informational or status message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The time when the event occurred.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the server on or for which the event occurred.
    serverName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'message', 'serverEvent_message' - A human-readable informational or status message.
--
-- 'createdAt', 'serverEvent_createdAt' - The time when the event occurred.
--
-- 'serverName', 'serverEvent_serverName' - The name of the server on or for which the event occurred.
newServerEvent ::
  ServerEvent
newServerEvent =
  ServerEvent'
    { logUrl = Prelude.Nothing,
      message = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      serverName = Prelude.Nothing
    }

-- | The Amazon S3 URL of the event\'s log file.
serverEvent_logUrl :: Lens.Lens' ServerEvent (Prelude.Maybe Prelude.Text)
serverEvent_logUrl = Lens.lens (\ServerEvent' {logUrl} -> logUrl) (\s@ServerEvent' {} a -> s {logUrl = a} :: ServerEvent)

-- | A human-readable informational or status message.
serverEvent_message :: Lens.Lens' ServerEvent (Prelude.Maybe Prelude.Text)
serverEvent_message = Lens.lens (\ServerEvent' {message} -> message) (\s@ServerEvent' {} a -> s {message = a} :: ServerEvent)

-- | The time when the event occurred.
serverEvent_createdAt :: Lens.Lens' ServerEvent (Prelude.Maybe Prelude.UTCTime)
serverEvent_createdAt = Lens.lens (\ServerEvent' {createdAt} -> createdAt) (\s@ServerEvent' {} a -> s {createdAt = a} :: ServerEvent) Prelude.. Lens.mapping Prelude._Time

-- | The name of the server on or for which the event occurred.
serverEvent_serverName :: Lens.Lens' ServerEvent (Prelude.Maybe Prelude.Text)
serverEvent_serverName = Lens.lens (\ServerEvent' {serverName} -> serverName) (\s@ServerEvent' {} a -> s {serverName = a} :: ServerEvent)

instance Prelude.FromJSON ServerEvent where
  parseJSON =
    Prelude.withObject
      "ServerEvent"
      ( \x ->
          ServerEvent'
            Prelude.<$> (x Prelude..:? "LogUrl")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "CreatedAt")
            Prelude.<*> (x Prelude..:? "ServerName")
      )

instance Prelude.Hashable ServerEvent

instance Prelude.NFData ServerEvent
