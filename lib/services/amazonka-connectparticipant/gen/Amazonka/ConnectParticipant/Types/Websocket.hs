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
-- Module      : Amazonka.ConnectParticipant.Types.Websocket
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectParticipant.Types.Websocket where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The websocket for the participant\'s connection.
--
-- /See:/ 'newWebsocket' smart constructor.
data Websocket = Websocket'
  { -- | The URL of the websocket.
    url :: Prelude.Maybe Prelude.Text,
    -- | The URL expiration timestamp in ISO date format.
    --
    -- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
    -- example, 2019-11-08T02:41:28.172Z.
    connectionExpiry :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Websocket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'websocket_url' - The URL of the websocket.
--
-- 'connectionExpiry', 'websocket_connectionExpiry' - The URL expiration timestamp in ISO date format.
--
-- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
-- example, 2019-11-08T02:41:28.172Z.
newWebsocket ::
  Websocket
newWebsocket =
  Websocket'
    { url = Prelude.Nothing,
      connectionExpiry = Prelude.Nothing
    }

-- | The URL of the websocket.
websocket_url :: Lens.Lens' Websocket (Prelude.Maybe Prelude.Text)
websocket_url = Lens.lens (\Websocket' {url} -> url) (\s@Websocket' {} a -> s {url = a} :: Websocket)

-- | The URL expiration timestamp in ISO date format.
--
-- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
-- example, 2019-11-08T02:41:28.172Z.
websocket_connectionExpiry :: Lens.Lens' Websocket (Prelude.Maybe Prelude.Text)
websocket_connectionExpiry = Lens.lens (\Websocket' {connectionExpiry} -> connectionExpiry) (\s@Websocket' {} a -> s {connectionExpiry = a} :: Websocket)

instance Core.FromJSON Websocket where
  parseJSON =
    Core.withObject
      "Websocket"
      ( \x ->
          Websocket'
            Prelude.<$> (x Core..:? "Url")
            Prelude.<*> (x Core..:? "ConnectionExpiry")
      )

instance Prelude.Hashable Websocket where
  hashWithSalt salt' Websocket' {..} =
    salt' `Prelude.hashWithSalt` connectionExpiry
      `Prelude.hashWithSalt` url

instance Prelude.NFData Websocket where
  rnf Websocket' {..} =
    Prelude.rnf url
      `Prelude.seq` Prelude.rnf connectionExpiry
