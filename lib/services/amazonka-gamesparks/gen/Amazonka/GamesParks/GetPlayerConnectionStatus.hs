{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GamesParks.GetPlayerConnectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of a player\'s connection to the game runtime.
--
-- It\'s possible for a single player to have multiple connections to the
-- game runtime. If a player is not connected, this operation returns an
-- empty list.
module Amazonka.GamesParks.GetPlayerConnectionStatus
  ( -- * Creating a Request
    GetPlayerConnectionStatus (..),
    newGetPlayerConnectionStatus,

    -- * Request Lenses
    getPlayerConnectionStatus_gameName,
    getPlayerConnectionStatus_playerId,
    getPlayerConnectionStatus_stageName,

    -- * Destructuring the Response
    GetPlayerConnectionStatusResponse (..),
    newGetPlayerConnectionStatusResponse,

    -- * Response Lenses
    getPlayerConnectionStatusResponse_connections,
    getPlayerConnectionStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPlayerConnectionStatus' smart constructor.
data GetPlayerConnectionStatus = GetPlayerConnectionStatus'
  { -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The unique identifier representing a player.
    playerId :: Prelude.Text,
    -- | The name of the stage.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPlayerConnectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameName', 'getPlayerConnectionStatus_gameName' - The name of the game.
--
-- 'playerId', 'getPlayerConnectionStatus_playerId' - The unique identifier representing a player.
--
-- 'stageName', 'getPlayerConnectionStatus_stageName' - The name of the stage.
newGetPlayerConnectionStatus ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'playerId'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  GetPlayerConnectionStatus
newGetPlayerConnectionStatus
  pGameName_
  pPlayerId_
  pStageName_ =
    GetPlayerConnectionStatus'
      { gameName = pGameName_,
        playerId = pPlayerId_,
        stageName = pStageName_
      }

-- | The name of the game.
getPlayerConnectionStatus_gameName :: Lens.Lens' GetPlayerConnectionStatus Prelude.Text
getPlayerConnectionStatus_gameName = Lens.lens (\GetPlayerConnectionStatus' {gameName} -> gameName) (\s@GetPlayerConnectionStatus' {} a -> s {gameName = a} :: GetPlayerConnectionStatus)

-- | The unique identifier representing a player.
getPlayerConnectionStatus_playerId :: Lens.Lens' GetPlayerConnectionStatus Prelude.Text
getPlayerConnectionStatus_playerId = Lens.lens (\GetPlayerConnectionStatus' {playerId} -> playerId) (\s@GetPlayerConnectionStatus' {} a -> s {playerId = a} :: GetPlayerConnectionStatus)

-- | The name of the stage.
getPlayerConnectionStatus_stageName :: Lens.Lens' GetPlayerConnectionStatus Prelude.Text
getPlayerConnectionStatus_stageName = Lens.lens (\GetPlayerConnectionStatus' {stageName} -> stageName) (\s@GetPlayerConnectionStatus' {} a -> s {stageName = a} :: GetPlayerConnectionStatus)

instance Core.AWSRequest GetPlayerConnectionStatus where
  type
    AWSResponse GetPlayerConnectionStatus =
      GetPlayerConnectionStatusResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPlayerConnectionStatusResponse'
            Prelude.<$> (x Data..?> "Connections" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPlayerConnectionStatus where
  hashWithSalt _salt GetPlayerConnectionStatus' {..} =
    _salt
      `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` playerId
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData GetPlayerConnectionStatus where
  rnf GetPlayerConnectionStatus' {..} =
    Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf playerId
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders GetPlayerConnectionStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetPlayerConnectionStatus where
  toPath GetPlayerConnectionStatus' {..} =
    Prelude.mconcat
      [ "/runtime/game/",
        Data.toBS gameName,
        "/stage/",
        Data.toBS stageName,
        "/player/",
        Data.toBS playerId,
        "/connection"
      ]

instance Data.ToQuery GetPlayerConnectionStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPlayerConnectionStatusResponse' smart constructor.
data GetPlayerConnectionStatusResponse = GetPlayerConnectionStatusResponse'
  { -- | The list of connection ids, one for each connection in use by the
    -- player.
    connections :: Prelude.Maybe [Connection],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPlayerConnectionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connections', 'getPlayerConnectionStatusResponse_connections' - The list of connection ids, one for each connection in use by the
-- player.
--
-- 'httpStatus', 'getPlayerConnectionStatusResponse_httpStatus' - The response's http status code.
newGetPlayerConnectionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPlayerConnectionStatusResponse
newGetPlayerConnectionStatusResponse pHttpStatus_ =
  GetPlayerConnectionStatusResponse'
    { connections =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of connection ids, one for each connection in use by the
-- player.
getPlayerConnectionStatusResponse_connections :: Lens.Lens' GetPlayerConnectionStatusResponse (Prelude.Maybe [Connection])
getPlayerConnectionStatusResponse_connections = Lens.lens (\GetPlayerConnectionStatusResponse' {connections} -> connections) (\s@GetPlayerConnectionStatusResponse' {} a -> s {connections = a} :: GetPlayerConnectionStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getPlayerConnectionStatusResponse_httpStatus :: Lens.Lens' GetPlayerConnectionStatusResponse Prelude.Int
getPlayerConnectionStatusResponse_httpStatus = Lens.lens (\GetPlayerConnectionStatusResponse' {httpStatus} -> httpStatus) (\s@GetPlayerConnectionStatusResponse' {} a -> s {httpStatus = a} :: GetPlayerConnectionStatusResponse)

instance
  Prelude.NFData
    GetPlayerConnectionStatusResponse
  where
  rnf GetPlayerConnectionStatusResponse' {..} =
    Prelude.rnf connections
      `Prelude.seq` Prelude.rnf httpStatus
