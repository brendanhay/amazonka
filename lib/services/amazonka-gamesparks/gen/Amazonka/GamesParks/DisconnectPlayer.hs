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
-- Module      : Amazonka.GamesParks.DisconnectPlayer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disconnects a player from the game runtime.
--
-- If a player has multiple connections, this operation attempts to close
-- all of them.
module Amazonka.GamesParks.DisconnectPlayer
  ( -- * Creating a Request
    DisconnectPlayer (..),
    newDisconnectPlayer,

    -- * Request Lenses
    disconnectPlayer_gameName,
    disconnectPlayer_playerId,
    disconnectPlayer_stageName,

    -- * Destructuring the Response
    DisconnectPlayerResponse (..),
    newDisconnectPlayerResponse,

    -- * Response Lenses
    disconnectPlayerResponse_disconnectFailures,
    disconnectPlayerResponse_disconnectSuccesses,
    disconnectPlayerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisconnectPlayer' smart constructor.
data DisconnectPlayer = DisconnectPlayer'
  { -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The unique identifier representing a player.
    playerId :: Prelude.Text,
    -- | The name of the stage.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisconnectPlayer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameName', 'disconnectPlayer_gameName' - The name of the game.
--
-- 'playerId', 'disconnectPlayer_playerId' - The unique identifier representing a player.
--
-- 'stageName', 'disconnectPlayer_stageName' - The name of the stage.
newDisconnectPlayer ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'playerId'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  DisconnectPlayer
newDisconnectPlayer pGameName_ pPlayerId_ pStageName_ =
  DisconnectPlayer'
    { gameName = pGameName_,
      playerId = pPlayerId_,
      stageName = pStageName_
    }

-- | The name of the game.
disconnectPlayer_gameName :: Lens.Lens' DisconnectPlayer Prelude.Text
disconnectPlayer_gameName = Lens.lens (\DisconnectPlayer' {gameName} -> gameName) (\s@DisconnectPlayer' {} a -> s {gameName = a} :: DisconnectPlayer)

-- | The unique identifier representing a player.
disconnectPlayer_playerId :: Lens.Lens' DisconnectPlayer Prelude.Text
disconnectPlayer_playerId = Lens.lens (\DisconnectPlayer' {playerId} -> playerId) (\s@DisconnectPlayer' {} a -> s {playerId = a} :: DisconnectPlayer)

-- | The name of the stage.
disconnectPlayer_stageName :: Lens.Lens' DisconnectPlayer Prelude.Text
disconnectPlayer_stageName = Lens.lens (\DisconnectPlayer' {stageName} -> stageName) (\s@DisconnectPlayer' {} a -> s {stageName = a} :: DisconnectPlayer)

instance Core.AWSRequest DisconnectPlayer where
  type
    AWSResponse DisconnectPlayer =
      DisconnectPlayerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisconnectPlayerResponse'
            Prelude.<$> ( x Data..?> "DisconnectFailures"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "DisconnectSuccesses"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisconnectPlayer where
  hashWithSalt _salt DisconnectPlayer' {..} =
    _salt `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` playerId
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData DisconnectPlayer where
  rnf DisconnectPlayer' {..} =
    Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf playerId
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders DisconnectPlayer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisconnectPlayer where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DisconnectPlayer where
  toPath DisconnectPlayer' {..} =
    Prelude.mconcat
      [ "/runtime/game/",
        Data.toBS gameName,
        "/stage/",
        Data.toBS stageName,
        "/player/",
        Data.toBS playerId,
        "/disconnect"
      ]

instance Data.ToQuery DisconnectPlayer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisconnectPlayerResponse' smart constructor.
data DisconnectPlayerResponse = DisconnectPlayerResponse'
  { -- | The list of the connection ids that could not be disconnected.
    disconnectFailures :: Prelude.Maybe [Prelude.Text],
    -- | The list of the connection ids that were disconnected.
    disconnectSuccesses :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisconnectPlayerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disconnectFailures', 'disconnectPlayerResponse_disconnectFailures' - The list of the connection ids that could not be disconnected.
--
-- 'disconnectSuccesses', 'disconnectPlayerResponse_disconnectSuccesses' - The list of the connection ids that were disconnected.
--
-- 'httpStatus', 'disconnectPlayerResponse_httpStatus' - The response's http status code.
newDisconnectPlayerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisconnectPlayerResponse
newDisconnectPlayerResponse pHttpStatus_ =
  DisconnectPlayerResponse'
    { disconnectFailures =
        Prelude.Nothing,
      disconnectSuccesses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of the connection ids that could not be disconnected.
disconnectPlayerResponse_disconnectFailures :: Lens.Lens' DisconnectPlayerResponse (Prelude.Maybe [Prelude.Text])
disconnectPlayerResponse_disconnectFailures = Lens.lens (\DisconnectPlayerResponse' {disconnectFailures} -> disconnectFailures) (\s@DisconnectPlayerResponse' {} a -> s {disconnectFailures = a} :: DisconnectPlayerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of the connection ids that were disconnected.
disconnectPlayerResponse_disconnectSuccesses :: Lens.Lens' DisconnectPlayerResponse (Prelude.Maybe [Prelude.Text])
disconnectPlayerResponse_disconnectSuccesses = Lens.lens (\DisconnectPlayerResponse' {disconnectSuccesses} -> disconnectSuccesses) (\s@DisconnectPlayerResponse' {} a -> s {disconnectSuccesses = a} :: DisconnectPlayerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disconnectPlayerResponse_httpStatus :: Lens.Lens' DisconnectPlayerResponse Prelude.Int
disconnectPlayerResponse_httpStatus = Lens.lens (\DisconnectPlayerResponse' {httpStatus} -> httpStatus) (\s@DisconnectPlayerResponse' {} a -> s {httpStatus = a} :: DisconnectPlayerResponse)

instance Prelude.NFData DisconnectPlayerResponse where
  rnf DisconnectPlayerResponse' {..} =
    Prelude.rnf disconnectFailures
      `Prelude.seq` Prelude.rnf disconnectSuccesses
      `Prelude.seq` Prelude.rnf httpStatus
