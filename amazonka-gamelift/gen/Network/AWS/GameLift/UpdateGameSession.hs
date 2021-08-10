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
-- Module      : Network.AWS.GameLift.UpdateGameSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates game session properties. This includes the session name, maximum
-- player count, protection policy, which controls whether or not an active
-- game session can be terminated during a scale-down event, and the player
-- session creation policy, which controls whether or not new players can
-- join the session. To update a game session, specify the game session ID
-- and the values you want to change. If successful, an updated GameSession
-- object is returned.
--
-- -   CreateGameSession
--
-- -   DescribeGameSessions
--
-- -   DescribeGameSessionDetails
--
-- -   SearchGameSessions
--
-- -   UpdateGameSession
--
-- -   GetGameSessionLogUrl
--
-- -   Game session placements
--
--     -   StartGameSessionPlacement
--
--     -   DescribeGameSessionPlacement
--
--     -   StopGameSessionPlacement
module Network.AWS.GameLift.UpdateGameSession
  ( -- * Creating a Request
    UpdateGameSession (..),
    newUpdateGameSession,

    -- * Request Lenses
    updateGameSession_playerSessionCreationPolicy,
    updateGameSession_maximumPlayerSessionCount,
    updateGameSession_name,
    updateGameSession_protectionPolicy,
    updateGameSession_gameSessionId,

    -- * Destructuring the Response
    UpdateGameSessionResponse (..),
    newUpdateGameSessionResponse,

    -- * Response Lenses
    updateGameSessionResponse_gameSession,
    updateGameSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newUpdateGameSession' smart constructor.
data UpdateGameSession = UpdateGameSession'
  { -- | Policy determining whether or not the game session accepts new players.
    playerSessionCreationPolicy :: Prelude.Maybe PlayerSessionCreationPolicy,
    -- | The maximum number of players that can be connected simultaneously to
    -- the game session.
    maximumPlayerSessionCount :: Prelude.Maybe Prelude.Natural,
    -- | A descriptive label that is associated with a game session. Session
    -- names do not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Game session protection policy to apply to this game session only.
    --
    -- -   __NoProtection__ -- The game session can be terminated during a
    --     scale-down event.
    --
    -- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
    --     it cannot be terminated during a scale-down event.
    protectionPolicy :: Prelude.Maybe ProtectionPolicy,
    -- | A unique identifier for the game session to update.
    gameSessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGameSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerSessionCreationPolicy', 'updateGameSession_playerSessionCreationPolicy' - Policy determining whether or not the game session accepts new players.
--
-- 'maximumPlayerSessionCount', 'updateGameSession_maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to
-- the game session.
--
-- 'name', 'updateGameSession_name' - A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
--
-- 'protectionPolicy', 'updateGameSession_protectionPolicy' - Game session protection policy to apply to this game session only.
--
-- -   __NoProtection__ -- The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
--
-- 'gameSessionId', 'updateGameSession_gameSessionId' - A unique identifier for the game session to update.
newUpdateGameSession ::
  -- | 'gameSessionId'
  Prelude.Text ->
  UpdateGameSession
newUpdateGameSession pGameSessionId_ =
  UpdateGameSession'
    { playerSessionCreationPolicy =
        Prelude.Nothing,
      maximumPlayerSessionCount = Prelude.Nothing,
      name = Prelude.Nothing,
      protectionPolicy = Prelude.Nothing,
      gameSessionId = pGameSessionId_
    }

-- | Policy determining whether or not the game session accepts new players.
updateGameSession_playerSessionCreationPolicy :: Lens.Lens' UpdateGameSession (Prelude.Maybe PlayerSessionCreationPolicy)
updateGameSession_playerSessionCreationPolicy = Lens.lens (\UpdateGameSession' {playerSessionCreationPolicy} -> playerSessionCreationPolicy) (\s@UpdateGameSession' {} a -> s {playerSessionCreationPolicy = a} :: UpdateGameSession)

-- | The maximum number of players that can be connected simultaneously to
-- the game session.
updateGameSession_maximumPlayerSessionCount :: Lens.Lens' UpdateGameSession (Prelude.Maybe Prelude.Natural)
updateGameSession_maximumPlayerSessionCount = Lens.lens (\UpdateGameSession' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@UpdateGameSession' {} a -> s {maximumPlayerSessionCount = a} :: UpdateGameSession)

-- | A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
updateGameSession_name :: Lens.Lens' UpdateGameSession (Prelude.Maybe Prelude.Text)
updateGameSession_name = Lens.lens (\UpdateGameSession' {name} -> name) (\s@UpdateGameSession' {} a -> s {name = a} :: UpdateGameSession)

-- | Game session protection policy to apply to this game session only.
--
-- -   __NoProtection__ -- The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
updateGameSession_protectionPolicy :: Lens.Lens' UpdateGameSession (Prelude.Maybe ProtectionPolicy)
updateGameSession_protectionPolicy = Lens.lens (\UpdateGameSession' {protectionPolicy} -> protectionPolicy) (\s@UpdateGameSession' {} a -> s {protectionPolicy = a} :: UpdateGameSession)

-- | A unique identifier for the game session to update.
updateGameSession_gameSessionId :: Lens.Lens' UpdateGameSession Prelude.Text
updateGameSession_gameSessionId = Lens.lens (\UpdateGameSession' {gameSessionId} -> gameSessionId) (\s@UpdateGameSession' {} a -> s {gameSessionId = a} :: UpdateGameSession)

instance Core.AWSRequest UpdateGameSession where
  type
    AWSResponse UpdateGameSession =
      UpdateGameSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGameSessionResponse'
            Prelude.<$> (x Core..?> "GameSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGameSession

instance Prelude.NFData UpdateGameSession

instance Core.ToHeaders UpdateGameSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.UpdateGameSession" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateGameSession where
  toJSON UpdateGameSession' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PlayerSessionCreationPolicy" Core..=)
              Prelude.<$> playerSessionCreationPolicy,
            ("MaximumPlayerSessionCount" Core..=)
              Prelude.<$> maximumPlayerSessionCount,
            ("Name" Core..=) Prelude.<$> name,
            ("ProtectionPolicy" Core..=)
              Prelude.<$> protectionPolicy,
            Prelude.Just
              ("GameSessionId" Core..= gameSessionId)
          ]
      )

instance Core.ToPath UpdateGameSession where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateGameSession where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newUpdateGameSessionResponse' smart constructor.
data UpdateGameSessionResponse = UpdateGameSessionResponse'
  { -- | The updated game session metadata.
    gameSession :: Prelude.Maybe GameSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGameSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSession', 'updateGameSessionResponse_gameSession' - The updated game session metadata.
--
-- 'httpStatus', 'updateGameSessionResponse_httpStatus' - The response's http status code.
newUpdateGameSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGameSessionResponse
newUpdateGameSessionResponse pHttpStatus_ =
  UpdateGameSessionResponse'
    { gameSession =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated game session metadata.
updateGameSessionResponse_gameSession :: Lens.Lens' UpdateGameSessionResponse (Prelude.Maybe GameSession)
updateGameSessionResponse_gameSession = Lens.lens (\UpdateGameSessionResponse' {gameSession} -> gameSession) (\s@UpdateGameSessionResponse' {} a -> s {gameSession = a} :: UpdateGameSessionResponse)

-- | The response's http status code.
updateGameSessionResponse_httpStatus :: Lens.Lens' UpdateGameSessionResponse Prelude.Int
updateGameSessionResponse_httpStatus = Lens.lens (\UpdateGameSessionResponse' {httpStatus} -> httpStatus) (\s@UpdateGameSessionResponse' {} a -> s {httpStatus = a} :: UpdateGameSessionResponse)

instance Prelude.NFData UpdateGameSessionResponse
