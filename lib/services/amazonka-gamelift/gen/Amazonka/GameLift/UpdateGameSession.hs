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
-- Module      : Amazonka.GameLift.UpdateGameSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the mutable properties of a game session.
--
-- To update a game session, specify the game session ID and the values you
-- want to change.
--
-- If successful, the updated @GameSession@ object is returned.
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.UpdateGameSession
  ( -- * Creating a Request
    UpdateGameSession (..),
    newUpdateGameSession,

    -- * Request Lenses
    updateGameSession_maximumPlayerSessionCount,
    updateGameSession_name,
    updateGameSession_playerSessionCreationPolicy,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGameSession' smart constructor.
data UpdateGameSession = UpdateGameSession'
  { -- | The maximum number of players that can be connected simultaneously to
    -- the game session.
    maximumPlayerSessionCount :: Prelude.Maybe Prelude.Natural,
    -- | A descriptive label that is associated with a game session. Session
    -- names do not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | A policy that determines whether the game session is accepting new
    -- players.
    playerSessionCreationPolicy :: Prelude.Maybe PlayerSessionCreationPolicy,
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
-- 'maximumPlayerSessionCount', 'updateGameSession_maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to
-- the game session.
--
-- 'name', 'updateGameSession_name' - A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
--
-- 'playerSessionCreationPolicy', 'updateGameSession_playerSessionCreationPolicy' - A policy that determines whether the game session is accepting new
-- players.
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
    { maximumPlayerSessionCount =
        Prelude.Nothing,
      name = Prelude.Nothing,
      playerSessionCreationPolicy = Prelude.Nothing,
      protectionPolicy = Prelude.Nothing,
      gameSessionId = pGameSessionId_
    }

-- | The maximum number of players that can be connected simultaneously to
-- the game session.
updateGameSession_maximumPlayerSessionCount :: Lens.Lens' UpdateGameSession (Prelude.Maybe Prelude.Natural)
updateGameSession_maximumPlayerSessionCount = Lens.lens (\UpdateGameSession' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@UpdateGameSession' {} a -> s {maximumPlayerSessionCount = a} :: UpdateGameSession)

-- | A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
updateGameSession_name :: Lens.Lens' UpdateGameSession (Prelude.Maybe Prelude.Text)
updateGameSession_name = Lens.lens (\UpdateGameSession' {name} -> name) (\s@UpdateGameSession' {} a -> s {name = a} :: UpdateGameSession)

-- | A policy that determines whether the game session is accepting new
-- players.
updateGameSession_playerSessionCreationPolicy :: Lens.Lens' UpdateGameSession (Prelude.Maybe PlayerSessionCreationPolicy)
updateGameSession_playerSessionCreationPolicy = Lens.lens (\UpdateGameSession' {playerSessionCreationPolicy} -> playerSessionCreationPolicy) (\s@UpdateGameSession' {} a -> s {playerSessionCreationPolicy = a} :: UpdateGameSession)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGameSessionResponse'
            Prelude.<$> (x Data..?> "GameSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGameSession where
  hashWithSalt _salt UpdateGameSession' {..} =
    _salt
      `Prelude.hashWithSalt` maximumPlayerSessionCount
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` playerSessionCreationPolicy
      `Prelude.hashWithSalt` protectionPolicy
      `Prelude.hashWithSalt` gameSessionId

instance Prelude.NFData UpdateGameSession where
  rnf UpdateGameSession' {..} =
    Prelude.rnf maximumPlayerSessionCount `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf playerSessionCreationPolicy `Prelude.seq`
          Prelude.rnf protectionPolicy `Prelude.seq`
            Prelude.rnf gameSessionId

instance Data.ToHeaders UpdateGameSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.UpdateGameSession" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGameSession where
  toJSON UpdateGameSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaximumPlayerSessionCount" Data..=)
              Prelude.<$> maximumPlayerSessionCount,
            ("Name" Data..=) Prelude.<$> name,
            ("PlayerSessionCreationPolicy" Data..=)
              Prelude.<$> playerSessionCreationPolicy,
            ("ProtectionPolicy" Data..=)
              Prelude.<$> protectionPolicy,
            Prelude.Just
              ("GameSessionId" Data..= gameSessionId)
          ]
      )

instance Data.ToPath UpdateGameSession where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateGameSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGameSessionResponse' smart constructor.
data UpdateGameSessionResponse = UpdateGameSessionResponse'
  { -- | The updated game session properties.
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
-- 'gameSession', 'updateGameSessionResponse_gameSession' - The updated game session properties.
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

-- | The updated game session properties.
updateGameSessionResponse_gameSession :: Lens.Lens' UpdateGameSessionResponse (Prelude.Maybe GameSession)
updateGameSessionResponse_gameSession = Lens.lens (\UpdateGameSessionResponse' {gameSession} -> gameSession) (\s@UpdateGameSessionResponse' {} a -> s {gameSession = a} :: UpdateGameSessionResponse)

-- | The response's http status code.
updateGameSessionResponse_httpStatus :: Lens.Lens' UpdateGameSessionResponse Prelude.Int
updateGameSessionResponse_httpStatus = Lens.lens (\UpdateGameSessionResponse' {httpStatus} -> httpStatus) (\s@UpdateGameSessionResponse' {} a -> s {httpStatus = a} :: UpdateGameSessionResponse)

instance Prelude.NFData UpdateGameSessionResponse where
  rnf UpdateGameSessionResponse' {..} =
    Prelude.rnf gameSession `Prelude.seq`
      Prelude.rnf httpStatus
