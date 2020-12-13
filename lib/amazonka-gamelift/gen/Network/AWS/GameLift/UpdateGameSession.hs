{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateGameSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates game session properties. This includes the session name, maximum player count, protection policy, which controls whether or not an active game session can be terminated during a scale-down event, and the player session creation policy, which controls whether or not new players can join the session. To update a game session, specify the game session ID and the values you want to change. If successful, an updated 'GameSession' object is returned.
--
--
--     * 'CreateGameSession'
--
--
--     * 'DescribeGameSessions'
--
--
--     * 'DescribeGameSessionDetails'
--
--
--     * 'SearchGameSessions'
--
--
--     * 'UpdateGameSession'
--
--
--     * 'GetGameSessionLogUrl'
--
--
--     * Game session placements
--
--     * 'StartGameSessionPlacement'
--
--
--     * 'DescribeGameSessionPlacement'
--
--
--     * 'StopGameSessionPlacement'
module Network.AWS.GameLift.UpdateGameSession
  ( -- * Creating a request
    UpdateGameSession (..),
    mkUpdateGameSession,

    -- ** Request lenses
    ugsGameSessionId,
    ugsMaximumPlayerSessionCount,
    ugsPlayerSessionCreationPolicy,
    ugsName,
    ugsProtectionPolicy,

    -- * Destructuring the response
    UpdateGameSessionResponse (..),
    mkUpdateGameSessionResponse,

    -- ** Response lenses
    ursGameSession,
    ursResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateGameSession' smart constructor.
data UpdateGameSession = UpdateGameSession'
  { -- | A unique identifier for the game session to update.
    gameSessionId :: Lude.Text,
    -- | The maximum number of players that can be connected simultaneously to the game session.
    maximumPlayerSessionCount :: Lude.Maybe Lude.Natural,
    -- | Policy determining whether or not the game session accepts new players.
    playerSessionCreationPolicy :: Lude.Maybe PlayerSessionCreationPolicy,
    -- | A descriptive label that is associated with a game session. Session names do not need to be unique.
    name :: Lude.Maybe Lude.Text,
    -- | Game session protection policy to apply to this game session only.
    --
    --
    --     * __NoProtection__ -- The game session can be terminated during a scale-down event.
    --
    --
    --     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
    protectionPolicy :: Lude.Maybe ProtectionPolicy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGameSession' with the minimum fields required to make a request.
--
-- * 'gameSessionId' - A unique identifier for the game session to update.
-- * 'maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to the game session.
-- * 'playerSessionCreationPolicy' - Policy determining whether or not the game session accepts new players.
-- * 'name' - A descriptive label that is associated with a game session. Session names do not need to be unique.
-- * 'protectionPolicy' - Game session protection policy to apply to this game session only.
--
--
--     * __NoProtection__ -- The game session can be terminated during a scale-down event.
--
--
--     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
mkUpdateGameSession ::
  -- | 'gameSessionId'
  Lude.Text ->
  UpdateGameSession
mkUpdateGameSession pGameSessionId_ =
  UpdateGameSession'
    { gameSessionId = pGameSessionId_,
      maximumPlayerSessionCount = Lude.Nothing,
      playerSessionCreationPolicy = Lude.Nothing,
      name = Lude.Nothing,
      protectionPolicy = Lude.Nothing
    }

-- | A unique identifier for the game session to update.
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsGameSessionId :: Lens.Lens' UpdateGameSession Lude.Text
ugsGameSessionId = Lens.lens (gameSessionId :: UpdateGameSession -> Lude.Text) (\s a -> s {gameSessionId = a} :: UpdateGameSession)
{-# DEPRECATED ugsGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | The maximum number of players that can be connected simultaneously to the game session.
--
-- /Note:/ Consider using 'maximumPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsMaximumPlayerSessionCount :: Lens.Lens' UpdateGameSession (Lude.Maybe Lude.Natural)
ugsMaximumPlayerSessionCount = Lens.lens (maximumPlayerSessionCount :: UpdateGameSession -> Lude.Maybe Lude.Natural) (\s a -> s {maximumPlayerSessionCount = a} :: UpdateGameSession)
{-# DEPRECATED ugsMaximumPlayerSessionCount "Use generic-lens or generic-optics with 'maximumPlayerSessionCount' instead." #-}

-- | Policy determining whether or not the game session accepts new players.
--
-- /Note:/ Consider using 'playerSessionCreationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsPlayerSessionCreationPolicy :: Lens.Lens' UpdateGameSession (Lude.Maybe PlayerSessionCreationPolicy)
ugsPlayerSessionCreationPolicy = Lens.lens (playerSessionCreationPolicy :: UpdateGameSession -> Lude.Maybe PlayerSessionCreationPolicy) (\s a -> s {playerSessionCreationPolicy = a} :: UpdateGameSession)
{-# DEPRECATED ugsPlayerSessionCreationPolicy "Use generic-lens or generic-optics with 'playerSessionCreationPolicy' instead." #-}

-- | A descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsName :: Lens.Lens' UpdateGameSession (Lude.Maybe Lude.Text)
ugsName = Lens.lens (name :: UpdateGameSession -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateGameSession)
{-# DEPRECATED ugsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Game session protection policy to apply to this game session only.
--
--
--     * __NoProtection__ -- The game session can be terminated during a scale-down event.
--
--
--     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
--
--
-- /Note:/ Consider using 'protectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsProtectionPolicy :: Lens.Lens' UpdateGameSession (Lude.Maybe ProtectionPolicy)
ugsProtectionPolicy = Lens.lens (protectionPolicy :: UpdateGameSession -> Lude.Maybe ProtectionPolicy) (\s a -> s {protectionPolicy = a} :: UpdateGameSession)
{-# DEPRECATED ugsProtectionPolicy "Use generic-lens or generic-optics with 'protectionPolicy' instead." #-}

instance Lude.AWSRequest UpdateGameSession where
  type Rs UpdateGameSession = UpdateGameSessionResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGameSessionResponse'
            Lude.<$> (x Lude..?> "GameSession") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGameSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.UpdateGameSession" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGameSession where
  toJSON UpdateGameSession' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GameSessionId" Lude..= gameSessionId),
            ("MaximumPlayerSessionCount" Lude..=)
              Lude.<$> maximumPlayerSessionCount,
            ("PlayerSessionCreationPolicy" Lude..=)
              Lude.<$> playerSessionCreationPolicy,
            ("Name" Lude..=) Lude.<$> name,
            ("ProtectionPolicy" Lude..=) Lude.<$> protectionPolicy
          ]
      )

instance Lude.ToPath UpdateGameSession where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGameSession where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateGameSessionResponse' smart constructor.
data UpdateGameSessionResponse = UpdateGameSessionResponse'
  { -- | The updated game session metadata.
    gameSession :: Lude.Maybe GameSession,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGameSessionResponse' with the minimum fields required to make a request.
--
-- * 'gameSession' - The updated game session metadata.
-- * 'responseStatus' - The response status code.
mkUpdateGameSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGameSessionResponse
mkUpdateGameSessionResponse pResponseStatus_ =
  UpdateGameSessionResponse'
    { gameSession = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The updated game session metadata.
--
-- /Note:/ Consider using 'gameSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursGameSession :: Lens.Lens' UpdateGameSessionResponse (Lude.Maybe GameSession)
ursGameSession = Lens.lens (gameSession :: UpdateGameSessionResponse -> Lude.Maybe GameSession) (\s a -> s {gameSession = a} :: UpdateGameSessionResponse)
{-# DEPRECATED ursGameSession "Use generic-lens or generic-optics with 'gameSession' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateGameSessionResponse Lude.Int
ursResponseStatus = Lens.lens (responseStatus :: UpdateGameSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGameSessionResponse)
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
