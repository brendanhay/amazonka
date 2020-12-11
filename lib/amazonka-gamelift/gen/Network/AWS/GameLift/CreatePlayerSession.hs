{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreatePlayerSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reserves an open player slot in an active game session. Before a player can be added, a game session must have an @ACTIVE@ status, have a creation policy of @ALLOW_ALL@ , and have an open player slot. To add a group of players to a game session, use 'CreatePlayerSessions' . When the player connects to the game server and references a player session ID, the game server contacts the Amazon GameLift service to validate the player reservation and accept the player.
--
-- To create a player session, specify a game session ID, player ID, and optionally a string of player data. If successful, a slot is reserved in the game session for the player and a new 'PlayerSession' object is returned. Player sessions cannot be updated.
-- /Available in Amazon GameLift Local./
--
--     * 'CreatePlayerSession'
--
--
--     * 'CreatePlayerSessions'
--
--
--     * 'DescribePlayerSessions'
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
module Network.AWS.GameLift.CreatePlayerSession
  ( -- * Creating a request
    CreatePlayerSession (..),
    mkCreatePlayerSession,

    -- ** Request lenses
    cPlayerData,
    cGameSessionId,
    cPlayerId,

    -- * Destructuring the response
    CreatePlayerSessionResponse (..),
    mkCreatePlayerSessionResponse,

    -- ** Response lenses
    cpsrsPlayerSession,
    cpsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreatePlayerSession' smart constructor.
data CreatePlayerSession = CreatePlayerSession'
  { playerData ::
      Lude.Maybe Lude.Text,
    gameSessionId :: Lude.Text,
    playerId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePlayerSession' with the minimum fields required to make a request.
--
-- * 'gameSessionId' - A unique identifier for the game session to add a player to.
-- * 'playerData' - Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
-- * 'playerId' - A unique identifier for a player. Player IDs are developer-defined.
mkCreatePlayerSession ::
  -- | 'gameSessionId'
  Lude.Text ->
  -- | 'playerId'
  Lude.Text ->
  CreatePlayerSession
mkCreatePlayerSession pGameSessionId_ pPlayerId_ =
  CreatePlayerSession'
    { playerData = Lude.Nothing,
      gameSessionId = pGameSessionId_,
      playerId = pPlayerId_
    }

-- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
--
-- /Note:/ Consider using 'playerData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPlayerData :: Lens.Lens' CreatePlayerSession (Lude.Maybe Lude.Text)
cPlayerData = Lens.lens (playerData :: CreatePlayerSession -> Lude.Maybe Lude.Text) (\s a -> s {playerData = a} :: CreatePlayerSession)
{-# DEPRECATED cPlayerData "Use generic-lens or generic-optics with 'playerData' instead." #-}

-- | A unique identifier for the game session to add a player to.
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cGameSessionId :: Lens.Lens' CreatePlayerSession Lude.Text
cGameSessionId = Lens.lens (gameSessionId :: CreatePlayerSession -> Lude.Text) (\s a -> s {gameSessionId = a} :: CreatePlayerSession)
{-# DEPRECATED cGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | A unique identifier for a player. Player IDs are developer-defined.
--
-- /Note:/ Consider using 'playerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPlayerId :: Lens.Lens' CreatePlayerSession Lude.Text
cPlayerId = Lens.lens (playerId :: CreatePlayerSession -> Lude.Text) (\s a -> s {playerId = a} :: CreatePlayerSession)
{-# DEPRECATED cPlayerId "Use generic-lens or generic-optics with 'playerId' instead." #-}

instance Lude.AWSRequest CreatePlayerSession where
  type Rs CreatePlayerSession = CreatePlayerSessionResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePlayerSessionResponse'
            Lude.<$> (x Lude..?> "PlayerSession")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePlayerSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.CreatePlayerSession" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePlayerSession where
  toJSON CreatePlayerSession' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PlayerData" Lude..=) Lude.<$> playerData,
            Lude.Just ("GameSessionId" Lude..= gameSessionId),
            Lude.Just ("PlayerId" Lude..= playerId)
          ]
      )

instance Lude.ToPath CreatePlayerSession where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePlayerSession where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreatePlayerSessionResponse' smart constructor.
data CreatePlayerSessionResponse = CreatePlayerSessionResponse'
  { playerSession ::
      Lude.Maybe PlayerSession,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePlayerSessionResponse' with the minimum fields required to make a request.
--
-- * 'playerSession' - Object that describes the newly created player session record.
-- * 'responseStatus' - The response status code.
mkCreatePlayerSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePlayerSessionResponse
mkCreatePlayerSessionResponse pResponseStatus_ =
  CreatePlayerSessionResponse'
    { playerSession = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Object that describes the newly created player session record.
--
-- /Note:/ Consider using 'playerSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsrsPlayerSession :: Lens.Lens' CreatePlayerSessionResponse (Lude.Maybe PlayerSession)
cpsrsPlayerSession = Lens.lens (playerSession :: CreatePlayerSessionResponse -> Lude.Maybe PlayerSession) (\s a -> s {playerSession = a} :: CreatePlayerSessionResponse)
{-# DEPRECATED cpsrsPlayerSession "Use generic-lens or generic-optics with 'playerSession' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsrsResponseStatus :: Lens.Lens' CreatePlayerSessionResponse Lude.Int
cpsrsResponseStatus = Lens.lens (responseStatus :: CreatePlayerSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePlayerSessionResponse)
{-# DEPRECATED cpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
