{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreatePlayerSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reserves open slots in a game session for a group of players. Before players can be added, a game session must have an @ACTIVE@ status, have a creation policy of @ALLOW_ALL@ , and have an open player slot. To add a single player to a game session, use 'CreatePlayerSession' . When a player connects to the game server and references a player session ID, the game server contacts the Amazon GameLift service to validate the player reservation and accept the player.
--
-- To create player sessions, specify a game session ID, a list of player IDs, and optionally a set of player data strings. If successful, a slot is reserved in the game session for each player and a set of new 'PlayerSession' objects is returned. Player sessions cannot be updated.
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
module Network.AWS.GameLift.CreatePlayerSessions
  ( -- * Creating a request
    CreatePlayerSessions (..),
    mkCreatePlayerSessions,

    -- ** Request lenses
    cpsGameSessionId,
    cpsPlayerDataMap,
    cpsPlayerIds,

    -- * Destructuring the response
    CreatePlayerSessionsResponse (..),
    mkCreatePlayerSessionsResponse,

    -- ** Response lenses
    cpsrsPlayerSessions,
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
-- /See:/ 'mkCreatePlayerSessions' smart constructor.
data CreatePlayerSessions = CreatePlayerSessions'
  { -- | A unique identifier for the game session to add players to.
    gameSessionId :: Lude.Text,
    -- | Map of string pairs, each specifying a player ID and a set of developer-defined information related to the player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game. Player data strings for player IDs not included in the @PlayerIds@ parameter are ignored.
    playerDataMap :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | List of unique identifiers for the players to be added.
    playerIds :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePlayerSessions' with the minimum fields required to make a request.
--
-- * 'gameSessionId' - A unique identifier for the game session to add players to.
-- * 'playerDataMap' - Map of string pairs, each specifying a player ID and a set of developer-defined information related to the player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game. Player data strings for player IDs not included in the @PlayerIds@ parameter are ignored.
-- * 'playerIds' - List of unique identifiers for the players to be added.
mkCreatePlayerSessions ::
  -- | 'gameSessionId'
  Lude.Text ->
  -- | 'playerIds'
  Lude.NonEmpty Lude.Text ->
  CreatePlayerSessions
mkCreatePlayerSessions pGameSessionId_ pPlayerIds_ =
  CreatePlayerSessions'
    { gameSessionId = pGameSessionId_,
      playerDataMap = Lude.Nothing,
      playerIds = pPlayerIds_
    }

-- | A unique identifier for the game session to add players to.
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsGameSessionId :: Lens.Lens' CreatePlayerSessions Lude.Text
cpsGameSessionId = Lens.lens (gameSessionId :: CreatePlayerSessions -> Lude.Text) (\s a -> s {gameSessionId = a} :: CreatePlayerSessions)
{-# DEPRECATED cpsGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | Map of string pairs, each specifying a player ID and a set of developer-defined information related to the player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game. Player data strings for player IDs not included in the @PlayerIds@ parameter are ignored.
--
-- /Note:/ Consider using 'playerDataMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsPlayerDataMap :: Lens.Lens' CreatePlayerSessions (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cpsPlayerDataMap = Lens.lens (playerDataMap :: CreatePlayerSessions -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {playerDataMap = a} :: CreatePlayerSessions)
{-# DEPRECATED cpsPlayerDataMap "Use generic-lens or generic-optics with 'playerDataMap' instead." #-}

-- | List of unique identifiers for the players to be added.
--
-- /Note:/ Consider using 'playerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsPlayerIds :: Lens.Lens' CreatePlayerSessions (Lude.NonEmpty Lude.Text)
cpsPlayerIds = Lens.lens (playerIds :: CreatePlayerSessions -> Lude.NonEmpty Lude.Text) (\s a -> s {playerIds = a} :: CreatePlayerSessions)
{-# DEPRECATED cpsPlayerIds "Use generic-lens or generic-optics with 'playerIds' instead." #-}

instance Lude.AWSRequest CreatePlayerSessions where
  type Rs CreatePlayerSessions = CreatePlayerSessionsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePlayerSessionsResponse'
            Lude.<$> (x Lude..?> "PlayerSessions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePlayerSessions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.CreatePlayerSessions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePlayerSessions where
  toJSON CreatePlayerSessions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("GameSessionId" Lude..= gameSessionId),
            ("PlayerDataMap" Lude..=) Lude.<$> playerDataMap,
            Lude.Just ("PlayerIds" Lude..= playerIds)
          ]
      )

instance Lude.ToPath CreatePlayerSessions where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePlayerSessions where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreatePlayerSessionsResponse' smart constructor.
data CreatePlayerSessionsResponse = CreatePlayerSessionsResponse'
  { -- | A collection of player session objects created for the added players.
    playerSessions :: Lude.Maybe [PlayerSession],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePlayerSessionsResponse' with the minimum fields required to make a request.
--
-- * 'playerSessions' - A collection of player session objects created for the added players.
-- * 'responseStatus' - The response status code.
mkCreatePlayerSessionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePlayerSessionsResponse
mkCreatePlayerSessionsResponse pResponseStatus_ =
  CreatePlayerSessionsResponse'
    { playerSessions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of player session objects created for the added players.
--
-- /Note:/ Consider using 'playerSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsrsPlayerSessions :: Lens.Lens' CreatePlayerSessionsResponse (Lude.Maybe [PlayerSession])
cpsrsPlayerSessions = Lens.lens (playerSessions :: CreatePlayerSessionsResponse -> Lude.Maybe [PlayerSession]) (\s a -> s {playerSessions = a} :: CreatePlayerSessionsResponse)
{-# DEPRECATED cpsrsPlayerSessions "Use generic-lens or generic-optics with 'playerSessions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsrsResponseStatus :: Lens.Lens' CreatePlayerSessionsResponse Lude.Int
cpsrsResponseStatus = Lens.lens (responseStatus :: CreatePlayerSessionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePlayerSessionsResponse)
{-# DEPRECATED cpsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
