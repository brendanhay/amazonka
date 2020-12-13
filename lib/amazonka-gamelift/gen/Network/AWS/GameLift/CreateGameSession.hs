{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateGameSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a multiplayer game session for players. This operation creates a game session record and assigns an available server process in the specified fleet to host the game session. A fleet must have an @ACTIVE@ status before a game session can be created in it.
--
-- To create a game session, specify either fleet ID or alias ID and indicate a maximum number of players to allow in the game session. You can also provide a name and game-specific properties for this game session. If successful, a 'GameSession' object is returned containing the game session properties and other settings you specified.
-- __Idempotency tokens.__ You can add a token that uniquely identifies game session requests. This is useful for ensuring that game session requests are idempotent. Multiple requests with the same idempotency token are processed only once; subsequent requests return the original result. All response values are the same with the exception of game session status, which may change.
-- __Resource creation limits.__ If you are creating a game session on a fleet with a resource creation limit policy in force, then you must specify a creator ID. Without this ID, Amazon GameLift has no way to evaluate the policy for this new game session request.
-- __Player acceptance policy.__ By default, newly created game sessions are open to new players. You can restrict new player access by using 'UpdateGameSession' to change the game session's player session creation policy.
-- __Game session logs.__ Logs are retained for all active game sessions for 14 days. To access the logs, call 'GetGameSessionLogUrl' to download the log files.
-- /Available in Amazon GameLift Local./
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
module Network.AWS.GameLift.CreateGameSession
  ( -- * Creating a request
    CreateGameSession (..),
    mkCreateGameSession,

    -- ** Request lenses
    cgsIdempotencyToken,
    cgsGameProperties,
    cgsGameSessionId,
    cgsAliasId,
    cgsMaximumPlayerSessionCount,
    cgsName,
    cgsGameSessionData,
    cgsFleetId,
    cgsCreatorId,

    -- * Destructuring the response
    CreateGameSessionResponse (..),
    mkCreateGameSessionResponse,

    -- ** Response lenses
    cgsrsGameSession,
    cgsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateGameSession' smart constructor.
data CreateGameSession = CreateGameSession'
  { -- | Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .) Idempotency tokens remain in use for 30 days after a game session has ended; game session objects are retained for this time period and then deleted.
    idempotencyToken :: Lude.Maybe Lude.Text,
    -- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
    gameProperties :: Lude.Maybe [GameProperty],
    -- | /This parameter is no longer preferred. Please use @IdempotencyToken@ instead./ Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .)
    gameSessionId :: Lude.Maybe Lude.Text,
    -- | A unique identifier for an alias associated with the fleet to create a game session in. You can use either the alias ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
    aliasId :: Lude.Maybe Lude.Text,
    -- | The maximum number of players that can be connected simultaneously to the game session.
    maximumPlayerSessionCount :: Lude.Natural,
    -- | A descriptive label that is associated with a game session. Session names do not need to be unique.
    name :: Lude.Maybe Lude.Text,
    -- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
    gameSessionData :: Lude.Maybe Lude.Text,
    -- | A unique identifier for a fleet to create a game session in. You can use either the fleet ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
    fleetId :: Lude.Maybe Lude.Text,
    -- | A unique identifier for a player or entity creating the game session. This ID is used to enforce a resource protection policy (if one exists) that limits the number of concurrent active game sessions one player can have.
    creatorId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGameSession' with the minimum fields required to make a request.
--
-- * 'idempotencyToken' - Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .) Idempotency tokens remain in use for 30 days after a game session has ended; game session objects are retained for this time period and then deleted.
-- * 'gameProperties' - Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
-- * 'gameSessionId' - /This parameter is no longer preferred. Please use @IdempotencyToken@ instead./ Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .)
-- * 'aliasId' - A unique identifier for an alias associated with the fleet to create a game session in. You can use either the alias ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
-- * 'maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to the game session.
-- * 'name' - A descriptive label that is associated with a game session. Session names do not need to be unique.
-- * 'gameSessionData' - Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
-- * 'fleetId' - A unique identifier for a fleet to create a game session in. You can use either the fleet ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
-- * 'creatorId' - A unique identifier for a player or entity creating the game session. This ID is used to enforce a resource protection policy (if one exists) that limits the number of concurrent active game sessions one player can have.
mkCreateGameSession ::
  -- | 'maximumPlayerSessionCount'
  Lude.Natural ->
  CreateGameSession
mkCreateGameSession pMaximumPlayerSessionCount_ =
  CreateGameSession'
    { idempotencyToken = Lude.Nothing,
      gameProperties = Lude.Nothing,
      gameSessionId = Lude.Nothing,
      aliasId = Lude.Nothing,
      maximumPlayerSessionCount = pMaximumPlayerSessionCount_,
      name = Lude.Nothing,
      gameSessionData = Lude.Nothing,
      fleetId = Lude.Nothing,
      creatorId = Lude.Nothing
    }

-- | Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .) Idempotency tokens remain in use for 30 days after a game session has ended; game session objects are retained for this time period and then deleted.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsIdempotencyToken :: Lens.Lens' CreateGameSession (Lude.Maybe Lude.Text)
cgsIdempotencyToken = Lens.lens (idempotencyToken :: CreateGameSession -> Lude.Maybe Lude.Text) (\s a -> s {idempotencyToken = a} :: CreateGameSession)
{-# DEPRECATED cgsIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsGameProperties :: Lens.Lens' CreateGameSession (Lude.Maybe [GameProperty])
cgsGameProperties = Lens.lens (gameProperties :: CreateGameSession -> Lude.Maybe [GameProperty]) (\s a -> s {gameProperties = a} :: CreateGameSession)
{-# DEPRECATED cgsGameProperties "Use generic-lens or generic-optics with 'gameProperties' instead." #-}

-- | /This parameter is no longer preferred. Please use @IdempotencyToken@ instead./ Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .)
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsGameSessionId :: Lens.Lens' CreateGameSession (Lude.Maybe Lude.Text)
cgsGameSessionId = Lens.lens (gameSessionId :: CreateGameSession -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionId = a} :: CreateGameSession)
{-# DEPRECATED cgsGameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead." #-}

-- | A unique identifier for an alias associated with the fleet to create a game session in. You can use either the alias ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsAliasId :: Lens.Lens' CreateGameSession (Lude.Maybe Lude.Text)
cgsAliasId = Lens.lens (aliasId :: CreateGameSession -> Lude.Maybe Lude.Text) (\s a -> s {aliasId = a} :: CreateGameSession)
{-# DEPRECATED cgsAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | The maximum number of players that can be connected simultaneously to the game session.
--
-- /Note:/ Consider using 'maximumPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsMaximumPlayerSessionCount :: Lens.Lens' CreateGameSession Lude.Natural
cgsMaximumPlayerSessionCount = Lens.lens (maximumPlayerSessionCount :: CreateGameSession -> Lude.Natural) (\s a -> s {maximumPlayerSessionCount = a} :: CreateGameSession)
{-# DEPRECATED cgsMaximumPlayerSessionCount "Use generic-lens or generic-optics with 'maximumPlayerSessionCount' instead." #-}

-- | A descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsName :: Lens.Lens' CreateGameSession (Lude.Maybe Lude.Text)
cgsName = Lens.lens (name :: CreateGameSession -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateGameSession)
{-# DEPRECATED cgsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsGameSessionData :: Lens.Lens' CreateGameSession (Lude.Maybe Lude.Text)
cgsGameSessionData = Lens.lens (gameSessionData :: CreateGameSession -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionData = a} :: CreateGameSession)
{-# DEPRECATED cgsGameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead." #-}

-- | A unique identifier for a fleet to create a game session in. You can use either the fleet ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsFleetId :: Lens.Lens' CreateGameSession (Lude.Maybe Lude.Text)
cgsFleetId = Lens.lens (fleetId :: CreateGameSession -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: CreateGameSession)
{-# DEPRECATED cgsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | A unique identifier for a player or entity creating the game session. This ID is used to enforce a resource protection policy (if one exists) that limits the number of concurrent active game sessions one player can have.
--
-- /Note:/ Consider using 'creatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsCreatorId :: Lens.Lens' CreateGameSession (Lude.Maybe Lude.Text)
cgsCreatorId = Lens.lens (creatorId :: CreateGameSession -> Lude.Maybe Lude.Text) (\s a -> s {creatorId = a} :: CreateGameSession)
{-# DEPRECATED cgsCreatorId "Use generic-lens or generic-optics with 'creatorId' instead." #-}

instance Lude.AWSRequest CreateGameSession where
  type Rs CreateGameSession = CreateGameSessionResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGameSessionResponse'
            Lude.<$> (x Lude..?> "GameSession") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGameSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.CreateGameSession" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateGameSession where
  toJSON CreateGameSession' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IdempotencyToken" Lude..=) Lude.<$> idempotencyToken,
            ("GameProperties" Lude..=) Lude.<$> gameProperties,
            ("GameSessionId" Lude..=) Lude.<$> gameSessionId,
            ("AliasId" Lude..=) Lude.<$> aliasId,
            Lude.Just
              ("MaximumPlayerSessionCount" Lude..= maximumPlayerSessionCount),
            ("Name" Lude..=) Lude.<$> name,
            ("GameSessionData" Lude..=) Lude.<$> gameSessionData,
            ("FleetId" Lude..=) Lude.<$> fleetId,
            ("CreatorId" Lude..=) Lude.<$> creatorId
          ]
      )

instance Lude.ToPath CreateGameSession where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateGameSession where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateGameSessionResponse' smart constructor.
data CreateGameSessionResponse = CreateGameSessionResponse'
  { -- | Object that describes the newly created game session record.
    gameSession :: Lude.Maybe GameSession,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGameSessionResponse' with the minimum fields required to make a request.
--
-- * 'gameSession' - Object that describes the newly created game session record.
-- * 'responseStatus' - The response status code.
mkCreateGameSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGameSessionResponse
mkCreateGameSessionResponse pResponseStatus_ =
  CreateGameSessionResponse'
    { gameSession = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Object that describes the newly created game session record.
--
-- /Note:/ Consider using 'gameSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsrsGameSession :: Lens.Lens' CreateGameSessionResponse (Lude.Maybe GameSession)
cgsrsGameSession = Lens.lens (gameSession :: CreateGameSessionResponse -> Lude.Maybe GameSession) (\s a -> s {gameSession = a} :: CreateGameSessionResponse)
{-# DEPRECATED cgsrsGameSession "Use generic-lens or generic-optics with 'gameSession' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsrsResponseStatus :: Lens.Lens' CreateGameSessionResponse Lude.Int
cgsrsResponseStatus = Lens.lens (responseStatus :: CreateGameSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGameSessionResponse)
{-# DEPRECATED cgsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
