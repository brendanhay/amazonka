{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
--
--
module Network.AWS.GameLift.CreateGameSession
    (
    -- * Creating a request
      CreateGameSession (..)
    , mkCreateGameSession
    -- ** Request lenses
    , cgsMaximumPlayerSessionCount
    , cgsAliasId
    , cgsCreatorId
    , cgsFleetId
    , cgsGameProperties
    , cgsGameSessionData
    , cgsGameSessionId
    , cgsIdempotencyToken
    , cgsName

    -- * Destructuring the response
    , CreateGameSessionResponse (..)
    , mkCreateGameSessionResponse
    -- ** Response lenses
    , cgsrrsGameSession
    , cgsrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateGameSession' smart constructor.
data CreateGameSession = CreateGameSession'
  { maximumPlayerSessionCount :: Core.Natural
    -- ^ The maximum number of players that can be connected simultaneously to the game session.
  , aliasId :: Core.Maybe Types.AliasId
    -- ^ A unique identifier for an alias associated with the fleet to create a game session in. You can use either the alias ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
  , creatorId :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A unique identifier for a player or entity creating the game session. This ID is used to enforce a resource protection policy (if one exists) that limits the number of concurrent active game sessions one player can have.
  , fleetId :: Core.Maybe Types.FleetIdOrArn
    -- ^ A unique identifier for a fleet to create a game session in. You can use either the fleet ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
  , gameProperties :: Core.Maybe [Types.GameProperty]
    -- ^ Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
  , gameSessionData :: Core.Maybe Types.GameSessionData
    -- ^ Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
  , gameSessionId :: Core.Maybe Types.IdStringModel
    -- ^ /This parameter is no longer preferred. Please use @IdempotencyToken@ instead./ Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .) 
  , idempotencyToken :: Core.Maybe Types.IdStringModel
    -- ^ Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .) Idempotency tokens remain in use for 30 days after a game session has ended; game session objects are retained for this time period and then deleted.
  , name :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A descriptive label that is associated with a game session. Session names do not need to be unique.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGameSession' value with any optional fields omitted.
mkCreateGameSession
    :: Core.Natural -- ^ 'maximumPlayerSessionCount'
    -> CreateGameSession
mkCreateGameSession maximumPlayerSessionCount
  = CreateGameSession'{maximumPlayerSessionCount,
                       aliasId = Core.Nothing, creatorId = Core.Nothing,
                       fleetId = Core.Nothing, gameProperties = Core.Nothing,
                       gameSessionData = Core.Nothing, gameSessionId = Core.Nothing,
                       idempotencyToken = Core.Nothing, name = Core.Nothing}

-- | The maximum number of players that can be connected simultaneously to the game session.
--
-- /Note:/ Consider using 'maximumPlayerSessionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsMaximumPlayerSessionCount :: Lens.Lens' CreateGameSession Core.Natural
cgsMaximumPlayerSessionCount = Lens.field @"maximumPlayerSessionCount"
{-# INLINEABLE cgsMaximumPlayerSessionCount #-}
{-# DEPRECATED maximumPlayerSessionCount "Use generic-lens or generic-optics with 'maximumPlayerSessionCount' instead"  #-}

-- | A unique identifier for an alias associated with the fleet to create a game session in. You can use either the alias ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsAliasId :: Lens.Lens' CreateGameSession (Core.Maybe Types.AliasId)
cgsAliasId = Lens.field @"aliasId"
{-# INLINEABLE cgsAliasId #-}
{-# DEPRECATED aliasId "Use generic-lens or generic-optics with 'aliasId' instead"  #-}

-- | A unique identifier for a player or entity creating the game session. This ID is used to enforce a resource protection policy (if one exists) that limits the number of concurrent active game sessions one player can have.
--
-- /Note:/ Consider using 'creatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsCreatorId :: Lens.Lens' CreateGameSession (Core.Maybe Types.NonZeroAndMaxString)
cgsCreatorId = Lens.field @"creatorId"
{-# INLINEABLE cgsCreatorId #-}
{-# DEPRECATED creatorId "Use generic-lens or generic-optics with 'creatorId' instead"  #-}

-- | A unique identifier for a fleet to create a game session in. You can use either the fleet ID or ARN value. Each request must reference either a fleet ID or alias ID, but not both.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsFleetId :: Lens.Lens' CreateGameSession (Core.Maybe Types.FleetIdOrArn)
cgsFleetId = Lens.field @"fleetId"
{-# INLINEABLE cgsFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsGameProperties :: Lens.Lens' CreateGameSession (Core.Maybe [Types.GameProperty])
cgsGameProperties = Lens.field @"gameProperties"
{-# INLINEABLE cgsGameProperties #-}
{-# DEPRECATED gameProperties "Use generic-lens or generic-optics with 'gameProperties' instead"  #-}

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- /Note:/ Consider using 'gameSessionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsGameSessionData :: Lens.Lens' CreateGameSession (Core.Maybe Types.GameSessionData)
cgsGameSessionData = Lens.field @"gameSessionData"
{-# INLINEABLE cgsGameSessionData #-}
{-# DEPRECATED gameSessionData "Use generic-lens or generic-optics with 'gameSessionData' instead"  #-}

-- | /This parameter is no longer preferred. Please use @IdempotencyToken@ instead./ Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .) 
--
-- /Note:/ Consider using 'gameSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsGameSessionId :: Lens.Lens' CreateGameSession (Core.Maybe Types.IdStringModel)
cgsGameSessionId = Lens.field @"gameSessionId"
{-# INLINEABLE cgsGameSessionId #-}
{-# DEPRECATED gameSessionId "Use generic-lens or generic-optics with 'gameSessionId' instead"  #-}

-- | Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .) Idempotency tokens remain in use for 30 days after a game session has ended; game session objects are retained for this time period and then deleted.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsIdempotencyToken :: Lens.Lens' CreateGameSession (Core.Maybe Types.IdStringModel)
cgsIdempotencyToken = Lens.field @"idempotencyToken"
{-# INLINEABLE cgsIdempotencyToken #-}
{-# DEPRECATED idempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead"  #-}

-- | A descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsName :: Lens.Lens' CreateGameSession (Core.Maybe Types.NonZeroAndMaxString)
cgsName = Lens.field @"name"
{-# INLINEABLE cgsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery CreateGameSession where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateGameSession where
        toHeaders CreateGameSession{..}
          = Core.pure ("X-Amz-Target", "GameLift.CreateGameSession") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateGameSession where
        toJSON CreateGameSession{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("MaximumPlayerSessionCount" Core..= maximumPlayerSessionCount),
                  ("AliasId" Core..=) Core.<$> aliasId,
                  ("CreatorId" Core..=) Core.<$> creatorId,
                  ("FleetId" Core..=) Core.<$> fleetId,
                  ("GameProperties" Core..=) Core.<$> gameProperties,
                  ("GameSessionData" Core..=) Core.<$> gameSessionData,
                  ("GameSessionId" Core..=) Core.<$> gameSessionId,
                  ("IdempotencyToken" Core..=) Core.<$> idempotencyToken,
                  ("Name" Core..=) Core.<$> name])

instance Core.AWSRequest CreateGameSession where
        type Rs CreateGameSession = CreateGameSessionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateGameSessionResponse' Core.<$>
                   (x Core..:? "GameSession") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateGameSessionResponse' smart constructor.
data CreateGameSessionResponse = CreateGameSessionResponse'
  { gameSession :: Core.Maybe Types.GameSession
    -- ^ Object that describes the newly created game session record.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateGameSessionResponse' value with any optional fields omitted.
mkCreateGameSessionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateGameSessionResponse
mkCreateGameSessionResponse responseStatus
  = CreateGameSessionResponse'{gameSession = Core.Nothing,
                               responseStatus}

-- | Object that describes the newly created game session record.
--
-- /Note:/ Consider using 'gameSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsrrsGameSession :: Lens.Lens' CreateGameSessionResponse (Core.Maybe Types.GameSession)
cgsrrsGameSession = Lens.field @"gameSession"
{-# INLINEABLE cgsrrsGameSession #-}
{-# DEPRECATED gameSession "Use generic-lens or generic-optics with 'gameSession' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsrrsResponseStatus :: Lens.Lens' CreateGameSessionResponse Core.Int
cgsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cgsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
