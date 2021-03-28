{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.StartMatchBackfill
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds new players to fill open slots in an existing game session. This operation can be used to add players to matched games that start with fewer than the maximum number of players or to replace players when they drop out. By backfilling with the same matchmaker used to create the original match, you ensure that new players meet the match criteria and maintain a consistent experience throughout the game session. You can backfill a match anytime after a game session has been created. 
--
-- To request a match backfill, specify a unique ticket ID, the existing game session's ARN, a matchmaking configuration, and a set of data that describes all current players in the game session. If successful, a match backfill ticket is created and returned with status set to QUEUED. The ticket is placed in the matchmaker's ticket pool and processed. Track the status of the ticket to respond as needed. 
-- The process of finding backfill matches is essentially identical to the initial matchmaking process. The matchmaker searches the pool and groups tickets together to form potential matches, allowing only one backfill ticket per potential match. Once the a match is formed, the matchmaker creates player sessions for the new players. All tickets in the match are updated with the game session's connection information, and the 'GameSession' object is updated to include matchmaker data on the new players. For more detail on how match backfill requests are processed, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/gamelift-match.html How Amazon GameLift FlexMatch Works> . 
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> 
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/gamelift-match.html How GameLift FlexMatch Works> 
-- __Related operations__ 
--
--     * 'StartMatchmaking' 
--
--
--     * 'DescribeMatchmaking' 
--
--
--     * 'StopMatchmaking' 
--
--
--     * 'AcceptMatch' 
--
--
--     * 'StartMatchBackfill' 
--
--
module Network.AWS.GameLift.StartMatchBackfill
    (
    -- * Creating a request
      StartMatchBackfill (..)
    , mkStartMatchBackfill
    -- ** Request lenses
    , smbConfigurationName
    , smbPlayers
    , smbGameSessionArn
    , smbTicketId

    -- * Destructuring the response
    , StartMatchBackfillResponse (..)
    , mkStartMatchBackfillResponse
    -- ** Response lenses
    , smbrrsMatchmakingTicket
    , smbrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkStartMatchBackfill' smart constructor.
data StartMatchBackfill = StartMatchBackfill'
  { configurationName :: Types.MatchmakingConfigurationName
    -- ^ Name of the matchmaker to use for this request. You can use either the configuration name or ARN value. The ARN of the matchmaker that was used with the original game session is listed in the 'GameSession' object, @MatchmakerData@ property.
  , players :: [Types.Player]
    -- ^ Match information on all players that are currently assigned to the game session. This information is used by the matchmaker to find new players and add them to the existing game.
--
--
--     * PlayerID, PlayerAttributes, Team -\\- This information is maintained in the 'GameSession' object, @MatchmakerData@ property, for all players who are currently assigned to the game session. The matchmaker data is in JSON syntax, formatted as a string. For more details, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> . 
--
--
--     * LatencyInMs -\\- If the matchmaker uses player latency, include a latency value, in milliseconds, for the Region that the game session is currently in. Do not include latency values for any other Region.
--
--
  , gameSessionArn :: Core.Maybe Types.ArnStringModel
    -- ^ Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a game session and uniquely identifies it. This is the same as the game session ID.
  , ticketId :: Core.Maybe Types.MatchmakingIdStringModel
    -- ^ A unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the match backfill ticket status and retrieve match results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartMatchBackfill' value with any optional fields omitted.
mkStartMatchBackfill
    :: Types.MatchmakingConfigurationName -- ^ 'configurationName'
    -> StartMatchBackfill
mkStartMatchBackfill configurationName
  = StartMatchBackfill'{configurationName, players = Core.mempty,
                        gameSessionArn = Core.Nothing, ticketId = Core.Nothing}

-- | Name of the matchmaker to use for this request. You can use either the configuration name or ARN value. The ARN of the matchmaker that was used with the original game session is listed in the 'GameSession' object, @MatchmakerData@ property.
--
-- /Note:/ Consider using 'configurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbConfigurationName :: Lens.Lens' StartMatchBackfill Types.MatchmakingConfigurationName
smbConfigurationName = Lens.field @"configurationName"
{-# INLINEABLE smbConfigurationName #-}
{-# DEPRECATED configurationName "Use generic-lens or generic-optics with 'configurationName' instead"  #-}

-- | Match information on all players that are currently assigned to the game session. This information is used by the matchmaker to find new players and add them to the existing game.
--
--
--     * PlayerID, PlayerAttributes, Team -\\- This information is maintained in the 'GameSession' object, @MatchmakerData@ property, for all players who are currently assigned to the game session. The matchmaker data is in JSON syntax, formatted as a string. For more details, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> . 
--
--
--     * LatencyInMs -\\- If the matchmaker uses player latency, include a latency value, in milliseconds, for the Region that the game session is currently in. Do not include latency values for any other Region.
--
--
--
-- /Note:/ Consider using 'players' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbPlayers :: Lens.Lens' StartMatchBackfill [Types.Player]
smbPlayers = Lens.field @"players"
{-# INLINEABLE smbPlayers #-}
{-# DEPRECATED players "Use generic-lens or generic-optics with 'players' instead"  #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a game session and uniquely identifies it. This is the same as the game session ID.
--
-- /Note:/ Consider using 'gameSessionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbGameSessionArn :: Lens.Lens' StartMatchBackfill (Core.Maybe Types.ArnStringModel)
smbGameSessionArn = Lens.field @"gameSessionArn"
{-# INLINEABLE smbGameSessionArn #-}
{-# DEPRECATED gameSessionArn "Use generic-lens or generic-optics with 'gameSessionArn' instead"  #-}

-- | A unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the match backfill ticket status and retrieve match results.
--
-- /Note:/ Consider using 'ticketId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbTicketId :: Lens.Lens' StartMatchBackfill (Core.Maybe Types.MatchmakingIdStringModel)
smbTicketId = Lens.field @"ticketId"
{-# INLINEABLE smbTicketId #-}
{-# DEPRECATED ticketId "Use generic-lens or generic-optics with 'ticketId' instead"  #-}

instance Core.ToQuery StartMatchBackfill where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartMatchBackfill where
        toHeaders StartMatchBackfill{..}
          = Core.pure ("X-Amz-Target", "GameLift.StartMatchBackfill") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartMatchBackfill where
        toJSON StartMatchBackfill{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConfigurationName" Core..= configurationName),
                  Core.Just ("Players" Core..= players),
                  ("GameSessionArn" Core..=) Core.<$> gameSessionArn,
                  ("TicketId" Core..=) Core.<$> ticketId])

instance Core.AWSRequest StartMatchBackfill where
        type Rs StartMatchBackfill = StartMatchBackfillResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartMatchBackfillResponse' Core.<$>
                   (x Core..:? "MatchmakingTicket") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkStartMatchBackfillResponse' smart constructor.
data StartMatchBackfillResponse = StartMatchBackfillResponse'
  { matchmakingTicket :: Core.Maybe Types.MatchmakingTicket
    -- ^ Ticket representing the backfill matchmaking request. This object includes the information in the request, ticket status, and match results as generated during the matchmaking process.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartMatchBackfillResponse' value with any optional fields omitted.
mkStartMatchBackfillResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartMatchBackfillResponse
mkStartMatchBackfillResponse responseStatus
  = StartMatchBackfillResponse'{matchmakingTicket = Core.Nothing,
                                responseStatus}

-- | Ticket representing the backfill matchmaking request. This object includes the information in the request, ticket status, and match results as generated during the matchmaking process.
--
-- /Note:/ Consider using 'matchmakingTicket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbrrsMatchmakingTicket :: Lens.Lens' StartMatchBackfillResponse (Core.Maybe Types.MatchmakingTicket)
smbrrsMatchmakingTicket = Lens.field @"matchmakingTicket"
{-# INLINEABLE smbrrsMatchmakingTicket #-}
{-# DEPRECATED matchmakingTicket "Use generic-lens or generic-optics with 'matchmakingTicket' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbrrsResponseStatus :: Lens.Lens' StartMatchBackfillResponse Core.Int
smbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE smbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
