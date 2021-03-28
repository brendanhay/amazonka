{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.StartMatchmaking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses FlexMatch to create a game match for a group of players based on custom matchmaking rules. If you're also using GameLift hosting, a new game session is started for the matched players. Each matchmaking request identifies one or more players to find a match for, and specifies the type of match to build, including the team configuration and the rules for an acceptable match. When a matchmaking request identifies a group of players who want to play together, FlexMatch finds additional players to fill the match. Match type, rules, and other features are defined in a @MatchmakingConfiguration@ . 
--
-- To start matchmaking, provide a unique ticket ID, specify a matchmaking configuration, and include the players to be matched. For each player, you must also include the player attribute values that are required by the matchmaking configuration (in the rule set). If successful, a matchmaking ticket is returned with status set to @QUEUED@ . 
-- Track the status of the ticket to respond as needed. If you're also using GameLift hosting, a successfully completed ticket contains game session connection information. Ticket status updates are tracked using event notification through Amazon Simple Notification Service (SNS), which is defined in the matchmaking configuration.
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-client.html Add FlexMatch to a Game Client> 
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Set Up FlexMatch Event Notification> 
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-tasks.html FlexMatch Integration Roadmap> 
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
module Network.AWS.GameLift.StartMatchmaking
    (
    -- * Creating a request
      StartMatchmaking (..)
    , mkStartMatchmaking
    -- ** Request lenses
    , sConfigurationName
    , sPlayers
    , sTicketId

    -- * Destructuring the response
    , StartMatchmakingResponse (..)
    , mkStartMatchmakingResponse
    -- ** Response lenses
    , srsMatchmakingTicket
    , srsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkStartMatchmaking' smart constructor.
data StartMatchmaking = StartMatchmaking'
  { configurationName :: Types.ConfigurationName
    -- ^ Name of the matchmaking configuration to use for this request. Matchmaking configurations must exist in the same Region as this request. You can use either the configuration name or ARN value.
  , players :: [Types.Player]
    -- ^ Information on each player to be matched. This information must include a player ID, and may contain player attributes and latency data to be used in the matchmaking process. After a successful match, @Player@ objects contain the name of the team the player is assigned to.
  , ticketId :: Core.Maybe Types.TicketId
    -- ^ A unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the matchmaking ticket status and retrieve match results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartMatchmaking' value with any optional fields omitted.
mkStartMatchmaking
    :: Types.ConfigurationName -- ^ 'configurationName'
    -> StartMatchmaking
mkStartMatchmaking configurationName
  = StartMatchmaking'{configurationName, players = Core.mempty,
                      ticketId = Core.Nothing}

-- | Name of the matchmaking configuration to use for this request. Matchmaking configurations must exist in the same Region as this request. You can use either the configuration name or ARN value.
--
-- /Note:/ Consider using 'configurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConfigurationName :: Lens.Lens' StartMatchmaking Types.ConfigurationName
sConfigurationName = Lens.field @"configurationName"
{-# INLINEABLE sConfigurationName #-}
{-# DEPRECATED configurationName "Use generic-lens or generic-optics with 'configurationName' instead"  #-}

-- | Information on each player to be matched. This information must include a player ID, and may contain player attributes and latency data to be used in the matchmaking process. After a successful match, @Player@ objects contain the name of the team the player is assigned to.
--
-- /Note:/ Consider using 'players' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPlayers :: Lens.Lens' StartMatchmaking [Types.Player]
sPlayers = Lens.field @"players"
{-# INLINEABLE sPlayers #-}
{-# DEPRECATED players "Use generic-lens or generic-optics with 'players' instead"  #-}

-- | A unique identifier for a matchmaking ticket. If no ticket ID is specified here, Amazon GameLift will generate one in the form of a UUID. Use this identifier to track the matchmaking ticket status and retrieve match results.
--
-- /Note:/ Consider using 'ticketId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTicketId :: Lens.Lens' StartMatchmaking (Core.Maybe Types.TicketId)
sTicketId = Lens.field @"ticketId"
{-# INLINEABLE sTicketId #-}
{-# DEPRECATED ticketId "Use generic-lens or generic-optics with 'ticketId' instead"  #-}

instance Core.ToQuery StartMatchmaking where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartMatchmaking where
        toHeaders StartMatchmaking{..}
          = Core.pure ("X-Amz-Target", "GameLift.StartMatchmaking") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartMatchmaking where
        toJSON StartMatchmaking{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConfigurationName" Core..= configurationName),
                  Core.Just ("Players" Core..= players),
                  ("TicketId" Core..=) Core.<$> ticketId])

instance Core.AWSRequest StartMatchmaking where
        type Rs StartMatchmaking = StartMatchmakingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartMatchmakingResponse' Core.<$>
                   (x Core..:? "MatchmakingTicket") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkStartMatchmakingResponse' smart constructor.
data StartMatchmakingResponse = StartMatchmakingResponse'
  { matchmakingTicket :: Core.Maybe Types.MatchmakingTicket
    -- ^ Ticket representing the matchmaking request. This object include the information included in the request, ticket status, and match results as generated during the matchmaking process.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartMatchmakingResponse' value with any optional fields omitted.
mkStartMatchmakingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartMatchmakingResponse
mkStartMatchmakingResponse responseStatus
  = StartMatchmakingResponse'{matchmakingTicket = Core.Nothing,
                              responseStatus}

-- | Ticket representing the matchmaking request. This object include the information included in the request, ticket status, and match results as generated during the matchmaking process.
--
-- /Note:/ Consider using 'matchmakingTicket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsMatchmakingTicket :: Lens.Lens' StartMatchmakingResponse (Core.Maybe Types.MatchmakingTicket)
srsMatchmakingTicket = Lens.field @"matchmakingTicket"
{-# INLINEABLE srsMatchmakingTicket #-}
{-# DEPRECATED matchmakingTicket "Use generic-lens or generic-optics with 'matchmakingTicket' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartMatchmakingResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
