{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchmakingTicket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.MatchmakingTicket
  ( MatchmakingTicket (..)
  -- * Smart constructor
  , mkMatchmakingTicket
  -- * Lenses
  , mtConfigurationArn
  , mtConfigurationName
  , mtEndTime
  , mtEstimatedWaitTime
  , mtGameSessionConnectionInfo
  , mtPlayers
  , mtStartTime
  , mtStatus
  , mtStatusMessage
  , mtStatusReason
  , mtTicketId
  ) where

import qualified Network.AWS.GameLift.Types.ConfigurationArn as Types
import qualified Network.AWS.GameLift.Types.ConfigurationName as Types
import qualified Network.AWS.GameLift.Types.GameSessionConnectionInfo as Types
import qualified Network.AWS.GameLift.Types.MatchmakingConfigurationStatus as Types
import qualified Network.AWS.GameLift.Types.Player as Types
import qualified Network.AWS.GameLift.Types.StringModel as Types
import qualified Network.AWS.GameLift.Types.TicketId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Ticket generated to track the progress of a matchmaking request. Each ticket is uniquely identified by a ticket ID, supplied by the requester, when creating a matchmaking request with 'StartMatchmaking' . Tickets can be retrieved by calling 'DescribeMatchmaking' with the ticket ID.
--
-- /See:/ 'mkMatchmakingTicket' smart constructor.
data MatchmakingTicket = MatchmakingTicket'
  { configurationArn :: Core.Maybe Types.ConfigurationArn
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift matchmaking configuration resource that is used with this ticket.
  , configurationName :: Core.Maybe Types.ConfigurationName
    -- ^ Name of the 'MatchmakingConfiguration' that is used with this ticket. Matchmaking configurations determine how players are grouped into a match and how a new game session is created for the match.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Time stamp indicating when this matchmaking request stopped being processed due to success, failure, or cancellation. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
  , estimatedWaitTime :: Core.Maybe Core.Natural
    -- ^ Average amount of time (in seconds) that players are currently waiting for a match. If there is not enough recent data, this property may be empty.
  , gameSessionConnectionInfo :: Core.Maybe Types.GameSessionConnectionInfo
    -- ^ Identifier and connection information of the game session created for the match. This information is added to the ticket only after the matchmaking request has been successfully completed. This parameter is not set when FlexMatch is being used without GameLift hosting.
  , players :: Core.Maybe [Types.Player]
    -- ^ A set of @Player@ objects, each representing a player to find matches for. Players are identified by a unique player ID and may include latency data for use during matchmaking. If the ticket is in status @COMPLETED@ , the @Player@ objects include the team the players were assigned to in the resulting match.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Time stamp indicating when this matchmaking request was received. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
  , status :: Core.Maybe Types.MatchmakingConfigurationStatus
    -- ^ Current status of the matchmaking request.
--
--
--     * __QUEUED__ -- The matchmaking request has been received and is currently waiting to be processed.
--
--
--     * __SEARCHING__ -- The matchmaking request is currently being processed. 
--
--
--     * __REQUIRES_ACCEPTANCE__ -- A match has been proposed and the players must accept the match (see 'AcceptMatch' ). This status is used only with requests that use a matchmaking configuration with a player acceptance requirement.
--
--
--     * __PLACING__ -- The FlexMatch engine has matched players and is in the process of placing a new game session for the match.
--
--
--     * __COMPLETED__ -- Players have been matched and a game session is ready to host the players. A ticket in this state contains the necessary connection information for players.
--
--
--     * __FAILED__ -- The matchmaking request was not completed.
--
--
--     * __CANCELLED__ -- The matchmaking request was canceled. This may be the result of a call to 'StopMatchmaking' or a proposed match that one or more players failed to accept.
--
--
--     * __TIMED_OUT__ -- The matchmaking request was not successful within the duration specified in the matchmaking configuration. 
--
--
  , statusMessage :: Core.Maybe Types.StringModel
    -- ^ Additional information about the current status.
  , statusReason :: Core.Maybe Types.StringModel
    -- ^ Code to explain the current status. For example, a status reason may indicate when a ticket has returned to @SEARCHING@ status after a proposed match fails to receive player acceptances.
  , ticketId :: Core.Maybe Types.TicketId
    -- ^ A unique identifier for a matchmaking ticket.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MatchmakingTicket' value with any optional fields omitted.
mkMatchmakingTicket
    :: MatchmakingTicket
mkMatchmakingTicket
  = MatchmakingTicket'{configurationArn = Core.Nothing,
                       configurationName = Core.Nothing, endTime = Core.Nothing,
                       estimatedWaitTime = Core.Nothing,
                       gameSessionConnectionInfo = Core.Nothing, players = Core.Nothing,
                       startTime = Core.Nothing, status = Core.Nothing,
                       statusMessage = Core.Nothing, statusReason = Core.Nothing,
                       ticketId = Core.Nothing}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift matchmaking configuration resource that is used with this ticket.
--
-- /Note:/ Consider using 'configurationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtConfigurationArn :: Lens.Lens' MatchmakingTicket (Core.Maybe Types.ConfigurationArn)
mtConfigurationArn = Lens.field @"configurationArn"
{-# INLINEABLE mtConfigurationArn #-}
{-# DEPRECATED configurationArn "Use generic-lens or generic-optics with 'configurationArn' instead"  #-}

-- | Name of the 'MatchmakingConfiguration' that is used with this ticket. Matchmaking configurations determine how players are grouped into a match and how a new game session is created for the match.
--
-- /Note:/ Consider using 'configurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtConfigurationName :: Lens.Lens' MatchmakingTicket (Core.Maybe Types.ConfigurationName)
mtConfigurationName = Lens.field @"configurationName"
{-# INLINEABLE mtConfigurationName #-}
{-# DEPRECATED configurationName "Use generic-lens or generic-optics with 'configurationName' instead"  #-}

-- | Time stamp indicating when this matchmaking request stopped being processed due to success, failure, or cancellation. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtEndTime :: Lens.Lens' MatchmakingTicket (Core.Maybe Core.NominalDiffTime)
mtEndTime = Lens.field @"endTime"
{-# INLINEABLE mtEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | Average amount of time (in seconds) that players are currently waiting for a match. If there is not enough recent data, this property may be empty.
--
-- /Note:/ Consider using 'estimatedWaitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtEstimatedWaitTime :: Lens.Lens' MatchmakingTicket (Core.Maybe Core.Natural)
mtEstimatedWaitTime = Lens.field @"estimatedWaitTime"
{-# INLINEABLE mtEstimatedWaitTime #-}
{-# DEPRECATED estimatedWaitTime "Use generic-lens or generic-optics with 'estimatedWaitTime' instead"  #-}

-- | Identifier and connection information of the game session created for the match. This information is added to the ticket only after the matchmaking request has been successfully completed. This parameter is not set when FlexMatch is being used without GameLift hosting.
--
-- /Note:/ Consider using 'gameSessionConnectionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtGameSessionConnectionInfo :: Lens.Lens' MatchmakingTicket (Core.Maybe Types.GameSessionConnectionInfo)
mtGameSessionConnectionInfo = Lens.field @"gameSessionConnectionInfo"
{-# INLINEABLE mtGameSessionConnectionInfo #-}
{-# DEPRECATED gameSessionConnectionInfo "Use generic-lens or generic-optics with 'gameSessionConnectionInfo' instead"  #-}

-- | A set of @Player@ objects, each representing a player to find matches for. Players are identified by a unique player ID and may include latency data for use during matchmaking. If the ticket is in status @COMPLETED@ , the @Player@ objects include the team the players were assigned to in the resulting match.
--
-- /Note:/ Consider using 'players' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtPlayers :: Lens.Lens' MatchmakingTicket (Core.Maybe [Types.Player])
mtPlayers = Lens.field @"players"
{-# INLINEABLE mtPlayers #-}
{-# DEPRECATED players "Use generic-lens or generic-optics with 'players' instead"  #-}

-- | Time stamp indicating when this matchmaking request was received. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtStartTime :: Lens.Lens' MatchmakingTicket (Core.Maybe Core.NominalDiffTime)
mtStartTime = Lens.field @"startTime"
{-# INLINEABLE mtStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | Current status of the matchmaking request.
--
--
--     * __QUEUED__ -- The matchmaking request has been received and is currently waiting to be processed.
--
--
--     * __SEARCHING__ -- The matchmaking request is currently being processed. 
--
--
--     * __REQUIRES_ACCEPTANCE__ -- A match has been proposed and the players must accept the match (see 'AcceptMatch' ). This status is used only with requests that use a matchmaking configuration with a player acceptance requirement.
--
--
--     * __PLACING__ -- The FlexMatch engine has matched players and is in the process of placing a new game session for the match.
--
--
--     * __COMPLETED__ -- Players have been matched and a game session is ready to host the players. A ticket in this state contains the necessary connection information for players.
--
--
--     * __FAILED__ -- The matchmaking request was not completed.
--
--
--     * __CANCELLED__ -- The matchmaking request was canceled. This may be the result of a call to 'StopMatchmaking' or a proposed match that one or more players failed to accept.
--
--
--     * __TIMED_OUT__ -- The matchmaking request was not successful within the duration specified in the matchmaking configuration. 
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtStatus :: Lens.Lens' MatchmakingTicket (Core.Maybe Types.MatchmakingConfigurationStatus)
mtStatus = Lens.field @"status"
{-# INLINEABLE mtStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Additional information about the current status.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtStatusMessage :: Lens.Lens' MatchmakingTicket (Core.Maybe Types.StringModel)
mtStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE mtStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | Code to explain the current status. For example, a status reason may indicate when a ticket has returned to @SEARCHING@ status after a proposed match fails to receive player acceptances.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtStatusReason :: Lens.Lens' MatchmakingTicket (Core.Maybe Types.StringModel)
mtStatusReason = Lens.field @"statusReason"
{-# INLINEABLE mtStatusReason #-}
{-# DEPRECATED statusReason "Use generic-lens or generic-optics with 'statusReason' instead"  #-}

-- | A unique identifier for a matchmaking ticket.
--
-- /Note:/ Consider using 'ticketId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtTicketId :: Lens.Lens' MatchmakingTicket (Core.Maybe Types.TicketId)
mtTicketId = Lens.field @"ticketId"
{-# INLINEABLE mtTicketId #-}
{-# DEPRECATED ticketId "Use generic-lens or generic-optics with 'ticketId' instead"  #-}

instance Core.FromJSON MatchmakingTicket where
        parseJSON
          = Core.withObject "MatchmakingTicket" Core.$
              \ x ->
                MatchmakingTicket' Core.<$>
                  (x Core..:? "ConfigurationArn") Core.<*>
                    x Core..:? "ConfigurationName"
                    Core.<*> x Core..:? "EndTime"
                    Core.<*> x Core..:? "EstimatedWaitTime"
                    Core.<*> x Core..:? "GameSessionConnectionInfo"
                    Core.<*> x Core..:? "Players"
                    Core.<*> x Core..:? "StartTime"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "StatusMessage"
                    Core.<*> x Core..:? "StatusReason"
                    Core.<*> x Core..:? "TicketId"
