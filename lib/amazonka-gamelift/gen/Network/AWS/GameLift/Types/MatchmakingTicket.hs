{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchmakingTicket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchmakingTicket
  ( MatchmakingTicket (..),

    -- * Smart constructor
    mkMatchmakingTicket,

    -- * Lenses
    mtStatus,
    mtConfigurationName,
    mtStartTime,
    mtGameSessionConnectionInfo,
    mtTicketId,
    mtEstimatedWaitTime,
    mtStatusMessage,
    mtEndTime,
    mtConfigurationARN,
    mtStatusReason,
    mtPlayers,
  )
where

import Network.AWS.GameLift.Types.GameSessionConnectionInfo
import Network.AWS.GameLift.Types.MatchmakingConfigurationStatus
import Network.AWS.GameLift.Types.Player
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Ticket generated to track the progress of a matchmaking request. Each ticket is uniquely identified by a ticket ID, supplied by the requester, when creating a matchmaking request with 'StartMatchmaking' . Tickets can be retrieved by calling 'DescribeMatchmaking' with the ticket ID.
--
-- /See:/ 'mkMatchmakingTicket' smart constructor.
data MatchmakingTicket = MatchmakingTicket'
  { -- | Current status of the matchmaking request.
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
    status :: Lude.Maybe MatchmakingConfigurationStatus,
    -- | Name of the 'MatchmakingConfiguration' that is used with this ticket. Matchmaking configurations determine how players are grouped into a match and how a new game session is created for the match.
    configurationName :: Lude.Maybe Lude.Text,
    -- | Time stamp indicating when this matchmaking request was received. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | Identifier and connection information of the game session created for the match. This information is added to the ticket only after the matchmaking request has been successfully completed. This parameter is not set when FlexMatch is being used without GameLift hosting.
    gameSessionConnectionInfo :: Lude.Maybe GameSessionConnectionInfo,
    -- | A unique identifier for a matchmaking ticket.
    ticketId :: Lude.Maybe Lude.Text,
    -- | Average amount of time (in seconds) that players are currently waiting for a match. If there is not enough recent data, this property may be empty.
    estimatedWaitTime :: Lude.Maybe Lude.Natural,
    -- | Additional information about the current status.
    statusMessage :: Lude.Maybe Lude.Text,
    -- | Time stamp indicating when this matchmaking request stopped being processed due to success, failure, or cancellation. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift matchmaking configuration resource that is used with this ticket.
    configurationARN :: Lude.Maybe Lude.Text,
    -- | Code to explain the current status. For example, a status reason may indicate when a ticket has returned to @SEARCHING@ status after a proposed match fails to receive player acceptances.
    statusReason :: Lude.Maybe Lude.Text,
    -- | A set of @Player@ objects, each representing a player to find matches for. Players are identified by a unique player ID and may include latency data for use during matchmaking. If the ticket is in status @COMPLETED@ , the @Player@ objects include the team the players were assigned to in the resulting match.
    players :: Lude.Maybe [Player]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MatchmakingTicket' with the minimum fields required to make a request.
--
-- * 'status' - Current status of the matchmaking request.
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
-- * 'configurationName' - Name of the 'MatchmakingConfiguration' that is used with this ticket. Matchmaking configurations determine how players are grouped into a match and how a new game session is created for the match.
-- * 'startTime' - Time stamp indicating when this matchmaking request was received. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'gameSessionConnectionInfo' - Identifier and connection information of the game session created for the match. This information is added to the ticket only after the matchmaking request has been successfully completed. This parameter is not set when FlexMatch is being used without GameLift hosting.
-- * 'ticketId' - A unique identifier for a matchmaking ticket.
-- * 'estimatedWaitTime' - Average amount of time (in seconds) that players are currently waiting for a match. If there is not enough recent data, this property may be empty.
-- * 'statusMessage' - Additional information about the current status.
-- * 'endTime' - Time stamp indicating when this matchmaking request stopped being processed due to success, failure, or cancellation. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'configurationARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift matchmaking configuration resource that is used with this ticket.
-- * 'statusReason' - Code to explain the current status. For example, a status reason may indicate when a ticket has returned to @SEARCHING@ status after a proposed match fails to receive player acceptances.
-- * 'players' - A set of @Player@ objects, each representing a player to find matches for. Players are identified by a unique player ID and may include latency data for use during matchmaking. If the ticket is in status @COMPLETED@ , the @Player@ objects include the team the players were assigned to in the resulting match.
mkMatchmakingTicket ::
  MatchmakingTicket
mkMatchmakingTicket =
  MatchmakingTicket'
    { status = Lude.Nothing,
      configurationName = Lude.Nothing,
      startTime = Lude.Nothing,
      gameSessionConnectionInfo = Lude.Nothing,
      ticketId = Lude.Nothing,
      estimatedWaitTime = Lude.Nothing,
      statusMessage = Lude.Nothing,
      endTime = Lude.Nothing,
      configurationARN = Lude.Nothing,
      statusReason = Lude.Nothing,
      players = Lude.Nothing
    }

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
mtStatus :: Lens.Lens' MatchmakingTicket (Lude.Maybe MatchmakingConfigurationStatus)
mtStatus = Lens.lens (status :: MatchmakingTicket -> Lude.Maybe MatchmakingConfigurationStatus) (\s a -> s {status = a} :: MatchmakingTicket)
{-# DEPRECATED mtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Name of the 'MatchmakingConfiguration' that is used with this ticket. Matchmaking configurations determine how players are grouped into a match and how a new game session is created for the match.
--
-- /Note:/ Consider using 'configurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtConfigurationName :: Lens.Lens' MatchmakingTicket (Lude.Maybe Lude.Text)
mtConfigurationName = Lens.lens (configurationName :: MatchmakingTicket -> Lude.Maybe Lude.Text) (\s a -> s {configurationName = a} :: MatchmakingTicket)
{-# DEPRECATED mtConfigurationName "Use generic-lens or generic-optics with 'configurationName' instead." #-}

-- | Time stamp indicating when this matchmaking request was received. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtStartTime :: Lens.Lens' MatchmakingTicket (Lude.Maybe Lude.Timestamp)
mtStartTime = Lens.lens (startTime :: MatchmakingTicket -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: MatchmakingTicket)
{-# DEPRECATED mtStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Identifier and connection information of the game session created for the match. This information is added to the ticket only after the matchmaking request has been successfully completed. This parameter is not set when FlexMatch is being used without GameLift hosting.
--
-- /Note:/ Consider using 'gameSessionConnectionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtGameSessionConnectionInfo :: Lens.Lens' MatchmakingTicket (Lude.Maybe GameSessionConnectionInfo)
mtGameSessionConnectionInfo = Lens.lens (gameSessionConnectionInfo :: MatchmakingTicket -> Lude.Maybe GameSessionConnectionInfo) (\s a -> s {gameSessionConnectionInfo = a} :: MatchmakingTicket)
{-# DEPRECATED mtGameSessionConnectionInfo "Use generic-lens or generic-optics with 'gameSessionConnectionInfo' instead." #-}

-- | A unique identifier for a matchmaking ticket.
--
-- /Note:/ Consider using 'ticketId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtTicketId :: Lens.Lens' MatchmakingTicket (Lude.Maybe Lude.Text)
mtTicketId = Lens.lens (ticketId :: MatchmakingTicket -> Lude.Maybe Lude.Text) (\s a -> s {ticketId = a} :: MatchmakingTicket)
{-# DEPRECATED mtTicketId "Use generic-lens or generic-optics with 'ticketId' instead." #-}

-- | Average amount of time (in seconds) that players are currently waiting for a match. If there is not enough recent data, this property may be empty.
--
-- /Note:/ Consider using 'estimatedWaitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtEstimatedWaitTime :: Lens.Lens' MatchmakingTicket (Lude.Maybe Lude.Natural)
mtEstimatedWaitTime = Lens.lens (estimatedWaitTime :: MatchmakingTicket -> Lude.Maybe Lude.Natural) (\s a -> s {estimatedWaitTime = a} :: MatchmakingTicket)
{-# DEPRECATED mtEstimatedWaitTime "Use generic-lens or generic-optics with 'estimatedWaitTime' instead." #-}

-- | Additional information about the current status.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtStatusMessage :: Lens.Lens' MatchmakingTicket (Lude.Maybe Lude.Text)
mtStatusMessage = Lens.lens (statusMessage :: MatchmakingTicket -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: MatchmakingTicket)
{-# DEPRECATED mtStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | Time stamp indicating when this matchmaking request stopped being processed due to success, failure, or cancellation. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtEndTime :: Lens.Lens' MatchmakingTicket (Lude.Maybe Lude.Timestamp)
mtEndTime = Lens.lens (endTime :: MatchmakingTicket -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: MatchmakingTicket)
{-# DEPRECATED mtEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift matchmaking configuration resource that is used with this ticket.
--
-- /Note:/ Consider using 'configurationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtConfigurationARN :: Lens.Lens' MatchmakingTicket (Lude.Maybe Lude.Text)
mtConfigurationARN = Lens.lens (configurationARN :: MatchmakingTicket -> Lude.Maybe Lude.Text) (\s a -> s {configurationARN = a} :: MatchmakingTicket)
{-# DEPRECATED mtConfigurationARN "Use generic-lens or generic-optics with 'configurationARN' instead." #-}

-- | Code to explain the current status. For example, a status reason may indicate when a ticket has returned to @SEARCHING@ status after a proposed match fails to receive player acceptances.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtStatusReason :: Lens.Lens' MatchmakingTicket (Lude.Maybe Lude.Text)
mtStatusReason = Lens.lens (statusReason :: MatchmakingTicket -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: MatchmakingTicket)
{-# DEPRECATED mtStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | A set of @Player@ objects, each representing a player to find matches for. Players are identified by a unique player ID and may include latency data for use during matchmaking. If the ticket is in status @COMPLETED@ , the @Player@ objects include the team the players were assigned to in the resulting match.
--
-- /Note:/ Consider using 'players' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtPlayers :: Lens.Lens' MatchmakingTicket (Lude.Maybe [Player])
mtPlayers = Lens.lens (players :: MatchmakingTicket -> Lude.Maybe [Player]) (\s a -> s {players = a} :: MatchmakingTicket)
{-# DEPRECATED mtPlayers "Use generic-lens or generic-optics with 'players' instead." #-}

instance Lude.FromJSON MatchmakingTicket where
  parseJSON =
    Lude.withObject
      "MatchmakingTicket"
      ( \x ->
          MatchmakingTicket'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ConfigurationName")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "GameSessionConnectionInfo")
            Lude.<*> (x Lude..:? "TicketId")
            Lude.<*> (x Lude..:? "EstimatedWaitTime")
            Lude.<*> (x Lude..:? "StatusMessage")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "ConfigurationArn")
            Lude.<*> (x Lude..:? "StatusReason")
            Lude.<*> (x Lude..:? "Players" Lude..!= Lude.mempty)
      )
