{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchmakingTicket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchmakingTicket where

import Network.AWS.GameLift.Types.GameSessionConnectionInfo
import Network.AWS.GameLift.Types.MatchmakingConfigurationStatus
import Network.AWS.GameLift.Types.Player
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Ticket generated to track the progress of a matchmaking request. Each ticket is uniquely identified by a ticket ID, supplied by the requester, when creating a matchmaking request with 'StartMatchmaking' . Tickets can be retrieved by calling 'DescribeMatchmaking' with the ticket ID.
--
--
--
-- /See:/ 'matchmakingTicket' smart constructor.
data MatchmakingTicket = MatchmakingTicket'
  { _mtStatus ::
      !(Maybe MatchmakingConfigurationStatus),
    _mtConfigurationName :: !(Maybe Text),
    _mtStartTime :: !(Maybe POSIX),
    _mtGameSessionConnectionInfo ::
      !(Maybe GameSessionConnectionInfo),
    _mtTicketId :: !(Maybe Text),
    _mtEstimatedWaitTime :: !(Maybe Nat),
    _mtStatusMessage :: !(Maybe Text),
    _mtEndTime :: !(Maybe POSIX),
    _mtConfigurationARN :: !(Maybe Text),
    _mtStatusReason :: !(Maybe Text),
    _mtPlayers :: !(Maybe [Player])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MatchmakingTicket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtStatus' - Current status of the matchmaking request.     * __QUEUED__ -- The matchmaking request has been received and is currently waiting to be processed.     * __SEARCHING__ -- The matchmaking request is currently being processed.      * __REQUIRES_ACCEPTANCE__ -- A match has been proposed and the players must accept the match (see 'AcceptMatch' ). This status is used only with requests that use a matchmaking configuration with a player acceptance requirement.     * __PLACING__ -- The FlexMatch engine has matched players and is in the process of placing a new game session for the match.     * __COMPLETED__ -- Players have been matched and a game session is ready to host the players. A ticket in this state contains the necessary connection information for players.     * __FAILED__ -- The matchmaking request was not completed.     * __CANCELLED__ -- The matchmaking request was canceled. This may be the result of a call to 'StopMatchmaking' or a proposed match that one or more players failed to accept.     * __TIMED_OUT__ -- The matchmaking request was not successful within the duration specified in the matchmaking configuration.
--
-- * 'mtConfigurationName' - Name of the 'MatchmakingConfiguration' that is used with this ticket. Matchmaking configurations determine how players are grouped into a match and how a new game session is created for the match.
--
-- * 'mtStartTime' - Time stamp indicating when this matchmaking request was received. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'mtGameSessionConnectionInfo' - Identifier and connection information of the game session created for the match. This information is added to the ticket only after the matchmaking request has been successfully completed. This parameter is not set when FlexMatch is being used without GameLift hosting.
--
-- * 'mtTicketId' - A unique identifier for a matchmaking ticket.
--
-- * 'mtEstimatedWaitTime' - Average amount of time (in seconds) that players are currently waiting for a match. If there is not enough recent data, this property may be empty.
--
-- * 'mtStatusMessage' - Additional information about the current status.
--
-- * 'mtEndTime' - Time stamp indicating when this matchmaking request stopped being processed due to success, failure, or cancellation. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'mtConfigurationARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift matchmaking configuration resource that is used with this ticket.
--
-- * 'mtStatusReason' - Code to explain the current status. For example, a status reason may indicate when a ticket has returned to @SEARCHING@ status after a proposed match fails to receive player acceptances.
--
-- * 'mtPlayers' - A set of @Player@ objects, each representing a player to find matches for. Players are identified by a unique player ID and may include latency data for use during matchmaking. If the ticket is in status @COMPLETED@ , the @Player@ objects include the team the players were assigned to in the resulting match.
matchmakingTicket ::
  MatchmakingTicket
matchmakingTicket =
  MatchmakingTicket'
    { _mtStatus = Nothing,
      _mtConfigurationName = Nothing,
      _mtStartTime = Nothing,
      _mtGameSessionConnectionInfo = Nothing,
      _mtTicketId = Nothing,
      _mtEstimatedWaitTime = Nothing,
      _mtStatusMessage = Nothing,
      _mtEndTime = Nothing,
      _mtConfigurationARN = Nothing,
      _mtStatusReason = Nothing,
      _mtPlayers = Nothing
    }

-- | Current status of the matchmaking request.     * __QUEUED__ -- The matchmaking request has been received and is currently waiting to be processed.     * __SEARCHING__ -- The matchmaking request is currently being processed.      * __REQUIRES_ACCEPTANCE__ -- A match has been proposed and the players must accept the match (see 'AcceptMatch' ). This status is used only with requests that use a matchmaking configuration with a player acceptance requirement.     * __PLACING__ -- The FlexMatch engine has matched players and is in the process of placing a new game session for the match.     * __COMPLETED__ -- Players have been matched and a game session is ready to host the players. A ticket in this state contains the necessary connection information for players.     * __FAILED__ -- The matchmaking request was not completed.     * __CANCELLED__ -- The matchmaking request was canceled. This may be the result of a call to 'StopMatchmaking' or a proposed match that one or more players failed to accept.     * __TIMED_OUT__ -- The matchmaking request was not successful within the duration specified in the matchmaking configuration.
mtStatus :: Lens' MatchmakingTicket (Maybe MatchmakingConfigurationStatus)
mtStatus = lens _mtStatus (\s a -> s {_mtStatus = a})

-- | Name of the 'MatchmakingConfiguration' that is used with this ticket. Matchmaking configurations determine how players are grouped into a match and how a new game session is created for the match.
mtConfigurationName :: Lens' MatchmakingTicket (Maybe Text)
mtConfigurationName = lens _mtConfigurationName (\s a -> s {_mtConfigurationName = a})

-- | Time stamp indicating when this matchmaking request was received. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
mtStartTime :: Lens' MatchmakingTicket (Maybe UTCTime)
mtStartTime = lens _mtStartTime (\s a -> s {_mtStartTime = a}) . mapping _Time

-- | Identifier and connection information of the game session created for the match. This information is added to the ticket only after the matchmaking request has been successfully completed. This parameter is not set when FlexMatch is being used without GameLift hosting.
mtGameSessionConnectionInfo :: Lens' MatchmakingTicket (Maybe GameSessionConnectionInfo)
mtGameSessionConnectionInfo = lens _mtGameSessionConnectionInfo (\s a -> s {_mtGameSessionConnectionInfo = a})

-- | A unique identifier for a matchmaking ticket.
mtTicketId :: Lens' MatchmakingTicket (Maybe Text)
mtTicketId = lens _mtTicketId (\s a -> s {_mtTicketId = a})

-- | Average amount of time (in seconds) that players are currently waiting for a match. If there is not enough recent data, this property may be empty.
mtEstimatedWaitTime :: Lens' MatchmakingTicket (Maybe Natural)
mtEstimatedWaitTime = lens _mtEstimatedWaitTime (\s a -> s {_mtEstimatedWaitTime = a}) . mapping _Nat

-- | Additional information about the current status.
mtStatusMessage :: Lens' MatchmakingTicket (Maybe Text)
mtStatusMessage = lens _mtStatusMessage (\s a -> s {_mtStatusMessage = a})

-- | Time stamp indicating when this matchmaking request stopped being processed due to success, failure, or cancellation. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
mtEndTime :: Lens' MatchmakingTicket (Maybe UTCTime)
mtEndTime = lens _mtEndTime (\s a -> s {_mtEndTime = a}) . mapping _Time

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift matchmaking configuration resource that is used with this ticket.
mtConfigurationARN :: Lens' MatchmakingTicket (Maybe Text)
mtConfigurationARN = lens _mtConfigurationARN (\s a -> s {_mtConfigurationARN = a})

-- | Code to explain the current status. For example, a status reason may indicate when a ticket has returned to @SEARCHING@ status after a proposed match fails to receive player acceptances.
mtStatusReason :: Lens' MatchmakingTicket (Maybe Text)
mtStatusReason = lens _mtStatusReason (\s a -> s {_mtStatusReason = a})

-- | A set of @Player@ objects, each representing a player to find matches for. Players are identified by a unique player ID and may include latency data for use during matchmaking. If the ticket is in status @COMPLETED@ , the @Player@ objects include the team the players were assigned to in the resulting match.
mtPlayers :: Lens' MatchmakingTicket [Player]
mtPlayers = lens _mtPlayers (\s a -> s {_mtPlayers = a}) . _Default . _Coerce

instance FromJSON MatchmakingTicket where
  parseJSON =
    withObject
      "MatchmakingTicket"
      ( \x ->
          MatchmakingTicket'
            <$> (x .:? "Status")
            <*> (x .:? "ConfigurationName")
            <*> (x .:? "StartTime")
            <*> (x .:? "GameSessionConnectionInfo")
            <*> (x .:? "TicketId")
            <*> (x .:? "EstimatedWaitTime")
            <*> (x .:? "StatusMessage")
            <*> (x .:? "EndTime")
            <*> (x .:? "ConfigurationArn")
            <*> (x .:? "StatusReason")
            <*> (x .:? "Players" .!= mempty)
      )

instance Hashable MatchmakingTicket

instance NFData MatchmakingTicket
