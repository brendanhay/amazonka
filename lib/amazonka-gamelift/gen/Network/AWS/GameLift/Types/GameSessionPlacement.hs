{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionPlacement where

import Network.AWS.GameLift.Types.GameProperty
import Network.AWS.GameLift.Types.GameSessionPlacementState
import Network.AWS.GameLift.Types.PlacedPlayerSession
import Network.AWS.GameLift.Types.PlayerLatency
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Object that describes a 'StartGameSessionPlacement' request. This object includes the full details of the original request plus the current status and start/end time stamps.
--
--
-- Game session placement-related operations include:
--
--     * 'StartGameSessionPlacement'
--
--     * 'DescribeGameSessionPlacement'
--
--     * 'StopGameSessionPlacement'
--
--
--
--
-- /See:/ 'gameSessionPlacement' smart constructor.
data GameSessionPlacement = GameSessionPlacement'
  { _gspStatus ::
      !(Maybe GameSessionPlacementState),
    _gspPlacementId :: !(Maybe Text),
    _gspGameProperties :: !(Maybe [GameProperty]),
    _gspIPAddress :: !(Maybe Text),
    _gspGameSessionName :: !(Maybe Text),
    _gspStartTime :: !(Maybe POSIX),
    _gspGameSessionId :: !(Maybe Text),
    _gspGameSessionRegion :: !(Maybe Text),
    _gspMatchmakerData :: !(Maybe Text),
    _gspMaximumPlayerSessionCount :: !(Maybe Nat),
    _gspEndTime :: !(Maybe POSIX),
    _gspGameSessionARN :: !(Maybe Text),
    _gspPlayerLatencies :: !(Maybe [PlayerLatency]),
    _gspGameSessionData :: !(Maybe Text),
    _gspDNSName :: !(Maybe Text),
    _gspGameSessionQueueName :: !(Maybe Text),
    _gspPlacedPlayerSessions ::
      !(Maybe [PlacedPlayerSession]),
    _gspPort :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GameSessionPlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspStatus' - Current status of the game session placement request.     * __PENDING__ -- The placement request is currently in the queue waiting to be processed.     * __FULFILLED__ -- A new game session and player sessions (if requested) have been successfully created. Values for /GameSessionArn/ and /GameSessionRegion/ are available.      * __CANCELLED__ -- The placement request was canceled with a call to 'StopGameSessionPlacement' .     * __TIMED_OUT__ -- A new game session was not successfully created before the time limit expired. You can resubmit the placement request as needed.     * __FAILED__ -- GameLift is not able to complete the process of placing the game session. Common reasons are the game session terminated before the placement process was completed, or an unexpected internal error.
--
-- * 'gspPlacementId' - A unique identifier for a game session placement.
--
-- * 'gspGameProperties' - Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- * 'gspIPAddress' - IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- * 'gspGameSessionName' - A descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- * 'gspStartTime' - Time stamp indicating when this request was placed in the queue. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'gspGameSessionId' - A unique identifier for the game session. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- * 'gspGameSessionRegion' - Name of the Region where the game session created by this placement request is running. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
--
-- * 'gspMatchmakerData' - Information on the matchmaking process for this game. Data is in JSON syntax, formatted as a string. It identifies the matchmaking configuration used to create the match, and contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> .
--
-- * 'gspMaximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to the game session.
--
-- * 'gspEndTime' - Time stamp indicating when this request was completed, canceled, or timed out.
--
-- * 'gspGameSessionARN' - Identifier for the game session created by this placement request. This value is set once the new game session is placed (placement status is @FULFILLED@ ). This identifier is unique across all Regions. You can use this value as a @GameSessionId@ value as needed.
--
-- * 'gspPlayerLatencies' - Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions.
--
-- * 'gspGameSessionData' - Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- * 'gspDNSName' - DNS identifier assigned to the instance that is running the game session. Values have the following format:     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .) When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
--
-- * 'gspGameSessionQueueName' - A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
--
-- * 'gspPlacedPlayerSessions' - A collection of information on player sessions created in response to the game session placement request. These player sessions are created only once a new game session is successfully placed (placement status is @FULFILLED@ ). This information includes the player ID (as provided in the placement request) and the corresponding player session ID. Retrieve full player sessions by calling 'DescribePlayerSessions' with the player session ID.
--
-- * 'gspPort' - Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
gameSessionPlacement ::
  GameSessionPlacement
gameSessionPlacement =
  GameSessionPlacement'
    { _gspStatus = Nothing,
      _gspPlacementId = Nothing,
      _gspGameProperties = Nothing,
      _gspIPAddress = Nothing,
      _gspGameSessionName = Nothing,
      _gspStartTime = Nothing,
      _gspGameSessionId = Nothing,
      _gspGameSessionRegion = Nothing,
      _gspMatchmakerData = Nothing,
      _gspMaximumPlayerSessionCount = Nothing,
      _gspEndTime = Nothing,
      _gspGameSessionARN = Nothing,
      _gspPlayerLatencies = Nothing,
      _gspGameSessionData = Nothing,
      _gspDNSName = Nothing,
      _gspGameSessionQueueName = Nothing,
      _gspPlacedPlayerSessions = Nothing,
      _gspPort = Nothing
    }

-- | Current status of the game session placement request.     * __PENDING__ -- The placement request is currently in the queue waiting to be processed.     * __FULFILLED__ -- A new game session and player sessions (if requested) have been successfully created. Values for /GameSessionArn/ and /GameSessionRegion/ are available.      * __CANCELLED__ -- The placement request was canceled with a call to 'StopGameSessionPlacement' .     * __TIMED_OUT__ -- A new game session was not successfully created before the time limit expired. You can resubmit the placement request as needed.     * __FAILED__ -- GameLift is not able to complete the process of placing the game session. Common reasons are the game session terminated before the placement process was completed, or an unexpected internal error.
gspStatus :: Lens' GameSessionPlacement (Maybe GameSessionPlacementState)
gspStatus = lens _gspStatus (\s a -> s {_gspStatus = a})

-- | A unique identifier for a game session placement.
gspPlacementId :: Lens' GameSessionPlacement (Maybe Text)
gspPlacementId = lens _gspPlacementId (\s a -> s {_gspPlacementId = a})

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
gspGameProperties :: Lens' GameSessionPlacement [GameProperty]
gspGameProperties = lens _gspGameProperties (\s a -> s {_gspGameProperties = a}) . _Default . _Coerce

-- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
gspIPAddress :: Lens' GameSessionPlacement (Maybe Text)
gspIPAddress = lens _gspIPAddress (\s a -> s {_gspIPAddress = a})

-- | A descriptive label that is associated with a game session. Session names do not need to be unique.
gspGameSessionName :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionName = lens _gspGameSessionName (\s a -> s {_gspGameSessionName = a})

-- | Time stamp indicating when this request was placed in the queue. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
gspStartTime :: Lens' GameSessionPlacement (Maybe UTCTime)
gspStartTime = lens _gspStartTime (\s a -> s {_gspStartTime = a}) . mapping _Time

-- | A unique identifier for the game session. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
gspGameSessionId :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionId = lens _gspGameSessionId (\s a -> s {_gspGameSessionId = a})

-- | Name of the Region where the game session created by this placement request is running. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
gspGameSessionRegion :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionRegion = lens _gspGameSessionRegion (\s a -> s {_gspGameSessionRegion = a})

-- | Information on the matchmaking process for this game. Data is in JSON syntax, formatted as a string. It identifies the matchmaking configuration used to create the match, and contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> .
gspMatchmakerData :: Lens' GameSessionPlacement (Maybe Text)
gspMatchmakerData = lens _gspMatchmakerData (\s a -> s {_gspMatchmakerData = a})

-- | The maximum number of players that can be connected simultaneously to the game session.
gspMaximumPlayerSessionCount :: Lens' GameSessionPlacement (Maybe Natural)
gspMaximumPlayerSessionCount = lens _gspMaximumPlayerSessionCount (\s a -> s {_gspMaximumPlayerSessionCount = a}) . mapping _Nat

-- | Time stamp indicating when this request was completed, canceled, or timed out.
gspEndTime :: Lens' GameSessionPlacement (Maybe UTCTime)
gspEndTime = lens _gspEndTime (\s a -> s {_gspEndTime = a}) . mapping _Time

-- | Identifier for the game session created by this placement request. This value is set once the new game session is placed (placement status is @FULFILLED@ ). This identifier is unique across all Regions. You can use this value as a @GameSessionId@ value as needed.
gspGameSessionARN :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionARN = lens _gspGameSessionARN (\s a -> s {_gspGameSessionARN = a})

-- | Set of values, expressed in milliseconds, indicating the amount of latency that a player experiences when connected to AWS Regions.
gspPlayerLatencies :: Lens' GameSessionPlacement [PlayerLatency]
gspPlayerLatencies = lens _gspPlayerLatencies (\s a -> s {_gspPlayerLatencies = a}) . _Default . _Coerce

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
gspGameSessionData :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionData = lens _gspGameSessionData (\s a -> s {_gspGameSessionData = a})

-- | DNS identifier assigned to the instance that is running the game session. Values have the following format:     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .) When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
gspDNSName :: Lens' GameSessionPlacement (Maybe Text)
gspDNSName = lens _gspDNSName (\s a -> s {_gspDNSName = a})

-- | A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
gspGameSessionQueueName :: Lens' GameSessionPlacement (Maybe Text)
gspGameSessionQueueName = lens _gspGameSessionQueueName (\s a -> s {_gspGameSessionQueueName = a})

-- | A collection of information on player sessions created in response to the game session placement request. These player sessions are created only once a new game session is successfully placed (placement status is @FULFILLED@ ). This information includes the player ID (as provided in the placement request) and the corresponding player session ID. Retrieve full player sessions by calling 'DescribePlayerSessions' with the player session ID.
gspPlacedPlayerSessions :: Lens' GameSessionPlacement [PlacedPlayerSession]
gspPlacedPlayerSessions = lens _gspPlacedPlayerSessions (\s a -> s {_gspPlacedPlayerSessions = a}) . _Default . _Coerce

-- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number. This value is set once the new game session is placed (placement status is @FULFILLED@ ).
gspPort :: Lens' GameSessionPlacement (Maybe Natural)
gspPort = lens _gspPort (\s a -> s {_gspPort = a}) . mapping _Nat

instance FromJSON GameSessionPlacement where
  parseJSON =
    withObject
      "GameSessionPlacement"
      ( \x ->
          GameSessionPlacement'
            <$> (x .:? "Status")
            <*> (x .:? "PlacementId")
            <*> (x .:? "GameProperties" .!= mempty)
            <*> (x .:? "IpAddress")
            <*> (x .:? "GameSessionName")
            <*> (x .:? "StartTime")
            <*> (x .:? "GameSessionId")
            <*> (x .:? "GameSessionRegion")
            <*> (x .:? "MatchmakerData")
            <*> (x .:? "MaximumPlayerSessionCount")
            <*> (x .:? "EndTime")
            <*> (x .:? "GameSessionArn")
            <*> (x .:? "PlayerLatencies" .!= mempty)
            <*> (x .:? "GameSessionData")
            <*> (x .:? "DnsName")
            <*> (x .:? "GameSessionQueueName")
            <*> (x .:? "PlacedPlayerSessions" .!= mempty)
            <*> (x .:? "Port")
      )

instance Hashable GameSessionPlacement

instance NFData GameSessionPlacement
