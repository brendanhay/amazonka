{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSession where

import Network.AWS.GameLift.Types.GameProperty
import Network.AWS.GameLift.Types.GameSessionStatus
import Network.AWS.GameLift.Types.GameSessionStatusReason
import Network.AWS.GameLift.Types.PlayerSessionCreationPolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Properties describing a game session.
--
--
-- A game session in ACTIVE status can host players. When a game session ends, its status is set to @TERMINATED@ .
--
-- Once the session ends, the game session object is retained for 30 days. This means you can reuse idempotency token values after this time. Game session logs are retained for 14 days.
--
--     * 'CreateGameSession'
--
--     * 'DescribeGameSessions'
--
--     * 'DescribeGameSessionDetails'
--
--     * 'SearchGameSessions'
--
--     * 'UpdateGameSession'
--
--     * 'GetGameSessionLogUrl'
--
--     * Game session placements
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
--
--
-- /See:/ 'gameSession' smart constructor.
data GameSession = GameSession'
  { _gsCreationTime :: !(Maybe POSIX),
    _gsStatus :: !(Maybe GameSessionStatus),
    _gsGameProperties :: !(Maybe [GameProperty]),
    _gsIPAddress :: !(Maybe Text),
    _gsGameSessionId :: !(Maybe Text),
    _gsMatchmakerData :: !(Maybe Text),
    _gsFleetARN :: !(Maybe Text),
    _gsMaximumPlayerSessionCount :: !(Maybe Nat),
    _gsTerminationTime :: !(Maybe POSIX),
    _gsPlayerSessionCreationPolicy ::
      !(Maybe PlayerSessionCreationPolicy),
    _gsName :: !(Maybe Text),
    _gsCurrentPlayerSessionCount :: !(Maybe Nat),
    _gsStatusReason :: !(Maybe GameSessionStatusReason),
    _gsGameSessionData :: !(Maybe Text),
    _gsFleetId :: !(Maybe Text),
    _gsDNSName :: !(Maybe Text),
    _gsCreatorId :: !(Maybe Text),
    _gsPort :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GameSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'gsStatus' - Current status of the game session. A game session must have an @ACTIVE@ status to have player sessions.
--
-- * 'gsGameProperties' - Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). You can search for active game sessions based on this custom data with 'SearchGameSessions' .
--
-- * 'gsIPAddress' - IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
--
-- * 'gsGameSessionId' - A unique identifier for the game session. A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .
--
-- * 'gsMatchmakerData' - Information about the matchmaking process that was used to create the game session. It is in JSON syntax, formatted as a string. In addition the matchmaking configuration used, it contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> . Matchmaker data is useful when requesting match backfills, and is updated whenever new players are added during a successful backfill (see 'StartMatchBackfill' ).
--
-- * 'gsFleetARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet that this game session is running on.
--
-- * 'gsMaximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to the game session.
--
-- * 'gsTerminationTime' - Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'gsPlayerSessionCreationPolicy' - Indicates whether or not the game session is accepting new players.
--
-- * 'gsName' - A descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- * 'gsCurrentPlayerSessionCount' - Number of players currently in the game session.
--
-- * 'gsStatusReason' - Provides additional information about game session status. @INTERRUPTED@ indicates that the game session was hosted on a spot instance that was reclaimed, causing the active game session to be terminated.
--
-- * 'gsGameSessionData' - Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- * 'gsFleetId' - A unique identifier for a fleet that the game session is running on.
--
-- * 'gsDNSName' - DNS identifier assigned to the instance that is running the game session. Values have the following format:     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .) When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
--
-- * 'gsCreatorId' - A unique identifier for a player. This ID is used to enforce a resource protection policy (if one exists), that limits the number of game sessions a player can create.
--
-- * 'gsPort' - Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
gameSession ::
  GameSession
gameSession =
  GameSession'
    { _gsCreationTime = Nothing,
      _gsStatus = Nothing,
      _gsGameProperties = Nothing,
      _gsIPAddress = Nothing,
      _gsGameSessionId = Nothing,
      _gsMatchmakerData = Nothing,
      _gsFleetARN = Nothing,
      _gsMaximumPlayerSessionCount = Nothing,
      _gsTerminationTime = Nothing,
      _gsPlayerSessionCreationPolicy = Nothing,
      _gsName = Nothing,
      _gsCurrentPlayerSessionCount = Nothing,
      _gsStatusReason = Nothing,
      _gsGameSessionData = Nothing,
      _gsFleetId = Nothing,
      _gsDNSName = Nothing,
      _gsCreatorId = Nothing,
      _gsPort = Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
gsCreationTime :: Lens' GameSession (Maybe UTCTime)
gsCreationTime = lens _gsCreationTime (\s a -> s {_gsCreationTime = a}) . mapping _Time

-- | Current status of the game session. A game session must have an @ACTIVE@ status to have player sessions.
gsStatus :: Lens' GameSession (Maybe GameSessionStatus)
gsStatus = lens _gsStatus (\s a -> s {_gsStatus = a})

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). You can search for active game sessions based on this custom data with 'SearchGameSessions' .
gsGameProperties :: Lens' GameSession [GameProperty]
gsGameProperties = lens _gsGameProperties (\s a -> s {_gsGameProperties = a}) . _Default . _Coerce

-- | IP address of the instance that is running the game session. When connecting to a Amazon GameLift game server, a client needs to reference an IP address (or DNS name) and port number.
gsIPAddress :: Lens' GameSession (Maybe Text)
gsIPAddress = lens _gsIPAddress (\s a -> s {_gsIPAddress = a})

-- | A unique identifier for the game session. A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .
gsGameSessionId :: Lens' GameSession (Maybe Text)
gsGameSessionId = lens _gsGameSessionId (\s a -> s {_gsGameSessionId = a})

-- | Information about the matchmaking process that was used to create the game session. It is in JSON syntax, formatted as a string. In addition the matchmaking configuration used, it contains data on all players assigned to the match, including player attributes and team assignments. For more details on matchmaker data, see <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data> . Matchmaker data is useful when requesting match backfills, and is updated whenever new players are added during a successful backfill (see 'StartMatchBackfill' ).
gsMatchmakerData :: Lens' GameSession (Maybe Text)
gsMatchmakerData = lens _gsMatchmakerData (\s a -> s {_gsMatchmakerData = a})

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet that this game session is running on.
gsFleetARN :: Lens' GameSession (Maybe Text)
gsFleetARN = lens _gsFleetARN (\s a -> s {_gsFleetARN = a})

-- | The maximum number of players that can be connected simultaneously to the game session.
gsMaximumPlayerSessionCount :: Lens' GameSession (Maybe Natural)
gsMaximumPlayerSessionCount = lens _gsMaximumPlayerSessionCount (\s a -> s {_gsMaximumPlayerSessionCount = a}) . mapping _Nat

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
gsTerminationTime :: Lens' GameSession (Maybe UTCTime)
gsTerminationTime = lens _gsTerminationTime (\s a -> s {_gsTerminationTime = a}) . mapping _Time

-- | Indicates whether or not the game session is accepting new players.
gsPlayerSessionCreationPolicy :: Lens' GameSession (Maybe PlayerSessionCreationPolicy)
gsPlayerSessionCreationPolicy = lens _gsPlayerSessionCreationPolicy (\s a -> s {_gsPlayerSessionCreationPolicy = a})

-- | A descriptive label that is associated with a game session. Session names do not need to be unique.
gsName :: Lens' GameSession (Maybe Text)
gsName = lens _gsName (\s a -> s {_gsName = a})

-- | Number of players currently in the game session.
gsCurrentPlayerSessionCount :: Lens' GameSession (Maybe Natural)
gsCurrentPlayerSessionCount = lens _gsCurrentPlayerSessionCount (\s a -> s {_gsCurrentPlayerSessionCount = a}) . mapping _Nat

-- | Provides additional information about game session status. @INTERRUPTED@ indicates that the game session was hosted on a spot instance that was reclaimed, causing the active game session to be terminated.
gsStatusReason :: Lens' GameSession (Maybe GameSessionStatusReason)
gsStatusReason = lens _gsStatusReason (\s a -> s {_gsStatusReason = a})

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
gsGameSessionData :: Lens' GameSession (Maybe Text)
gsGameSessionData = lens _gsGameSessionData (\s a -> s {_gsGameSessionData = a})

-- | A unique identifier for a fleet that the game session is running on.
gsFleetId :: Lens' GameSession (Maybe Text)
gsFleetId = lens _gsFleetId (\s a -> s {_gsFleetId = a})

-- | DNS identifier assigned to the instance that is running the game session. Values have the following format:     * TLS-enabled fleets: @<unique identifier>.<region identifier>.amazongamelift.com@ .     * Non-TLS-enabled fleets: @ec2-<unique identifier>.compute.amazonaws.com@ . (See <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing> .) When connecting to a game session that is running on a TLS-enabled fleet, you must use the DNS name, not the IP address.
gsDNSName :: Lens' GameSession (Maybe Text)
gsDNSName = lens _gsDNSName (\s a -> s {_gsDNSName = a})

-- | A unique identifier for a player. This ID is used to enforce a resource protection policy (if one exists), that limits the number of game sessions a player can create.
gsCreatorId :: Lens' GameSession (Maybe Text)
gsCreatorId = lens _gsCreatorId (\s a -> s {_gsCreatorId = a})

-- | Port number for the game session. To connect to a Amazon GameLift game server, an app needs both the IP address and port number.
gsPort :: Lens' GameSession (Maybe Natural)
gsPort = lens _gsPort (\s a -> s {_gsPort = a}) . mapping _Nat

instance FromJSON GameSession where
  parseJSON =
    withObject
      "GameSession"
      ( \x ->
          GameSession'
            <$> (x .:? "CreationTime")
            <*> (x .:? "Status")
            <*> (x .:? "GameProperties" .!= mempty)
            <*> (x .:? "IpAddress")
            <*> (x .:? "GameSessionId")
            <*> (x .:? "MatchmakerData")
            <*> (x .:? "FleetArn")
            <*> (x .:? "MaximumPlayerSessionCount")
            <*> (x .:? "TerminationTime")
            <*> (x .:? "PlayerSessionCreationPolicy")
            <*> (x .:? "Name")
            <*> (x .:? "CurrentPlayerSessionCount")
            <*> (x .:? "StatusReason")
            <*> (x .:? "GameSessionData")
            <*> (x .:? "FleetId")
            <*> (x .:? "DnsName")
            <*> (x .:? "CreatorId")
            <*> (x .:? "Port")
      )

instance Hashable GameSession

instance NFData GameSession
