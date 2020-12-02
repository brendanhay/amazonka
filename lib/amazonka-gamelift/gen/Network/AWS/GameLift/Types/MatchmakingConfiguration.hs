{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchmakingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchmakingConfiguration where

import Network.AWS.GameLift.Types.BackfillMode
import Network.AWS.GameLift.Types.FlexMatchMode
import Network.AWS.GameLift.Types.GameProperty
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Guidelines for use with FlexMatch to match players into games. All matchmaking requests must specify a matchmaking configuration.
--
--
--
-- /See:/ 'matchmakingConfiguration' smart constructor.
data MatchmakingConfiguration = MatchmakingConfiguration'
  { _mcCreationTime ::
      !(Maybe POSIX),
    _mcBackfillMode :: !(Maybe BackfillMode),
    _mcGameProperties ::
      !(Maybe [GameProperty]),
    _mcRuleSetName :: !(Maybe Text),
    _mcAcceptanceTimeoutSeconds ::
      !(Maybe Nat),
    _mcRequestTimeoutSeconds :: !(Maybe Nat),
    _mcNotificationTarget :: !(Maybe Text),
    _mcFlexMatchMode ::
      !(Maybe FlexMatchMode),
    _mcGameSessionQueueARNs ::
      !(Maybe [Text]),
    _mcName :: !(Maybe Text),
    _mcCustomEventData :: !(Maybe Text),
    _mcConfigurationARN :: !(Maybe Text),
    _mcAcceptanceRequired :: !(Maybe Bool),
    _mcGameSessionData :: !(Maybe Text),
    _mcDescription :: !(Maybe Text),
    _mcAdditionalPlayerCount :: !(Maybe Nat),
    _mcRuleSetARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MatchmakingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcCreationTime' - The time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'mcBackfillMode' - The method used to backfill game sessions created with this matchmaking configuration. MANUAL indicates that the game makes backfill requests or does not use the match backfill feature. AUTOMATIC indicates that GameLift creates 'StartMatchBackfill' requests whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- * 'mcGameProperties' - A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- * 'mcRuleSetName' - A unique identifier for a matchmaking rule set to use with this configuration. A matchmaking configuration can only use rule sets that are defined in the same Region.
--
-- * 'mcAcceptanceTimeoutSeconds' - The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
--
-- * 'mcRequestTimeoutSeconds' - The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
--
-- * 'mcNotificationTarget' - An SNS topic ARN that is set up to receive matchmaking notifications.
--
-- * 'mcFlexMatchMode' - Indicates whether this matchmaking configuration is being used with GameLift hosting or as a standalone matchmaking solution.      * __STANDALONE__ - FlexMatch forms matches and returns match information, including players and team assignments, in a <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded> event.     * __WITH_QUEUE__ - FlexMatch forms matches and uses the specified GameLift queue to start a game session for the match.
--
-- * 'mcGameSessionQueueARNs' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. Thais property is not set when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- * 'mcName' - A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
--
-- * 'mcCustomEventData' - Information to attach to all events related to the matchmaking configuration.
--
-- * 'mcConfigurationARN' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift matchmaking configuration resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift configuration ARN, the resource ID matches the /Name/ value.
--
-- * 'mcAcceptanceRequired' - A flag that indicates whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE. When this option is enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
--
-- * 'mcGameSessionData' - A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- * 'mcDescription' - A descriptive label that is associated with matchmaking configuration.
--
-- * 'mcAdditionalPlayerCount' - The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- * 'mcRuleSetARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift matchmaking rule set resource that this configuration uses.
matchmakingConfiguration ::
  MatchmakingConfiguration
matchmakingConfiguration =
  MatchmakingConfiguration'
    { _mcCreationTime = Nothing,
      _mcBackfillMode = Nothing,
      _mcGameProperties = Nothing,
      _mcRuleSetName = Nothing,
      _mcAcceptanceTimeoutSeconds = Nothing,
      _mcRequestTimeoutSeconds = Nothing,
      _mcNotificationTarget = Nothing,
      _mcFlexMatchMode = Nothing,
      _mcGameSessionQueueARNs = Nothing,
      _mcName = Nothing,
      _mcCustomEventData = Nothing,
      _mcConfigurationARN = Nothing,
      _mcAcceptanceRequired = Nothing,
      _mcGameSessionData = Nothing,
      _mcDescription = Nothing,
      _mcAdditionalPlayerCount = Nothing,
      _mcRuleSetARN = Nothing
    }

-- | The time stamp indicating when this data object was created. The format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
mcCreationTime :: Lens' MatchmakingConfiguration (Maybe UTCTime)
mcCreationTime = lens _mcCreationTime (\s a -> s {_mcCreationTime = a}) . mapping _Time

-- | The method used to backfill game sessions created with this matchmaking configuration. MANUAL indicates that the game makes backfill requests or does not use the match backfill feature. AUTOMATIC indicates that GameLift creates 'StartMatchBackfill' requests whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
mcBackfillMode :: Lens' MatchmakingConfiguration (Maybe BackfillMode)
mcBackfillMode = lens _mcBackfillMode (\s a -> s {_mcBackfillMode = a})

-- | A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
mcGameProperties :: Lens' MatchmakingConfiguration [GameProperty]
mcGameProperties = lens _mcGameProperties (\s a -> s {_mcGameProperties = a}) . _Default . _Coerce

-- | A unique identifier for a matchmaking rule set to use with this configuration. A matchmaking configuration can only use rule sets that are defined in the same Region.
mcRuleSetName :: Lens' MatchmakingConfiguration (Maybe Text)
mcRuleSetName = lens _mcRuleSetName (\s a -> s {_mcRuleSetName = a})

-- | The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
mcAcceptanceTimeoutSeconds :: Lens' MatchmakingConfiguration (Maybe Natural)
mcAcceptanceTimeoutSeconds = lens _mcAcceptanceTimeoutSeconds (\s a -> s {_mcAcceptanceTimeoutSeconds = a}) . mapping _Nat

-- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
mcRequestTimeoutSeconds :: Lens' MatchmakingConfiguration (Maybe Natural)
mcRequestTimeoutSeconds = lens _mcRequestTimeoutSeconds (\s a -> s {_mcRequestTimeoutSeconds = a}) . mapping _Nat

-- | An SNS topic ARN that is set up to receive matchmaking notifications.
mcNotificationTarget :: Lens' MatchmakingConfiguration (Maybe Text)
mcNotificationTarget = lens _mcNotificationTarget (\s a -> s {_mcNotificationTarget = a})

-- | Indicates whether this matchmaking configuration is being used with GameLift hosting or as a standalone matchmaking solution.      * __STANDALONE__ - FlexMatch forms matches and returns match information, including players and team assignments, in a <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded> event.     * __WITH_QUEUE__ - FlexMatch forms matches and uses the specified GameLift queue to start a game session for the match.
mcFlexMatchMode :: Lens' MatchmakingConfiguration (Maybe FlexMatchMode)
mcFlexMatchMode = lens _mcFlexMatchMode (\s a -> s {_mcFlexMatchMode = a})

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. Thais property is not set when @FlexMatchMode@ is set to @STANDALONE@ .
mcGameSessionQueueARNs :: Lens' MatchmakingConfiguration [Text]
mcGameSessionQueueARNs = lens _mcGameSessionQueueARNs (\s a -> s {_mcGameSessionQueueARNs = a}) . _Default . _Coerce

-- | A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
mcName :: Lens' MatchmakingConfiguration (Maybe Text)
mcName = lens _mcName (\s a -> s {_mcName = a})

-- | Information to attach to all events related to the matchmaking configuration.
mcCustomEventData :: Lens' MatchmakingConfiguration (Maybe Text)
mcCustomEventData = lens _mcCustomEventData (\s a -> s {_mcCustomEventData = a})

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift matchmaking configuration resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift configuration ARN, the resource ID matches the /Name/ value.
mcConfigurationARN :: Lens' MatchmakingConfiguration (Maybe Text)
mcConfigurationARN = lens _mcConfigurationARN (\s a -> s {_mcConfigurationARN = a})

-- | A flag that indicates whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE. When this option is enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
mcAcceptanceRequired :: Lens' MatchmakingConfiguration (Maybe Bool)
mcAcceptanceRequired = lens _mcAcceptanceRequired (\s a -> s {_mcAcceptanceRequired = a})

-- | A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
mcGameSessionData :: Lens' MatchmakingConfiguration (Maybe Text)
mcGameSessionData = lens _mcGameSessionData (\s a -> s {_mcGameSessionData = a})

-- | A descriptive label that is associated with matchmaking configuration.
mcDescription :: Lens' MatchmakingConfiguration (Maybe Text)
mcDescription = lens _mcDescription (\s a -> s {_mcDescription = a})

-- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used when @FlexMatchMode@ is set to @STANDALONE@ .
mcAdditionalPlayerCount :: Lens' MatchmakingConfiguration (Maybe Natural)
mcAdditionalPlayerCount = lens _mcAdditionalPlayerCount (\s a -> s {_mcAdditionalPlayerCount = a}) . mapping _Nat

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift matchmaking rule set resource that this configuration uses.
mcRuleSetARN :: Lens' MatchmakingConfiguration (Maybe Text)
mcRuleSetARN = lens _mcRuleSetARN (\s a -> s {_mcRuleSetARN = a})

instance FromJSON MatchmakingConfiguration where
  parseJSON =
    withObject
      "MatchmakingConfiguration"
      ( \x ->
          MatchmakingConfiguration'
            <$> (x .:? "CreationTime")
            <*> (x .:? "BackfillMode")
            <*> (x .:? "GameProperties" .!= mempty)
            <*> (x .:? "RuleSetName")
            <*> (x .:? "AcceptanceTimeoutSeconds")
            <*> (x .:? "RequestTimeoutSeconds")
            <*> (x .:? "NotificationTarget")
            <*> (x .:? "FlexMatchMode")
            <*> (x .:? "GameSessionQueueArns" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "CustomEventData")
            <*> (x .:? "ConfigurationArn")
            <*> (x .:? "AcceptanceRequired")
            <*> (x .:? "GameSessionData")
            <*> (x .:? "Description")
            <*> (x .:? "AdditionalPlayerCount")
            <*> (x .:? "RuleSetArn")
      )

instance Hashable MatchmakingConfiguration

instance NFData MatchmakingConfiguration
