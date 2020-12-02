{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateMatchmakingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines a new matchmaking configuration for use with FlexMatch. Whether your are using FlexMatch with GameLift hosting or as a standalone matchmaking service, the matchmaking configuration sets out rules for matching players and forming teams. If you're also using GameLift hosting, it defines how to start game sessions for each match. Your matchmaking system can use multiple configurations to handle different game scenarios. All matchmaking requests ('StartMatchmaking' or 'StartMatchBackfill' ) identify the matchmaking configuration to use and provide player attributes consistent with that configuration.
--
--
-- To create a matchmaking configuration, you must provide the following: configuration name and FlexMatch mode (with or without GameLift hosting); a rule set that specifies how to evaluate players and find acceptable matches; whether player acceptance is required; and the maximum time allowed for a matchmaking attempt. When using FlexMatch with GameLift hosting, you also need to identify the game session queue to use when starting a game session for the match.
--
-- In addition, you must set up an Amazon Simple Notification Service (SNS) to receive matchmaking notifications, and provide the topic ARN in the matchmaking configuration. An alternative method, continuously polling ticket status with 'DescribeMatchmaking' , is only suitable for games in development with low matchmaking usage.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/gamelift-match.html FlexMatch Developer Guide>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-configuration.html Design a FlexMatch Matchmaker>
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Set Up FlexMatch Event Notification>
--
-- __Related operations__
--
--     * 'CreateMatchmakingConfiguration'
--
--     * 'DescribeMatchmakingConfigurations'
--
--     * 'UpdateMatchmakingConfiguration'
--
--     * 'DeleteMatchmakingConfiguration'
--
--     * 'CreateMatchmakingRuleSet'
--
--     * 'DescribeMatchmakingRuleSets'
--
--     * 'ValidateMatchmakingRuleSet'
--
--     * 'DeleteMatchmakingRuleSet'
module Network.AWS.GameLift.CreateMatchmakingConfiguration
  ( -- * Creating a Request
    createMatchmakingConfiguration,
    CreateMatchmakingConfiguration,

    -- * Request Lenses
    cmcBackfillMode,
    cmcGameProperties,
    cmcAcceptanceTimeoutSeconds,
    cmcNotificationTarget,
    cmcFlexMatchMode,
    cmcGameSessionQueueARNs,
    cmcCustomEventData,
    cmcGameSessionData,
    cmcDescription,
    cmcTags,
    cmcAdditionalPlayerCount,
    cmcName,
    cmcRequestTimeoutSeconds,
    cmcAcceptanceRequired,
    cmcRuleSetName,

    -- * Destructuring the Response
    createMatchmakingConfigurationResponse,
    CreateMatchmakingConfigurationResponse,

    -- * Response Lenses
    cmcrsConfiguration,
    cmcrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request operation.
--
--
--
-- /See:/ 'createMatchmakingConfiguration' smart constructor.
data CreateMatchmakingConfiguration = CreateMatchmakingConfiguration'
  { _cmcBackfillMode ::
      !(Maybe BackfillMode),
    _cmcGameProperties ::
      !(Maybe [GameProperty]),
    _cmcAcceptanceTimeoutSeconds ::
      !(Maybe Nat),
    _cmcNotificationTarget ::
      !(Maybe Text),
    _cmcFlexMatchMode ::
      !(Maybe FlexMatchMode),
    _cmcGameSessionQueueARNs ::
      !(Maybe [Text]),
    _cmcCustomEventData ::
      !(Maybe Text),
    _cmcGameSessionData ::
      !(Maybe Text),
    _cmcDescription ::
      !(Maybe Text),
    _cmcTags :: !(Maybe [Tag]),
    _cmcAdditionalPlayerCount ::
      !(Maybe Nat),
    _cmcName :: !Text,
    _cmcRequestTimeoutSeconds ::
      !Nat,
    _cmcAcceptanceRequired ::
      !Bool,
    _cmcRuleSetName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMatchmakingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmcBackfillMode' - The method used to backfill game sessions that are created with this matchmaking configuration. Specify @MANUAL@ when your game manages backfill requests manually or does not use the match backfill feature. Specify @AUTOMATIC@ to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- * 'cmcGameProperties' - A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- * 'cmcAcceptanceTimeoutSeconds' - The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
--
-- * 'cmcNotificationTarget' - An SNS topic ARN that is set up to receive matchmaking notifications.
--
-- * 'cmcFlexMatchMode' - Indicates whether this matchmaking configuration is being used with GameLift hosting or as a standalone matchmaking solution.      * __STANDALONE__ - FlexMatch forms matches and returns match information, including players and team assignments, in a <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded> event.     * __WITH_QUEUE__ - FlexMatch forms matches and uses the specified GameLift queue to start a game session for the match.
--
-- * 'cmcGameSessionQueueARNs' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@ , do not set this parameter.
--
-- * 'cmcCustomEventData' - Information to be added to all events related to this matchmaking configuration.
--
-- * 'cmcGameSessionData' - A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- * 'cmcDescription' - A human-readable description of the matchmaking configuration.
--
-- * 'cmcTags' - A list of labels to assign to the new matchmaking configuration resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- * 'cmcAdditionalPlayerCount' - The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- * 'cmcName' - A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
--
-- * 'cmcRequestTimeoutSeconds' - The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
--
-- * 'cmcAcceptanceRequired' - A flag that determines whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to @TRUE@ . With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
--
-- * 'cmcRuleSetName' - A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
createMatchmakingConfiguration ::
  -- | 'cmcName'
  Text ->
  -- | 'cmcRequestTimeoutSeconds'
  Natural ->
  -- | 'cmcAcceptanceRequired'
  Bool ->
  -- | 'cmcRuleSetName'
  Text ->
  CreateMatchmakingConfiguration
createMatchmakingConfiguration
  pName_
  pRequestTimeoutSeconds_
  pAcceptanceRequired_
  pRuleSetName_ =
    CreateMatchmakingConfiguration'
      { _cmcBackfillMode = Nothing,
        _cmcGameProperties = Nothing,
        _cmcAcceptanceTimeoutSeconds = Nothing,
        _cmcNotificationTarget = Nothing,
        _cmcFlexMatchMode = Nothing,
        _cmcGameSessionQueueARNs = Nothing,
        _cmcCustomEventData = Nothing,
        _cmcGameSessionData = Nothing,
        _cmcDescription = Nothing,
        _cmcTags = Nothing,
        _cmcAdditionalPlayerCount = Nothing,
        _cmcName = pName_,
        _cmcRequestTimeoutSeconds = _Nat # pRequestTimeoutSeconds_,
        _cmcAcceptanceRequired = pAcceptanceRequired_,
        _cmcRuleSetName = pRuleSetName_
      }

-- | The method used to backfill game sessions that are created with this matchmaking configuration. Specify @MANUAL@ when your game manages backfill requests manually or does not use the match backfill feature. Specify @AUTOMATIC@ to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
cmcBackfillMode :: Lens' CreateMatchmakingConfiguration (Maybe BackfillMode)
cmcBackfillMode = lens _cmcBackfillMode (\s a -> s {_cmcBackfillMode = a})

-- | A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
cmcGameProperties :: Lens' CreateMatchmakingConfiguration [GameProperty]
cmcGameProperties = lens _cmcGameProperties (\s a -> s {_cmcGameProperties = a}) . _Default . _Coerce

-- | The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
cmcAcceptanceTimeoutSeconds :: Lens' CreateMatchmakingConfiguration (Maybe Natural)
cmcAcceptanceTimeoutSeconds = lens _cmcAcceptanceTimeoutSeconds (\s a -> s {_cmcAcceptanceTimeoutSeconds = a}) . mapping _Nat

-- | An SNS topic ARN that is set up to receive matchmaking notifications.
cmcNotificationTarget :: Lens' CreateMatchmakingConfiguration (Maybe Text)
cmcNotificationTarget = lens _cmcNotificationTarget (\s a -> s {_cmcNotificationTarget = a})

-- | Indicates whether this matchmaking configuration is being used with GameLift hosting or as a standalone matchmaking solution.      * __STANDALONE__ - FlexMatch forms matches and returns match information, including players and team assignments, in a <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded> event.     * __WITH_QUEUE__ - FlexMatch forms matches and uses the specified GameLift queue to start a game session for the match.
cmcFlexMatchMode :: Lens' CreateMatchmakingConfiguration (Maybe FlexMatchMode)
cmcFlexMatchMode = lens _cmcFlexMatchMode (\s a -> s {_cmcFlexMatchMode = a})

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@ , do not set this parameter.
cmcGameSessionQueueARNs :: Lens' CreateMatchmakingConfiguration [Text]
cmcGameSessionQueueARNs = lens _cmcGameSessionQueueARNs (\s a -> s {_cmcGameSessionQueueARNs = a}) . _Default . _Coerce

-- | Information to be added to all events related to this matchmaking configuration.
cmcCustomEventData :: Lens' CreateMatchmakingConfiguration (Maybe Text)
cmcCustomEventData = lens _cmcCustomEventData (\s a -> s {_cmcCustomEventData = a})

-- | A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
cmcGameSessionData :: Lens' CreateMatchmakingConfiguration (Maybe Text)
cmcGameSessionData = lens _cmcGameSessionData (\s a -> s {_cmcGameSessionData = a})

-- | A human-readable description of the matchmaking configuration.
cmcDescription :: Lens' CreateMatchmakingConfiguration (Maybe Text)
cmcDescription = lens _cmcDescription (\s a -> s {_cmcDescription = a})

-- | A list of labels to assign to the new matchmaking configuration resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
cmcTags :: Lens' CreateMatchmakingConfiguration [Tag]
cmcTags = lens _cmcTags (\s a -> s {_cmcTags = a}) . _Default . _Coerce

-- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
cmcAdditionalPlayerCount :: Lens' CreateMatchmakingConfiguration (Maybe Natural)
cmcAdditionalPlayerCount = lens _cmcAdditionalPlayerCount (\s a -> s {_cmcAdditionalPlayerCount = a}) . mapping _Nat

-- | A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
cmcName :: Lens' CreateMatchmakingConfiguration Text
cmcName = lens _cmcName (\s a -> s {_cmcName = a})

-- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
cmcRequestTimeoutSeconds :: Lens' CreateMatchmakingConfiguration Natural
cmcRequestTimeoutSeconds = lens _cmcRequestTimeoutSeconds (\s a -> s {_cmcRequestTimeoutSeconds = a}) . _Nat

-- | A flag that determines whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to @TRUE@ . With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
cmcAcceptanceRequired :: Lens' CreateMatchmakingConfiguration Bool
cmcAcceptanceRequired = lens _cmcAcceptanceRequired (\s a -> s {_cmcAcceptanceRequired = a})

-- | A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
cmcRuleSetName :: Lens' CreateMatchmakingConfiguration Text
cmcRuleSetName = lens _cmcRuleSetName (\s a -> s {_cmcRuleSetName = a})

instance AWSRequest CreateMatchmakingConfiguration where
  type
    Rs CreateMatchmakingConfiguration =
      CreateMatchmakingConfigurationResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          CreateMatchmakingConfigurationResponse'
            <$> (x .?> "Configuration") <*> (pure (fromEnum s))
      )

instance Hashable CreateMatchmakingConfiguration

instance NFData CreateMatchmakingConfiguration

instance ToHeaders CreateMatchmakingConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.CreateMatchmakingConfiguration" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateMatchmakingConfiguration where
  toJSON CreateMatchmakingConfiguration' {..} =
    object
      ( catMaybes
          [ ("BackfillMode" .=) <$> _cmcBackfillMode,
            ("GameProperties" .=) <$> _cmcGameProperties,
            ("AcceptanceTimeoutSeconds" .=) <$> _cmcAcceptanceTimeoutSeconds,
            ("NotificationTarget" .=) <$> _cmcNotificationTarget,
            ("FlexMatchMode" .=) <$> _cmcFlexMatchMode,
            ("GameSessionQueueArns" .=) <$> _cmcGameSessionQueueARNs,
            ("CustomEventData" .=) <$> _cmcCustomEventData,
            ("GameSessionData" .=) <$> _cmcGameSessionData,
            ("Description" .=) <$> _cmcDescription,
            ("Tags" .=) <$> _cmcTags,
            ("AdditionalPlayerCount" .=) <$> _cmcAdditionalPlayerCount,
            Just ("Name" .= _cmcName),
            Just ("RequestTimeoutSeconds" .= _cmcRequestTimeoutSeconds),
            Just ("AcceptanceRequired" .= _cmcAcceptanceRequired),
            Just ("RuleSetName" .= _cmcRuleSetName)
          ]
      )

instance ToPath CreateMatchmakingConfiguration where
  toPath = const "/"

instance ToQuery CreateMatchmakingConfiguration where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'createMatchmakingConfigurationResponse' smart constructor.
data CreateMatchmakingConfigurationResponse = CreateMatchmakingConfigurationResponse'
  { _cmcrsConfiguration ::
      !( Maybe
           MatchmakingConfiguration
       ),
    _cmcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMatchmakingConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmcrsConfiguration' - Object that describes the newly created matchmaking configuration.
--
-- * 'cmcrsResponseStatus' - -- | The response status code.
createMatchmakingConfigurationResponse ::
  -- | 'cmcrsResponseStatus'
  Int ->
  CreateMatchmakingConfigurationResponse
createMatchmakingConfigurationResponse pResponseStatus_ =
  CreateMatchmakingConfigurationResponse'
    { _cmcrsConfiguration =
        Nothing,
      _cmcrsResponseStatus = pResponseStatus_
    }

-- | Object that describes the newly created matchmaking configuration.
cmcrsConfiguration :: Lens' CreateMatchmakingConfigurationResponse (Maybe MatchmakingConfiguration)
cmcrsConfiguration = lens _cmcrsConfiguration (\s a -> s {_cmcrsConfiguration = a})

-- | -- | The response status code.
cmcrsResponseStatus :: Lens' CreateMatchmakingConfigurationResponse Int
cmcrsResponseStatus = lens _cmcrsResponseStatus (\s a -> s {_cmcrsResponseStatus = a})

instance NFData CreateMatchmakingConfigurationResponse
