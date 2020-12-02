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
-- Module      : Network.AWS.GameLift.UpdateMatchmakingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a FlexMatch matchmaking configuration. These changes affect all matches and game sessions that are created after the update. To update settings, specify the configuration name to be updated and provide the new settings.
--
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-configuration.html Design a FlexMatch Matchmaker>
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
module Network.AWS.GameLift.UpdateMatchmakingConfiguration
  ( -- * Creating a Request
    updateMatchmakingConfiguration,
    UpdateMatchmakingConfiguration,

    -- * Request Lenses
    umcBackfillMode,
    umcGameProperties,
    umcRuleSetName,
    umcAcceptanceTimeoutSeconds,
    umcRequestTimeoutSeconds,
    umcNotificationTarget,
    umcFlexMatchMode,
    umcGameSessionQueueARNs,
    umcCustomEventData,
    umcAcceptanceRequired,
    umcGameSessionData,
    umcDescription,
    umcAdditionalPlayerCount,
    umcName,

    -- * Destructuring the Response
    updateMatchmakingConfigurationResponse,
    UpdateMatchmakingConfigurationResponse,

    -- * Response Lenses
    umcrsConfiguration,
    umcrsResponseStatus,
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
-- /See:/ 'updateMatchmakingConfiguration' smart constructor.
data UpdateMatchmakingConfiguration = UpdateMatchmakingConfiguration'
  { _umcBackfillMode ::
      !(Maybe BackfillMode),
    _umcGameProperties ::
      !(Maybe [GameProperty]),
    _umcRuleSetName ::
      !(Maybe Text),
    _umcAcceptanceTimeoutSeconds ::
      !(Maybe Nat),
    _umcRequestTimeoutSeconds ::
      !(Maybe Nat),
    _umcNotificationTarget ::
      !(Maybe Text),
    _umcFlexMatchMode ::
      !(Maybe FlexMatchMode),
    _umcGameSessionQueueARNs ::
      !(Maybe [Text]),
    _umcCustomEventData ::
      !(Maybe Text),
    _umcAcceptanceRequired ::
      !(Maybe Bool),
    _umcGameSessionData ::
      !(Maybe Text),
    _umcDescription ::
      !(Maybe Text),
    _umcAdditionalPlayerCount ::
      !(Maybe Nat),
    _umcName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMatchmakingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umcBackfillMode' - The method that is used to backfill game sessions created with this matchmaking configuration. Specify MANUAL when your game manages backfill requests manually or does not use the match backfill feature. Specify AUTOMATIC to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
--
-- * 'umcGameProperties' - A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- * 'umcRuleSetName' - A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
--
-- * 'umcAcceptanceTimeoutSeconds' - The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
--
-- * 'umcRequestTimeoutSeconds' - The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
--
-- * 'umcNotificationTarget' - An SNS topic ARN that is set up to receive matchmaking notifications. See <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up Notifications for Matchmaking> for more information.
--
-- * 'umcFlexMatchMode' - Indicates whether this matchmaking configuration is being used with GameLift hosting or as a standalone matchmaking solution.      * __STANDALONE__ - FlexMatch forms matches and returns match information, including players and team assignments, in a <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded> event.     * __WITH_QUEUE__ - FlexMatch forms matches and uses the specified GameLift queue to start a game session for the match.
--
-- * 'umcGameSessionQueueARNs' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@ , do not set this parameter.
--
-- * 'umcCustomEventData' - Information to add to all events related to the matchmaking configuration.
--
-- * 'umcAcceptanceRequired' - A flag that indicates whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE. With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
--
-- * 'umcGameSessionData' - A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- * 'umcDescription' - A descriptive label that is associated with matchmaking configuration.
--
-- * 'umcAdditionalPlayerCount' - The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
--
-- * 'umcName' - A unique identifier for a matchmaking configuration to update. You can use either the configuration name or ARN value.
updateMatchmakingConfiguration ::
  -- | 'umcName'
  Text ->
  UpdateMatchmakingConfiguration
updateMatchmakingConfiguration pName_ =
  UpdateMatchmakingConfiguration'
    { _umcBackfillMode = Nothing,
      _umcGameProperties = Nothing,
      _umcRuleSetName = Nothing,
      _umcAcceptanceTimeoutSeconds = Nothing,
      _umcRequestTimeoutSeconds = Nothing,
      _umcNotificationTarget = Nothing,
      _umcFlexMatchMode = Nothing,
      _umcGameSessionQueueARNs = Nothing,
      _umcCustomEventData = Nothing,
      _umcAcceptanceRequired = Nothing,
      _umcGameSessionData = Nothing,
      _umcDescription = Nothing,
      _umcAdditionalPlayerCount = Nothing,
      _umcName = pName_
    }

-- | The method that is used to backfill game sessions created with this matchmaking configuration. Specify MANUAL when your game manages backfill requests manually or does not use the match backfill feature. Specify AUTOMATIC to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-backfill.html Backfill Existing Games with FlexMatch> . Automatic backfill is not available when @FlexMatchMode@ is set to @STANDALONE@ .
umcBackfillMode :: Lens' UpdateMatchmakingConfiguration (Maybe BackfillMode)
umcBackfillMode = lens _umcBackfillMode (\s a -> s {_umcBackfillMode = a})

-- | A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
umcGameProperties :: Lens' UpdateMatchmakingConfiguration [GameProperty]
umcGameProperties = lens _umcGameProperties (\s a -> s {_umcGameProperties = a}) . _Default . _Coerce

-- | A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
umcRuleSetName :: Lens' UpdateMatchmakingConfiguration (Maybe Text)
umcRuleSetName = lens _umcRuleSetName (\s a -> s {_umcRuleSetName = a})

-- | The length of time (in seconds) to wait for players to accept a proposed match, if acceptance is required. If any player rejects the match or fails to accept before the timeout, the tickets are returned to the ticket pool and continue to be evaluated for an acceptable match.
umcAcceptanceTimeoutSeconds :: Lens' UpdateMatchmakingConfiguration (Maybe Natural)
umcAcceptanceTimeoutSeconds = lens _umcAcceptanceTimeoutSeconds (\s a -> s {_umcAcceptanceTimeoutSeconds = a}) . mapping _Nat

-- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
umcRequestTimeoutSeconds :: Lens' UpdateMatchmakingConfiguration (Maybe Natural)
umcRequestTimeoutSeconds = lens _umcRequestTimeoutSeconds (\s a -> s {_umcRequestTimeoutSeconds = a}) . mapping _Nat

-- | An SNS topic ARN that is set up to receive matchmaking notifications. See <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-notification.html Setting up Notifications for Matchmaking> for more information.
umcNotificationTarget :: Lens' UpdateMatchmakingConfiguration (Maybe Text)
umcNotificationTarget = lens _umcNotificationTarget (\s a -> s {_umcNotificationTarget = a})

-- | Indicates whether this matchmaking configuration is being used with GameLift hosting or as a standalone matchmaking solution.      * __STANDALONE__ - FlexMatch forms matches and returns match information, including players and team assignments, in a <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-events.html#match-events-matchmakingsucceeded MatchmakingSucceeded> event.     * __WITH_QUEUE__ - FlexMatch forms matches and uses the specified GameLift queue to start a game session for the match.
umcFlexMatchMode :: Lens' UpdateMatchmakingConfiguration (Maybe FlexMatchMode)
umcFlexMatchMode = lens _umcFlexMatchMode (\s a -> s {_umcFlexMatchMode = a})

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. Queues can be located in any Region. Queues are used to start new GameLift-hosted game sessions for matches that are created with this matchmaking configuration. If @FlexMatchMode@ is set to @STANDALONE@ , do not set this parameter.
umcGameSessionQueueARNs :: Lens' UpdateMatchmakingConfiguration [Text]
umcGameSessionQueueARNs = lens _umcGameSessionQueueARNs (\s a -> s {_umcGameSessionQueueARNs = a}) . _Default . _Coerce

-- | Information to add to all events related to the matchmaking configuration.
umcCustomEventData :: Lens' UpdateMatchmakingConfiguration (Maybe Text)
umcCustomEventData = lens _umcCustomEventData (\s a -> s {_umcCustomEventData = a})

-- | A flag that indicates whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE. With this option enabled, matchmaking tickets use the status @REQUIRES_ACCEPTANCE@ to indicate when a completed potential match is waiting for player acceptance.
umcAcceptanceRequired :: Lens' UpdateMatchmakingConfiguration (Maybe Bool)
umcAcceptanceRequired = lens _umcAcceptanceRequired (\s a -> s {_umcAcceptanceRequired = a})

-- | A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
umcGameSessionData :: Lens' UpdateMatchmakingConfiguration (Maybe Text)
umcGameSessionData = lens _umcGameSessionData (\s a -> s {_umcGameSessionData = a})

-- | A descriptive label that is associated with matchmaking configuration.
umcDescription :: Lens' UpdateMatchmakingConfiguration (Maybe Text)
umcDescription = lens _umcDescription (\s a -> s {_umcDescription = a})

-- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match. This parameter is not used if @FlexMatchMode@ is set to @STANDALONE@ .
umcAdditionalPlayerCount :: Lens' UpdateMatchmakingConfiguration (Maybe Natural)
umcAdditionalPlayerCount = lens _umcAdditionalPlayerCount (\s a -> s {_umcAdditionalPlayerCount = a}) . mapping _Nat

-- | A unique identifier for a matchmaking configuration to update. You can use either the configuration name or ARN value.
umcName :: Lens' UpdateMatchmakingConfiguration Text
umcName = lens _umcName (\s a -> s {_umcName = a})

instance AWSRequest UpdateMatchmakingConfiguration where
  type
    Rs UpdateMatchmakingConfiguration =
      UpdateMatchmakingConfigurationResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          UpdateMatchmakingConfigurationResponse'
            <$> (x .?> "Configuration") <*> (pure (fromEnum s))
      )

instance Hashable UpdateMatchmakingConfiguration

instance NFData UpdateMatchmakingConfiguration

instance ToHeaders UpdateMatchmakingConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.UpdateMatchmakingConfiguration" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateMatchmakingConfiguration where
  toJSON UpdateMatchmakingConfiguration' {..} =
    object
      ( catMaybes
          [ ("BackfillMode" .=) <$> _umcBackfillMode,
            ("GameProperties" .=) <$> _umcGameProperties,
            ("RuleSetName" .=) <$> _umcRuleSetName,
            ("AcceptanceTimeoutSeconds" .=) <$> _umcAcceptanceTimeoutSeconds,
            ("RequestTimeoutSeconds" .=) <$> _umcRequestTimeoutSeconds,
            ("NotificationTarget" .=) <$> _umcNotificationTarget,
            ("FlexMatchMode" .=) <$> _umcFlexMatchMode,
            ("GameSessionQueueArns" .=) <$> _umcGameSessionQueueARNs,
            ("CustomEventData" .=) <$> _umcCustomEventData,
            ("AcceptanceRequired" .=) <$> _umcAcceptanceRequired,
            ("GameSessionData" .=) <$> _umcGameSessionData,
            ("Description" .=) <$> _umcDescription,
            ("AdditionalPlayerCount" .=) <$> _umcAdditionalPlayerCount,
            Just ("Name" .= _umcName)
          ]
      )

instance ToPath UpdateMatchmakingConfiguration where
  toPath = const "/"

instance ToQuery UpdateMatchmakingConfiguration where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'updateMatchmakingConfigurationResponse' smart constructor.
data UpdateMatchmakingConfigurationResponse = UpdateMatchmakingConfigurationResponse'
  { _umcrsConfiguration ::
      !( Maybe
           MatchmakingConfiguration
       ),
    _umcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMatchmakingConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umcrsConfiguration' - The updated matchmaking configuration.
--
-- * 'umcrsResponseStatus' - -- | The response status code.
updateMatchmakingConfigurationResponse ::
  -- | 'umcrsResponseStatus'
  Int ->
  UpdateMatchmakingConfigurationResponse
updateMatchmakingConfigurationResponse pResponseStatus_ =
  UpdateMatchmakingConfigurationResponse'
    { _umcrsConfiguration =
        Nothing,
      _umcrsResponseStatus = pResponseStatus_
    }

-- | The updated matchmaking configuration.
umcrsConfiguration :: Lens' UpdateMatchmakingConfigurationResponse (Maybe MatchmakingConfiguration)
umcrsConfiguration = lens _umcrsConfiguration (\s a -> s {_umcrsConfiguration = a})

-- | -- | The response status code.
umcrsResponseStatus :: Lens' UpdateMatchmakingConfigurationResponse Int
umcrsResponseStatus = lens _umcrsResponseStatus (\s a -> s {_umcrsResponseStatus = a})

instance NFData UpdateMatchmakingConfigurationResponse
