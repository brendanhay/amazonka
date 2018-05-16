{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateMatchmakingConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a FlexMatch matchmaking configuration. To update settings, specify the configuration name to be updated and provide the new settings.
--
--
-- Operations related to match configurations and rule sets include:
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
--
--
module Network.AWS.GameLift.UpdateMatchmakingConfiguration
    (
    -- * Creating a Request
      updateMatchmakingConfiguration
    , UpdateMatchmakingConfiguration
    -- * Request Lenses
    , umcGameProperties
    , umcRuleSetName
    , umcAcceptanceTimeoutSeconds
    , umcRequestTimeoutSeconds
    , umcNotificationTarget
    , umcGameSessionQueueARNs
    , umcCustomEventData
    , umcAcceptanceRequired
    , umcGameSessionData
    , umcDescription
    , umcAdditionalPlayerCount
    , umcName

    -- * Destructuring the Response
    , updateMatchmakingConfigurationResponse
    , UpdateMatchmakingConfigurationResponse
    -- * Response Lenses
    , umcrsConfiguration
    , umcrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'updateMatchmakingConfiguration' smart constructor.
data UpdateMatchmakingConfiguration = UpdateMatchmakingConfiguration'
  { _umcGameProperties           :: !(Maybe [GameProperty])
  , _umcRuleSetName              :: !(Maybe Text)
  , _umcAcceptanceTimeoutSeconds :: !(Maybe Nat)
  , _umcRequestTimeoutSeconds    :: !(Maybe Nat)
  , _umcNotificationTarget       :: !(Maybe Text)
  , _umcGameSessionQueueARNs     :: !(Maybe [Text])
  , _umcCustomEventData          :: !(Maybe Text)
  , _umcAcceptanceRequired       :: !(Maybe Bool)
  , _umcGameSessionData          :: !(Maybe Text)
  , _umcDescription              :: !(Maybe Text)
  , _umcAdditionalPlayerCount    :: !(Maybe Nat)
  , _umcName                     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateMatchmakingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umcGameProperties' - Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match.
--
-- * 'umcRuleSetName' - Unique identifier for a matchmaking rule set to use with this configuration. A matchmaking configuration can only use rule sets that are defined in the same region.
--
-- * 'umcAcceptanceTimeoutSeconds' - Length of time (in seconds) to wait for players to accept a proposed match. If any player rejects the match or fails to accept before the timeout, the ticket continues to look for an acceptable match.
--
-- * 'umcRequestTimeoutSeconds' - Maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that time out can be resubmitted as needed.
--
-- * 'umcNotificationTarget' - SNS topic ARN that is set up to receive matchmaking notifications. See <http://docs.aws.amazon.com/gamelift/latest/developerguide/match-notification.html Setting up Notifications for Matchmaking> for more information.
--
-- * 'umcGameSessionQueueARNs' - Amazon Resource Name (<http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) that is assigned to a game session queue and uniquely identifies it. Format is @arn:aws:gamelift:<region>::fleet/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@ . These queues are used when placing game sessions for matches that are created with this matchmaking configuration. Queues can be located in any region.
--
-- * 'umcCustomEventData' - Information to attached to all events related to the matchmaking configuration.
--
-- * 'umcAcceptanceRequired' - Flag that determines whether or not a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE.
--
-- * 'umcGameSessionData' - Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match.
--
-- * 'umcDescription' - Descriptive label that is associated with matchmaking configuration.
--
-- * 'umcAdditionalPlayerCount' - Number of player slots in a match to keep open for future players. For example, if the configuration's rule set specifies a match for a single 12-person team, and the additional player count is set to 2, only 10 players are selected for the match.
--
-- * 'umcName' - Unique identifier for a matchmaking configuration to update.
updateMatchmakingConfiguration
    :: Text -- ^ 'umcName'
    -> UpdateMatchmakingConfiguration
updateMatchmakingConfiguration pName_ =
  UpdateMatchmakingConfiguration'
    { _umcGameProperties = Nothing
    , _umcRuleSetName = Nothing
    , _umcAcceptanceTimeoutSeconds = Nothing
    , _umcRequestTimeoutSeconds = Nothing
    , _umcNotificationTarget = Nothing
    , _umcGameSessionQueueARNs = Nothing
    , _umcCustomEventData = Nothing
    , _umcAcceptanceRequired = Nothing
    , _umcGameSessionData = Nothing
    , _umcDescription = Nothing
    , _umcAdditionalPlayerCount = Nothing
    , _umcName = pName_
    }


-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match.
umcGameProperties :: Lens' UpdateMatchmakingConfiguration [GameProperty]
umcGameProperties = lens _umcGameProperties (\ s a -> s{_umcGameProperties = a}) . _Default . _Coerce

-- | Unique identifier for a matchmaking rule set to use with this configuration. A matchmaking configuration can only use rule sets that are defined in the same region.
umcRuleSetName :: Lens' UpdateMatchmakingConfiguration (Maybe Text)
umcRuleSetName = lens _umcRuleSetName (\ s a -> s{_umcRuleSetName = a})

-- | Length of time (in seconds) to wait for players to accept a proposed match. If any player rejects the match or fails to accept before the timeout, the ticket continues to look for an acceptable match.
umcAcceptanceTimeoutSeconds :: Lens' UpdateMatchmakingConfiguration (Maybe Natural)
umcAcceptanceTimeoutSeconds = lens _umcAcceptanceTimeoutSeconds (\ s a -> s{_umcAcceptanceTimeoutSeconds = a}) . mapping _Nat

-- | Maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that time out can be resubmitted as needed.
umcRequestTimeoutSeconds :: Lens' UpdateMatchmakingConfiguration (Maybe Natural)
umcRequestTimeoutSeconds = lens _umcRequestTimeoutSeconds (\ s a -> s{_umcRequestTimeoutSeconds = a}) . mapping _Nat

-- | SNS topic ARN that is set up to receive matchmaking notifications. See <http://docs.aws.amazon.com/gamelift/latest/developerguide/match-notification.html Setting up Notifications for Matchmaking> for more information.
umcNotificationTarget :: Lens' UpdateMatchmakingConfiguration (Maybe Text)
umcNotificationTarget = lens _umcNotificationTarget (\ s a -> s{_umcNotificationTarget = a})

-- | Amazon Resource Name (<http://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) that is assigned to a game session queue and uniquely identifies it. Format is @arn:aws:gamelift:<region>::fleet/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@ . These queues are used when placing game sessions for matches that are created with this matchmaking configuration. Queues can be located in any region.
umcGameSessionQueueARNs :: Lens' UpdateMatchmakingConfiguration [Text]
umcGameSessionQueueARNs = lens _umcGameSessionQueueARNs (\ s a -> s{_umcGameSessionQueueARNs = a}) . _Default . _Coerce

-- | Information to attached to all events related to the matchmaking configuration.
umcCustomEventData :: Lens' UpdateMatchmakingConfiguration (Maybe Text)
umcCustomEventData = lens _umcCustomEventData (\ s a -> s{_umcCustomEventData = a})

-- | Flag that determines whether or not a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to TRUE.
umcAcceptanceRequired :: Lens' UpdateMatchmakingConfiguration (Maybe Bool)
umcAcceptanceRequired = lens _umcAcceptanceRequired (\ s a -> s{_umcAcceptanceRequired = a})

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match.
umcGameSessionData :: Lens' UpdateMatchmakingConfiguration (Maybe Text)
umcGameSessionData = lens _umcGameSessionData (\ s a -> s{_umcGameSessionData = a})

-- | Descriptive label that is associated with matchmaking configuration.
umcDescription :: Lens' UpdateMatchmakingConfiguration (Maybe Text)
umcDescription = lens _umcDescription (\ s a -> s{_umcDescription = a})

-- | Number of player slots in a match to keep open for future players. For example, if the configuration's rule set specifies a match for a single 12-person team, and the additional player count is set to 2, only 10 players are selected for the match.
umcAdditionalPlayerCount :: Lens' UpdateMatchmakingConfiguration (Maybe Natural)
umcAdditionalPlayerCount = lens _umcAdditionalPlayerCount (\ s a -> s{_umcAdditionalPlayerCount = a}) . mapping _Nat

-- | Unique identifier for a matchmaking configuration to update.
umcName :: Lens' UpdateMatchmakingConfiguration Text
umcName = lens _umcName (\ s a -> s{_umcName = a})

instance AWSRequest UpdateMatchmakingConfiguration
         where
        type Rs UpdateMatchmakingConfiguration =
             UpdateMatchmakingConfigurationResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 UpdateMatchmakingConfigurationResponse' <$>
                   (x .?> "Configuration") <*> (pure (fromEnum s)))

instance Hashable UpdateMatchmakingConfiguration
         where

instance NFData UpdateMatchmakingConfiguration where

instance ToHeaders UpdateMatchmakingConfiguration
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.UpdateMatchmakingConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateMatchmakingConfiguration where
        toJSON UpdateMatchmakingConfiguration'{..}
          = object
              (catMaybes
                 [("GameProperties" .=) <$> _umcGameProperties,
                  ("RuleSetName" .=) <$> _umcRuleSetName,
                  ("AcceptanceTimeoutSeconds" .=) <$>
                    _umcAcceptanceTimeoutSeconds,
                  ("RequestTimeoutSeconds" .=) <$>
                    _umcRequestTimeoutSeconds,
                  ("NotificationTarget" .=) <$> _umcNotificationTarget,
                  ("GameSessionQueueArns" .=) <$>
                    _umcGameSessionQueueARNs,
                  ("CustomEventData" .=) <$> _umcCustomEventData,
                  ("AcceptanceRequired" .=) <$> _umcAcceptanceRequired,
                  ("GameSessionData" .=) <$> _umcGameSessionData,
                  ("Description" .=) <$> _umcDescription,
                  ("AdditionalPlayerCount" .=) <$>
                    _umcAdditionalPlayerCount,
                  Just ("Name" .= _umcName)])

instance ToPath UpdateMatchmakingConfiguration where
        toPath = const "/"

instance ToQuery UpdateMatchmakingConfiguration where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'updateMatchmakingConfigurationResponse' smart constructor.
data UpdateMatchmakingConfigurationResponse = UpdateMatchmakingConfigurationResponse'
  { _umcrsConfiguration  :: !(Maybe MatchmakingConfiguration)
  , _umcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateMatchmakingConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umcrsConfiguration' - Object that describes the updated matchmaking configuration.
--
-- * 'umcrsResponseStatus' - -- | The response status code.
updateMatchmakingConfigurationResponse
    :: Int -- ^ 'umcrsResponseStatus'
    -> UpdateMatchmakingConfigurationResponse
updateMatchmakingConfigurationResponse pResponseStatus_ =
  UpdateMatchmakingConfigurationResponse'
    {_umcrsConfiguration = Nothing, _umcrsResponseStatus = pResponseStatus_}


-- | Object that describes the updated matchmaking configuration.
umcrsConfiguration :: Lens' UpdateMatchmakingConfigurationResponse (Maybe MatchmakingConfiguration)
umcrsConfiguration = lens _umcrsConfiguration (\ s a -> s{_umcrsConfiguration = a})

-- | -- | The response status code.
umcrsResponseStatus :: Lens' UpdateMatchmakingConfigurationResponse Int
umcrsResponseStatus = lens _umcrsResponseStatus (\ s a -> s{_umcrsResponseStatus = a})

instance NFData
           UpdateMatchmakingConfigurationResponse
         where
