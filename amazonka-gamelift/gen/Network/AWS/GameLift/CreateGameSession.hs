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
-- Module      : Network.AWS.GameLift.CreateGameSession
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a multiplayer game session for players. This action creates a game session record and assigns an available server process in the specified fleet to host the game session. A fleet must have an @ACTIVE@ status before a game session can be created in it.
--
--
-- To create a game session, specify either fleet ID or alias ID and indicate a maximum number of players to allow in the game session. You can also provide a name and game-specific properties for this game session. If successful, a 'GameSession' object is returned containing the game session properties and other settings you specified.
--
-- __Idempotency tokens.__ You can add a token that uniquely identifies game session requests. This is useful for ensuring that game session requests are idempotent. Multiple requests with the same idempotency token are processed only once; subsequent requests return the original result. All response values are the same with the exception of game session status, which may change.
--
-- __Resource creation limits.__ If you are creating a game session on a fleet with a resource creation limit policy in force, then you must specify a creator ID. Without this ID, Amazon GameLift has no way to evaluate the policy for this new game session request.
--
-- __Player acceptance policy.__ By default, newly created game sessions are open to new players. You can restrict new player access by using 'UpdateGameSession' to change the game session's player session creation policy.
--
-- __Game session logs.__ Logs are retained for all active game sessions for 14 days. To access the logs, call 'GetGameSessionLogUrl' to download the log files.
--
-- /Available in Amazon GameLift Local./
--
-- Game-session-related operations include:
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
module Network.AWS.GameLift.CreateGameSession
    (
    -- * Creating a Request
      createGameSession
    , CreateGameSession
    -- * Request Lenses
    , cgsIdempotencyToken
    , cgsGameProperties
    , cgsGameSessionId
    , cgsAliasId
    , cgsName
    , cgsGameSessionData
    , cgsFleetId
    , cgsCreatorId
    , cgsMaximumPlayerSessionCount

    -- * Destructuring the Response
    , createGameSessionResponse
    , CreateGameSessionResponse
    -- * Response Lenses
    , cgsrsGameSession
    , cgsrsResponseStatus
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
-- /See:/ 'createGameSession' smart constructor.
data CreateGameSession = CreateGameSession'
  { _cgsIdempotencyToken          :: !(Maybe Text)
  , _cgsGameProperties            :: !(Maybe [GameProperty])
  , _cgsGameSessionId             :: !(Maybe Text)
  , _cgsAliasId                   :: !(Maybe Text)
  , _cgsName                      :: !(Maybe Text)
  , _cgsGameSessionData           :: !(Maybe Text)
  , _cgsFleetId                   :: !(Maybe Text)
  , _cgsCreatorId                 :: !(Maybe Text)
  , _cgsMaximumPlayerSessionCount :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGameSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsIdempotencyToken' - Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .) Idempotency tokens remain in use for 30 days after a game session has ended; game session objects are retained for this time period and then deleted.
--
-- * 'cgsGameProperties' - Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- * 'cgsGameSessionId' - /This parameter is no longer preferred. Please use @IdempotencyToken@ instead./ Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .)
--
-- * 'cgsAliasId' - Unique identifier for an alias associated with the fleet to create a game session in. Each request must reference either a fleet ID or alias ID, but not both.
--
-- * 'cgsName' - Descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- * 'cgsGameSessionData' - Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
--
-- * 'cgsFleetId' - Unique identifier for a fleet to create a game session in. Each request must reference either a fleet ID or alias ID, but not both.
--
-- * 'cgsCreatorId' - Unique identifier for a player or entity creating the game session. This ID is used to enforce a resource protection policy (if one exists) that limits the number of concurrent active game sessions one player can have.
--
-- * 'cgsMaximumPlayerSessionCount' - Maximum number of players that can be connected simultaneously to the game session.
createGameSession
    :: Natural -- ^ 'cgsMaximumPlayerSessionCount'
    -> CreateGameSession
createGameSession pMaximumPlayerSessionCount_ =
  CreateGameSession'
    { _cgsIdempotencyToken = Nothing
    , _cgsGameProperties = Nothing
    , _cgsGameSessionId = Nothing
    , _cgsAliasId = Nothing
    , _cgsName = Nothing
    , _cgsGameSessionData = Nothing
    , _cgsFleetId = Nothing
    , _cgsCreatorId = Nothing
    , _cgsMaximumPlayerSessionCount = _Nat # pMaximumPlayerSessionCount_
    }


-- | Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .) Idempotency tokens remain in use for 30 days after a game session has ended; game session objects are retained for this time period and then deleted.
cgsIdempotencyToken :: Lens' CreateGameSession (Maybe Text)
cgsIdempotencyToken = lens _cgsIdempotencyToken (\ s a -> s{_cgsIdempotencyToken = a})

-- | Set of custom properties for a game session, formatted as key:value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
cgsGameProperties :: Lens' CreateGameSession [GameProperty]
cgsGameProperties = lens _cgsGameProperties (\ s a -> s{_cgsGameProperties = a}) . _Default . _Coerce

-- | /This parameter is no longer preferred. Please use @IdempotencyToken@ instead./ Custom string that uniquely identifies a request for a new game session. Maximum token length is 48 characters. If provided, this string is included in the new game session's ID. (A game session ARN has the following format: @arn:aws:gamelift:<region>::gamesession/<fleet ID>/<custom ID string or idempotency token>@ .)
cgsGameSessionId :: Lens' CreateGameSession (Maybe Text)
cgsGameSessionId = lens _cgsGameSessionId (\ s a -> s{_cgsGameSessionId = a})

-- | Unique identifier for an alias associated with the fleet to create a game session in. Each request must reference either a fleet ID or alias ID, but not both.
cgsAliasId :: Lens' CreateGameSession (Maybe Text)
cgsAliasId = lens _cgsAliasId (\ s a -> s{_cgsAliasId = a})

-- | Descriptive label that is associated with a game session. Session names do not need to be unique.
cgsName :: Lens' CreateGameSession (Maybe Text)
cgsName = lens _cgsName (\ s a -> s{_cgsName = a})

-- | Set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ).
cgsGameSessionData :: Lens' CreateGameSession (Maybe Text)
cgsGameSessionData = lens _cgsGameSessionData (\ s a -> s{_cgsGameSessionData = a})

-- | Unique identifier for a fleet to create a game session in. Each request must reference either a fleet ID or alias ID, but not both.
cgsFleetId :: Lens' CreateGameSession (Maybe Text)
cgsFleetId = lens _cgsFleetId (\ s a -> s{_cgsFleetId = a})

-- | Unique identifier for a player or entity creating the game session. This ID is used to enforce a resource protection policy (if one exists) that limits the number of concurrent active game sessions one player can have.
cgsCreatorId :: Lens' CreateGameSession (Maybe Text)
cgsCreatorId = lens _cgsCreatorId (\ s a -> s{_cgsCreatorId = a})

-- | Maximum number of players that can be connected simultaneously to the game session.
cgsMaximumPlayerSessionCount :: Lens' CreateGameSession Natural
cgsMaximumPlayerSessionCount = lens _cgsMaximumPlayerSessionCount (\ s a -> s{_cgsMaximumPlayerSessionCount = a}) . _Nat

instance AWSRequest CreateGameSession where
        type Rs CreateGameSession = CreateGameSessionResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 CreateGameSessionResponse' <$>
                   (x .?> "GameSession") <*> (pure (fromEnum s)))

instance Hashable CreateGameSession where

instance NFData CreateGameSession where

instance ToHeaders CreateGameSession where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.CreateGameSession" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateGameSession where
        toJSON CreateGameSession'{..}
          = object
              (catMaybes
                 [("IdempotencyToken" .=) <$> _cgsIdempotencyToken,
                  ("GameProperties" .=) <$> _cgsGameProperties,
                  ("GameSessionId" .=) <$> _cgsGameSessionId,
                  ("AliasId" .=) <$> _cgsAliasId,
                  ("Name" .=) <$> _cgsName,
                  ("GameSessionData" .=) <$> _cgsGameSessionData,
                  ("FleetId" .=) <$> _cgsFleetId,
                  ("CreatorId" .=) <$> _cgsCreatorId,
                  Just
                    ("MaximumPlayerSessionCount" .=
                       _cgsMaximumPlayerSessionCount)])

instance ToPath CreateGameSession where
        toPath = const "/"

instance ToQuery CreateGameSession where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'createGameSessionResponse' smart constructor.
data CreateGameSessionResponse = CreateGameSessionResponse'
  { _cgsrsGameSession    :: !(Maybe GameSession)
  , _cgsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGameSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsrsGameSession' - Object that describes the newly created game session record.
--
-- * 'cgsrsResponseStatus' - -- | The response status code.
createGameSessionResponse
    :: Int -- ^ 'cgsrsResponseStatus'
    -> CreateGameSessionResponse
createGameSessionResponse pResponseStatus_ =
  CreateGameSessionResponse'
    {_cgsrsGameSession = Nothing, _cgsrsResponseStatus = pResponseStatus_}


-- | Object that describes the newly created game session record.
cgsrsGameSession :: Lens' CreateGameSessionResponse (Maybe GameSession)
cgsrsGameSession = lens _cgsrsGameSession (\ s a -> s{_cgsrsGameSession = a})

-- | -- | The response status code.
cgsrsResponseStatus :: Lens' CreateGameSessionResponse Int
cgsrsResponseStatus = lens _cgsrsResponseStatus (\ s a -> s{_cgsrsResponseStatus = a})

instance NFData CreateGameSessionResponse where
