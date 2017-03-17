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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a multiplayer game session for players. This action creates a game session record and assigns an available server process in the specified fleet to host the game session. A fleet must have an @ACTIVE@ status before a game session can be created in it.
--
--
-- To create a game session, specify either fleet ID or alias ID, and indicate a maximum number of players to allow in the game session. You can also provide a name and game-specific properties for this game session. If successful, a 'GameSession' object is returned containing session properties, including an IP address. By default, newly created game sessions allow new players to join. Use 'UpdateGameSession' to change the game session's player session creation policy.
--
-- When creating a game session on a fleet with a resource limit creation policy, the request should include a creator ID. If none is provided, Amazon GameLift does not evaluate the fleet's resource limit creation policy.
--
module Network.AWS.GameLift.CreateGameSession
    (
    -- * Creating a Request
      createGameSession
    , CreateGameSession
    -- * Request Lenses
    , cgsGameProperties
    , cgsGameSessionId
    , cgsAliasId
    , cgsName
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

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'createGameSession' smart constructor.
data CreateGameSession = CreateGameSession'
    { _cgsGameProperties            :: !(Maybe [GameProperty])
    , _cgsGameSessionId             :: !(Maybe Text)
    , _cgsAliasId                   :: !(Maybe Text)
    , _cgsName                      :: !(Maybe Text)
    , _cgsFleetId                   :: !(Maybe Text)
    , _cgsCreatorId                 :: !(Maybe Text)
    , _cgsMaximumPlayerSessionCount :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateGameSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsGameProperties' - Set of developer-defined properties for a game session. These properties are passed to the server process hosting the game session.
--
-- * 'cgsGameSessionId' - Custom string to include in the game session ID, with a maximum length of 48 characters. A game session ID has the following format: "arn:aws:gamelift:<region>::gamesession/<fleet ID>/<game session ID>". If provided, the custom string is used for the game session ID string. This value cannot be updated once a game session is created.
--
-- * 'cgsAliasId' - Unique identifier for an alias associated with the fleet to create a game session in. Each request must reference either a fleet ID or alias ID, but not both.
--
-- * 'cgsName' - Descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- * 'cgsFleetId' - Unique identifier for a fleet to create a game session in. Each request must reference either a fleet ID or alias ID, but not both.
--
-- * 'cgsCreatorId' - > player-id; or entity creating the game session. This ID is used to enforce a resource protection policy (if one exists) that limits the number of concurrent active game sessions one player can have.
--
-- * 'cgsMaximumPlayerSessionCount' - Maximum number of players that can be connected simultaneously to the game session.
createGameSession
    :: Natural -- ^ 'cgsMaximumPlayerSessionCount'
    -> CreateGameSession
createGameSession pMaximumPlayerSessionCount_ =
    CreateGameSession'
    { _cgsGameProperties = Nothing
    , _cgsGameSessionId = Nothing
    , _cgsAliasId = Nothing
    , _cgsName = Nothing
    , _cgsFleetId = Nothing
    , _cgsCreatorId = Nothing
    , _cgsMaximumPlayerSessionCount = _Nat # pMaximumPlayerSessionCount_
    }

-- | Set of developer-defined properties for a game session. These properties are passed to the server process hosting the game session.
cgsGameProperties :: Lens' CreateGameSession [GameProperty]
cgsGameProperties = lens _cgsGameProperties (\ s a -> s{_cgsGameProperties = a}) . _Default . _Coerce;

-- | Custom string to include in the game session ID, with a maximum length of 48 characters. A game session ID has the following format: "arn:aws:gamelift:<region>::gamesession/<fleet ID>/<game session ID>". If provided, the custom string is used for the game session ID string. This value cannot be updated once a game session is created.
cgsGameSessionId :: Lens' CreateGameSession (Maybe Text)
cgsGameSessionId = lens _cgsGameSessionId (\ s a -> s{_cgsGameSessionId = a});

-- | Unique identifier for an alias associated with the fleet to create a game session in. Each request must reference either a fleet ID or alias ID, but not both.
cgsAliasId :: Lens' CreateGameSession (Maybe Text)
cgsAliasId = lens _cgsAliasId (\ s a -> s{_cgsAliasId = a});

-- | Descriptive label that is associated with a game session. Session names do not need to be unique.
cgsName :: Lens' CreateGameSession (Maybe Text)
cgsName = lens _cgsName (\ s a -> s{_cgsName = a});

-- | Unique identifier for a fleet to create a game session in. Each request must reference either a fleet ID or alias ID, but not both.
cgsFleetId :: Lens' CreateGameSession (Maybe Text)
cgsFleetId = lens _cgsFleetId (\ s a -> s{_cgsFleetId = a});

-- | > player-id; or entity creating the game session. This ID is used to enforce a resource protection policy (if one exists) that limits the number of concurrent active game sessions one player can have.
cgsCreatorId :: Lens' CreateGameSession (Maybe Text)
cgsCreatorId = lens _cgsCreatorId (\ s a -> s{_cgsCreatorId = a});

-- | Maximum number of players that can be connected simultaneously to the game session.
cgsMaximumPlayerSessionCount :: Lens' CreateGameSession Natural
cgsMaximumPlayerSessionCount = lens _cgsMaximumPlayerSessionCount (\ s a -> s{_cgsMaximumPlayerSessionCount = a}) . _Nat;

instance AWSRequest CreateGameSession where
        type Rs CreateGameSession = CreateGameSessionResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 CreateGameSessionResponse' <$>
                   (x .?> "GameSession") <*> (pure (fromEnum s)))

instance Hashable CreateGameSession

instance NFData CreateGameSession

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
                 [("GameProperties" .=) <$> _cgsGameProperties,
                  ("GameSessionId" .=) <$> _cgsGameSessionId,
                  ("AliasId" .=) <$> _cgsAliasId,
                  ("Name" .=) <$> _cgsName,
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _cgsrsGameSession = Nothing
    , _cgsrsResponseStatus = pResponseStatus_
    }

-- | Object that describes the newly created game session record.
cgsrsGameSession :: Lens' CreateGameSessionResponse (Maybe GameSession)
cgsrsGameSession = lens _cgsrsGameSession (\ s a -> s{_cgsrsGameSession = a});

-- | -- | The response status code.
cgsrsResponseStatus :: Lens' CreateGameSessionResponse Int
cgsrsResponseStatus = lens _cgsrsResponseStatus (\ s a -> s{_cgsrsResponseStatus = a});

instance NFData CreateGameSessionResponse
