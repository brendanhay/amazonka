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
-- Creates a multiplayer game session for players. This action creates a
-- game session record and assigns the new session to an instance in the
-- specified fleet, which activates the server initialization process in
-- your game server. A fleet must be in an ACTIVE state before a game
-- session can be created for it.
--
-- To create a game session, specify either a fleet ID or an alias ID and
-- indicate the maximum number of players the game session allows. You can
-- also provide a name and a set of properties for your game (optional). If
-- successful, a < GameSession> object is returned containing session
-- properties, including an IP address. By default, newly created game
-- sessions are set to accept adding any new players to the game session.
-- Use < UpdateGameSession> to change the creation policy.
module Network.AWS.GameLift.CreateGameSession
    (
    -- * Creating a Request
      createGameSession
    , CreateGameSession
    -- * Request Lenses
    , cgsGameProperties
    , cgsAliasId
    , cgsName
    , cgsFleetId
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
-- /See:/ 'createGameSession' smart constructor.
data CreateGameSession = CreateGameSession'
    { _cgsGameProperties            :: !(Maybe [GameProperty])
    , _cgsAliasId                   :: !(Maybe Text)
    , _cgsName                      :: !(Maybe Text)
    , _cgsFleetId                   :: !(Maybe Text)
    , _cgsMaximumPlayerSessionCount :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateGameSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsGameProperties'
--
-- * 'cgsAliasId'
--
-- * 'cgsName'
--
-- * 'cgsFleetId'
--
-- * 'cgsMaximumPlayerSessionCount'
createGameSession
    :: Natural -- ^ 'cgsMaximumPlayerSessionCount'
    -> CreateGameSession
createGameSession pMaximumPlayerSessionCount_ =
    CreateGameSession'
    { _cgsGameProperties = Nothing
    , _cgsAliasId = Nothing
    , _cgsName = Nothing
    , _cgsFleetId = Nothing
    , _cgsMaximumPlayerSessionCount = _Nat # pMaximumPlayerSessionCount_
    }

-- | Set of properties used to administer a game session. These properties
-- are passed to your game server.
cgsGameProperties :: Lens' CreateGameSession [GameProperty]
cgsGameProperties = lens _cgsGameProperties (\ s a -> s{_cgsGameProperties = a}) . _Default . _Coerce;

-- | Unique identifier for a fleet alias. Each request must reference either
-- a fleet ID or alias ID, but not both.
cgsAliasId :: Lens' CreateGameSession (Maybe Text)
cgsAliasId = lens _cgsAliasId (\ s a -> s{_cgsAliasId = a});

-- | Descriptive label associated with this game session. Session names do
-- not need to be unique.
cgsName :: Lens' CreateGameSession (Maybe Text)
cgsName = lens _cgsName (\ s a -> s{_cgsName = a});

-- | Unique identifier for a fleet. Each request must reference either a
-- fleet ID or alias ID, but not both.
cgsFleetId :: Lens' CreateGameSession (Maybe Text)
cgsFleetId = lens _cgsFleetId (\ s a -> s{_cgsFleetId = a});

-- | Maximum number of players that can be connected simultaneously to the
-- game session.
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
                  ("AliasId" .=) <$> _cgsAliasId,
                  ("Name" .=) <$> _cgsName,
                  ("FleetId" .=) <$> _cgsFleetId,
                  Just
                    ("MaximumPlayerSessionCount" .=
                       _cgsMaximumPlayerSessionCount)])

instance ToPath CreateGameSession where
        toPath = const "/"

instance ToQuery CreateGameSession where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
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
-- * 'cgsrsGameSession'
--
-- * 'cgsrsResponseStatus'
createGameSessionResponse
    :: Int -- ^ 'cgsrsResponseStatus'
    -> CreateGameSessionResponse
createGameSessionResponse pResponseStatus_ =
    CreateGameSessionResponse'
    { _cgsrsGameSession = Nothing
    , _cgsrsResponseStatus = pResponseStatus_
    }

-- | Object containing the newly created game session record.
cgsrsGameSession :: Lens' CreateGameSessionResponse (Maybe GameSession)
cgsrsGameSession = lens _cgsrsGameSession (\ s a -> s{_cgsrsGameSession = a});

-- | The response status code.
cgsrsResponseStatus :: Lens' CreateGameSessionResponse Int
cgsrsResponseStatus = lens _cgsrsResponseStatus (\ s a -> s{_cgsrsResponseStatus = a});

instance NFData CreateGameSessionResponse
