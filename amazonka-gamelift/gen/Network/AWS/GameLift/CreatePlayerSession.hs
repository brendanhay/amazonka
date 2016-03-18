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
-- Module      : Network.AWS.GameLift.CreatePlayerSession
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a player to a game session and creates a player session record. A
-- game session must be in an ACTIVE state, have a creation policy of
-- ALLOW_ALL, and have an open player slot before players can be added to
-- the session.
--
-- To create a player session, specify a game session ID and player ID. If
-- successful, the player is added to the game session and a new
-- < PlayerSession> object is returned.
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/CreatePlayerSession.html AWS API Reference> for CreatePlayerSession.
module Network.AWS.GameLift.CreatePlayerSession
    (
    -- * Creating a Request
      createPlayerSession
    , CreatePlayerSession
    -- * Request Lenses
    , cGameSessionId
    , cPlayerId

    -- * Destructuring the Response
    , createPlayerSessionResponse
    , CreatePlayerSessionResponse
    -- * Response Lenses
    , cpsrsPlayerSession
    , cpsrsResponseStatus
    ) where

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for a request action.
--
-- /See:/ 'createPlayerSession' smart constructor.
data CreatePlayerSession = CreatePlayerSession'
    { _cGameSessionId :: !Text
    , _cPlayerId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreatePlayerSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cGameSessionId'
--
-- * 'cPlayerId'
createPlayerSession
    :: Text -- ^ 'cGameSessionId'
    -> Text -- ^ 'cPlayerId'
    -> CreatePlayerSession
createPlayerSession pGameSessionId_ pPlayerId_ =
    CreatePlayerSession'
    { _cGameSessionId = pGameSessionId_
    , _cPlayerId = pPlayerId_
    }

-- | Unique identifier for a game session. Specify the game session you want
-- to add a player to.
cGameSessionId :: Lens' CreatePlayerSession Text
cGameSessionId = lens _cGameSessionId (\ s a -> s{_cGameSessionId = a});

-- | Unique identifier for the player to be added.
cPlayerId :: Lens' CreatePlayerSession Text
cPlayerId = lens _cPlayerId (\ s a -> s{_cPlayerId = a});

instance AWSRequest CreatePlayerSession where
        type Rs CreatePlayerSession =
             CreatePlayerSessionResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 CreatePlayerSessionResponse' <$>
                   (x .?> "PlayerSession") <*> (pure (fromEnum s)))

instance ToHeaders CreatePlayerSession where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.CreatePlayerSession" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePlayerSession where
        toJSON CreatePlayerSession'{..}
          = object
              (catMaybes
                 [Just ("GameSessionId" .= _cGameSessionId),
                  Just ("PlayerId" .= _cPlayerId)])

instance ToPath CreatePlayerSession where
        toPath = const "/"

instance ToQuery CreatePlayerSession where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
-- /See:/ 'createPlayerSessionResponse' smart constructor.
data CreatePlayerSessionResponse = CreatePlayerSessionResponse'
    { _cpsrsPlayerSession  :: !(Maybe PlayerSession)
    , _cpsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreatePlayerSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpsrsPlayerSession'
--
-- * 'cpsrsResponseStatus'
createPlayerSessionResponse
    :: Int -- ^ 'cpsrsResponseStatus'
    -> CreatePlayerSessionResponse
createPlayerSessionResponse pResponseStatus_ =
    CreatePlayerSessionResponse'
    { _cpsrsPlayerSession = Nothing
    , _cpsrsResponseStatus = pResponseStatus_
    }

-- | Object containing the newly created player session record.
cpsrsPlayerSession :: Lens' CreatePlayerSessionResponse (Maybe PlayerSession)
cpsrsPlayerSession = lens _cpsrsPlayerSession (\ s a -> s{_cpsrsPlayerSession = a});

-- | The response status code.
cpsrsResponseStatus :: Lens' CreatePlayerSessionResponse Int
cpsrsResponseStatus = lens _cpsrsResponseStatus (\ s a -> s{_cpsrsResponseStatus = a});
