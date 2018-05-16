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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a player to a game session and creates a player session record. Before a player can be added, a game session must have an @ACTIVE@ status, have a creation policy of @ALLOW_ALL@ , and have an open player slot. To add a group of players to a game session, use 'CreatePlayerSessions' .
--
--
-- To create a player session, specify a game session ID, player ID, and optionally a string of player data. If successful, the player is added to the game session and a new 'PlayerSession' object is returned. Player sessions cannot be updated.
--
-- /Available in Amazon GameLift Local./
--
-- Player-session-related operations include:
--
--     * 'CreatePlayerSession'
--
--     * 'CreatePlayerSessions'
--
--     * 'DescribePlayerSessions'
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
module Network.AWS.GameLift.CreatePlayerSession
    (
    -- * Creating a Request
      createPlayerSession
    , CreatePlayerSession
    -- * Request Lenses
    , cPlayerData
    , cGameSessionId
    , cPlayerId

    -- * Destructuring the Response
    , createPlayerSessionResponse
    , CreatePlayerSessionResponse
    -- * Response Lenses
    , cpsrsPlayerSession
    , cpsrsResponseStatus
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
-- /See:/ 'createPlayerSession' smart constructor.
data CreatePlayerSession = CreatePlayerSession'
  { _cPlayerData    :: !(Maybe Text)
  , _cGameSessionId :: !Text
  , _cPlayerId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePlayerSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cPlayerData' - Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
--
-- * 'cGameSessionId' - Unique identifier for the game session to add a player to.
--
-- * 'cPlayerId' - Unique identifier for a player. Player IDs are developer-defined.
createPlayerSession
    :: Text -- ^ 'cGameSessionId'
    -> Text -- ^ 'cPlayerId'
    -> CreatePlayerSession
createPlayerSession pGameSessionId_ pPlayerId_ =
  CreatePlayerSession'
    { _cPlayerData = Nothing
    , _cGameSessionId = pGameSessionId_
    , _cPlayerId = pPlayerId_
    }


-- | Developer-defined information related to a player. Amazon GameLift does not use this data, so it can be formatted as needed for use in the game.
cPlayerData :: Lens' CreatePlayerSession (Maybe Text)
cPlayerData = lens _cPlayerData (\ s a -> s{_cPlayerData = a})

-- | Unique identifier for the game session to add a player to.
cGameSessionId :: Lens' CreatePlayerSession Text
cGameSessionId = lens _cGameSessionId (\ s a -> s{_cGameSessionId = a})

-- | Unique identifier for a player. Player IDs are developer-defined.
cPlayerId :: Lens' CreatePlayerSession Text
cPlayerId = lens _cPlayerId (\ s a -> s{_cPlayerId = a})

instance AWSRequest CreatePlayerSession where
        type Rs CreatePlayerSession =
             CreatePlayerSessionResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 CreatePlayerSessionResponse' <$>
                   (x .?> "PlayerSession") <*> (pure (fromEnum s)))

instance Hashable CreatePlayerSession where

instance NFData CreatePlayerSession where

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
                 [("PlayerData" .=) <$> _cPlayerData,
                  Just ("GameSessionId" .= _cGameSessionId),
                  Just ("PlayerId" .= _cPlayerId)])

instance ToPath CreatePlayerSession where
        toPath = const "/"

instance ToQuery CreatePlayerSession where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'createPlayerSessionResponse' smart constructor.
data CreatePlayerSessionResponse = CreatePlayerSessionResponse'
  { _cpsrsPlayerSession  :: !(Maybe PlayerSession)
  , _cpsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePlayerSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpsrsPlayerSession' - Object that describes the newly created player session record.
--
-- * 'cpsrsResponseStatus' - -- | The response status code.
createPlayerSessionResponse
    :: Int -- ^ 'cpsrsResponseStatus'
    -> CreatePlayerSessionResponse
createPlayerSessionResponse pResponseStatus_ =
  CreatePlayerSessionResponse'
    {_cpsrsPlayerSession = Nothing, _cpsrsResponseStatus = pResponseStatus_}


-- | Object that describes the newly created player session record.
cpsrsPlayerSession :: Lens' CreatePlayerSessionResponse (Maybe PlayerSession)
cpsrsPlayerSession = lens _cpsrsPlayerSession (\ s a -> s{_cpsrsPlayerSession = a})

-- | -- | The response status code.
cpsrsResponseStatus :: Lens' CreatePlayerSessionResponse Int
cpsrsResponseStatus = lens _cpsrsResponseStatus (\ s a -> s{_cpsrsResponseStatus = a})

instance NFData CreatePlayerSessionResponse where
