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
-- Module      : Network.AWS.GameLift.UpdateGameSession
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates game session properties. This includes the session name, maximum player count, protection policy, which controls whether or not an active game session can be terminated during a scale-down event, and the player session creation policy, which controls whether or not new players can join the session. To update a game session, specify the game session ID and the values you want to change. If successful, an updated 'GameSession' object is returned.
--
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
module Network.AWS.GameLift.UpdateGameSession
    (
    -- * Creating a Request
      updateGameSession
    , UpdateGameSession
    -- * Request Lenses
    , ugsMaximumPlayerSessionCount
    , ugsPlayerSessionCreationPolicy
    , ugsName
    , ugsProtectionPolicy
    , ugsGameSessionId

    -- * Destructuring the Response
    , updateGameSessionResponse
    , UpdateGameSessionResponse
    -- * Response Lenses
    , ugsrsGameSession
    , ugsrsResponseStatus
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
-- /See:/ 'updateGameSession' smart constructor.
data UpdateGameSession = UpdateGameSession'
  { _ugsMaximumPlayerSessionCount   :: !(Maybe Nat)
  , _ugsPlayerSessionCreationPolicy :: !(Maybe PlayerSessionCreationPolicy)
  , _ugsName                        :: !(Maybe Text)
  , _ugsProtectionPolicy            :: !(Maybe ProtectionPolicy)
  , _ugsGameSessionId               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGameSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsMaximumPlayerSessionCount' - Maximum number of players that can be connected simultaneously to the game session.
--
-- * 'ugsPlayerSessionCreationPolicy' - Policy determining whether or not the game session accepts new players.
--
-- * 'ugsName' - Descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- * 'ugsProtectionPolicy' - Game session protection policy to apply to this game session only.     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
-- * 'ugsGameSessionId' - Unique identifier for the game session to update.
updateGameSession
    :: Text -- ^ 'ugsGameSessionId'
    -> UpdateGameSession
updateGameSession pGameSessionId_ =
  UpdateGameSession'
    { _ugsMaximumPlayerSessionCount = Nothing
    , _ugsPlayerSessionCreationPolicy = Nothing
    , _ugsName = Nothing
    , _ugsProtectionPolicy = Nothing
    , _ugsGameSessionId = pGameSessionId_
    }


-- | Maximum number of players that can be connected simultaneously to the game session.
ugsMaximumPlayerSessionCount :: Lens' UpdateGameSession (Maybe Natural)
ugsMaximumPlayerSessionCount = lens _ugsMaximumPlayerSessionCount (\ s a -> s{_ugsMaximumPlayerSessionCount = a}) . mapping _Nat

-- | Policy determining whether or not the game session accepts new players.
ugsPlayerSessionCreationPolicy :: Lens' UpdateGameSession (Maybe PlayerSessionCreationPolicy)
ugsPlayerSessionCreationPolicy = lens _ugsPlayerSessionCreationPolicy (\ s a -> s{_ugsPlayerSessionCreationPolicy = a})

-- | Descriptive label that is associated with a game session. Session names do not need to be unique.
ugsName :: Lens' UpdateGameSession (Maybe Text)
ugsName = lens _ugsName (\ s a -> s{_ugsName = a})

-- | Game session protection policy to apply to this game session only.     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
ugsProtectionPolicy :: Lens' UpdateGameSession (Maybe ProtectionPolicy)
ugsProtectionPolicy = lens _ugsProtectionPolicy (\ s a -> s{_ugsProtectionPolicy = a})

-- | Unique identifier for the game session to update.
ugsGameSessionId :: Lens' UpdateGameSession Text
ugsGameSessionId = lens _ugsGameSessionId (\ s a -> s{_ugsGameSessionId = a})

instance AWSRequest UpdateGameSession where
        type Rs UpdateGameSession = UpdateGameSessionResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 UpdateGameSessionResponse' <$>
                   (x .?> "GameSession") <*> (pure (fromEnum s)))

instance Hashable UpdateGameSession where

instance NFData UpdateGameSession where

instance ToHeaders UpdateGameSession where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.UpdateGameSession" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateGameSession where
        toJSON UpdateGameSession'{..}
          = object
              (catMaybes
                 [("MaximumPlayerSessionCount" .=) <$>
                    _ugsMaximumPlayerSessionCount,
                  ("PlayerSessionCreationPolicy" .=) <$>
                    _ugsPlayerSessionCreationPolicy,
                  ("Name" .=) <$> _ugsName,
                  ("ProtectionPolicy" .=) <$> _ugsProtectionPolicy,
                  Just ("GameSessionId" .= _ugsGameSessionId)])

instance ToPath UpdateGameSession where
        toPath = const "/"

instance ToQuery UpdateGameSession where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'updateGameSessionResponse' smart constructor.
data UpdateGameSessionResponse = UpdateGameSessionResponse'
  { _ugsrsGameSession    :: !(Maybe GameSession)
  , _ugsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGameSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsrsGameSession' - Object that contains the updated game session metadata.
--
-- * 'ugsrsResponseStatus' - -- | The response status code.
updateGameSessionResponse
    :: Int -- ^ 'ugsrsResponseStatus'
    -> UpdateGameSessionResponse
updateGameSessionResponse pResponseStatus_ =
  UpdateGameSessionResponse'
    {_ugsrsGameSession = Nothing, _ugsrsResponseStatus = pResponseStatus_}


-- | Object that contains the updated game session metadata.
ugsrsGameSession :: Lens' UpdateGameSessionResponse (Maybe GameSession)
ugsrsGameSession = lens _ugsrsGameSession (\ s a -> s{_ugsrsGameSession = a})

-- | -- | The response status code.
ugsrsResponseStatus :: Lens' UpdateGameSessionResponse Int
ugsrsResponseStatus = lens _ugsrsResponseStatus (\ s a -> s{_ugsrsResponseStatus = a})

instance NFData UpdateGameSessionResponse where
