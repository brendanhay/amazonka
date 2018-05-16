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
-- Module      : Network.AWS.GameLift.StopGameSessionPlacement
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a game session placement that is in @PENDING@ status. To stop a placement, provide the placement ID values. If successful, the placement is moved to @CANCELLED@ status.
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
module Network.AWS.GameLift.StopGameSessionPlacement
    (
    -- * Creating a Request
      stopGameSessionPlacement
    , StopGameSessionPlacement
    -- * Request Lenses
    , sPlacementId

    -- * Destructuring the Response
    , stopGameSessionPlacementResponse
    , StopGameSessionPlacementResponse
    -- * Response Lenses
    , storsGameSessionPlacement
    , storsResponseStatus
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
-- /See:/ 'stopGameSessionPlacement' smart constructor.
newtype StopGameSessionPlacement = StopGameSessionPlacement'
  { _sPlacementId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopGameSessionPlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sPlacementId' - Unique identifier for a game session placement to cancel.
stopGameSessionPlacement
    :: Text -- ^ 'sPlacementId'
    -> StopGameSessionPlacement
stopGameSessionPlacement pPlacementId_ =
  StopGameSessionPlacement' {_sPlacementId = pPlacementId_}


-- | Unique identifier for a game session placement to cancel.
sPlacementId :: Lens' StopGameSessionPlacement Text
sPlacementId = lens _sPlacementId (\ s a -> s{_sPlacementId = a})

instance AWSRequest StopGameSessionPlacement where
        type Rs StopGameSessionPlacement =
             StopGameSessionPlacementResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 StopGameSessionPlacementResponse' <$>
                   (x .?> "GameSessionPlacement") <*>
                     (pure (fromEnum s)))

instance Hashable StopGameSessionPlacement where

instance NFData StopGameSessionPlacement where

instance ToHeaders StopGameSessionPlacement where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.StopGameSessionPlacement" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopGameSessionPlacement where
        toJSON StopGameSessionPlacement'{..}
          = object
              (catMaybes [Just ("PlacementId" .= _sPlacementId)])

instance ToPath StopGameSessionPlacement where
        toPath = const "/"

instance ToQuery StopGameSessionPlacement where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'stopGameSessionPlacementResponse' smart constructor.
data StopGameSessionPlacementResponse = StopGameSessionPlacementResponse'
  { _storsGameSessionPlacement :: !(Maybe GameSessionPlacement)
  , _storsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopGameSessionPlacementResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'storsGameSessionPlacement' - Object that describes the canceled game session placement, with @CANCELLED@ status and an end time stamp.
--
-- * 'storsResponseStatus' - -- | The response status code.
stopGameSessionPlacementResponse
    :: Int -- ^ 'storsResponseStatus'
    -> StopGameSessionPlacementResponse
stopGameSessionPlacementResponse pResponseStatus_ =
  StopGameSessionPlacementResponse'
    { _storsGameSessionPlacement = Nothing
    , _storsResponseStatus = pResponseStatus_
    }


-- | Object that describes the canceled game session placement, with @CANCELLED@ status and an end time stamp.
storsGameSessionPlacement :: Lens' StopGameSessionPlacementResponse (Maybe GameSessionPlacement)
storsGameSessionPlacement = lens _storsGameSessionPlacement (\ s a -> s{_storsGameSessionPlacement = a})

-- | -- | The response status code.
storsResponseStatus :: Lens' StopGameSessionPlacementResponse Int
storsResponseStatus = lens _storsResponseStatus (\ s a -> s{_storsResponseStatus = a})

instance NFData StopGameSessionPlacementResponse
         where
