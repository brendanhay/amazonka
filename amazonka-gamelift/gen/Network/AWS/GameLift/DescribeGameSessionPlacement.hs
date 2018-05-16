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
-- Module      : Network.AWS.GameLift.DescribeGameSessionPlacement
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties and current status of a game session placement request. To get game session placement details, specify the placement ID. If successful, a 'GameSessionPlacement' object is returned.
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
module Network.AWS.GameLift.DescribeGameSessionPlacement
    (
    -- * Creating a Request
      describeGameSessionPlacement
    , DescribeGameSessionPlacement
    -- * Request Lenses
    , dgspPlacementId

    -- * Destructuring the Response
    , describeGameSessionPlacementResponse
    , DescribeGameSessionPlacementResponse
    -- * Response Lenses
    , dgsprsGameSessionPlacement
    , dgsprsResponseStatus
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
-- /See:/ 'describeGameSessionPlacement' smart constructor.
newtype DescribeGameSessionPlacement = DescribeGameSessionPlacement'
  { _dgspPlacementId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGameSessionPlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgspPlacementId' - Unique identifier for a game session placement to retrieve.
describeGameSessionPlacement
    :: Text -- ^ 'dgspPlacementId'
    -> DescribeGameSessionPlacement
describeGameSessionPlacement pPlacementId_ =
  DescribeGameSessionPlacement' {_dgspPlacementId = pPlacementId_}


-- | Unique identifier for a game session placement to retrieve.
dgspPlacementId :: Lens' DescribeGameSessionPlacement Text
dgspPlacementId = lens _dgspPlacementId (\ s a -> s{_dgspPlacementId = a})

instance AWSRequest DescribeGameSessionPlacement
         where
        type Rs DescribeGameSessionPlacement =
             DescribeGameSessionPlacementResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeGameSessionPlacementResponse' <$>
                   (x .?> "GameSessionPlacement") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeGameSessionPlacement where

instance NFData DescribeGameSessionPlacement where

instance ToHeaders DescribeGameSessionPlacement where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeGameSessionPlacement" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeGameSessionPlacement where
        toJSON DescribeGameSessionPlacement'{..}
          = object
              (catMaybes
                 [Just ("PlacementId" .= _dgspPlacementId)])

instance ToPath DescribeGameSessionPlacement where
        toPath = const "/"

instance ToQuery DescribeGameSessionPlacement where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeGameSessionPlacementResponse' smart constructor.
data DescribeGameSessionPlacementResponse = DescribeGameSessionPlacementResponse'
  { _dgsprsGameSessionPlacement :: !(Maybe GameSessionPlacement)
  , _dgsprsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGameSessionPlacementResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsprsGameSessionPlacement' - Object that describes the requested game session placement.
--
-- * 'dgsprsResponseStatus' - -- | The response status code.
describeGameSessionPlacementResponse
    :: Int -- ^ 'dgsprsResponseStatus'
    -> DescribeGameSessionPlacementResponse
describeGameSessionPlacementResponse pResponseStatus_ =
  DescribeGameSessionPlacementResponse'
    { _dgsprsGameSessionPlacement = Nothing
    , _dgsprsResponseStatus = pResponseStatus_
    }


-- | Object that describes the requested game session placement.
dgsprsGameSessionPlacement :: Lens' DescribeGameSessionPlacementResponse (Maybe GameSessionPlacement)
dgsprsGameSessionPlacement = lens _dgsprsGameSessionPlacement (\ s a -> s{_dgsprsGameSessionPlacement = a})

-- | -- | The response status code.
dgsprsResponseStatus :: Lens' DescribeGameSessionPlacementResponse Int
dgsprsResponseStatus = lens _dgsprsResponseStatus (\ s a -> s{_dgsprsResponseStatus = a})

instance NFData DescribeGameSessionPlacementResponse
         where
