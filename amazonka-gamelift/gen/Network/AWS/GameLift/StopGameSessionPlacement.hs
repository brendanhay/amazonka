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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a game session placement that is in Pending status. To stop a placement, provide the placement ID values. If successful, the placement is moved to Cancelled status.
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
    , srsGameSessionPlacement
    , srsResponseStatus
    ) where

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'stopGameSessionPlacement' smart constructor.
newtype StopGameSessionPlacement = StopGameSessionPlacement'
    { _sPlacementId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StopGameSessionPlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sPlacementId' - Unique identifier for a game session placement to cancel.
stopGameSessionPlacement
    :: Text -- ^ 'sPlacementId'
    -> StopGameSessionPlacement
stopGameSessionPlacement pPlacementId_ =
    StopGameSessionPlacement'
    { _sPlacementId = pPlacementId_
    }

-- | Unique identifier for a game session placement to cancel.
sPlacementId :: Lens' StopGameSessionPlacement Text
sPlacementId = lens _sPlacementId (\ s a -> s{_sPlacementId = a});

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

instance Hashable StopGameSessionPlacement

instance NFData StopGameSessionPlacement

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

-- | /See:/ 'stopGameSessionPlacementResponse' smart constructor.
data StopGameSessionPlacementResponse = StopGameSessionPlacementResponse'
    { _srsGameSessionPlacement :: !(Maybe GameSessionPlacement)
    , _srsResponseStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StopGameSessionPlacementResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsGameSessionPlacement' - Object that describes the cancelled game session placement, with cancelled status and an end time stamp.
--
-- * 'srsResponseStatus' - -- | The response status code.
stopGameSessionPlacementResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StopGameSessionPlacementResponse
stopGameSessionPlacementResponse pResponseStatus_ =
    StopGameSessionPlacementResponse'
    { _srsGameSessionPlacement = Nothing
    , _srsResponseStatus = pResponseStatus_
    }

-- | Object that describes the cancelled game session placement, with cancelled status and an end time stamp.
srsGameSessionPlacement :: Lens' StopGameSessionPlacementResponse (Maybe GameSessionPlacement)
srsGameSessionPlacement = lens _srsGameSessionPlacement (\ s a -> s{_srsGameSessionPlacement = a});

-- | -- | The response status code.
srsResponseStatus :: Lens' StopGameSessionPlacementResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a});

instance NFData StopGameSessionPlacementResponse
