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
-- Module      : Network.AWS.GameLift.UpdateGameSessionQueue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a game session queue, which determines how new game session requests in the queue are processed. To update settings, specify the queue name to be updated and provide the new settings. When updating destinations, provide a complete list of destinations.
--
--
-- Queue-related operations include:
--
--     * 'CreateGameSessionQueue'
--
--     * 'DescribeGameSessionQueues'
--
--     * 'UpdateGameSessionQueue'
--
--     * 'DeleteGameSessionQueue'
--
--
--
module Network.AWS.GameLift.UpdateGameSessionQueue
    (
    -- * Creating a Request
      updateGameSessionQueue
    , UpdateGameSessionQueue
    -- * Request Lenses
    , ugsqPlayerLatencyPolicies
    , ugsqTimeoutInSeconds
    , ugsqDestinations
    , ugsqName

    -- * Destructuring the Response
    , updateGameSessionQueueResponse
    , UpdateGameSessionQueueResponse
    -- * Response Lenses
    , ugsqrsGameSessionQueue
    , ugsqrsResponseStatus
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
-- /See:/ 'updateGameSessionQueue' smart constructor.
data UpdateGameSessionQueue = UpdateGameSessionQueue'
  { _ugsqPlayerLatencyPolicies :: !(Maybe [PlayerLatencyPolicy])
  , _ugsqTimeoutInSeconds      :: !(Maybe Nat)
  , _ugsqDestinations          :: !(Maybe [GameSessionQueueDestination])
  , _ugsqName                  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGameSessionQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsqPlayerLatencyPolicies' - Collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, it is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement. When updating policies, provide a complete collection of policies.
--
-- * 'ugsqTimeoutInSeconds' - Maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
--
-- * 'ugsqDestinations' - List of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order. When updating this list, provide a complete list of destinations.
--
-- * 'ugsqName' - Descriptive label that is associated with game session queue. Queue names must be unique within each region.
updateGameSessionQueue
    :: Text -- ^ 'ugsqName'
    -> UpdateGameSessionQueue
updateGameSessionQueue pName_ =
  UpdateGameSessionQueue'
    { _ugsqPlayerLatencyPolicies = Nothing
    , _ugsqTimeoutInSeconds = Nothing
    , _ugsqDestinations = Nothing
    , _ugsqName = pName_
    }


-- | Collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, it is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement. When updating policies, provide a complete collection of policies.
ugsqPlayerLatencyPolicies :: Lens' UpdateGameSessionQueue [PlayerLatencyPolicy]
ugsqPlayerLatencyPolicies = lens _ugsqPlayerLatencyPolicies (\ s a -> s{_ugsqPlayerLatencyPolicies = a}) . _Default . _Coerce

-- | Maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
ugsqTimeoutInSeconds :: Lens' UpdateGameSessionQueue (Maybe Natural)
ugsqTimeoutInSeconds = lens _ugsqTimeoutInSeconds (\ s a -> s{_ugsqTimeoutInSeconds = a}) . mapping _Nat

-- | List of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order. When updating this list, provide a complete list of destinations.
ugsqDestinations :: Lens' UpdateGameSessionQueue [GameSessionQueueDestination]
ugsqDestinations = lens _ugsqDestinations (\ s a -> s{_ugsqDestinations = a}) . _Default . _Coerce

-- | Descriptive label that is associated with game session queue. Queue names must be unique within each region.
ugsqName :: Lens' UpdateGameSessionQueue Text
ugsqName = lens _ugsqName (\ s a -> s{_ugsqName = a})

instance AWSRequest UpdateGameSessionQueue where
        type Rs UpdateGameSessionQueue =
             UpdateGameSessionQueueResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 UpdateGameSessionQueueResponse' <$>
                   (x .?> "GameSessionQueue") <*> (pure (fromEnum s)))

instance Hashable UpdateGameSessionQueue where

instance NFData UpdateGameSessionQueue where

instance ToHeaders UpdateGameSessionQueue where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.UpdateGameSessionQueue" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateGameSessionQueue where
        toJSON UpdateGameSessionQueue'{..}
          = object
              (catMaybes
                 [("PlayerLatencyPolicies" .=) <$>
                    _ugsqPlayerLatencyPolicies,
                  ("TimeoutInSeconds" .=) <$> _ugsqTimeoutInSeconds,
                  ("Destinations" .=) <$> _ugsqDestinations,
                  Just ("Name" .= _ugsqName)])

instance ToPath UpdateGameSessionQueue where
        toPath = const "/"

instance ToQuery UpdateGameSessionQueue where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'updateGameSessionQueueResponse' smart constructor.
data UpdateGameSessionQueueResponse = UpdateGameSessionQueueResponse'
  { _ugsqrsGameSessionQueue :: !(Maybe GameSessionQueue)
  , _ugsqrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGameSessionQueueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsqrsGameSessionQueue' - Object that describes the newly updated game session queue.
--
-- * 'ugsqrsResponseStatus' - -- | The response status code.
updateGameSessionQueueResponse
    :: Int -- ^ 'ugsqrsResponseStatus'
    -> UpdateGameSessionQueueResponse
updateGameSessionQueueResponse pResponseStatus_ =
  UpdateGameSessionQueueResponse'
    { _ugsqrsGameSessionQueue = Nothing
    , _ugsqrsResponseStatus = pResponseStatus_
    }


-- | Object that describes the newly updated game session queue.
ugsqrsGameSessionQueue :: Lens' UpdateGameSessionQueueResponse (Maybe GameSessionQueue)
ugsqrsGameSessionQueue = lens _ugsqrsGameSessionQueue (\ s a -> s{_ugsqrsGameSessionQueue = a})

-- | -- | The response status code.
ugsqrsResponseStatus :: Lens' UpdateGameSessionQueueResponse Int
ugsqrsResponseStatus = lens _ugsqrsResponseStatus (\ s a -> s{_ugsqrsResponseStatus = a})

instance NFData UpdateGameSessionQueueResponse where
