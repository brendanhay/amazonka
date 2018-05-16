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
-- Module      : Network.AWS.GameLift.CreateGameSessionQueue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Establishes a new queue for processing requests to place new game sessions. A queue identifies where new game sessions can be hosted -- by specifying a list of destinations (fleets or aliases) -- and how long requests can wait in the queue before timing out. You can set up a queue to try to place game sessions on fleets in multiple regions. To add placement requests to a queue, call 'StartGameSessionPlacement' and reference the queue name.
--
--
-- __Destination order.__ When processing a request for a game session, Amazon GameLift tries each destination in order until it finds one with available resources to host the new game session. A queue's default order is determined by how destinations are listed. The default order is overridden when a game session placement request provides player latency information. Player latency information enables Amazon GameLift to prioritize destinations where players report the lowest average latency, as a result placing the new game session where the majority of players will have the best possible gameplay experience.
--
-- __Player latency policies.__ For placement requests containing player latency information, use player latency policies to protect individual players from very high latencies. With a latency cap, even when a destination can deliver a low latency for most players, the game is not placed where any individual player is reporting latency higher than a policy's maximum. A queue can have multiple latency policies, which are enforced consecutively starting with the policy with the lowest latency cap. Use multiple policies to gradually relax latency controls; for example, you might set a policy with a low latency cap for the first 60 seconds, a second policy with a higher cap for the next 60 seconds, etc.
--
-- To create a new queue, provide a name, timeout value, a list of destinations and, if desired, a set of latency policies. If successful, a new queue object is returned.
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
module Network.AWS.GameLift.CreateGameSessionQueue
    (
    -- * Creating a Request
      createGameSessionQueue
    , CreateGameSessionQueue
    -- * Request Lenses
    , cgsqPlayerLatencyPolicies
    , cgsqTimeoutInSeconds
    , cgsqDestinations
    , cgsqName

    -- * Destructuring the Response
    , createGameSessionQueueResponse
    , CreateGameSessionQueueResponse
    -- * Response Lenses
    , cgsqrsGameSessionQueue
    , cgsqrsResponseStatus
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
-- /See:/ 'createGameSessionQueue' smart constructor.
data CreateGameSessionQueue = CreateGameSessionQueue'
  { _cgsqPlayerLatencyPolicies :: !(Maybe [PlayerLatencyPolicy])
  , _cgsqTimeoutInSeconds      :: !(Maybe Nat)
  , _cgsqDestinations          :: !(Maybe [GameSessionQueueDestination])
  , _cgsqName                  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGameSessionQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsqPlayerLatencyPolicies' - Collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, it is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement. A player latency policy must set a value for MaximumIndividualPlayerLatencyMilliseconds; if none is set, this API requests will fail.
--
-- * 'cgsqTimeoutInSeconds' - Maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
--
-- * 'cgsqDestinations' - List of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
--
-- * 'cgsqName' - Descriptive label that is associated with game session queue. Queue names must be unique within each region.
createGameSessionQueue
    :: Text -- ^ 'cgsqName'
    -> CreateGameSessionQueue
createGameSessionQueue pName_ =
  CreateGameSessionQueue'
    { _cgsqPlayerLatencyPolicies = Nothing
    , _cgsqTimeoutInSeconds = Nothing
    , _cgsqDestinations = Nothing
    , _cgsqName = pName_
    }


-- | Collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, it is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement. A player latency policy must set a value for MaximumIndividualPlayerLatencyMilliseconds; if none is set, this API requests will fail.
cgsqPlayerLatencyPolicies :: Lens' CreateGameSessionQueue [PlayerLatencyPolicy]
cgsqPlayerLatencyPolicies = lens _cgsqPlayerLatencyPolicies (\ s a -> s{_cgsqPlayerLatencyPolicies = a}) . _Default . _Coerce

-- | Maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
cgsqTimeoutInSeconds :: Lens' CreateGameSessionQueue (Maybe Natural)
cgsqTimeoutInSeconds = lens _cgsqTimeoutInSeconds (\ s a -> s{_cgsqTimeoutInSeconds = a}) . mapping _Nat

-- | List of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
cgsqDestinations :: Lens' CreateGameSessionQueue [GameSessionQueueDestination]
cgsqDestinations = lens _cgsqDestinations (\ s a -> s{_cgsqDestinations = a}) . _Default . _Coerce

-- | Descriptive label that is associated with game session queue. Queue names must be unique within each region.
cgsqName :: Lens' CreateGameSessionQueue Text
cgsqName = lens _cgsqName (\ s a -> s{_cgsqName = a})

instance AWSRequest CreateGameSessionQueue where
        type Rs CreateGameSessionQueue =
             CreateGameSessionQueueResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 CreateGameSessionQueueResponse' <$>
                   (x .?> "GameSessionQueue") <*> (pure (fromEnum s)))

instance Hashable CreateGameSessionQueue where

instance NFData CreateGameSessionQueue where

instance ToHeaders CreateGameSessionQueue where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.CreateGameSessionQueue" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateGameSessionQueue where
        toJSON CreateGameSessionQueue'{..}
          = object
              (catMaybes
                 [("PlayerLatencyPolicies" .=) <$>
                    _cgsqPlayerLatencyPolicies,
                  ("TimeoutInSeconds" .=) <$> _cgsqTimeoutInSeconds,
                  ("Destinations" .=) <$> _cgsqDestinations,
                  Just ("Name" .= _cgsqName)])

instance ToPath CreateGameSessionQueue where
        toPath = const "/"

instance ToQuery CreateGameSessionQueue where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'createGameSessionQueueResponse' smart constructor.
data CreateGameSessionQueueResponse = CreateGameSessionQueueResponse'
  { _cgsqrsGameSessionQueue :: !(Maybe GameSessionQueue)
  , _cgsqrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGameSessionQueueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsqrsGameSessionQueue' - Object that describes the newly created game session queue.
--
-- * 'cgsqrsResponseStatus' - -- | The response status code.
createGameSessionQueueResponse
    :: Int -- ^ 'cgsqrsResponseStatus'
    -> CreateGameSessionQueueResponse
createGameSessionQueueResponse pResponseStatus_ =
  CreateGameSessionQueueResponse'
    { _cgsqrsGameSessionQueue = Nothing
    , _cgsqrsResponseStatus = pResponseStatus_
    }


-- | Object that describes the newly created game session queue.
cgsqrsGameSessionQueue :: Lens' CreateGameSessionQueueResponse (Maybe GameSessionQueue)
cgsqrsGameSessionQueue = lens _cgsqrsGameSessionQueue (\ s a -> s{_cgsqrsGameSessionQueue = a})

-- | -- | The response status code.
cgsqrsResponseStatus :: Lens' CreateGameSessionQueueResponse Int
cgsqrsResponseStatus = lens _cgsqrsResponseStatus (\ s a -> s{_cgsqrsResponseStatus = a})

instance NFData CreateGameSessionQueueResponse where
