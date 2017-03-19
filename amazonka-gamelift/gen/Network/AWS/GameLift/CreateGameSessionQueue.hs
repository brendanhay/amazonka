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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Establishes a new queue for processing requests for new game sessions. A queue identifies where new game sessions can be hosted--by specifying a list of fleet destinations--and how long a request can remain in the queue waiting to be placed before timing out. Requests for new game sessions are added to a queue by calling 'StartGameSessionPlacement' and referencing the queue name.
--
--
-- When processing a request for a game session, Amazon GameLift tries each destination in order until it finds one with available resources to host the new game session. A queue's default order is determined by how destinations are listed. This default order can be overridden in a game session placement request.
--
-- To create a new queue, provide a name, timeout value, and a list of destinations. If successful, a new queue object is returned.
--
module Network.AWS.GameLift.CreateGameSessionQueue
    (
    -- * Creating a Request
      createGameSessionQueue
    , CreateGameSessionQueue
    -- * Request Lenses
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

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createGameSessionQueue' smart constructor.
data CreateGameSessionQueue = CreateGameSessionQueue'
    { _cgsqTimeoutInSeconds :: !(Maybe Nat)
    , _cgsqDestinations     :: !(Maybe [GameSessionQueueDestination])
    , _cgsqName             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateGameSessionQueue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsqTimeoutInSeconds' - Maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a TIMED_OUT status.
--
-- * 'cgsqDestinations' - List of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
--
-- * 'cgsqName' - Descriptive label that is associated with queue. Queue names must be unique within each region.
createGameSessionQueue
    :: Text -- ^ 'cgsqName'
    -> CreateGameSessionQueue
createGameSessionQueue pName_ =
    CreateGameSessionQueue'
    { _cgsqTimeoutInSeconds = Nothing
    , _cgsqDestinations = Nothing
    , _cgsqName = pName_
    }

-- | Maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a TIMED_OUT status.
cgsqTimeoutInSeconds :: Lens' CreateGameSessionQueue (Maybe Natural)
cgsqTimeoutInSeconds = lens _cgsqTimeoutInSeconds (\ s a -> s{_cgsqTimeoutInSeconds = a}) . mapping _Nat;

-- | List of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
cgsqDestinations :: Lens' CreateGameSessionQueue [GameSessionQueueDestination]
cgsqDestinations = lens _cgsqDestinations (\ s a -> s{_cgsqDestinations = a}) . _Default . _Coerce;

-- | Descriptive label that is associated with queue. Queue names must be unique within each region.
cgsqName :: Lens' CreateGameSessionQueue Text
cgsqName = lens _cgsqName (\ s a -> s{_cgsqName = a});

instance AWSRequest CreateGameSessionQueue where
        type Rs CreateGameSessionQueue =
             CreateGameSessionQueueResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 CreateGameSessionQueueResponse' <$>
                   (x .?> "GameSessionQueue") <*> (pure (fromEnum s)))

instance Hashable CreateGameSessionQueue

instance NFData CreateGameSessionQueue

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
                 [("TimeoutInSeconds" .=) <$> _cgsqTimeoutInSeconds,
                  ("Destinations" .=) <$> _cgsqDestinations,
                  Just ("Name" .= _cgsqName)])

instance ToPath CreateGameSessionQueue where
        toPath = const "/"

instance ToQuery CreateGameSessionQueue where
        toQuery = const mempty

-- | /See:/ 'createGameSessionQueueResponse' smart constructor.
data CreateGameSessionQueueResponse = CreateGameSessionQueueResponse'
    { _cgsqrsGameSessionQueue :: !(Maybe GameSessionQueue)
    , _cgsqrsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
cgsqrsGameSessionQueue = lens _cgsqrsGameSessionQueue (\ s a -> s{_cgsqrsGameSessionQueue = a});

-- | -- | The response status code.
cgsqrsResponseStatus :: Lens' CreateGameSessionQueueResponse Int
cgsqrsResponseStatus = lens _cgsqrsResponseStatus (\ s a -> s{_cgsqrsResponseStatus = a});

instance NFData CreateGameSessionQueueResponse
