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
-- Module      : Network.AWS.GameLift.StartGameSessionPlacement
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places a request for a new game session in a queue (see 'CreateGameSessionQueue' ). When processing a placement request, Amazon GameLift attempts to create a new game session on one of the fleets associated with the queue. If no resources are available, Amazon GameLift tries again with another and so on until resources are found or the placement request times out. A game session placement request can also request player sessions. When a new game session is successfully created, Amazon GameLift creates a player session for each player included in the request.
--
--
-- When placing a game session, by default Amazon GameLift tries each fleet in the order they are listed in the queue configuration. Ideally, a queue's destinations are listed in preference order. Alternatively, when requesting a game session with players, you can also provide latency data for each player in relevant regions. Latency data indicates the performance lag a player experiences when connected to a fleet in the region. Amazon GameLift uses latency data to reorder the list of destinations to place the game session in a region with minimal lag. If latency data is provided for multiple players, Amazon GameLift calculates each region's average lag for all players and reorders to get the best game play across all players.
--
-- To place a new game session request, specify the queue name and a set of game session properties and settings. Also provide a unique ID (such as a UUID) for the placement. You'll use this ID to track the status of the placement request. Optionally, provide a set of IDs and player data for each player you want to join to the new game session. To optimize game play for the players, also provide latency data for all players. If successful, a new game session placement is created. To track the status of a placement request, call 'DescribeGameSessionPlacement' and check the request's status. If the status is Fulfilled, a new game session has been created and a game session ARN and region are referenced. If the placement request times out, you have the option of resubmitting the request or retrying it with a different queue.
--
module Network.AWS.GameLift.StartGameSessionPlacement
    (
    -- * Creating a Request
      startGameSessionPlacement
    , StartGameSessionPlacement
    -- * Request Lenses
    , sgspGameProperties
    , sgspGameSessionName
    , sgspPlayerLatencies
    , sgspDesiredPlayerSessions
    , sgspPlacementId
    , sgspGameSessionQueueName
    , sgspMaximumPlayerSessionCount

    -- * Destructuring the Response
    , startGameSessionPlacementResponse
    , StartGameSessionPlacementResponse
    -- * Response Lenses
    , sgsprsGameSessionPlacement
    , sgsprsResponseStatus
    ) where

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'startGameSessionPlacement' smart constructor.
data StartGameSessionPlacement = StartGameSessionPlacement'
    { _sgspGameProperties            :: !(Maybe [GameProperty])
    , _sgspGameSessionName           :: !(Maybe Text)
    , _sgspPlayerLatencies           :: !(Maybe [PlayerLatency])
    , _sgspDesiredPlayerSessions     :: !(Maybe [DesiredPlayerSession])
    , _sgspPlacementId               :: !Text
    , _sgspGameSessionQueueName      :: !Text
    , _sgspMaximumPlayerSessionCount :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartGameSessionPlacement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgspGameProperties' - Set of developer-defined properties for a game session. These properties are passed to the server process hosting the game session.
--
-- * 'sgspGameSessionName' - Descriptive label that is associated with a game session. Session names do not need to be unique.
--
-- * 'sgspPlayerLatencies' - Set of values, expressed in milliseconds, indicating the amount of latency that players experience when connected to AWS regions. This information is relevant when requesting player sessions. Latency information provided for player IDs not included in /DesiredPlayerSessions/ are ignored.
--
-- * 'sgspDesiredPlayerSessions' - Set of information on each player to create a player session for.
--
-- * 'sgspPlacementId' - Unique identifier to assign to the new game session placement. This value is developer-defined. The value must be unique across all regions and cannot be reused unless you are resubmitting a cancelled or timed-out placement request.
--
-- * 'sgspGameSessionQueueName' - Name of the queue to use to place the new game session.
--
-- * 'sgspMaximumPlayerSessionCount' - Maximum number of players that can be connected simultaneously to the game session.
startGameSessionPlacement
    :: Text -- ^ 'sgspPlacementId'
    -> Text -- ^ 'sgspGameSessionQueueName'
    -> Natural -- ^ 'sgspMaximumPlayerSessionCount'
    -> StartGameSessionPlacement
startGameSessionPlacement pPlacementId_ pGameSessionQueueName_ pMaximumPlayerSessionCount_ =
    StartGameSessionPlacement'
    { _sgspGameProperties = Nothing
    , _sgspGameSessionName = Nothing
    , _sgspPlayerLatencies = Nothing
    , _sgspDesiredPlayerSessions = Nothing
    , _sgspPlacementId = pPlacementId_
    , _sgspGameSessionQueueName = pGameSessionQueueName_
    , _sgspMaximumPlayerSessionCount = _Nat # pMaximumPlayerSessionCount_
    }

-- | Set of developer-defined properties for a game session. These properties are passed to the server process hosting the game session.
sgspGameProperties :: Lens' StartGameSessionPlacement [GameProperty]
sgspGameProperties = lens _sgspGameProperties (\ s a -> s{_sgspGameProperties = a}) . _Default . _Coerce;

-- | Descriptive label that is associated with a game session. Session names do not need to be unique.
sgspGameSessionName :: Lens' StartGameSessionPlacement (Maybe Text)
sgspGameSessionName = lens _sgspGameSessionName (\ s a -> s{_sgspGameSessionName = a});

-- | Set of values, expressed in milliseconds, indicating the amount of latency that players experience when connected to AWS regions. This information is relevant when requesting player sessions. Latency information provided for player IDs not included in /DesiredPlayerSessions/ are ignored.
sgspPlayerLatencies :: Lens' StartGameSessionPlacement [PlayerLatency]
sgspPlayerLatencies = lens _sgspPlayerLatencies (\ s a -> s{_sgspPlayerLatencies = a}) . _Default . _Coerce;

-- | Set of information on each player to create a player session for.
sgspDesiredPlayerSessions :: Lens' StartGameSessionPlacement [DesiredPlayerSession]
sgspDesiredPlayerSessions = lens _sgspDesiredPlayerSessions (\ s a -> s{_sgspDesiredPlayerSessions = a}) . _Default . _Coerce;

-- | Unique identifier to assign to the new game session placement. This value is developer-defined. The value must be unique across all regions and cannot be reused unless you are resubmitting a cancelled or timed-out placement request.
sgspPlacementId :: Lens' StartGameSessionPlacement Text
sgspPlacementId = lens _sgspPlacementId (\ s a -> s{_sgspPlacementId = a});

-- | Name of the queue to use to place the new game session.
sgspGameSessionQueueName :: Lens' StartGameSessionPlacement Text
sgspGameSessionQueueName = lens _sgspGameSessionQueueName (\ s a -> s{_sgspGameSessionQueueName = a});

-- | Maximum number of players that can be connected simultaneously to the game session.
sgspMaximumPlayerSessionCount :: Lens' StartGameSessionPlacement Natural
sgspMaximumPlayerSessionCount = lens _sgspMaximumPlayerSessionCount (\ s a -> s{_sgspMaximumPlayerSessionCount = a}) . _Nat;

instance AWSRequest StartGameSessionPlacement where
        type Rs StartGameSessionPlacement =
             StartGameSessionPlacementResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 StartGameSessionPlacementResponse' <$>
                   (x .?> "GameSessionPlacement") <*>
                     (pure (fromEnum s)))

instance Hashable StartGameSessionPlacement

instance NFData StartGameSessionPlacement

instance ToHeaders StartGameSessionPlacement where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.StartGameSessionPlacement" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartGameSessionPlacement where
        toJSON StartGameSessionPlacement'{..}
          = object
              (catMaybes
                 [("GameProperties" .=) <$> _sgspGameProperties,
                  ("GameSessionName" .=) <$> _sgspGameSessionName,
                  ("PlayerLatencies" .=) <$> _sgspPlayerLatencies,
                  ("DesiredPlayerSessions" .=) <$>
                    _sgspDesiredPlayerSessions,
                  Just ("PlacementId" .= _sgspPlacementId),
                  Just
                    ("GameSessionQueueName" .=
                       _sgspGameSessionQueueName),
                  Just
                    ("MaximumPlayerSessionCount" .=
                       _sgspMaximumPlayerSessionCount)])

instance ToPath StartGameSessionPlacement where
        toPath = const "/"

instance ToQuery StartGameSessionPlacement where
        toQuery = const mempty

-- | /See:/ 'startGameSessionPlacementResponse' smart constructor.
data StartGameSessionPlacementResponse = StartGameSessionPlacementResponse'
    { _sgsprsGameSessionPlacement :: !(Maybe GameSessionPlacement)
    , _sgsprsResponseStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartGameSessionPlacementResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgsprsGameSessionPlacement' - Object that describes the newly created game session placement. This object includes all the information provided in the request, as well as start/end time stamps and placement status.
--
-- * 'sgsprsResponseStatus' - -- | The response status code.
startGameSessionPlacementResponse
    :: Int -- ^ 'sgsprsResponseStatus'
    -> StartGameSessionPlacementResponse
startGameSessionPlacementResponse pResponseStatus_ =
    StartGameSessionPlacementResponse'
    { _sgsprsGameSessionPlacement = Nothing
    , _sgsprsResponseStatus = pResponseStatus_
    }

-- | Object that describes the newly created game session placement. This object includes all the information provided in the request, as well as start/end time stamps and placement status.
sgsprsGameSessionPlacement :: Lens' StartGameSessionPlacementResponse (Maybe GameSessionPlacement)
sgsprsGameSessionPlacement = lens _sgsprsGameSessionPlacement (\ s a -> s{_sgsprsGameSessionPlacement = a});

-- | -- | The response status code.
sgsprsResponseStatus :: Lens' StartGameSessionPlacementResponse Int
sgsprsResponseStatus = lens _sgsprsResponseStatus (\ s a -> s{_sgsprsResponseStatus = a});

instance NFData StartGameSessionPlacementResponse
