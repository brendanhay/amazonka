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
-- Module      : Network.AWS.GameLift.DescribeGameSessions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a set of one or more game sessions. Request a specific game session or request all game sessions on a fleet. Alternatively, use 'SearchGameSessions' to request a set of active game sessions that are filtered by certain criteria. To retrieve protection policy settings for game sessions, use 'DescribeGameSessionDetails' .
--
--
-- To get game sessions, specify one of the following: game session ID, fleet ID, or alias ID. You can filter this request by game session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'GameSession' object is returned for each game session matching the request.
--
-- /Available in Amazon GameLift Local./
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
module Network.AWS.GameLift.DescribeGameSessions
    (
    -- * Creating a Request
      describeGameSessions
    , DescribeGameSessions
    -- * Request Lenses
    , dgsGameSessionId
    , dgsAliasId
    , dgsNextToken
    , dgsStatusFilter
    , dgsLimit
    , dgsFleetId

    -- * Destructuring the Response
    , describeGameSessionsResponse
    , DescribeGameSessionsResponse
    -- * Response Lenses
    , dgsrsGameSessions
    , dgsrsNextToken
    , dgsrsResponseStatus
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
-- /See:/ 'describeGameSessions' smart constructor.
data DescribeGameSessions = DescribeGameSessions'
  { _dgsGameSessionId :: !(Maybe Text)
  , _dgsAliasId       :: !(Maybe Text)
  , _dgsNextToken     :: !(Maybe Text)
  , _dgsStatusFilter  :: !(Maybe Text)
  , _dgsLimit         :: !(Maybe Nat)
  , _dgsFleetId       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGameSessions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsGameSessionId' - Unique identifier for the game session to retrieve. You can use either a @GameSessionId@ or @GameSessionArn@ value.
--
-- * 'dgsAliasId' - Unique identifier for an alias associated with the fleet to retrieve all game sessions for.
--
-- * 'dgsNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
--
-- * 'dgsStatusFilter' - Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ , and @TERMINATING@ (the last two are transitory).
--
-- * 'dgsLimit' - Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- * 'dgsFleetId' - Unique identifier for a fleet to retrieve all game sessions for.
describeGameSessions
    :: DescribeGameSessions
describeGameSessions =
  DescribeGameSessions'
    { _dgsGameSessionId = Nothing
    , _dgsAliasId = Nothing
    , _dgsNextToken = Nothing
    , _dgsStatusFilter = Nothing
    , _dgsLimit = Nothing
    , _dgsFleetId = Nothing
    }


-- | Unique identifier for the game session to retrieve. You can use either a @GameSessionId@ or @GameSessionArn@ value.
dgsGameSessionId :: Lens' DescribeGameSessions (Maybe Text)
dgsGameSessionId = lens _dgsGameSessionId (\ s a -> s{_dgsGameSessionId = a})

-- | Unique identifier for an alias associated with the fleet to retrieve all game sessions for.
dgsAliasId :: Lens' DescribeGameSessions (Maybe Text)
dgsAliasId = lens _dgsAliasId (\ s a -> s{_dgsAliasId = a})

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
dgsNextToken :: Lens' DescribeGameSessions (Maybe Text)
dgsNextToken = lens _dgsNextToken (\ s a -> s{_dgsNextToken = a})

-- | Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ , and @TERMINATING@ (the last two are transitory).
dgsStatusFilter :: Lens' DescribeGameSessions (Maybe Text)
dgsStatusFilter = lens _dgsStatusFilter (\ s a -> s{_dgsStatusFilter = a})

-- | Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
dgsLimit :: Lens' DescribeGameSessions (Maybe Natural)
dgsLimit = lens _dgsLimit (\ s a -> s{_dgsLimit = a}) . mapping _Nat

-- | Unique identifier for a fleet to retrieve all game sessions for.
dgsFleetId :: Lens' DescribeGameSessions (Maybe Text)
dgsFleetId = lens _dgsFleetId (\ s a -> s{_dgsFleetId = a})

instance AWSRequest DescribeGameSessions where
        type Rs DescribeGameSessions =
             DescribeGameSessionsResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeGameSessionsResponse' <$>
                   (x .?> "GameSessions" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeGameSessions where

instance NFData DescribeGameSessions where

instance ToHeaders DescribeGameSessions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeGameSessions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeGameSessions where
        toJSON DescribeGameSessions'{..}
          = object
              (catMaybes
                 [("GameSessionId" .=) <$> _dgsGameSessionId,
                  ("AliasId" .=) <$> _dgsAliasId,
                  ("NextToken" .=) <$> _dgsNextToken,
                  ("StatusFilter" .=) <$> _dgsStatusFilter,
                  ("Limit" .=) <$> _dgsLimit,
                  ("FleetId" .=) <$> _dgsFleetId])

instance ToPath DescribeGameSessions where
        toPath = const "/"

instance ToQuery DescribeGameSessions where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeGameSessionsResponse' smart constructor.
data DescribeGameSessionsResponse = DescribeGameSessionsResponse'
  { _dgsrsGameSessions   :: !(Maybe [GameSession])
  , _dgsrsNextToken      :: !(Maybe Text)
  , _dgsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGameSessionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsrsGameSessions' - Collection of objects containing game session properties for each session matching the request.
--
-- * 'dgsrsNextToken' - Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- * 'dgsrsResponseStatus' - -- | The response status code.
describeGameSessionsResponse
    :: Int -- ^ 'dgsrsResponseStatus'
    -> DescribeGameSessionsResponse
describeGameSessionsResponse pResponseStatus_ =
  DescribeGameSessionsResponse'
    { _dgsrsGameSessions = Nothing
    , _dgsrsNextToken = Nothing
    , _dgsrsResponseStatus = pResponseStatus_
    }


-- | Collection of objects containing game session properties for each session matching the request.
dgsrsGameSessions :: Lens' DescribeGameSessionsResponse [GameSession]
dgsrsGameSessions = lens _dgsrsGameSessions (\ s a -> s{_dgsrsGameSessions = a}) . _Default . _Coerce

-- | Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
dgsrsNextToken :: Lens' DescribeGameSessionsResponse (Maybe Text)
dgsrsNextToken = lens _dgsrsNextToken (\ s a -> s{_dgsrsNextToken = a})

-- | -- | The response status code.
dgsrsResponseStatus :: Lens' DescribeGameSessionsResponse Int
dgsrsResponseStatus = lens _dgsrsResponseStatus (\ s a -> s{_dgsrsResponseStatus = a})

instance NFData DescribeGameSessionsResponse where
