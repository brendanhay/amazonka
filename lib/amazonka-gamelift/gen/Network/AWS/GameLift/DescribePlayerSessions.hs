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
-- Module      : Network.AWS.GameLift.DescribePlayerSessions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for one or more player sessions. This action can be used in several ways: (1) provide a @PlayerSessionId@ to request properties for a specific player session; (2) provide a @GameSessionId@ to request properties for all player sessions in the specified game session; (3) provide a @PlayerId@ to request properties for all player sessions of a specified player.
--
--
-- To get game session record(s), specify only one of the following: a player session ID, a game session ID, or a player ID. You can filter this request by player session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'PlayerSession' object is returned for each session matching the request.
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
module Network.AWS.GameLift.DescribePlayerSessions
    (
    -- * Creating a Request
      describePlayerSessions
    , DescribePlayerSessions
    -- * Request Lenses
    , dpssGameSessionId
    , dpssNextToken
    , dpssLimit
    , dpssPlayerSessionId
    , dpssPlayerId
    , dpssPlayerSessionStatusFilter

    -- * Destructuring the Response
    , describePlayerSessionsResponse
    , DescribePlayerSessionsResponse
    -- * Response Lenses
    , dpsrsNextToken
    , dpsrsPlayerSessions
    , dpsrsResponseStatus
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
-- /See:/ 'describePlayerSessions' smart constructor.
data DescribePlayerSessions = DescribePlayerSessions'
  { _dpssGameSessionId             :: !(Maybe Text)
  , _dpssNextToken                 :: !(Maybe Text)
  , _dpssLimit                     :: !(Maybe Nat)
  , _dpssPlayerSessionId           :: !(Maybe Text)
  , _dpssPlayerId                  :: !(Maybe Text)
  , _dpssPlayerSessionStatusFilter :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePlayerSessions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpssGameSessionId' - Unique identifier for the game session to retrieve player sessions for.
--
-- * 'dpssNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value. If a player session ID is specified, this parameter is ignored.
--
-- * 'dpssLimit' - Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. If a player session ID is specified, this parameter is ignored.
--
-- * 'dpssPlayerSessionId' - Unique identifier for a player session to retrieve.
--
-- * 'dpssPlayerId' - Unique identifier for a player to retrieve player sessions for.
--
-- * 'dpssPlayerSessionStatusFilter' - Player session status to filter results on. Possible player session statuses include the following:     * __RESERVED__ -- The player session request has been received, but the player has not yet connected to the server process and/or been validated.      * __ACTIVE__ -- The player has been validated by the server process and is currently connected.     * __COMPLETED__ -- The player connection has been dropped.     * __TIMEDOUT__ -- A player session request was received, but the player did not connect and/or was not validated within the timeout limit (60 seconds).
describePlayerSessions
    :: DescribePlayerSessions
describePlayerSessions =
  DescribePlayerSessions'
    { _dpssGameSessionId = Nothing
    , _dpssNextToken = Nothing
    , _dpssLimit = Nothing
    , _dpssPlayerSessionId = Nothing
    , _dpssPlayerId = Nothing
    , _dpssPlayerSessionStatusFilter = Nothing
    }


-- | Unique identifier for the game session to retrieve player sessions for.
dpssGameSessionId :: Lens' DescribePlayerSessions (Maybe Text)
dpssGameSessionId = lens _dpssGameSessionId (\ s a -> s{_dpssGameSessionId = a})

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value. If a player session ID is specified, this parameter is ignored.
dpssNextToken :: Lens' DescribePlayerSessions (Maybe Text)
dpssNextToken = lens _dpssNextToken (\ s a -> s{_dpssNextToken = a})

-- | Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. If a player session ID is specified, this parameter is ignored.
dpssLimit :: Lens' DescribePlayerSessions (Maybe Natural)
dpssLimit = lens _dpssLimit (\ s a -> s{_dpssLimit = a}) . mapping _Nat

-- | Unique identifier for a player session to retrieve.
dpssPlayerSessionId :: Lens' DescribePlayerSessions (Maybe Text)
dpssPlayerSessionId = lens _dpssPlayerSessionId (\ s a -> s{_dpssPlayerSessionId = a})

-- | Unique identifier for a player to retrieve player sessions for.
dpssPlayerId :: Lens' DescribePlayerSessions (Maybe Text)
dpssPlayerId = lens _dpssPlayerId (\ s a -> s{_dpssPlayerId = a})

-- | Player session status to filter results on. Possible player session statuses include the following:     * __RESERVED__ -- The player session request has been received, but the player has not yet connected to the server process and/or been validated.      * __ACTIVE__ -- The player has been validated by the server process and is currently connected.     * __COMPLETED__ -- The player connection has been dropped.     * __TIMEDOUT__ -- A player session request was received, but the player did not connect and/or was not validated within the timeout limit (60 seconds).
dpssPlayerSessionStatusFilter :: Lens' DescribePlayerSessions (Maybe Text)
dpssPlayerSessionStatusFilter = lens _dpssPlayerSessionStatusFilter (\ s a -> s{_dpssPlayerSessionStatusFilter = a})

instance AWSRequest DescribePlayerSessions where
        type Rs DescribePlayerSessions =
             DescribePlayerSessionsResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribePlayerSessionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "PlayerSessions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribePlayerSessions where

instance NFData DescribePlayerSessions where

instance ToHeaders DescribePlayerSessions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribePlayerSessions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribePlayerSessions where
        toJSON DescribePlayerSessions'{..}
          = object
              (catMaybes
                 [("GameSessionId" .=) <$> _dpssGameSessionId,
                  ("NextToken" .=) <$> _dpssNextToken,
                  ("Limit" .=) <$> _dpssLimit,
                  ("PlayerSessionId" .=) <$> _dpssPlayerSessionId,
                  ("PlayerId" .=) <$> _dpssPlayerId,
                  ("PlayerSessionStatusFilter" .=) <$>
                    _dpssPlayerSessionStatusFilter])

instance ToPath DescribePlayerSessions where
        toPath = const "/"

instance ToQuery DescribePlayerSessions where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describePlayerSessionsResponse' smart constructor.
data DescribePlayerSessionsResponse = DescribePlayerSessionsResponse'
  { _dpsrsNextToken      :: !(Maybe Text)
  , _dpsrsPlayerSessions :: !(Maybe [PlayerSession])
  , _dpsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePlayerSessionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpsrsNextToken' - Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- * 'dpsrsPlayerSessions' - Collection of objects containing properties for each player session that matches the request.
--
-- * 'dpsrsResponseStatus' - -- | The response status code.
describePlayerSessionsResponse
    :: Int -- ^ 'dpsrsResponseStatus'
    -> DescribePlayerSessionsResponse
describePlayerSessionsResponse pResponseStatus_ =
  DescribePlayerSessionsResponse'
    { _dpsrsNextToken = Nothing
    , _dpsrsPlayerSessions = Nothing
    , _dpsrsResponseStatus = pResponseStatus_
    }


-- | Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
dpsrsNextToken :: Lens' DescribePlayerSessionsResponse (Maybe Text)
dpsrsNextToken = lens _dpsrsNextToken (\ s a -> s{_dpsrsNextToken = a})

-- | Collection of objects containing properties for each player session that matches the request.
dpsrsPlayerSessions :: Lens' DescribePlayerSessionsResponse [PlayerSession]
dpsrsPlayerSessions = lens _dpsrsPlayerSessions (\ s a -> s{_dpsrsPlayerSessions = a}) . _Default . _Coerce

-- | -- | The response status code.
dpsrsResponseStatus :: Lens' DescribePlayerSessionsResponse Int
dpsrsResponseStatus = lens _dpsrsResponseStatus (\ s a -> s{_dpsrsResponseStatus = a})

instance NFData DescribePlayerSessionsResponse where
