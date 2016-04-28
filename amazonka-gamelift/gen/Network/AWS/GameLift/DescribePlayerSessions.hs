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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for one or more player sessions. This action can be
-- used in several ways: (1) provide a /PlayerSessionId/ parameter to
-- request properties for a specific player session; (2) provide a
-- /GameSessionId/ parameter to request properties for all player sessions
-- in the specified game session; (3) provide a /PlayerId/ parameter to
-- request properties for all player sessions of a specified player.
--
-- To get game session record(s), specify only one of the following: a
-- player session ID, a game session ID, or a player ID. You can filter
-- this request by player session status. Use the pagination parameters to
-- retrieve results as a set of sequential pages. If successful, a
-- < PlayerSession> object is returned for each session matching the
-- request.
module Network.AWS.GameLift.DescribePlayerSessions
    (
    -- * Creating a Request
      describePlayerSessions
    , DescribePlayerSessions
    -- * Request Lenses
    , dpsGameSessionId
    , dpsNextToken
    , dpsLimit
    , dpsPlayerSessionId
    , dpsPlayerId
    , dpsPlayerSessionStatusFilter

    -- * Destructuring the Response
    , describePlayerSessionsResponse
    , DescribePlayerSessionsResponse
    -- * Response Lenses
    , dpsrsNextToken
    , dpsrsPlayerSessions
    , dpsrsResponseStatus
    ) where

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for a request action.
--
-- /See:/ 'describePlayerSessions' smart constructor.
data DescribePlayerSessions = DescribePlayerSessions'
    { _dpsGameSessionId             :: !(Maybe Text)
    , _dpsNextToken                 :: !(Maybe Text)
    , _dpsLimit                     :: !(Maybe Nat)
    , _dpsPlayerSessionId           :: !(Maybe Text)
    , _dpsPlayerId                  :: !(Maybe Text)
    , _dpsPlayerSessionStatusFilter :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribePlayerSessions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpsGameSessionId'
--
-- * 'dpsNextToken'
--
-- * 'dpsLimit'
--
-- * 'dpsPlayerSessionId'
--
-- * 'dpsPlayerId'
--
-- * 'dpsPlayerSessionStatusFilter'
describePlayerSessions
    :: DescribePlayerSessions
describePlayerSessions =
    DescribePlayerSessions'
    { _dpsGameSessionId = Nothing
    , _dpsNextToken = Nothing
    , _dpsLimit = Nothing
    , _dpsPlayerSessionId = Nothing
    , _dpsPlayerId = Nothing
    , _dpsPlayerSessionStatusFilter = Nothing
    }

-- | Unique identifier for a game session.
dpsGameSessionId :: Lens' DescribePlayerSessions (Maybe Text)
dpsGameSessionId = lens _dpsGameSessionId (\ s a -> s{_dpsGameSessionId = a});

-- | Token indicating the start of the next sequential page of results. Use
-- the token that is returned with a previous call to this action. To
-- specify the start of the result set, do not specify a value. If a player
-- session ID is specified, this parameter is ignored.
dpsNextToken :: Lens' DescribePlayerSessions (Maybe Text)
dpsNextToken = lens _dpsNextToken (\ s a -> s{_dpsNextToken = a});

-- | Maximum number of results to return. You can use this parameter with
-- /NextToken/ to get results as a set of sequential pages. If a player
-- session ID is specified, this parameter is ignored.
dpsLimit :: Lens' DescribePlayerSessions (Maybe Natural)
dpsLimit = lens _dpsLimit (\ s a -> s{_dpsLimit = a}) . mapping _Nat;

-- | Unique identifier for a player session.
dpsPlayerSessionId :: Lens' DescribePlayerSessions (Maybe Text)
dpsPlayerSessionId = lens _dpsPlayerSessionId (\ s a -> s{_dpsPlayerSessionId = a});

-- | Unique identifier for a player.
dpsPlayerId :: Lens' DescribePlayerSessions (Maybe Text)
dpsPlayerId = lens _dpsPlayerId (\ s a -> s{_dpsPlayerId = a});

-- | Player session status to filter results on. Possible player session
-- states include:
--
-- -   RESERVED: The player session request has been received, but the
--     player has not yet connected to the game server and\/or been
--     validated.
-- -   ACTIVE: The player has been validated by the game server and is
--     currently connected.
-- -   COMPLETED: The player connection has been dropped.
-- -   TIMEDOUT: A player session request was received, but the player did
--     not connect and\/or was not validated within the time-out limit (60
--     seconds).
dpsPlayerSessionStatusFilter :: Lens' DescribePlayerSessions (Maybe Text)
dpsPlayerSessionStatusFilter = lens _dpsPlayerSessionStatusFilter (\ s a -> s{_dpsPlayerSessionStatusFilter = a});

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

instance Hashable DescribePlayerSessions

instance NFData DescribePlayerSessions

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
                 [("GameSessionId" .=) <$> _dpsGameSessionId,
                  ("NextToken" .=) <$> _dpsNextToken,
                  ("Limit" .=) <$> _dpsLimit,
                  ("PlayerSessionId" .=) <$> _dpsPlayerSessionId,
                  ("PlayerId" .=) <$> _dpsPlayerId,
                  ("PlayerSessionStatusFilter" .=) <$>
                    _dpsPlayerSessionStatusFilter])

instance ToPath DescribePlayerSessions where
        toPath = const "/"

instance ToQuery DescribePlayerSessions where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
-- /See:/ 'describePlayerSessionsResponse' smart constructor.
data DescribePlayerSessionsResponse = DescribePlayerSessionsResponse'
    { _dpsrsNextToken      :: !(Maybe Text)
    , _dpsrsPlayerSessions :: !(Maybe [PlayerSession])
    , _dpsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribePlayerSessionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpsrsNextToken'
--
-- * 'dpsrsPlayerSessions'
--
-- * 'dpsrsResponseStatus'
describePlayerSessionsResponse
    :: Int -- ^ 'dpsrsResponseStatus'
    -> DescribePlayerSessionsResponse
describePlayerSessionsResponse pResponseStatus_ =
    DescribePlayerSessionsResponse'
    { _dpsrsNextToken = Nothing
    , _dpsrsPlayerSessions = Nothing
    , _dpsrsResponseStatus = pResponseStatus_
    }

-- | Token indicating where to resume retrieving results on the next call to
-- this action. If no token is returned, these results represent the end of
-- the list.
--
-- If a request has a limit that exactly matches the number of remaining
-- results, a token is returned even though there are no more results to
-- retrieve.
dpsrsNextToken :: Lens' DescribePlayerSessionsResponse (Maybe Text)
dpsrsNextToken = lens _dpsrsNextToken (\ s a -> s{_dpsrsNextToken = a});

-- | Collection of objects containing properties for each player session that
-- matches the request.
dpsrsPlayerSessions :: Lens' DescribePlayerSessionsResponse [PlayerSession]
dpsrsPlayerSessions = lens _dpsrsPlayerSessions (\ s a -> s{_dpsrsPlayerSessions = a}) . _Default . _Coerce;

-- | The response status code.
dpsrsResponseStatus :: Lens' DescribePlayerSessionsResponse Int
dpsrsResponseStatus = lens _dpsrsResponseStatus (\ s a -> s{_dpsrsResponseStatus = a});
