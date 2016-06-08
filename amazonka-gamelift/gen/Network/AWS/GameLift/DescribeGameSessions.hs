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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for one or more game sessions. This action can be used in several ways: (1) provide a /GameSessionId/ to request properties for a specific game session; (2) provide a /FleetId/ or an /AliasId/ to request properties for all game sessions running on a fleet.
--
-- To get game session record(s), specify just one of the following: game session ID, fleet ID, or alias ID. You can filter this request by game session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a < GameSession> object is returned for each session matching the request.
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

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for a request action.
--
-- /See:/ 'describeGameSessions' smart constructor.
data DescribeGameSessions = DescribeGameSessions'
    { _dgsGameSessionId :: !(Maybe Text)
    , _dgsAliasId       :: !(Maybe Text)
    , _dgsNextToken     :: !(Maybe Text)
    , _dgsStatusFilter  :: !(Maybe Text)
    , _dgsLimit         :: !(Maybe Nat)
    , _dgsFleetId       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeGameSessions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsGameSessionId'
--
-- * 'dgsAliasId'
--
-- * 'dgsNextToken'
--
-- * 'dgsStatusFilter'
--
-- * 'dgsLimit'
--
-- * 'dgsFleetId'
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

-- | Unique identifier for a game session. Specify the game session to retrieve information on.
dgsGameSessionId :: Lens' DescribeGameSessions (Maybe Text)
dgsGameSessionId = lens _dgsGameSessionId (\ s a -> s{_dgsGameSessionId = a});

-- | Unique identifier for a fleet alias. Specify an alias to retrieve information on all game sessions active on the fleet.
dgsAliasId :: Lens' DescribeGameSessions (Maybe Text)
dgsAliasId = lens _dgsAliasId (\ s a -> s{_dgsAliasId = a});

-- | Token indicating the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To specify the start of the result set, do not specify a value.
dgsNextToken :: Lens' DescribeGameSessions (Maybe Text)
dgsNextToken = lens _dgsNextToken (\ s a -> s{_dgsNextToken = a});

-- | Game session status to filter results on. Possible game session states include ACTIVE, TERMINATED, ACTIVATING and TERMINATING (the last two are transitory).
dgsStatusFilter :: Lens' DescribeGameSessions (Maybe Text)
dgsStatusFilter = lens _dgsStatusFilter (\ s a -> s{_dgsStatusFilter = a});

-- | Maximum number of results to return. You can use this parameter with /NextToken/ to get results as a set of sequential pages.
dgsLimit :: Lens' DescribeGameSessions (Maybe Natural)
dgsLimit = lens _dgsLimit (\ s a -> s{_dgsLimit = a}) . mapping _Nat;

-- | Unique identifier for a fleet. Specify a fleet to retrieve information on all game sessions active on the fleet.
dgsFleetId :: Lens' DescribeGameSessions (Maybe Text)
dgsFleetId = lens _dgsFleetId (\ s a -> s{_dgsFleetId = a});

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

instance Hashable DescribeGameSessions

instance NFData DescribeGameSessions

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
-- /See:/ 'describeGameSessionsResponse' smart constructor.
data DescribeGameSessionsResponse = DescribeGameSessionsResponse'
    { _dgsrsGameSessions   :: !(Maybe [GameSession])
    , _dgsrsNextToken      :: !(Maybe Text)
    , _dgsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeGameSessionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsrsGameSessions'
--
-- * 'dgsrsNextToken'
--
-- * 'dgsrsResponseStatus'
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
dgsrsGameSessions = lens _dgsrsGameSessions (\ s a -> s{_dgsrsGameSessions = a}) . _Default . _Coerce;

-- | Token indicating where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- If a request has a limit that exactly matches the number of remaining results, a token is returned even though there are no more results to retrieve.
dgsrsNextToken :: Lens' DescribeGameSessionsResponse (Maybe Text)
dgsrsNextToken = lens _dgsrsNextToken (\ s a -> s{_dgsrsNextToken = a});

-- | The response status code.
dgsrsResponseStatus :: Lens' DescribeGameSessionsResponse Int
dgsrsResponseStatus = lens _dgsrsResponseStatus (\ s a -> s{_dgsrsResponseStatus = a});

instance NFData DescribeGameSessionsResponse
