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
-- Module      : Network.AWS.GameLift.DescribeGameSessionDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties, including the protection policy in force, for one or more game sessions. This action can be used in several ways: (1) provide a @GameSessionId@ or @GameSessionArn@ to request details for a specific game session; (2) provide either a @FleetId@ or an @AliasId@ to request properties for all game sessions running on a fleet.
--
--
-- To get game session record(s), specify just one of the following: game session ID, fleet ID, or alias ID. You can filter this request by game session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'GameSessionDetail' object is returned for each session matching the request.
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
module Network.AWS.GameLift.DescribeGameSessionDetails
    (
    -- * Creating a Request
      describeGameSessionDetails
    , DescribeGameSessionDetails
    -- * Request Lenses
    , dgsdGameSessionId
    , dgsdAliasId
    , dgsdNextToken
    , dgsdStatusFilter
    , dgsdLimit
    , dgsdFleetId

    -- * Destructuring the Response
    , describeGameSessionDetailsResponse
    , DescribeGameSessionDetailsResponse
    -- * Response Lenses
    , dgsdrsGameSessionDetails
    , dgsdrsNextToken
    , dgsdrsResponseStatus
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
-- /See:/ 'describeGameSessionDetails' smart constructor.
data DescribeGameSessionDetails = DescribeGameSessionDetails'
  { _dgsdGameSessionId :: !(Maybe Text)
  , _dgsdAliasId       :: !(Maybe Text)
  , _dgsdNextToken     :: !(Maybe Text)
  , _dgsdStatusFilter  :: !(Maybe Text)
  , _dgsdLimit         :: !(Maybe Nat)
  , _dgsdFleetId       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGameSessionDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsdGameSessionId' - Unique identifier for the game session to retrieve.
--
-- * 'dgsdAliasId' - Unique identifier for an alias associated with the fleet to retrieve all game sessions for.
--
-- * 'dgsdNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
--
-- * 'dgsdStatusFilter' - Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ and @TERMINATING@ (the last two are transitory).
--
-- * 'dgsdLimit' - Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- * 'dgsdFleetId' - Unique identifier for a fleet to retrieve all game sessions active on the fleet.
describeGameSessionDetails
    :: DescribeGameSessionDetails
describeGameSessionDetails =
  DescribeGameSessionDetails'
    { _dgsdGameSessionId = Nothing
    , _dgsdAliasId = Nothing
    , _dgsdNextToken = Nothing
    , _dgsdStatusFilter = Nothing
    , _dgsdLimit = Nothing
    , _dgsdFleetId = Nothing
    }


-- | Unique identifier for the game session to retrieve.
dgsdGameSessionId :: Lens' DescribeGameSessionDetails (Maybe Text)
dgsdGameSessionId = lens _dgsdGameSessionId (\ s a -> s{_dgsdGameSessionId = a})

-- | Unique identifier for an alias associated with the fleet to retrieve all game sessions for.
dgsdAliasId :: Lens' DescribeGameSessionDetails (Maybe Text)
dgsdAliasId = lens _dgsdAliasId (\ s a -> s{_dgsdAliasId = a})

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
dgsdNextToken :: Lens' DescribeGameSessionDetails (Maybe Text)
dgsdNextToken = lens _dgsdNextToken (\ s a -> s{_dgsdNextToken = a})

-- | Game session status to filter results on. Possible game session statuses include @ACTIVE@ , @TERMINATED@ , @ACTIVATING@ and @TERMINATING@ (the last two are transitory).
dgsdStatusFilter :: Lens' DescribeGameSessionDetails (Maybe Text)
dgsdStatusFilter = lens _dgsdStatusFilter (\ s a -> s{_dgsdStatusFilter = a})

-- | Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
dgsdLimit :: Lens' DescribeGameSessionDetails (Maybe Natural)
dgsdLimit = lens _dgsdLimit (\ s a -> s{_dgsdLimit = a}) . mapping _Nat

-- | Unique identifier for a fleet to retrieve all game sessions active on the fleet.
dgsdFleetId :: Lens' DescribeGameSessionDetails (Maybe Text)
dgsdFleetId = lens _dgsdFleetId (\ s a -> s{_dgsdFleetId = a})

instance AWSRequest DescribeGameSessionDetails where
        type Rs DescribeGameSessionDetails =
             DescribeGameSessionDetailsResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeGameSessionDetailsResponse' <$>
                   (x .?> "GameSessionDetails" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeGameSessionDetails where

instance NFData DescribeGameSessionDetails where

instance ToHeaders DescribeGameSessionDetails where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeGameSessionDetails" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeGameSessionDetails where
        toJSON DescribeGameSessionDetails'{..}
          = object
              (catMaybes
                 [("GameSessionId" .=) <$> _dgsdGameSessionId,
                  ("AliasId" .=) <$> _dgsdAliasId,
                  ("NextToken" .=) <$> _dgsdNextToken,
                  ("StatusFilter" .=) <$> _dgsdStatusFilter,
                  ("Limit" .=) <$> _dgsdLimit,
                  ("FleetId" .=) <$> _dgsdFleetId])

instance ToPath DescribeGameSessionDetails where
        toPath = const "/"

instance ToQuery DescribeGameSessionDetails where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeGameSessionDetailsResponse' smart constructor.
data DescribeGameSessionDetailsResponse = DescribeGameSessionDetailsResponse'
  { _dgsdrsGameSessionDetails :: !(Maybe [GameSessionDetail])
  , _dgsdrsNextToken          :: !(Maybe Text)
  , _dgsdrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGameSessionDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsdrsGameSessionDetails' - Collection of objects containing game session properties and the protection policy currently in force for each session matching the request.
--
-- * 'dgsdrsNextToken' - Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- * 'dgsdrsResponseStatus' - -- | The response status code.
describeGameSessionDetailsResponse
    :: Int -- ^ 'dgsdrsResponseStatus'
    -> DescribeGameSessionDetailsResponse
describeGameSessionDetailsResponse pResponseStatus_ =
  DescribeGameSessionDetailsResponse'
    { _dgsdrsGameSessionDetails = Nothing
    , _dgsdrsNextToken = Nothing
    , _dgsdrsResponseStatus = pResponseStatus_
    }


-- | Collection of objects containing game session properties and the protection policy currently in force for each session matching the request.
dgsdrsGameSessionDetails :: Lens' DescribeGameSessionDetailsResponse [GameSessionDetail]
dgsdrsGameSessionDetails = lens _dgsdrsGameSessionDetails (\ s a -> s{_dgsdrsGameSessionDetails = a}) . _Default . _Coerce

-- | Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
dgsdrsNextToken :: Lens' DescribeGameSessionDetailsResponse (Maybe Text)
dgsdrsNextToken = lens _dgsdrsNextToken (\ s a -> s{_dgsdrsNextToken = a})

-- | -- | The response status code.
dgsdrsResponseStatus :: Lens' DescribeGameSessionDetailsResponse Int
dgsdrsResponseStatus = lens _dgsdrsResponseStatus (\ s a -> s{_dgsdrsResponseStatus = a})

instance NFData DescribeGameSessionDetailsResponse
         where
