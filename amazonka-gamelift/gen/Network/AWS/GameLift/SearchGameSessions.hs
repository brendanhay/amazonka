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
-- Module      : Network.AWS.GameLift.SearchGameSessions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all active game sessions that match a set of search criteria and sorts them in a specified order. You can search or sort by the following game session attributes:
--
--
--     * __gameSessionId__ -- Unique identifier for the game session. You can use either a @GameSessionId@ or @GameSessionArn@ value.
--
--     * __gameSessionName__ -- Name assigned to a game session. This value is set when requesting a new game session with 'CreateGameSession' or updating with 'UpdateGameSession' . Game session names do not need to be unique to a game session.
--
--     * __gameSessionProperties__ -- Custom data defined in a game session's @GameProperty@ parameter. @GameProperty@ values are stored as key:value pairs; the filter expression must indicate the key and a string to search the data values for. For example, to search for game sessions with custom data containing the key:value pair "gameMode:brawl", specify the following: @gameSessionProperties.gameMode = "brawl"@ . All custom data values are searched as strings.
--
--     * __maximumSessions__ -- Maximum number of player sessions allowed for a game session. This value is set when requesting a new game session with 'CreateGameSession' or updating with 'UpdateGameSession' .
--
--     * __creationTimeMillis__ -- Value indicating when a game session was created. It is expressed in Unix time as milliseconds.
--
--     * __playerSessionCount__ -- Number of players currently connected to a game session. This value changes rapidly as players join the session or drop out.
--
--     * __hasAvailablePlayerSessions__ -- Boolean value indicating whether a game session has reached its maximum number of players. It is highly recommended that all search requests include this filter attribute to optimize search performance and return only sessions that players can join.
--
--
--
-- To search or sort, specify either a fleet ID or an alias ID, and provide a search filter expression, a sort expression, or both. If successful, a collection of 'GameSession' objects matching the request is returned. Use the pagination parameters to retrieve results as a set of sequential pages.
--
-- You can search for game sessions one fleet at a time only. To find game sessions across multiple fleets, you must search each fleet separately and combine the results. This search feature finds only game sessions that are in @ACTIVE@ status. To locate games in statuses other than active, use 'DescribeGameSessionDetails' .
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
module Network.AWS.GameLift.SearchGameSessions
    (
    -- * Creating a Request
      searchGameSessions
    , SearchGameSessions
    -- * Request Lenses
    , sgsFilterExpression
    , sgsSortExpression
    , sgsAliasId
    , sgsNextToken
    , sgsLimit
    , sgsFleetId

    -- * Destructuring the Response
    , searchGameSessionsResponse
    , SearchGameSessionsResponse
    -- * Response Lenses
    , sgsrsGameSessions
    , sgsrsNextToken
    , sgsrsResponseStatus
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
-- /See:/ 'searchGameSessions' smart constructor.
data SearchGameSessions = SearchGameSessions'
  { _sgsFilterExpression :: !(Maybe Text)
  , _sgsSortExpression   :: !(Maybe Text)
  , _sgsAliasId          :: !(Maybe Text)
  , _sgsNextToken        :: !(Maybe Text)
  , _sgsLimit            :: !(Maybe Nat)
  , _sgsFleetId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchGameSessions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgsFilterExpression' - String containing the search criteria for the session search. If no filter expression is included, the request returns results for all game sessions in the fleet that are in @ACTIVE@ status. A filter expression can contain one or multiple conditions. Each condition consists of the following:     * __Operand__ -- Name of a game session attribute. Valid values are @gameSessionName@ , @gameSessionId@ , @gameSessionProperties@ , @maximumSessions@ , @creationTimeMillis@ , @playerSessionCount@ , @hasAvailablePlayerSessions@ .     * __Comparator__ -- Valid comparators are: @=@ , @<>@ , @<@ , @>@ , @<=@ , @>=@ .      * __Value__ -- Value to be searched for. Values may be numbers, boolean values (true/false) or strings depending on the operand. String values are case sensitive and must be enclosed in single quotes. Special characters must be escaped. Boolean and string values can only be used with the comparators @=@ and @<>@ . For example, the following filter expression searches on @gameSessionName@ : "@FilterExpression": "gameSessionName = 'Matt\\'s Awesome Game 1'"@ .  To chain multiple conditions in a single expression, use the logical keywords @AND@ , @OR@ , and @NOT@ and parentheses as needed. For example: @x AND y AND NOT z@ , @NOT (x OR y)@ . Session search evaluates conditions from left to right using the following precedence rules:     * @=@ , @<>@ , @<@ , @>@ , @<=@ , @>=@      * Parentheses     * NOT     * AND     * OR For example, this filter expression retrieves game sessions hosting at least ten players that have an open player slot: @"maximumSessions>=10 AND hasAvailablePlayerSessions=true"@ .
--
-- * 'sgsSortExpression' - Instructions on how to sort the search results. If no sort expression is included, the request returns results in random order. A sort expression consists of the following elements:     * __Operand__ -- Name of a game session attribute. Valid values are @gameSessionName@ , @gameSessionId@ , @gameSessionProperties@ , @maximumSessions@ , @creationTimeMillis@ , @playerSessionCount@ , @hasAvailablePlayerSessions@ .     * __Order__ -- Valid sort orders are @ASC@ (ascending) and @DESC@ (descending). For example, this sort expression returns the oldest active sessions first: @"SortExpression": "creationTimeMillis ASC"@ . Results with a null value for the sort operand are returned at the end of the list.
--
-- * 'sgsAliasId' - Unique identifier for an alias associated with the fleet to search for active game sessions. Each request must reference either a fleet ID or alias ID, but not both.
--
-- * 'sgsNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
--
-- * 'sgsLimit' - Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. The maximum number of results returned is 20, even if this value is not set or is set higher than 20.
--
-- * 'sgsFleetId' - Unique identifier for a fleet to search for active game sessions. Each request must reference either a fleet ID or alias ID, but not both.
searchGameSessions
    :: SearchGameSessions
searchGameSessions =
  SearchGameSessions'
    { _sgsFilterExpression = Nothing
    , _sgsSortExpression = Nothing
    , _sgsAliasId = Nothing
    , _sgsNextToken = Nothing
    , _sgsLimit = Nothing
    , _sgsFleetId = Nothing
    }


-- | String containing the search criteria for the session search. If no filter expression is included, the request returns results for all game sessions in the fleet that are in @ACTIVE@ status. A filter expression can contain one or multiple conditions. Each condition consists of the following:     * __Operand__ -- Name of a game session attribute. Valid values are @gameSessionName@ , @gameSessionId@ , @gameSessionProperties@ , @maximumSessions@ , @creationTimeMillis@ , @playerSessionCount@ , @hasAvailablePlayerSessions@ .     * __Comparator__ -- Valid comparators are: @=@ , @<>@ , @<@ , @>@ , @<=@ , @>=@ .      * __Value__ -- Value to be searched for. Values may be numbers, boolean values (true/false) or strings depending on the operand. String values are case sensitive and must be enclosed in single quotes. Special characters must be escaped. Boolean and string values can only be used with the comparators @=@ and @<>@ . For example, the following filter expression searches on @gameSessionName@ : "@FilterExpression": "gameSessionName = 'Matt\\'s Awesome Game 1'"@ .  To chain multiple conditions in a single expression, use the logical keywords @AND@ , @OR@ , and @NOT@ and parentheses as needed. For example: @x AND y AND NOT z@ , @NOT (x OR y)@ . Session search evaluates conditions from left to right using the following precedence rules:     * @=@ , @<>@ , @<@ , @>@ , @<=@ , @>=@      * Parentheses     * NOT     * AND     * OR For example, this filter expression retrieves game sessions hosting at least ten players that have an open player slot: @"maximumSessions>=10 AND hasAvailablePlayerSessions=true"@ .
sgsFilterExpression :: Lens' SearchGameSessions (Maybe Text)
sgsFilterExpression = lens _sgsFilterExpression (\ s a -> s{_sgsFilterExpression = a})

-- | Instructions on how to sort the search results. If no sort expression is included, the request returns results in random order. A sort expression consists of the following elements:     * __Operand__ -- Name of a game session attribute. Valid values are @gameSessionName@ , @gameSessionId@ , @gameSessionProperties@ , @maximumSessions@ , @creationTimeMillis@ , @playerSessionCount@ , @hasAvailablePlayerSessions@ .     * __Order__ -- Valid sort orders are @ASC@ (ascending) and @DESC@ (descending). For example, this sort expression returns the oldest active sessions first: @"SortExpression": "creationTimeMillis ASC"@ . Results with a null value for the sort operand are returned at the end of the list.
sgsSortExpression :: Lens' SearchGameSessions (Maybe Text)
sgsSortExpression = lens _sgsSortExpression (\ s a -> s{_sgsSortExpression = a})

-- | Unique identifier for an alias associated with the fleet to search for active game sessions. Each request must reference either a fleet ID or alias ID, but not both.
sgsAliasId :: Lens' SearchGameSessions (Maybe Text)
sgsAliasId = lens _sgsAliasId (\ s a -> s{_sgsAliasId = a})

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
sgsNextToken :: Lens' SearchGameSessions (Maybe Text)
sgsNextToken = lens _sgsNextToken (\ s a -> s{_sgsNextToken = a})

-- | Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. The maximum number of results returned is 20, even if this value is not set or is set higher than 20.
sgsLimit :: Lens' SearchGameSessions (Maybe Natural)
sgsLimit = lens _sgsLimit (\ s a -> s{_sgsLimit = a}) . mapping _Nat

-- | Unique identifier for a fleet to search for active game sessions. Each request must reference either a fleet ID or alias ID, but not both.
sgsFleetId :: Lens' SearchGameSessions (Maybe Text)
sgsFleetId = lens _sgsFleetId (\ s a -> s{_sgsFleetId = a})

instance AWSRequest SearchGameSessions where
        type Rs SearchGameSessions =
             SearchGameSessionsResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 SearchGameSessionsResponse' <$>
                   (x .?> "GameSessions" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable SearchGameSessions where

instance NFData SearchGameSessions where

instance ToHeaders SearchGameSessions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.SearchGameSessions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SearchGameSessions where
        toJSON SearchGameSessions'{..}
          = object
              (catMaybes
                 [("FilterExpression" .=) <$> _sgsFilterExpression,
                  ("SortExpression" .=) <$> _sgsSortExpression,
                  ("AliasId" .=) <$> _sgsAliasId,
                  ("NextToken" .=) <$> _sgsNextToken,
                  ("Limit" .=) <$> _sgsLimit,
                  ("FleetId" .=) <$> _sgsFleetId])

instance ToPath SearchGameSessions where
        toPath = const "/"

instance ToQuery SearchGameSessions where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'searchGameSessionsResponse' smart constructor.
data SearchGameSessionsResponse = SearchGameSessionsResponse'
  { _sgsrsGameSessions   :: !(Maybe [GameSession])
  , _sgsrsNextToken      :: !(Maybe Text)
  , _sgsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SearchGameSessionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgsrsGameSessions' - Collection of objects containing game session properties for each session matching the request.
--
-- * 'sgsrsNextToken' - Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- * 'sgsrsResponseStatus' - -- | The response status code.
searchGameSessionsResponse
    :: Int -- ^ 'sgsrsResponseStatus'
    -> SearchGameSessionsResponse
searchGameSessionsResponse pResponseStatus_ =
  SearchGameSessionsResponse'
    { _sgsrsGameSessions = Nothing
    , _sgsrsNextToken = Nothing
    , _sgsrsResponseStatus = pResponseStatus_
    }


-- | Collection of objects containing game session properties for each session matching the request.
sgsrsGameSessions :: Lens' SearchGameSessionsResponse [GameSession]
sgsrsGameSessions = lens _sgsrsGameSessions (\ s a -> s{_sgsrsGameSessions = a}) . _Default . _Coerce

-- | Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
sgsrsNextToken :: Lens' SearchGameSessionsResponse (Maybe Text)
sgsrsNextToken = lens _sgsrsNextToken (\ s a -> s{_sgsrsNextToken = a})

-- | -- | The response status code.
sgsrsResponseStatus :: Lens' SearchGameSessionsResponse Int
sgsrsResponseStatus = lens _sgsrsResponseStatus (\ s a -> s{_sgsrsResponseStatus = a})

instance NFData SearchGameSessionsResponse where
