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
-- Module      : Network.AWS.GameLift.DescribeGameSessionQueues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the properties for one or more game session queues. When requesting multiple queues, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'GameSessionQueue' object is returned for each requested queue. When specifying a list of queues, objects are returned only for queues that currently exist in the region.
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
module Network.AWS.GameLift.DescribeGameSessionQueues
    (
    -- * Creating a Request
      describeGameSessionQueues
    , DescribeGameSessionQueues
    -- * Request Lenses
    , dgsqNextToken
    , dgsqNames
    , dgsqLimit

    -- * Destructuring the Response
    , describeGameSessionQueuesResponse
    , DescribeGameSessionQueuesResponse
    -- * Response Lenses
    , drsNextToken
    , drsGameSessionQueues
    , drsResponseStatus
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
-- /See:/ 'describeGameSessionQueues' smart constructor.
data DescribeGameSessionQueues = DescribeGameSessionQueues'
  { _dgsqNextToken :: !(Maybe Text)
  , _dgsqNames     :: !(Maybe [Text])
  , _dgsqLimit     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGameSessionQueues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgsqNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
--
-- * 'dgsqNames' - List of queue names to retrieve information for. To request settings for all queues, leave this parameter empty.
--
-- * 'dgsqLimit' - Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
describeGameSessionQueues
    :: DescribeGameSessionQueues
describeGameSessionQueues =
  DescribeGameSessionQueues'
    {_dgsqNextToken = Nothing, _dgsqNames = Nothing, _dgsqLimit = Nothing}


-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
dgsqNextToken :: Lens' DescribeGameSessionQueues (Maybe Text)
dgsqNextToken = lens _dgsqNextToken (\ s a -> s{_dgsqNextToken = a})

-- | List of queue names to retrieve information for. To request settings for all queues, leave this parameter empty.
dgsqNames :: Lens' DescribeGameSessionQueues [Text]
dgsqNames = lens _dgsqNames (\ s a -> s{_dgsqNames = a}) . _Default . _Coerce

-- | Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
dgsqLimit :: Lens' DescribeGameSessionQueues (Maybe Natural)
dgsqLimit = lens _dgsqLimit (\ s a -> s{_dgsqLimit = a}) . mapping _Nat

instance AWSRequest DescribeGameSessionQueues where
        type Rs DescribeGameSessionQueues =
             DescribeGameSessionQueuesResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeGameSessionQueuesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "GameSessionQueues" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeGameSessionQueues where

instance NFData DescribeGameSessionQueues where

instance ToHeaders DescribeGameSessionQueues where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeGameSessionQueues" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeGameSessionQueues where
        toJSON DescribeGameSessionQueues'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dgsqNextToken,
                  ("Names" .=) <$> _dgsqNames,
                  ("Limit" .=) <$> _dgsqLimit])

instance ToPath DescribeGameSessionQueues where
        toPath = const "/"

instance ToQuery DescribeGameSessionQueues where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeGameSessionQueuesResponse' smart constructor.
data DescribeGameSessionQueuesResponse = DescribeGameSessionQueuesResponse'
  { _drsNextToken         :: !(Maybe Text)
  , _drsGameSessionQueues :: !(Maybe [GameSessionQueue])
  , _drsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGameSessionQueuesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsNextToken' - Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- * 'drsGameSessionQueues' - Collection of objects that describes the requested game session queues.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeGameSessionQueuesResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeGameSessionQueuesResponse
describeGameSessionQueuesResponse pResponseStatus_ =
  DescribeGameSessionQueuesResponse'
    { _drsNextToken = Nothing
    , _drsGameSessionQueues = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
drsNextToken :: Lens' DescribeGameSessionQueuesResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a})

-- | Collection of objects that describes the requested game session queues.
drsGameSessionQueues :: Lens' DescribeGameSessionQueuesResponse [GameSessionQueue]
drsGameSessionQueues = lens _drsGameSessionQueues (\ s a -> s{_drsGameSessionQueues = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeGameSessionQueuesResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeGameSessionQueuesResponse
         where
