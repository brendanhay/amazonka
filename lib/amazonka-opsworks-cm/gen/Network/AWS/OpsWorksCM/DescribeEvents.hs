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
-- Module      : Network.AWS.OpsWorksCM.DescribeEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes events for a specified server. Results are ordered by time, with newest events first.
--
--
-- This operation is synchronous.
--
-- A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
--
module Network.AWS.OpsWorksCM.DescribeEvents
    (
    -- * Creating a Request
      describeEvents
    , DescribeEvents
    -- * Request Lenses
    , deNextToken
    , deMaxResults
    , deServerName

    -- * Destructuring the Response
    , describeEventsResponse
    , DescribeEventsResponse
    -- * Response Lenses
    , dersServerEvents
    , dersNextToken
    , dersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorksCM.Types
import Network.AWS.OpsWorksCM.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEvents' smart constructor.
data DescribeEvents = DescribeEvents'
  { _deNextToken  :: !(Maybe Text)
  , _deMaxResults :: !(Maybe Nat)
  , _deServerName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deNextToken' - NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeEvents@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
--
-- * 'deMaxResults' - To receive a paginated response, use this parameter to specify the maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- * 'deServerName' - The name of the server for which you want to view events.
describeEvents
    :: Text -- ^ 'deServerName'
    -> DescribeEvents
describeEvents pServerName_ =
  DescribeEvents'
    { _deNextToken = Nothing
    , _deMaxResults = Nothing
    , _deServerName = pServerName_
    }


-- | NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeEvents@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
deNextToken :: Lens' DescribeEvents (Maybe Text)
deNextToken = lens _deNextToken (\ s a -> s{_deNextToken = a})

-- | To receive a paginated response, use this parameter to specify the maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
deMaxResults :: Lens' DescribeEvents (Maybe Natural)
deMaxResults = lens _deMaxResults (\ s a -> s{_deMaxResults = a}) . mapping _Nat

-- | The name of the server for which you want to view events.
deServerName :: Lens' DescribeEvents Text
deServerName = lens _deServerName (\ s a -> s{_deServerName = a})

instance AWSRequest DescribeEvents where
        type Rs DescribeEvents = DescribeEventsResponse
        request = postJSON opsWorksCM
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventsResponse' <$>
                   (x .?> "ServerEvents" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEvents where

instance NFData DescribeEvents where

instance ToHeaders DescribeEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorksCM_V2016_11_01.DescribeEvents" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEvents where
        toJSON DescribeEvents'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _deNextToken,
                  ("MaxResults" .=) <$> _deMaxResults,
                  Just ("ServerName" .= _deServerName)])

instance ToPath DescribeEvents where
        toPath = const "/"

instance ToQuery DescribeEvents where
        toQuery = const mempty

-- | /See:/ 'describeEventsResponse' smart constructor.
data DescribeEventsResponse = DescribeEventsResponse'
  { _dersServerEvents   :: !(Maybe [ServerEvent])
  , _dersNextToken      :: !(Maybe Text)
  , _dersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersServerEvents' - Contains the response to a @DescribeEvents@ request.
--
-- * 'dersNextToken' - NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeEvents@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
--
-- * 'dersResponseStatus' - -- | The response status code.
describeEventsResponse
    :: Int -- ^ 'dersResponseStatus'
    -> DescribeEventsResponse
describeEventsResponse pResponseStatus_ =
  DescribeEventsResponse'
    { _dersServerEvents = Nothing
    , _dersNextToken = Nothing
    , _dersResponseStatus = pResponseStatus_
    }


-- | Contains the response to a @DescribeEvents@ request.
dersServerEvents :: Lens' DescribeEventsResponse [ServerEvent]
dersServerEvents = lens _dersServerEvents (\ s a -> s{_dersServerEvents = a}) . _Default . _Coerce

-- | NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeEvents@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
dersNextToken :: Lens' DescribeEventsResponse (Maybe Text)
dersNextToken = lens _dersNextToken (\ s a -> s{_dersNextToken = a})

-- | -- | The response status code.
dersResponseStatus :: Lens' DescribeEventsResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

instance NFData DescribeEventsResponse where
