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
-- Module      : Network.AWS.Athena.ListQueryExecutions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of all available query execution IDs.
--
--
-- For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.Athena.ListQueryExecutions
    (
    -- * Creating a Request
      listQueryExecutions
    , ListQueryExecutions
    -- * Request Lenses
    , lqeNextToken
    , lqeMaxResults

    -- * Destructuring the Response
    , listQueryExecutionsResponse
    , ListQueryExecutionsResponse
    -- * Response Lenses
    , lqersQueryExecutionIds
    , lqersNextToken
    , lqersResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listQueryExecutions' smart constructor.
data ListQueryExecutions = ListQueryExecutions'
  { _lqeNextToken  :: !(Maybe Text)
  , _lqeMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQueryExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqeNextToken' - The token that specifies where to start pagination if a previous request was truncated.
--
-- * 'lqeMaxResults' - The maximum number of query executions to return in this request.
listQueryExecutions
    :: ListQueryExecutions
listQueryExecutions =
  ListQueryExecutions' {_lqeNextToken = Nothing, _lqeMaxResults = Nothing}


-- | The token that specifies where to start pagination if a previous request was truncated.
lqeNextToken :: Lens' ListQueryExecutions (Maybe Text)
lqeNextToken = lens _lqeNextToken (\ s a -> s{_lqeNextToken = a})

-- | The maximum number of query executions to return in this request.
lqeMaxResults :: Lens' ListQueryExecutions (Maybe Natural)
lqeMaxResults = lens _lqeMaxResults (\ s a -> s{_lqeMaxResults = a}) . mapping _Nat

instance AWSPager ListQueryExecutions where
        page rq rs
          | stop (rs ^. lqersNextToken) = Nothing
          | stop (rs ^. lqersQueryExecutionIds) = Nothing
          | otherwise =
            Just $ rq & lqeNextToken .~ rs ^. lqersNextToken

instance AWSRequest ListQueryExecutions where
        type Rs ListQueryExecutions =
             ListQueryExecutionsResponse
        request = postJSON athena
        response
          = receiveJSON
              (\ s h x ->
                 ListQueryExecutionsResponse' <$>
                   (x .?> "QueryExecutionIds") <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListQueryExecutions where

instance NFData ListQueryExecutions where

instance ToHeaders ListQueryExecutions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.ListQueryExecutions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListQueryExecutions where
        toJSON ListQueryExecutions'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lqeNextToken,
                  ("MaxResults" .=) <$> _lqeMaxResults])

instance ToPath ListQueryExecutions where
        toPath = const "/"

instance ToQuery ListQueryExecutions where
        toQuery = const mempty

-- | /See:/ 'listQueryExecutionsResponse' smart constructor.
data ListQueryExecutionsResponse = ListQueryExecutionsResponse'
  { _lqersQueryExecutionIds :: !(Maybe (List1 Text))
  , _lqersNextToken         :: !(Maybe Text)
  , _lqersResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQueryExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqersQueryExecutionIds' - The unique IDs of each query execution as an array of strings.
--
-- * 'lqersNextToken' - A token to be used by the next request if this request is truncated.
--
-- * 'lqersResponseStatus' - -- | The response status code.
listQueryExecutionsResponse
    :: Int -- ^ 'lqersResponseStatus'
    -> ListQueryExecutionsResponse
listQueryExecutionsResponse pResponseStatus_ =
  ListQueryExecutionsResponse'
    { _lqersQueryExecutionIds = Nothing
    , _lqersNextToken = Nothing
    , _lqersResponseStatus = pResponseStatus_
    }


-- | The unique IDs of each query execution as an array of strings.
lqersQueryExecutionIds :: Lens' ListQueryExecutionsResponse (Maybe (NonEmpty Text))
lqersQueryExecutionIds = lens _lqersQueryExecutionIds (\ s a -> s{_lqersQueryExecutionIds = a}) . mapping _List1

-- | A token to be used by the next request if this request is truncated.
lqersNextToken :: Lens' ListQueryExecutionsResponse (Maybe Text)
lqersNextToken = lens _lqersNextToken (\ s a -> s{_lqersNextToken = a})

-- | -- | The response status code.
lqersResponseStatus :: Lens' ListQueryExecutionsResponse Int
lqersResponseStatus = lens _lqersResponseStatus (\ s a -> s{_lqersResponseStatus = a})

instance NFData ListQueryExecutionsResponse where
