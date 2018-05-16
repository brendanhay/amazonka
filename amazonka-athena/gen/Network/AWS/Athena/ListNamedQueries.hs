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
-- Module      : Network.AWS.Athena.ListNamedQueries
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of all available query IDs.
--
--
-- For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.Athena.ListNamedQueries
    (
    -- * Creating a Request
      listNamedQueries
    , ListNamedQueries
    -- * Request Lenses
    , lnqNextToken
    , lnqMaxResults

    -- * Destructuring the Response
    , listNamedQueriesResponse
    , ListNamedQueriesResponse
    -- * Response Lenses
    , lnqrsNextToken
    , lnqrsNamedQueryIds
    , lnqrsResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listNamedQueries' smart constructor.
data ListNamedQueries = ListNamedQueries'
  { _lnqNextToken  :: !(Maybe Text)
  , _lnqMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListNamedQueries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lnqNextToken' - The token that specifies where to start pagination if a previous request was truncated.
--
-- * 'lnqMaxResults' - The maximum number of queries to return in this request.
listNamedQueries
    :: ListNamedQueries
listNamedQueries =
  ListNamedQueries' {_lnqNextToken = Nothing, _lnqMaxResults = Nothing}


-- | The token that specifies where to start pagination if a previous request was truncated.
lnqNextToken :: Lens' ListNamedQueries (Maybe Text)
lnqNextToken = lens _lnqNextToken (\ s a -> s{_lnqNextToken = a})

-- | The maximum number of queries to return in this request.
lnqMaxResults :: Lens' ListNamedQueries (Maybe Natural)
lnqMaxResults = lens _lnqMaxResults (\ s a -> s{_lnqMaxResults = a}) . mapping _Nat

instance AWSPager ListNamedQueries where
        page rq rs
          | stop (rs ^. lnqrsNextToken) = Nothing
          | stop (rs ^. lnqrsNamedQueryIds) = Nothing
          | otherwise =
            Just $ rq & lnqNextToken .~ rs ^. lnqrsNextToken

instance AWSRequest ListNamedQueries where
        type Rs ListNamedQueries = ListNamedQueriesResponse
        request = postJSON athena
        response
          = receiveJSON
              (\ s h x ->
                 ListNamedQueriesResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "NamedQueryIds") <*>
                     (pure (fromEnum s)))

instance Hashable ListNamedQueries where

instance NFData ListNamedQueries where

instance ToHeaders ListNamedQueries where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.ListNamedQueries" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListNamedQueries where
        toJSON ListNamedQueries'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lnqNextToken,
                  ("MaxResults" .=) <$> _lnqMaxResults])

instance ToPath ListNamedQueries where
        toPath = const "/"

instance ToQuery ListNamedQueries where
        toQuery = const mempty

-- | /See:/ 'listNamedQueriesResponse' smart constructor.
data ListNamedQueriesResponse = ListNamedQueriesResponse'
  { _lnqrsNextToken      :: !(Maybe Text)
  , _lnqrsNamedQueryIds  :: !(Maybe (List1 Text))
  , _lnqrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListNamedQueriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lnqrsNextToken' - A token to be used by the next request if this request is truncated.
--
-- * 'lnqrsNamedQueryIds' - The list of unique query IDs.
--
-- * 'lnqrsResponseStatus' - -- | The response status code.
listNamedQueriesResponse
    :: Int -- ^ 'lnqrsResponseStatus'
    -> ListNamedQueriesResponse
listNamedQueriesResponse pResponseStatus_ =
  ListNamedQueriesResponse'
    { _lnqrsNextToken = Nothing
    , _lnqrsNamedQueryIds = Nothing
    , _lnqrsResponseStatus = pResponseStatus_
    }


-- | A token to be used by the next request if this request is truncated.
lnqrsNextToken :: Lens' ListNamedQueriesResponse (Maybe Text)
lnqrsNextToken = lens _lnqrsNextToken (\ s a -> s{_lnqrsNextToken = a})

-- | The list of unique query IDs.
lnqrsNamedQueryIds :: Lens' ListNamedQueriesResponse (Maybe (NonEmpty Text))
lnqrsNamedQueryIds = lens _lnqrsNamedQueryIds (\ s a -> s{_lnqrsNamedQueryIds = a}) . mapping _List1

-- | -- | The response status code.
lnqrsResponseStatus :: Lens' ListNamedQueriesResponse Int
lnqrsResponseStatus = lens _lnqrsResponseStatus (\ s a -> s{_lnqrsResponseStatus = a})

instance NFData ListNamedQueriesResponse where
