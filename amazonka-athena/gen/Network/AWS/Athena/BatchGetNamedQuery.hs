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
-- Module      : Network.AWS.Athena.BatchGetNamedQuery
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of a single named query or a list of up to 50 queries, which you provide as an array of query ID strings. Use 'ListNamedQueries' to get the list of named query IDs. If information could not be retrieved for a submitted query ID, information about the query ID submitted is listed under 'UnprocessedNamedQueryId' . Named queries are different from executed queries. Use 'BatchGetQueryExecution' to get details about each unique query execution, and 'ListQueryExecutions' to get a list of query execution IDs.
--
--
module Network.AWS.Athena.BatchGetNamedQuery
    (
    -- * Creating a Request
      batchGetNamedQuery
    , BatchGetNamedQuery
    -- * Request Lenses
    , bgnqNamedQueryIds

    -- * Destructuring the Response
    , batchGetNamedQueryResponse
    , BatchGetNamedQueryResponse
    -- * Response Lenses
    , bgnqrsNamedQueries
    , bgnqrsUnprocessedNamedQueryIds
    , bgnqrsResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetNamedQuery' smart constructor.
newtype BatchGetNamedQuery = BatchGetNamedQuery'
  { _bgnqNamedQueryIds :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetNamedQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgnqNamedQueryIds' - An array of query IDs.
batchGetNamedQuery
    :: NonEmpty Text -- ^ 'bgnqNamedQueryIds'
    -> BatchGetNamedQuery
batchGetNamedQuery pNamedQueryIds_ =
  BatchGetNamedQuery' {_bgnqNamedQueryIds = _List1 # pNamedQueryIds_}


-- | An array of query IDs.
bgnqNamedQueryIds :: Lens' BatchGetNamedQuery (NonEmpty Text)
bgnqNamedQueryIds = lens _bgnqNamedQueryIds (\ s a -> s{_bgnqNamedQueryIds = a}) . _List1

instance AWSRequest BatchGetNamedQuery where
        type Rs BatchGetNamedQuery =
             BatchGetNamedQueryResponse
        request = postJSON athena
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetNamedQueryResponse' <$>
                   (x .?> "NamedQueries" .!@ mempty) <*>
                     (x .?> "UnprocessedNamedQueryIds" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable BatchGetNamedQuery where

instance NFData BatchGetNamedQuery where

instance ToHeaders BatchGetNamedQuery where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.BatchGetNamedQuery" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetNamedQuery where
        toJSON BatchGetNamedQuery'{..}
          = object
              (catMaybes
                 [Just ("NamedQueryIds" .= _bgnqNamedQueryIds)])

instance ToPath BatchGetNamedQuery where
        toPath = const "/"

instance ToQuery BatchGetNamedQuery where
        toQuery = const mempty

-- | /See:/ 'batchGetNamedQueryResponse' smart constructor.
data BatchGetNamedQueryResponse = BatchGetNamedQueryResponse'
  { _bgnqrsNamedQueries             :: !(Maybe [NamedQuery])
  , _bgnqrsUnprocessedNamedQueryIds :: !(Maybe [UnprocessedNamedQueryId])
  , _bgnqrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetNamedQueryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgnqrsNamedQueries' - Information about the named query IDs submitted.
--
-- * 'bgnqrsUnprocessedNamedQueryIds' - Information about provided query IDs.
--
-- * 'bgnqrsResponseStatus' - -- | The response status code.
batchGetNamedQueryResponse
    :: Int -- ^ 'bgnqrsResponseStatus'
    -> BatchGetNamedQueryResponse
batchGetNamedQueryResponse pResponseStatus_ =
  BatchGetNamedQueryResponse'
    { _bgnqrsNamedQueries = Nothing
    , _bgnqrsUnprocessedNamedQueryIds = Nothing
    , _bgnqrsResponseStatus = pResponseStatus_
    }


-- | Information about the named query IDs submitted.
bgnqrsNamedQueries :: Lens' BatchGetNamedQueryResponse [NamedQuery]
bgnqrsNamedQueries = lens _bgnqrsNamedQueries (\ s a -> s{_bgnqrsNamedQueries = a}) . _Default . _Coerce

-- | Information about provided query IDs.
bgnqrsUnprocessedNamedQueryIds :: Lens' BatchGetNamedQueryResponse [UnprocessedNamedQueryId]
bgnqrsUnprocessedNamedQueryIds = lens _bgnqrsUnprocessedNamedQueryIds (\ s a -> s{_bgnqrsUnprocessedNamedQueryIds = a}) . _Default . _Coerce

-- | -- | The response status code.
bgnqrsResponseStatus :: Lens' BatchGetNamedQueryResponse Int
bgnqrsResponseStatus = lens _bgnqrsResponseStatus (\ s a -> s{_bgnqrsResponseStatus = a})

instance NFData BatchGetNamedQueryResponse where
