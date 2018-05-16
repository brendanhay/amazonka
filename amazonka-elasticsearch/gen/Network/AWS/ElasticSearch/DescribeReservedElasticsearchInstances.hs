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
-- Module      : Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reserved Elasticsearch instances for this account.
--
--
module Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstances
    (
    -- * Creating a Request
      describeReservedElasticsearchInstances
    , DescribeReservedElasticsearchInstances
    -- * Request Lenses
    , dreiReservedElasticsearchInstanceId
    , dreiNextToken
    , dreiMaxResults

    -- * Destructuring the Response
    , describeReservedElasticsearchInstancesResponse
    , DescribeReservedElasticsearchInstancesResponse
    -- * Response Lenses
    , dreirsReservedElasticsearchInstances
    , dreirsNextToken
    , dreirsResponseStatus
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for parameters to @DescribeReservedElasticsearchInstances@
--
--
--
-- /See:/ 'describeReservedElasticsearchInstances' smart constructor.
data DescribeReservedElasticsearchInstances = DescribeReservedElasticsearchInstances'
  { _dreiReservedElasticsearchInstanceId :: !(Maybe Text)
  , _dreiNextToken                       :: !(Maybe Text)
  , _dreiMaxResults                      :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedElasticsearchInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dreiReservedElasticsearchInstanceId' - The reserved instance identifier filter value. Use this parameter to show only the reservation that matches the specified reserved Elasticsearch instance ID.
--
-- * 'dreiNextToken' - NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
--
-- * 'dreiMaxResults' - Set this value to limit the number of results returned. If not specified, defaults to 100.
describeReservedElasticsearchInstances
    :: DescribeReservedElasticsearchInstances
describeReservedElasticsearchInstances =
  DescribeReservedElasticsearchInstances'
    { _dreiReservedElasticsearchInstanceId = Nothing
    , _dreiNextToken = Nothing
    , _dreiMaxResults = Nothing
    }


-- | The reserved instance identifier filter value. Use this parameter to show only the reservation that matches the specified reserved Elasticsearch instance ID.
dreiReservedElasticsearchInstanceId :: Lens' DescribeReservedElasticsearchInstances (Maybe Text)
dreiReservedElasticsearchInstanceId = lens _dreiReservedElasticsearchInstanceId (\ s a -> s{_dreiReservedElasticsearchInstanceId = a})

-- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
dreiNextToken :: Lens' DescribeReservedElasticsearchInstances (Maybe Text)
dreiNextToken = lens _dreiNextToken (\ s a -> s{_dreiNextToken = a})

-- | Set this value to limit the number of results returned. If not specified, defaults to 100.
dreiMaxResults :: Lens' DescribeReservedElasticsearchInstances (Maybe Int)
dreiMaxResults = lens _dreiMaxResults (\ s a -> s{_dreiMaxResults = a})

instance AWSRequest
           DescribeReservedElasticsearchInstances
         where
        type Rs DescribeReservedElasticsearchInstances =
             DescribeReservedElasticsearchInstancesResponse
        request = get elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 DescribeReservedElasticsearchInstancesResponse' <$>
                   (x .?> "ReservedElasticsearchInstances" .!@ mempty)
                     <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeReservedElasticsearchInstances
         where

instance NFData
           DescribeReservedElasticsearchInstances
         where

instance ToHeaders
           DescribeReservedElasticsearchInstances
         where
        toHeaders = const mempty

instance ToPath
           DescribeReservedElasticsearchInstances
         where
        toPath = const "/2015-01-01/es/reservedInstances"

instance ToQuery
           DescribeReservedElasticsearchInstances
         where
        toQuery DescribeReservedElasticsearchInstances'{..}
          = mconcat
              ["reservationId" =:
                 _dreiReservedElasticsearchInstanceId,
               "nextToken" =: _dreiNextToken,
               "maxResults" =: _dreiMaxResults]

-- | Container for results from @DescribeReservedElasticsearchInstances@
--
--
--
-- /See:/ 'describeReservedElasticsearchInstancesResponse' smart constructor.
data DescribeReservedElasticsearchInstancesResponse = DescribeReservedElasticsearchInstancesResponse'
  { _dreirsReservedElasticsearchInstances :: !(Maybe [ReservedElasticsearchInstance])
  , _dreirsNextToken :: !(Maybe Text)
  , _dreirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedElasticsearchInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dreirsReservedElasticsearchInstances' - List of reserved Elasticsearch instances.
--
-- * 'dreirsNextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'dreirsResponseStatus' - -- | The response status code.
describeReservedElasticsearchInstancesResponse
    :: Int -- ^ 'dreirsResponseStatus'
    -> DescribeReservedElasticsearchInstancesResponse
describeReservedElasticsearchInstancesResponse pResponseStatus_ =
  DescribeReservedElasticsearchInstancesResponse'
    { _dreirsReservedElasticsearchInstances = Nothing
    , _dreirsNextToken = Nothing
    , _dreirsResponseStatus = pResponseStatus_
    }


-- | List of reserved Elasticsearch instances.
dreirsReservedElasticsearchInstances :: Lens' DescribeReservedElasticsearchInstancesResponse [ReservedElasticsearchInstance]
dreirsReservedElasticsearchInstances = lens _dreirsReservedElasticsearchInstances (\ s a -> s{_dreirsReservedElasticsearchInstances = a}) . _Default . _Coerce

-- | Provides an identifier to allow retrieval of paginated results.
dreirsNextToken :: Lens' DescribeReservedElasticsearchInstancesResponse (Maybe Text)
dreirsNextToken = lens _dreirsNextToken (\ s a -> s{_dreirsNextToken = a})

-- | -- | The response status code.
dreirsResponseStatus :: Lens' DescribeReservedElasticsearchInstancesResponse Int
dreirsResponseStatus = lens _dreirsResponseStatus (\ s a -> s{_dreirsResponseStatus = a})

instance NFData
           DescribeReservedElasticsearchInstancesResponse
         where
