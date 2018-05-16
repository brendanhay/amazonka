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
-- Module      : Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists available reserved Elasticsearch instance offerings.
--
--
module Network.AWS.ElasticSearch.DescribeReservedElasticsearchInstanceOfferings
    (
    -- * Creating a Request
      describeReservedElasticsearchInstanceOfferings
    , DescribeReservedElasticsearchInstanceOfferings
    -- * Request Lenses
    , dreioReservedElasticsearchInstanceOfferingId
    , dreioNextToken
    , dreioMaxResults

    -- * Destructuring the Response
    , describeReservedElasticsearchInstanceOfferingsResponse
    , DescribeReservedElasticsearchInstanceOfferingsResponse
    -- * Response Lenses
    , dreiorsReservedElasticsearchInstanceOfferings
    , dreiorsNextToken
    , dreiorsResponseStatus
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for parameters to @DescribeReservedElasticsearchInstanceOfferings@
--
--
--
-- /See:/ 'describeReservedElasticsearchInstanceOfferings' smart constructor.
data DescribeReservedElasticsearchInstanceOfferings = DescribeReservedElasticsearchInstanceOfferings'
  { _dreioReservedElasticsearchInstanceOfferingId :: !(Maybe Text)
  , _dreioNextToken                               :: !(Maybe Text)
  , _dreioMaxResults                              :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedElasticsearchInstanceOfferings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dreioReservedElasticsearchInstanceOfferingId' - The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier.
--
-- * 'dreioNextToken' - NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
--
-- * 'dreioMaxResults' - Set this value to limit the number of results returned. If not specified, defaults to 100.
describeReservedElasticsearchInstanceOfferings
    :: DescribeReservedElasticsearchInstanceOfferings
describeReservedElasticsearchInstanceOfferings =
  DescribeReservedElasticsearchInstanceOfferings'
    { _dreioReservedElasticsearchInstanceOfferingId = Nothing
    , _dreioNextToken = Nothing
    , _dreioMaxResults = Nothing
    }


-- | The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier.
dreioReservedElasticsearchInstanceOfferingId :: Lens' DescribeReservedElasticsearchInstanceOfferings (Maybe Text)
dreioReservedElasticsearchInstanceOfferingId = lens _dreioReservedElasticsearchInstanceOfferingId (\ s a -> s{_dreioReservedElasticsearchInstanceOfferingId = a})

-- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
dreioNextToken :: Lens' DescribeReservedElasticsearchInstanceOfferings (Maybe Text)
dreioNextToken = lens _dreioNextToken (\ s a -> s{_dreioNextToken = a})

-- | Set this value to limit the number of results returned. If not specified, defaults to 100.
dreioMaxResults :: Lens' DescribeReservedElasticsearchInstanceOfferings (Maybe Int)
dreioMaxResults = lens _dreioMaxResults (\ s a -> s{_dreioMaxResults = a})

instance AWSRequest
           DescribeReservedElasticsearchInstanceOfferings
         where
        type Rs
               DescribeReservedElasticsearchInstanceOfferings
             =
             DescribeReservedElasticsearchInstanceOfferingsResponse
        request = get elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 DescribeReservedElasticsearchInstanceOfferingsResponse'
                   <$>
                   (x .?> "ReservedElasticsearchInstanceOfferings" .!@
                      mempty)
                     <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeReservedElasticsearchInstanceOfferings
         where

instance NFData
           DescribeReservedElasticsearchInstanceOfferings
         where

instance ToHeaders
           DescribeReservedElasticsearchInstanceOfferings
         where
        toHeaders = const mempty

instance ToPath
           DescribeReservedElasticsearchInstanceOfferings
         where
        toPath
          = const "/2015-01-01/es/reservedInstanceOfferings"

instance ToQuery
           DescribeReservedElasticsearchInstanceOfferings
         where
        toQuery
          DescribeReservedElasticsearchInstanceOfferings'{..}
          = mconcat
              ["offeringId" =:
                 _dreioReservedElasticsearchInstanceOfferingId,
               "nextToken" =: _dreioNextToken,
               "maxResults" =: _dreioMaxResults]

-- | Container for results from @DescribeReservedElasticsearchInstanceOfferings@
--
--
--
-- /See:/ 'describeReservedElasticsearchInstanceOfferingsResponse' smart constructor.
data DescribeReservedElasticsearchInstanceOfferingsResponse = DescribeReservedElasticsearchInstanceOfferingsResponse'
  { _dreiorsReservedElasticsearchInstanceOfferings :: !(Maybe [ReservedElasticsearchInstanceOffering])
  , _dreiorsNextToken :: !(Maybe Text)
  , _dreiorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedElasticsearchInstanceOfferingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dreiorsReservedElasticsearchInstanceOfferings' - List of reserved Elasticsearch instance offerings
--
-- * 'dreiorsNextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'dreiorsResponseStatus' - -- | The response status code.
describeReservedElasticsearchInstanceOfferingsResponse
    :: Int -- ^ 'dreiorsResponseStatus'
    -> DescribeReservedElasticsearchInstanceOfferingsResponse
describeReservedElasticsearchInstanceOfferingsResponse pResponseStatus_ =
  DescribeReservedElasticsearchInstanceOfferingsResponse'
    { _dreiorsReservedElasticsearchInstanceOfferings = Nothing
    , _dreiorsNextToken = Nothing
    , _dreiorsResponseStatus = pResponseStatus_
    }


-- | List of reserved Elasticsearch instance offerings
dreiorsReservedElasticsearchInstanceOfferings :: Lens' DescribeReservedElasticsearchInstanceOfferingsResponse [ReservedElasticsearchInstanceOffering]
dreiorsReservedElasticsearchInstanceOfferings = lens _dreiorsReservedElasticsearchInstanceOfferings (\ s a -> s{_dreiorsReservedElasticsearchInstanceOfferings = a}) . _Default . _Coerce

-- | Provides an identifier to allow retrieval of paginated results.
dreiorsNextToken :: Lens' DescribeReservedElasticsearchInstanceOfferingsResponse (Maybe Text)
dreiorsNextToken = lens _dreiorsNextToken (\ s a -> s{_dreiorsNextToken = a})

-- | -- | The response status code.
dreiorsResponseStatus :: Lens' DescribeReservedElasticsearchInstanceOfferingsResponse Int
dreiorsResponseStatus = lens _dreiorsResponseStatus (\ s a -> s{_dreiorsResponseStatus = a})

instance NFData
           DescribeReservedElasticsearchInstanceOfferingsResponse
         where
