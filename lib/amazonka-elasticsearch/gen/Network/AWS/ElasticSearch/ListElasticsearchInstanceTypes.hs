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
-- Module      : Network.AWS.ElasticSearch.ListElasticsearchInstanceTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all Elasticsearch instance types that are supported for given ElasticsearchVersion
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.ListElasticsearchInstanceTypes
    (
    -- * Creating a Request
      listElasticsearchInstanceTypes
    , ListElasticsearchInstanceTypes
    -- * Request Lenses
    , leitNextToken
    , leitDomainName
    , leitMaxResults
    , leitElasticsearchVersion

    -- * Destructuring the Response
    , listElasticsearchInstanceTypesResponse
    , ListElasticsearchInstanceTypesResponse
    -- * Response Lenses
    , leitrsElasticsearchInstanceTypes
    , leitrsNextToken
    , leitrsResponseStatus
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'ListElasticsearchInstanceTypes' @ operation.
--
--
--
-- /See:/ 'listElasticsearchInstanceTypes' smart constructor.
data ListElasticsearchInstanceTypes = ListElasticsearchInstanceTypes'
  { _leitNextToken            :: !(Maybe Text)
  , _leitDomainName           :: !(Maybe Text)
  , _leitMaxResults           :: !(Maybe Int)
  , _leitElasticsearchVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListElasticsearchInstanceTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leitNextToken' - NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
--
-- * 'leitDomainName' - DomainName represents the name of the Domain that we are trying to modify. This should be present only if we are querying for list of available Elasticsearch instance types when modifying existing domain.
--
-- * 'leitMaxResults' - Set this value to limit the number of results returned. Value provided must be greater than 30 else it wont be honored.
--
-- * 'leitElasticsearchVersion' - Version of Elasticsearch for which list of supported elasticsearch instance types are needed.
listElasticsearchInstanceTypes
    :: Text -- ^ 'leitElasticsearchVersion'
    -> ListElasticsearchInstanceTypes
listElasticsearchInstanceTypes pElasticsearchVersion_ =
  ListElasticsearchInstanceTypes'
    { _leitNextToken = Nothing
    , _leitDomainName = Nothing
    , _leitMaxResults = Nothing
    , _leitElasticsearchVersion = pElasticsearchVersion_
    }


-- | NextToken should be sent in case if earlier API call produced result containing NextToken. It is used for pagination.
leitNextToken :: Lens' ListElasticsearchInstanceTypes (Maybe Text)
leitNextToken = lens _leitNextToken (\ s a -> s{_leitNextToken = a})

-- | DomainName represents the name of the Domain that we are trying to modify. This should be present only if we are querying for list of available Elasticsearch instance types when modifying existing domain.
leitDomainName :: Lens' ListElasticsearchInstanceTypes (Maybe Text)
leitDomainName = lens _leitDomainName (\ s a -> s{_leitDomainName = a})

-- | Set this value to limit the number of results returned. Value provided must be greater than 30 else it wont be honored.
leitMaxResults :: Lens' ListElasticsearchInstanceTypes (Maybe Int)
leitMaxResults = lens _leitMaxResults (\ s a -> s{_leitMaxResults = a})

-- | Version of Elasticsearch for which list of supported elasticsearch instance types are needed.
leitElasticsearchVersion :: Lens' ListElasticsearchInstanceTypes Text
leitElasticsearchVersion = lens _leitElasticsearchVersion (\ s a -> s{_leitElasticsearchVersion = a})

instance AWSPager ListElasticsearchInstanceTypes
         where
        page rq rs
          | stop (rs ^. leitrsNextToken) = Nothing
          | stop (rs ^. leitrsElasticsearchInstanceTypes) =
            Nothing
          | otherwise =
            Just $ rq & leitNextToken .~ rs ^. leitrsNextToken

instance AWSRequest ListElasticsearchInstanceTypes
         where
        type Rs ListElasticsearchInstanceTypes =
             ListElasticsearchInstanceTypesResponse
        request = get elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 ListElasticsearchInstanceTypesResponse' <$>
                   (x .?> "ElasticsearchInstanceTypes" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListElasticsearchInstanceTypes
         where

instance NFData ListElasticsearchInstanceTypes where

instance ToHeaders ListElasticsearchInstanceTypes
         where
        toHeaders = const mempty

instance ToPath ListElasticsearchInstanceTypes where
        toPath ListElasticsearchInstanceTypes'{..}
          = mconcat
              ["/2015-01-01/es/instanceTypes/",
               toBS _leitElasticsearchVersion]

instance ToQuery ListElasticsearchInstanceTypes where
        toQuery ListElasticsearchInstanceTypes'{..}
          = mconcat
              ["nextToken" =: _leitNextToken,
               "domainName" =: _leitDomainName,
               "maxResults" =: _leitMaxResults]

-- | Container for the parameters returned by @'ListElasticsearchInstanceTypes' @ operation.
--
--
--
-- /See:/ 'listElasticsearchInstanceTypesResponse' smart constructor.
data ListElasticsearchInstanceTypesResponse = ListElasticsearchInstanceTypesResponse'
  { _leitrsElasticsearchInstanceTypes :: !(Maybe [ESPartitionInstanceType])
  , _leitrsNextToken                  :: !(Maybe Text)
  , _leitrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListElasticsearchInstanceTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leitrsElasticsearchInstanceTypes' - List of instance types supported by Amazon Elasticsearch service for given @'ElasticsearchVersion' @
--
-- * 'leitrsNextToken' - In case if there are more results available NextToken would be present, make further request to the same API with received NextToken to paginate remaining results.
--
-- * 'leitrsResponseStatus' - -- | The response status code.
listElasticsearchInstanceTypesResponse
    :: Int -- ^ 'leitrsResponseStatus'
    -> ListElasticsearchInstanceTypesResponse
listElasticsearchInstanceTypesResponse pResponseStatus_ =
  ListElasticsearchInstanceTypesResponse'
    { _leitrsElasticsearchInstanceTypes = Nothing
    , _leitrsNextToken = Nothing
    , _leitrsResponseStatus = pResponseStatus_
    }


-- | List of instance types supported by Amazon Elasticsearch service for given @'ElasticsearchVersion' @
leitrsElasticsearchInstanceTypes :: Lens' ListElasticsearchInstanceTypesResponse [ESPartitionInstanceType]
leitrsElasticsearchInstanceTypes = lens _leitrsElasticsearchInstanceTypes (\ s a -> s{_leitrsElasticsearchInstanceTypes = a}) . _Default . _Coerce

-- | In case if there are more results available NextToken would be present, make further request to the same API with received NextToken to paginate remaining results.
leitrsNextToken :: Lens' ListElasticsearchInstanceTypesResponse (Maybe Text)
leitrsNextToken = lens _leitrsNextToken (\ s a -> s{_leitrsNextToken = a})

-- | -- | The response status code.
leitrsResponseStatus :: Lens' ListElasticsearchInstanceTypesResponse Int
leitrsResponseStatus = lens _leitrsResponseStatus (\ s a -> s{_leitrsResponseStatus = a})

instance NFData
           ListElasticsearchInstanceTypesResponse
         where
