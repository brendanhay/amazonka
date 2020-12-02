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
-- Module      : Network.AWS.ElasticSearch.DescribeElasticsearchDomains
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns domain configuration information about the specified Elasticsearch domains, including the domain ID, domain endpoint, and domain ARN.
--
--
module Network.AWS.ElasticSearch.DescribeElasticsearchDomains
    (
    -- * Creating a Request
      describeElasticsearchDomains
    , DescribeElasticsearchDomains
    -- * Request Lenses
    , dedDomainNames

    -- * Destructuring the Response
    , describeElasticsearchDomainsResponse
    , DescribeElasticsearchDomainsResponse
    -- * Response Lenses
    , drsResponseStatus
    , drsDomainStatusList
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DescribeElasticsearchDomains' @ operation. By default, the API returns the status of all Elasticsearch domains.
--
--
--
-- /See:/ 'describeElasticsearchDomains' smart constructor.
newtype DescribeElasticsearchDomains = DescribeElasticsearchDomains'
  { _dedDomainNames :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticsearchDomains' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedDomainNames' - The Elasticsearch domains for which you want information.
describeElasticsearchDomains
    :: DescribeElasticsearchDomains
describeElasticsearchDomains =
  DescribeElasticsearchDomains' {_dedDomainNames = mempty}


-- | The Elasticsearch domains for which you want information.
dedDomainNames :: Lens' DescribeElasticsearchDomains [Text]
dedDomainNames = lens _dedDomainNames (\ s a -> s{_dedDomainNames = a}) . _Coerce

instance AWSRequest DescribeElasticsearchDomains
         where
        type Rs DescribeElasticsearchDomains =
             DescribeElasticsearchDomainsResponse
        request = postJSON elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 DescribeElasticsearchDomainsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "DomainStatusList" .!@ mempty))

instance Hashable DescribeElasticsearchDomains where

instance NFData DescribeElasticsearchDomains where

instance ToHeaders DescribeElasticsearchDomains where
        toHeaders = const mempty

instance ToJSON DescribeElasticsearchDomains where
        toJSON DescribeElasticsearchDomains'{..}
          = object
              (catMaybes [Just ("DomainNames" .= _dedDomainNames)])

instance ToPath DescribeElasticsearchDomains where
        toPath = const "/2015-01-01/es/domain-info"

instance ToQuery DescribeElasticsearchDomains where
        toQuery = const mempty

-- | The result of a @DescribeElasticsearchDomains@ request. Contains the status of the specified domains or all domains owned by the account.
--
--
--
-- /See:/ 'describeElasticsearchDomainsResponse' smart constructor.
data DescribeElasticsearchDomainsResponse = DescribeElasticsearchDomainsResponse'
  { _drsResponseStatus   :: !Int
  , _drsDomainStatusList :: ![ElasticsearchDomainStatus]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticsearchDomainsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
--
-- * 'drsDomainStatusList' - The status of the domains requested in the @DescribeElasticsearchDomains@ request.
describeElasticsearchDomainsResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeElasticsearchDomainsResponse
describeElasticsearchDomainsResponse pResponseStatus_ =
  DescribeElasticsearchDomainsResponse'
    {_drsResponseStatus = pResponseStatus_, _drsDomainStatusList = mempty}


-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeElasticsearchDomainsResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

-- | The status of the domains requested in the @DescribeElasticsearchDomains@ request.
drsDomainStatusList :: Lens' DescribeElasticsearchDomainsResponse [ElasticsearchDomainStatus]
drsDomainStatusList = lens _drsDomainStatusList (\ s a -> s{_drsDomainStatusList = a}) . _Coerce

instance NFData DescribeElasticsearchDomainsResponse
         where
