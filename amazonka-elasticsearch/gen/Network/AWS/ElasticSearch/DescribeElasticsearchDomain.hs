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
-- Module      : Network.AWS.ElasticSearch.DescribeElasticsearchDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns domain configuration information about the specified Elasticsearch domain, including the domain ID, domain endpoint, and domain ARN.
--
--
module Network.AWS.ElasticSearch.DescribeElasticsearchDomain
    (
    -- * Creating a Request
      describeElasticsearchDomain
    , DescribeElasticsearchDomain
    -- * Request Lenses
    , dedDomainName

    -- * Destructuring the Response
    , describeElasticsearchDomainResponse
    , DescribeElasticsearchDomainResponse
    -- * Response Lenses
    , dedrsResponseStatus
    , dedrsDomainStatus
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DescribeElasticsearchDomain' @ operation.
--
--
--
-- /See:/ 'describeElasticsearchDomain' smart constructor.
newtype DescribeElasticsearchDomain = DescribeElasticsearchDomain'
  { _dedDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticsearchDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedDomainName' - The name of the Elasticsearch domain for which you want information.
describeElasticsearchDomain
    :: Text -- ^ 'dedDomainName'
    -> DescribeElasticsearchDomain
describeElasticsearchDomain pDomainName_ =
  DescribeElasticsearchDomain' {_dedDomainName = pDomainName_}


-- | The name of the Elasticsearch domain for which you want information.
dedDomainName :: Lens' DescribeElasticsearchDomain Text
dedDomainName = lens _dedDomainName (\ s a -> s{_dedDomainName = a})

instance AWSRequest DescribeElasticsearchDomain where
        type Rs DescribeElasticsearchDomain =
             DescribeElasticsearchDomainResponse
        request = get elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 DescribeElasticsearchDomainResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "DomainStatus"))

instance Hashable DescribeElasticsearchDomain where

instance NFData DescribeElasticsearchDomain where

instance ToHeaders DescribeElasticsearchDomain where
        toHeaders = const mempty

instance ToPath DescribeElasticsearchDomain where
        toPath DescribeElasticsearchDomain'{..}
          = mconcat
              ["/2015-01-01/es/domain/", toBS _dedDomainName]

instance ToQuery DescribeElasticsearchDomain where
        toQuery = const mempty

-- | The result of a @DescribeElasticsearchDomain@ request. Contains the status of the domain specified in the request.
--
--
--
-- /See:/ 'describeElasticsearchDomainResponse' smart constructor.
data DescribeElasticsearchDomainResponse = DescribeElasticsearchDomainResponse'
  { _dedrsResponseStatus :: !Int
  , _dedrsDomainStatus   :: !ElasticsearchDomainStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticsearchDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedrsResponseStatus' - -- | The response status code.
--
-- * 'dedrsDomainStatus' - The current status of the Elasticsearch domain.
describeElasticsearchDomainResponse
    :: Int -- ^ 'dedrsResponseStatus'
    -> ElasticsearchDomainStatus -- ^ 'dedrsDomainStatus'
    -> DescribeElasticsearchDomainResponse
describeElasticsearchDomainResponse pResponseStatus_ pDomainStatus_ =
  DescribeElasticsearchDomainResponse'
    { _dedrsResponseStatus = pResponseStatus_
    , _dedrsDomainStatus = pDomainStatus_
    }


-- | -- | The response status code.
dedrsResponseStatus :: Lens' DescribeElasticsearchDomainResponse Int
dedrsResponseStatus = lens _dedrsResponseStatus (\ s a -> s{_dedrsResponseStatus = a})

-- | The current status of the Elasticsearch domain.
dedrsDomainStatus :: Lens' DescribeElasticsearchDomainResponse ElasticsearchDomainStatus
dedrsDomainStatus = lens _dedrsDomainStatus (\ s a -> s{_dedrsDomainStatus = a})

instance NFData DescribeElasticsearchDomainResponse
         where
