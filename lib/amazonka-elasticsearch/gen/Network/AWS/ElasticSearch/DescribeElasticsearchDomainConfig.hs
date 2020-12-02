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
-- Module      : Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides cluster configuration information about the specified Elasticsearch domain, such as the state, creation date, update version, and update date for cluster options.
--
--
module Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig
    (
    -- * Creating a Request
      describeElasticsearchDomainConfig
    , DescribeElasticsearchDomainConfig
    -- * Request Lenses
    , dedcDomainName

    -- * Destructuring the Response
    , describeElasticsearchDomainConfigResponse
    , DescribeElasticsearchDomainConfigResponse
    -- * Response Lenses
    , dedcrsResponseStatus
    , dedcrsDomainConfig
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @DescribeElasticsearchDomainConfig@ operation. Specifies the domain name for which you want configuration information.
--
--
--
-- /See:/ 'describeElasticsearchDomainConfig' smart constructor.
newtype DescribeElasticsearchDomainConfig = DescribeElasticsearchDomainConfig'
  { _dedcDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticsearchDomainConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedcDomainName' - The Elasticsearch domain that you want to get information about.
describeElasticsearchDomainConfig
    :: Text -- ^ 'dedcDomainName'
    -> DescribeElasticsearchDomainConfig
describeElasticsearchDomainConfig pDomainName_ =
  DescribeElasticsearchDomainConfig' {_dedcDomainName = pDomainName_}


-- | The Elasticsearch domain that you want to get information about.
dedcDomainName :: Lens' DescribeElasticsearchDomainConfig Text
dedcDomainName = lens _dedcDomainName (\ s a -> s{_dedcDomainName = a})

instance AWSRequest DescribeElasticsearchDomainConfig
         where
        type Rs DescribeElasticsearchDomainConfig =
             DescribeElasticsearchDomainConfigResponse
        request = get elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 DescribeElasticsearchDomainConfigResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "DomainConfig"))

instance Hashable DescribeElasticsearchDomainConfig
         where

instance NFData DescribeElasticsearchDomainConfig
         where

instance ToHeaders DescribeElasticsearchDomainConfig
         where
        toHeaders = const mempty

instance ToPath DescribeElasticsearchDomainConfig
         where
        toPath DescribeElasticsearchDomainConfig'{..}
          = mconcat
              ["/2015-01-01/es/domain/", toBS _dedcDomainName,
               "/config"]

instance ToQuery DescribeElasticsearchDomainConfig
         where
        toQuery = const mempty

-- | The result of a @DescribeElasticsearchDomainConfig@ request. Contains the configuration information of the requested domain.
--
--
--
-- /See:/ 'describeElasticsearchDomainConfigResponse' smart constructor.
data DescribeElasticsearchDomainConfigResponse = DescribeElasticsearchDomainConfigResponse'
  { _dedcrsResponseStatus :: !Int
  , _dedcrsDomainConfig   :: !ElasticsearchDomainConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticsearchDomainConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedcrsResponseStatus' - -- | The response status code.
--
-- * 'dedcrsDomainConfig' - The configuration information of the domain requested in the @DescribeElasticsearchDomainConfig@ request.
describeElasticsearchDomainConfigResponse
    :: Int -- ^ 'dedcrsResponseStatus'
    -> ElasticsearchDomainConfig -- ^ 'dedcrsDomainConfig'
    -> DescribeElasticsearchDomainConfigResponse
describeElasticsearchDomainConfigResponse pResponseStatus_ pDomainConfig_ =
  DescribeElasticsearchDomainConfigResponse'
    { _dedcrsResponseStatus = pResponseStatus_
    , _dedcrsDomainConfig = pDomainConfig_
    }


-- | -- | The response status code.
dedcrsResponseStatus :: Lens' DescribeElasticsearchDomainConfigResponse Int
dedcrsResponseStatus = lens _dedcrsResponseStatus (\ s a -> s{_dedcrsResponseStatus = a})

-- | The configuration information of the domain requested in the @DescribeElasticsearchDomainConfig@ request.
dedcrsDomainConfig :: Lens' DescribeElasticsearchDomainConfigResponse ElasticsearchDomainConfig
dedcrsDomainConfig = lens _dedcrsDomainConfig (\ s a -> s{_dedcrsDomainConfig = a})

instance NFData
           DescribeElasticsearchDomainConfigResponse
         where
