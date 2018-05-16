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
-- Module      : Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe Elasticsearch Limits for a given InstanceType and ElasticsearchVersion. When modifying existing Domain, specify the @'DomainName' @ to know what Limits are supported for modifying.
--
--
module Network.AWS.ElasticSearch.DescribeElasticsearchInstanceTypeLimits
    (
    -- * Creating a Request
      describeElasticsearchInstanceTypeLimits
    , DescribeElasticsearchInstanceTypeLimits
    -- * Request Lenses
    , deitlDomainName
    , deitlInstanceType
    , deitlElasticsearchVersion

    -- * Destructuring the Response
    , describeElasticsearchInstanceTypeLimitsResponse
    , DescribeElasticsearchInstanceTypeLimitsResponse
    -- * Response Lenses
    , deitlrsLimitsByRole
    , deitlrsResponseStatus
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to @'DescribeElasticsearchInstanceTypeLimits' @ operation.
--
--
--
-- /See:/ 'describeElasticsearchInstanceTypeLimits' smart constructor.
data DescribeElasticsearchInstanceTypeLimits = DescribeElasticsearchInstanceTypeLimits'
  { _deitlDomainName           :: !(Maybe Text)
  , _deitlInstanceType         :: !ESPartitionInstanceType
  , _deitlElasticsearchVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticsearchInstanceTypeLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deitlDomainName' - DomainName represents the name of the Domain that we are trying to modify. This should be present only if we are querying for Elasticsearch @'Limits' @ for existing domain.
--
-- * 'deitlInstanceType' - The instance type for an Elasticsearch cluster for which Elasticsearch @'Limits' @ are needed.
--
-- * 'deitlElasticsearchVersion' - Version of Elasticsearch for which @'Limits' @ are needed.
describeElasticsearchInstanceTypeLimits
    :: ESPartitionInstanceType -- ^ 'deitlInstanceType'
    -> Text -- ^ 'deitlElasticsearchVersion'
    -> DescribeElasticsearchInstanceTypeLimits
describeElasticsearchInstanceTypeLimits pInstanceType_ pElasticsearchVersion_ =
  DescribeElasticsearchInstanceTypeLimits'
    { _deitlDomainName = Nothing
    , _deitlInstanceType = pInstanceType_
    , _deitlElasticsearchVersion = pElasticsearchVersion_
    }


-- | DomainName represents the name of the Domain that we are trying to modify. This should be present only if we are querying for Elasticsearch @'Limits' @ for existing domain.
deitlDomainName :: Lens' DescribeElasticsearchInstanceTypeLimits (Maybe Text)
deitlDomainName = lens _deitlDomainName (\ s a -> s{_deitlDomainName = a})

-- | The instance type for an Elasticsearch cluster for which Elasticsearch @'Limits' @ are needed.
deitlInstanceType :: Lens' DescribeElasticsearchInstanceTypeLimits ESPartitionInstanceType
deitlInstanceType = lens _deitlInstanceType (\ s a -> s{_deitlInstanceType = a})

-- | Version of Elasticsearch for which @'Limits' @ are needed.
deitlElasticsearchVersion :: Lens' DescribeElasticsearchInstanceTypeLimits Text
deitlElasticsearchVersion = lens _deitlElasticsearchVersion (\ s a -> s{_deitlElasticsearchVersion = a})

instance AWSRequest
           DescribeElasticsearchInstanceTypeLimits
         where
        type Rs DescribeElasticsearchInstanceTypeLimits =
             DescribeElasticsearchInstanceTypeLimitsResponse
        request = get elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 DescribeElasticsearchInstanceTypeLimitsResponse' <$>
                   (x .?> "LimitsByRole" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable
           DescribeElasticsearchInstanceTypeLimits
         where

instance NFData
           DescribeElasticsearchInstanceTypeLimits
         where

instance ToHeaders
           DescribeElasticsearchInstanceTypeLimits
         where
        toHeaders = const mempty

instance ToPath
           DescribeElasticsearchInstanceTypeLimits
         where
        toPath DescribeElasticsearchInstanceTypeLimits'{..}
          = mconcat
              ["/2015-01-01/es/instanceTypeLimits/",
               toBS _deitlElasticsearchVersion, "/",
               toBS _deitlInstanceType]

instance ToQuery
           DescribeElasticsearchInstanceTypeLimits
         where
        toQuery DescribeElasticsearchInstanceTypeLimits'{..}
          = mconcat ["domainName" =: _deitlDomainName]

-- | Container for the parameters received from @'DescribeElasticsearchInstanceTypeLimits' @ operation.
--
--
--
-- /See:/ 'describeElasticsearchInstanceTypeLimitsResponse' smart constructor.
data DescribeElasticsearchInstanceTypeLimitsResponse = DescribeElasticsearchInstanceTypeLimitsResponse'
  { _deitlrsLimitsByRole   :: !(Maybe (Map Text Limits))
  , _deitlrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticsearchInstanceTypeLimitsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deitlrsLimitsByRole' - Undocumented member.
--
-- * 'deitlrsResponseStatus' - -- | The response status code.
describeElasticsearchInstanceTypeLimitsResponse
    :: Int -- ^ 'deitlrsResponseStatus'
    -> DescribeElasticsearchInstanceTypeLimitsResponse
describeElasticsearchInstanceTypeLimitsResponse pResponseStatus_ =
  DescribeElasticsearchInstanceTypeLimitsResponse'
    {_deitlrsLimitsByRole = Nothing, _deitlrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
deitlrsLimitsByRole :: Lens' DescribeElasticsearchInstanceTypeLimitsResponse (HashMap Text Limits)
deitlrsLimitsByRole = lens _deitlrsLimitsByRole (\ s a -> s{_deitlrsLimitsByRole = a}) . _Default . _Map

-- | -- | The response status code.
deitlrsResponseStatus :: Lens' DescribeElasticsearchInstanceTypeLimitsResponse Int
deitlrsResponseStatus = lens _deitlrsResponseStatus (\ s a -> s{_deitlrsResponseStatus = a})

instance NFData
           DescribeElasticsearchInstanceTypeLimitsResponse
         where
