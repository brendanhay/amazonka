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
-- Module      : Network.AWS.ElasticSearch.CreateElasticsearchDomain
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Elasticsearch domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains> in the /Amazon Elasticsearch Service Developer Guide/.
module Network.AWS.ElasticSearch.CreateElasticsearchDomain
    (
    -- * Creating a Request
      createElasticsearchDomain
    , CreateElasticsearchDomain
    -- * Request Lenses
    , cedEBSOptions
    , cedAccessPolicies
    , cedElasticsearchClusterConfig
    , cedSnapshotOptions
    , cedAdvancedOptions
    , cedDomainName

    -- * Destructuring the Response
    , createElasticsearchDomainResponse
    , CreateElasticsearchDomainResponse
    -- * Response Lenses
    , cedrsDomainStatus
    , cedrsResponseStatus
    ) where

import           Network.AWS.ElasticSearch.Types
import           Network.AWS.ElasticSearch.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createElasticsearchDomain' smart constructor.
data CreateElasticsearchDomain = CreateElasticsearchDomain'
    { _cedEBSOptions                 :: !(Maybe EBSOptions)
    , _cedAccessPolicies             :: !(Maybe Text)
    , _cedElasticsearchClusterConfig :: !(Maybe ElasticsearchClusterConfig)
    , _cedSnapshotOptions            :: !(Maybe SnapshotOptions)
    , _cedAdvancedOptions            :: !(Maybe (Map Text Text))
    , _cedDomainName                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateElasticsearchDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cedEBSOptions'
--
-- * 'cedAccessPolicies'
--
-- * 'cedElasticsearchClusterConfig'
--
-- * 'cedSnapshotOptions'
--
-- * 'cedAdvancedOptions'
--
-- * 'cedDomainName'
createElasticsearchDomain
    :: Text -- ^ 'cedDomainName'
    -> CreateElasticsearchDomain
createElasticsearchDomain pDomainName_ =
    CreateElasticsearchDomain'
    { _cedEBSOptions = Nothing
    , _cedAccessPolicies = Nothing
    , _cedElasticsearchClusterConfig = Nothing
    , _cedSnapshotOptions = Nothing
    , _cedAdvancedOptions = Nothing
    , _cedDomainName = pDomainName_
    }

-- | Options to enable, disable and specify the type and size of EBS storage volumes.
cedEBSOptions :: Lens' CreateElasticsearchDomain (Maybe EBSOptions)
cedEBSOptions = lens _cedEBSOptions (\ s a -> s{_cedEBSOptions = a});

-- | IAM access policy as a JSON-formatted string.
cedAccessPolicies :: Lens' CreateElasticsearchDomain (Maybe Text)
cedAccessPolicies = lens _cedAccessPolicies (\ s a -> s{_cedAccessPolicies = a});

-- | Configuration options for an Elasticsearch domain. Specifies the instance type and number of instances in the domain cluster.
cedElasticsearchClusterConfig :: Lens' CreateElasticsearchDomain (Maybe ElasticsearchClusterConfig)
cedElasticsearchClusterConfig = lens _cedElasticsearchClusterConfig (\ s a -> s{_cedElasticsearchClusterConfig = a});

-- | Option to set time, in UTC format, of the daily automated snapshot. Default value is 0 hours.
cedSnapshotOptions :: Lens' CreateElasticsearchDomain (Maybe SnapshotOptions)
cedSnapshotOptions = lens _cedSnapshotOptions (\ s a -> s{_cedSnapshotOptions = a});

-- | Option to allow references to indices in an HTTP request body. Must be 'false' when configuring access to individual sub-resources. By default, the value is 'true'. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
cedAdvancedOptions :: Lens' CreateElasticsearchDomain (HashMap Text Text)
cedAdvancedOptions = lens _cedAdvancedOptions (\ s a -> s{_cedAdvancedOptions = a}) . _Default . _Map;

-- | The name of the Elasticsearch domain that you are creating. Domain names are unique across the domains owned by an account within an AWS region. Domain names must start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
cedDomainName :: Lens' CreateElasticsearchDomain Text
cedDomainName = lens _cedDomainName (\ s a -> s{_cedDomainName = a});

instance AWSRequest CreateElasticsearchDomain where
        type Rs CreateElasticsearchDomain =
             CreateElasticsearchDomainResponse
        request = postJSON elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 CreateElasticsearchDomainResponse' <$>
                   (x .?> "DomainStatus") <*> (pure (fromEnum s)))

instance Hashable CreateElasticsearchDomain

instance NFData CreateElasticsearchDomain

instance ToHeaders CreateElasticsearchDomain where
        toHeaders = const mempty

instance ToJSON CreateElasticsearchDomain where
        toJSON CreateElasticsearchDomain'{..}
          = object
              (catMaybes
                 [("EBSOptions" .=) <$> _cedEBSOptions,
                  ("AccessPolicies" .=) <$> _cedAccessPolicies,
                  ("ElasticsearchClusterConfig" .=) <$>
                    _cedElasticsearchClusterConfig,
                  ("SnapshotOptions" .=) <$> _cedSnapshotOptions,
                  ("AdvancedOptions" .=) <$> _cedAdvancedOptions,
                  Just ("DomainName" .= _cedDomainName)])

instance ToPath CreateElasticsearchDomain where
        toPath = const "/2015-01-01/es/domain"

instance ToQuery CreateElasticsearchDomain where
        toQuery = const mempty

-- | The result of a 'CreateElasticsearchDomain' operation. Contains the status of the newly created Elasticsearch domain.
--
-- /See:/ 'createElasticsearchDomainResponse' smart constructor.
data CreateElasticsearchDomainResponse = CreateElasticsearchDomainResponse'
    { _cedrsDomainStatus   :: !(Maybe ElasticsearchDomainStatus)
    , _cedrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateElasticsearchDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cedrsDomainStatus'
--
-- * 'cedrsResponseStatus'
createElasticsearchDomainResponse
    :: Int -- ^ 'cedrsResponseStatus'
    -> CreateElasticsearchDomainResponse
createElasticsearchDomainResponse pResponseStatus_ =
    CreateElasticsearchDomainResponse'
    { _cedrsDomainStatus = Nothing
    , _cedrsResponseStatus = pResponseStatus_
    }

-- | The status of the newly created Elasticsearch domain.
cedrsDomainStatus :: Lens' CreateElasticsearchDomainResponse (Maybe ElasticsearchDomainStatus)
cedrsDomainStatus = lens _cedrsDomainStatus (\ s a -> s{_cedrsDomainStatus = a});

-- | The response status code.
cedrsResponseStatus :: Lens' CreateElasticsearchDomainResponse Int
cedrsResponseStatus = lens _cedrsResponseStatus (\ s a -> s{_cedrsResponseStatus = a});

instance NFData CreateElasticsearchDomainResponse
