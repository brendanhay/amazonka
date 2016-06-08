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
-- Module      : Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the cluster configuration of the specified Elasticsearch domain, setting as setting the instance type and the number of instances.
module Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig
    (
    -- * Creating a Request
      updateElasticsearchDomainConfig
    , UpdateElasticsearchDomainConfig
    -- * Request Lenses
    , uedcEBSOptions
    , uedcAccessPolicies
    , uedcElasticsearchClusterConfig
    , uedcSnapshotOptions
    , uedcAdvancedOptions
    , uedcDomainName

    -- * Destructuring the Response
    , updateElasticsearchDomainConfigResponse
    , UpdateElasticsearchDomainConfigResponse
    -- * Response Lenses
    , uedcrsResponseStatus
    , uedcrsDomainConfig
    ) where

import           Network.AWS.ElasticSearch.Types
import           Network.AWS.ElasticSearch.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the 'UpdateElasticsearchDomain' operation. Specifies the type and number of instances in the domain cluster.
--
-- /See:/ 'updateElasticsearchDomainConfig' smart constructor.
data UpdateElasticsearchDomainConfig = UpdateElasticsearchDomainConfig'
    { _uedcEBSOptions                 :: !(Maybe EBSOptions)
    , _uedcAccessPolicies             :: !(Maybe Text)
    , _uedcElasticsearchClusterConfig :: !(Maybe ElasticsearchClusterConfig)
    , _uedcSnapshotOptions            :: !(Maybe SnapshotOptions)
    , _uedcAdvancedOptions            :: !(Maybe (Map Text Text))
    , _uedcDomainName                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateElasticsearchDomainConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uedcEBSOptions'
--
-- * 'uedcAccessPolicies'
--
-- * 'uedcElasticsearchClusterConfig'
--
-- * 'uedcSnapshotOptions'
--
-- * 'uedcAdvancedOptions'
--
-- * 'uedcDomainName'
updateElasticsearchDomainConfig
    :: Text -- ^ 'uedcDomainName'
    -> UpdateElasticsearchDomainConfig
updateElasticsearchDomainConfig pDomainName_ =
    UpdateElasticsearchDomainConfig'
    { _uedcEBSOptions = Nothing
    , _uedcAccessPolicies = Nothing
    , _uedcElasticsearchClusterConfig = Nothing
    , _uedcSnapshotOptions = Nothing
    , _uedcAdvancedOptions = Nothing
    , _uedcDomainName = pDomainName_
    }

-- | Specify the type and size of the EBS volume that you want to use.
uedcEBSOptions :: Lens' UpdateElasticsearchDomainConfig (Maybe EBSOptions)
uedcEBSOptions = lens _uedcEBSOptions (\ s a -> s{_uedcEBSOptions = a});

-- | IAM access policy as a JSON-formatted string.
uedcAccessPolicies :: Lens' UpdateElasticsearchDomainConfig (Maybe Text)
uedcAccessPolicies = lens _uedcAccessPolicies (\ s a -> s{_uedcAccessPolicies = a});

-- | The type and number of instances to instantiate for the domain cluster.
uedcElasticsearchClusterConfig :: Lens' UpdateElasticsearchDomainConfig (Maybe ElasticsearchClusterConfig)
uedcElasticsearchClusterConfig = lens _uedcElasticsearchClusterConfig (\ s a -> s{_uedcElasticsearchClusterConfig = a});

-- | Option to set the time, in UTC format, for the daily automated snapshot. Default value is '0' hours.
uedcSnapshotOptions :: Lens' UpdateElasticsearchDomainConfig (Maybe SnapshotOptions)
uedcSnapshotOptions = lens _uedcSnapshotOptions (\ s a -> s{_uedcSnapshotOptions = a});

-- | Modifies the advanced option to allow references to indices in an HTTP request body. Must be 'false' when configuring access to individual sub-resources. By default, the value is 'true'. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
uedcAdvancedOptions :: Lens' UpdateElasticsearchDomainConfig (HashMap Text Text)
uedcAdvancedOptions = lens _uedcAdvancedOptions (\ s a -> s{_uedcAdvancedOptions = a}) . _Default . _Map;

-- | The name of the Elasticsearch domain that you are updating.
uedcDomainName :: Lens' UpdateElasticsearchDomainConfig Text
uedcDomainName = lens _uedcDomainName (\ s a -> s{_uedcDomainName = a});

instance AWSRequest UpdateElasticsearchDomainConfig
         where
        type Rs UpdateElasticsearchDomainConfig =
             UpdateElasticsearchDomainConfigResponse
        request = postJSON elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 UpdateElasticsearchDomainConfigResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "DomainConfig"))

instance Hashable UpdateElasticsearchDomainConfig

instance NFData UpdateElasticsearchDomainConfig

instance ToHeaders UpdateElasticsearchDomainConfig
         where
        toHeaders = const mempty

instance ToJSON UpdateElasticsearchDomainConfig where
        toJSON UpdateElasticsearchDomainConfig'{..}
          = object
              (catMaybes
                 [("EBSOptions" .=) <$> _uedcEBSOptions,
                  ("AccessPolicies" .=) <$> _uedcAccessPolicies,
                  ("ElasticsearchClusterConfig" .=) <$>
                    _uedcElasticsearchClusterConfig,
                  ("SnapshotOptions" .=) <$> _uedcSnapshotOptions,
                  ("AdvancedOptions" .=) <$> _uedcAdvancedOptions])

instance ToPath UpdateElasticsearchDomainConfig where
        toPath UpdateElasticsearchDomainConfig'{..}
          = mconcat
              ["/2015-01-01/es/domain/", toBS _uedcDomainName,
               "/config"]

instance ToQuery UpdateElasticsearchDomainConfig
         where
        toQuery = const mempty

-- | The result of an 'UpdateElasticsearchDomain' request. Contains the status of the Elasticsearch domain being updated.
--
-- /See:/ 'updateElasticsearchDomainConfigResponse' smart constructor.
data UpdateElasticsearchDomainConfigResponse = UpdateElasticsearchDomainConfigResponse'
    { _uedcrsResponseStatus :: !Int
    , _uedcrsDomainConfig   :: !ElasticsearchDomainConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateElasticsearchDomainConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uedcrsResponseStatus'
--
-- * 'uedcrsDomainConfig'
updateElasticsearchDomainConfigResponse
    :: Int -- ^ 'uedcrsResponseStatus'
    -> ElasticsearchDomainConfig -- ^ 'uedcrsDomainConfig'
    -> UpdateElasticsearchDomainConfigResponse
updateElasticsearchDomainConfigResponse pResponseStatus_ pDomainConfig_ =
    UpdateElasticsearchDomainConfigResponse'
    { _uedcrsResponseStatus = pResponseStatus_
    , _uedcrsDomainConfig = pDomainConfig_
    }

-- | The response status code.
uedcrsResponseStatus :: Lens' UpdateElasticsearchDomainConfigResponse Int
uedcrsResponseStatus = lens _uedcrsResponseStatus (\ s a -> s{_uedcrsResponseStatus = a});

-- | The status of the updated Elasticsearch domain.
uedcrsDomainConfig :: Lens' UpdateElasticsearchDomainConfigResponse ElasticsearchDomainConfig
uedcrsDomainConfig = lens _uedcrsDomainConfig (\ s a -> s{_uedcrsDomainConfig = a});

instance NFData
         UpdateElasticsearchDomainConfigResponse
