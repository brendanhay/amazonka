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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Elasticsearch domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains> in the /Amazon Elasticsearch Service Developer Guide/ .
--
--
module Network.AWS.ElasticSearch.CreateElasticsearchDomain
    (
    -- * Creating a Request
      createElasticsearchDomain
    , CreateElasticsearchDomain
    -- * Request Lenses
    , cedEBSOptions
    , cedAccessPolicies
    , cedLogPublishingOptions
    , cedElasticsearchClusterConfig
    , cedSnapshotOptions
    , cedCognitoOptions
    , cedEncryptionAtRestOptions
    , cedVPCOptions
    , cedAdvancedOptions
    , cedElasticsearchVersion
    , cedDomainName

    -- * Destructuring the Response
    , createElasticsearchDomainResponse
    , CreateElasticsearchDomainResponse
    -- * Response Lenses
    , cedrsDomainStatus
    , cedrsResponseStatus
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createElasticsearchDomain' smart constructor.
data CreateElasticsearchDomain = CreateElasticsearchDomain'
  { _cedEBSOptions                 :: !(Maybe EBSOptions)
  , _cedAccessPolicies             :: !(Maybe Text)
  , _cedLogPublishingOptions       :: !(Maybe (Map LogType LogPublishingOption))
  , _cedElasticsearchClusterConfig :: !(Maybe ElasticsearchClusterConfig)
  , _cedSnapshotOptions            :: !(Maybe SnapshotOptions)
  , _cedCognitoOptions             :: !(Maybe CognitoOptions)
  , _cedEncryptionAtRestOptions    :: !(Maybe EncryptionAtRestOptions)
  , _cedVPCOptions                 :: !(Maybe VPCOptions)
  , _cedAdvancedOptions            :: !(Maybe (Map Text Text))
  , _cedElasticsearchVersion       :: !(Maybe Text)
  , _cedDomainName                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateElasticsearchDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cedEBSOptions' - Options to enable, disable and specify the type and size of EBS storage volumes.
--
-- * 'cedAccessPolicies' - IAM access policy as a JSON-formatted string.
--
-- * 'cedLogPublishingOptions' - Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
--
-- * 'cedElasticsearchClusterConfig' - Configuration options for an Elasticsearch domain. Specifies the instance type and number of instances in the domain cluster.
--
-- * 'cedSnapshotOptions' - Option to set time, in UTC format, of the daily automated snapshot. Default value is 0 hours.
--
-- * 'cedCognitoOptions' - Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- * 'cedEncryptionAtRestOptions' - Specifies the Encryption At Rest Options.
--
-- * 'cedVPCOptions' - Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
--
-- * 'cedAdvancedOptions' - Option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
--
-- * 'cedElasticsearchVersion' - String of format X.Y to specify version for the Elasticsearch domain eg. "1.5" or "2.3". For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains> in the /Amazon Elasticsearch Service Developer Guide/ .
--
-- * 'cedDomainName' - The name of the Elasticsearch domain that you are creating. Domain names are unique across the domains owned by an account within an AWS region. Domain names must start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
createElasticsearchDomain
    :: Text -- ^ 'cedDomainName'
    -> CreateElasticsearchDomain
createElasticsearchDomain pDomainName_ =
  CreateElasticsearchDomain'
    { _cedEBSOptions = Nothing
    , _cedAccessPolicies = Nothing
    , _cedLogPublishingOptions = Nothing
    , _cedElasticsearchClusterConfig = Nothing
    , _cedSnapshotOptions = Nothing
    , _cedCognitoOptions = Nothing
    , _cedEncryptionAtRestOptions = Nothing
    , _cedVPCOptions = Nothing
    , _cedAdvancedOptions = Nothing
    , _cedElasticsearchVersion = Nothing
    , _cedDomainName = pDomainName_
    }


-- | Options to enable, disable and specify the type and size of EBS storage volumes.
cedEBSOptions :: Lens' CreateElasticsearchDomain (Maybe EBSOptions)
cedEBSOptions = lens _cedEBSOptions (\ s a -> s{_cedEBSOptions = a})

-- | IAM access policy as a JSON-formatted string.
cedAccessPolicies :: Lens' CreateElasticsearchDomain (Maybe Text)
cedAccessPolicies = lens _cedAccessPolicies (\ s a -> s{_cedAccessPolicies = a})

-- | Map of @LogType@ and @LogPublishingOption@ , each containing options to publish a given type of Elasticsearch log.
cedLogPublishingOptions :: Lens' CreateElasticsearchDomain (HashMap LogType LogPublishingOption)
cedLogPublishingOptions = lens _cedLogPublishingOptions (\ s a -> s{_cedLogPublishingOptions = a}) . _Default . _Map

-- | Configuration options for an Elasticsearch domain. Specifies the instance type and number of instances in the domain cluster.
cedElasticsearchClusterConfig :: Lens' CreateElasticsearchDomain (Maybe ElasticsearchClusterConfig)
cedElasticsearchClusterConfig = lens _cedElasticsearchClusterConfig (\ s a -> s{_cedElasticsearchClusterConfig = a})

-- | Option to set time, in UTC format, of the daily automated snapshot. Default value is 0 hours.
cedSnapshotOptions :: Lens' CreateElasticsearchDomain (Maybe SnapshotOptions)
cedSnapshotOptions = lens _cedSnapshotOptions (\ s a -> s{_cedSnapshotOptions = a})

-- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
cedCognitoOptions :: Lens' CreateElasticsearchDomain (Maybe CognitoOptions)
cedCognitoOptions = lens _cedCognitoOptions (\ s a -> s{_cedCognitoOptions = a})

-- | Specifies the Encryption At Rest Options.
cedEncryptionAtRestOptions :: Lens' CreateElasticsearchDomain (Maybe EncryptionAtRestOptions)
cedEncryptionAtRestOptions = lens _cedEncryptionAtRestOptions (\ s a -> s{_cedEncryptionAtRestOptions = a})

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html#es-creating-vpc Creating a VPC> in /VPC Endpoints for Amazon Elasticsearch Service Domains/
cedVPCOptions :: Lens' CreateElasticsearchDomain (Maybe VPCOptions)
cedVPCOptions = lens _cedVPCOptions (\ s a -> s{_cedVPCOptions = a})

-- | Option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
cedAdvancedOptions :: Lens' CreateElasticsearchDomain (HashMap Text Text)
cedAdvancedOptions = lens _cedAdvancedOptions (\ s a -> s{_cedAdvancedOptions = a}) . _Default . _Map

-- | String of format X.Y to specify version for the Elasticsearch domain eg. "1.5" or "2.3". For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomains Creating Elasticsearch Domains> in the /Amazon Elasticsearch Service Developer Guide/ .
cedElasticsearchVersion :: Lens' CreateElasticsearchDomain (Maybe Text)
cedElasticsearchVersion = lens _cedElasticsearchVersion (\ s a -> s{_cedElasticsearchVersion = a})

-- | The name of the Elasticsearch domain that you are creating. Domain names are unique across the domains owned by an account within an AWS region. Domain names must start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
cedDomainName :: Lens' CreateElasticsearchDomain Text
cedDomainName = lens _cedDomainName (\ s a -> s{_cedDomainName = a})

instance AWSRequest CreateElasticsearchDomain where
        type Rs CreateElasticsearchDomain =
             CreateElasticsearchDomainResponse
        request = postJSON elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 CreateElasticsearchDomainResponse' <$>
                   (x .?> "DomainStatus") <*> (pure (fromEnum s)))

instance Hashable CreateElasticsearchDomain where

instance NFData CreateElasticsearchDomain where

instance ToHeaders CreateElasticsearchDomain where
        toHeaders = const mempty

instance ToJSON CreateElasticsearchDomain where
        toJSON CreateElasticsearchDomain'{..}
          = object
              (catMaybes
                 [("EBSOptions" .=) <$> _cedEBSOptions,
                  ("AccessPolicies" .=) <$> _cedAccessPolicies,
                  ("LogPublishingOptions" .=) <$>
                    _cedLogPublishingOptions,
                  ("ElasticsearchClusterConfig" .=) <$>
                    _cedElasticsearchClusterConfig,
                  ("SnapshotOptions" .=) <$> _cedSnapshotOptions,
                  ("CognitoOptions" .=) <$> _cedCognitoOptions,
                  ("EncryptionAtRestOptions" .=) <$>
                    _cedEncryptionAtRestOptions,
                  ("VPCOptions" .=) <$> _cedVPCOptions,
                  ("AdvancedOptions" .=) <$> _cedAdvancedOptions,
                  ("ElasticsearchVersion" .=) <$>
                    _cedElasticsearchVersion,
                  Just ("DomainName" .= _cedDomainName)])

instance ToPath CreateElasticsearchDomain where
        toPath = const "/2015-01-01/es/domain"

instance ToQuery CreateElasticsearchDomain where
        toQuery = const mempty

-- | The result of a @CreateElasticsearchDomain@ operation. Contains the status of the newly created Elasticsearch domain.
--
--
--
-- /See:/ 'createElasticsearchDomainResponse' smart constructor.
data CreateElasticsearchDomainResponse = CreateElasticsearchDomainResponse'
  { _cedrsDomainStatus   :: !(Maybe ElasticsearchDomainStatus)
  , _cedrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateElasticsearchDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cedrsDomainStatus' - The status of the newly created Elasticsearch domain.
--
-- * 'cedrsResponseStatus' - -- | The response status code.
createElasticsearchDomainResponse
    :: Int -- ^ 'cedrsResponseStatus'
    -> CreateElasticsearchDomainResponse
createElasticsearchDomainResponse pResponseStatus_ =
  CreateElasticsearchDomainResponse'
    {_cedrsDomainStatus = Nothing, _cedrsResponseStatus = pResponseStatus_}


-- | The status of the newly created Elasticsearch domain.
cedrsDomainStatus :: Lens' CreateElasticsearchDomainResponse (Maybe ElasticsearchDomainStatus)
cedrsDomainStatus = lens _cedrsDomainStatus (\ s a -> s{_cedrsDomainStatus = a})

-- | -- | The response status code.
cedrsResponseStatus :: Lens' CreateElasticsearchDomainResponse Int
cedrsResponseStatus = lens _cedrsResponseStatus (\ s a -> s{_cedrsResponseStatus = a})

instance NFData CreateElasticsearchDomainResponse
         where
