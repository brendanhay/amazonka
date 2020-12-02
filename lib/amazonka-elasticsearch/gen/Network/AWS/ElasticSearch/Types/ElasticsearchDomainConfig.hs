{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig where

import Network.AWS.ElasticSearch.Types.AccessPoliciesStatus
import Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
import Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
import Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
import Network.AWS.ElasticSearch.Types.EBSOptionsStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus
import Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
import Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
import Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration of an Elasticsearch domain.
--
--
--
-- /See:/ 'elasticsearchDomainConfig' smart constructor.
data ElasticsearchDomainConfig = ElasticsearchDomainConfig'
  { _edcEBSOptions ::
      !(Maybe EBSOptionsStatus),
    _edcNodeToNodeEncryptionOptions ::
      !( Maybe
           NodeToNodeEncryptionOptionsStatus
       ),
    _edcAccessPolicies ::
      !(Maybe AccessPoliciesStatus),
    _edcLogPublishingOptions ::
      !(Maybe LogPublishingOptionsStatus),
    _edcAdvancedSecurityOptions ::
      !(Maybe AdvancedSecurityOptionsStatus),
    _edcElasticsearchClusterConfig ::
      !( Maybe
           ElasticsearchClusterConfigStatus
       ),
    _edcSnapshotOptions ::
      !(Maybe SnapshotOptionsStatus),
    _edcCognitoOptions ::
      !(Maybe CognitoOptionsStatus),
    _edcEncryptionAtRestOptions ::
      !(Maybe EncryptionAtRestOptionsStatus),
    _edcVPCOptions ::
      !(Maybe VPCDerivedInfoStatus),
    _edcDomainEndpointOptions ::
      !(Maybe DomainEndpointOptionsStatus),
    _edcAdvancedOptions ::
      !(Maybe AdvancedOptionsStatus),
    _edcElasticsearchVersion ::
      !(Maybe ElasticsearchVersionStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticsearchDomainConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edcEBSOptions' - Specifies the @EBSOptions@ for the Elasticsearch domain.
--
-- * 'edcNodeToNodeEncryptionOptions' - Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch domain.
--
-- * 'edcAccessPolicies' - IAM access policy as a JSON-formatted string.
--
-- * 'edcLogPublishingOptions' - Log publishing options for the given domain.
--
-- * 'edcAdvancedSecurityOptions' - Specifies @AdvancedSecurityOptions@ for the domain.
--
-- * 'edcElasticsearchClusterConfig' - Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
--
-- * 'edcSnapshotOptions' - Specifies the @SnapshotOptions@ for the Elasticsearch domain.
--
-- * 'edcCognitoOptions' - The @CognitoOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- * 'edcEncryptionAtRestOptions' - Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
--
-- * 'edcVPCOptions' - The @VPCOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
-- * 'edcDomainEndpointOptions' - Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
--
-- * 'edcAdvancedOptions' - Specifies the @AdvancedOptions@ for the domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options> for more information.
--
-- * 'edcElasticsearchVersion' - String of format X.Y to specify version for the Elasticsearch domain.
elasticsearchDomainConfig ::
  ElasticsearchDomainConfig
elasticsearchDomainConfig =
  ElasticsearchDomainConfig'
    { _edcEBSOptions = Nothing,
      _edcNodeToNodeEncryptionOptions = Nothing,
      _edcAccessPolicies = Nothing,
      _edcLogPublishingOptions = Nothing,
      _edcAdvancedSecurityOptions = Nothing,
      _edcElasticsearchClusterConfig = Nothing,
      _edcSnapshotOptions = Nothing,
      _edcCognitoOptions = Nothing,
      _edcEncryptionAtRestOptions = Nothing,
      _edcVPCOptions = Nothing,
      _edcDomainEndpointOptions = Nothing,
      _edcAdvancedOptions = Nothing,
      _edcElasticsearchVersion = Nothing
    }

-- | Specifies the @EBSOptions@ for the Elasticsearch domain.
edcEBSOptions :: Lens' ElasticsearchDomainConfig (Maybe EBSOptionsStatus)
edcEBSOptions = lens _edcEBSOptions (\s a -> s {_edcEBSOptions = a})

-- | Specifies the @NodeToNodeEncryptionOptions@ for the Elasticsearch domain.
edcNodeToNodeEncryptionOptions :: Lens' ElasticsearchDomainConfig (Maybe NodeToNodeEncryptionOptionsStatus)
edcNodeToNodeEncryptionOptions = lens _edcNodeToNodeEncryptionOptions (\s a -> s {_edcNodeToNodeEncryptionOptions = a})

-- | IAM access policy as a JSON-formatted string.
edcAccessPolicies :: Lens' ElasticsearchDomainConfig (Maybe AccessPoliciesStatus)
edcAccessPolicies = lens _edcAccessPolicies (\s a -> s {_edcAccessPolicies = a})

-- | Log publishing options for the given domain.
edcLogPublishingOptions :: Lens' ElasticsearchDomainConfig (Maybe LogPublishingOptionsStatus)
edcLogPublishingOptions = lens _edcLogPublishingOptions (\s a -> s {_edcLogPublishingOptions = a})

-- | Specifies @AdvancedSecurityOptions@ for the domain.
edcAdvancedSecurityOptions :: Lens' ElasticsearchDomainConfig (Maybe AdvancedSecurityOptionsStatus)
edcAdvancedSecurityOptions = lens _edcAdvancedSecurityOptions (\s a -> s {_edcAdvancedSecurityOptions = a})

-- | Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
edcElasticsearchClusterConfig :: Lens' ElasticsearchDomainConfig (Maybe ElasticsearchClusterConfigStatus)
edcElasticsearchClusterConfig = lens _edcElasticsearchClusterConfig (\s a -> s {_edcElasticsearchClusterConfig = a})

-- | Specifies the @SnapshotOptions@ for the Elasticsearch domain.
edcSnapshotOptions :: Lens' ElasticsearchDomainConfig (Maybe SnapshotOptionsStatus)
edcSnapshotOptions = lens _edcSnapshotOptions (\s a -> s {_edcSnapshotOptions = a})

-- | The @CognitoOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
edcCognitoOptions :: Lens' ElasticsearchDomainConfig (Maybe CognitoOptionsStatus)
edcCognitoOptions = lens _edcCognitoOptions (\s a -> s {_edcCognitoOptions = a})

-- | Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
edcEncryptionAtRestOptions :: Lens' ElasticsearchDomainConfig (Maybe EncryptionAtRestOptionsStatus)
edcEncryptionAtRestOptions = lens _edcEncryptionAtRestOptions (\s a -> s {_edcEncryptionAtRestOptions = a})

-- | The @VPCOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
edcVPCOptions :: Lens' ElasticsearchDomainConfig (Maybe VPCDerivedInfoStatus)
edcVPCOptions = lens _edcVPCOptions (\s a -> s {_edcVPCOptions = a})

-- | Specifies the @DomainEndpointOptions@ for the Elasticsearch domain.
edcDomainEndpointOptions :: Lens' ElasticsearchDomainConfig (Maybe DomainEndpointOptionsStatus)
edcDomainEndpointOptions = lens _edcDomainEndpointOptions (\s a -> s {_edcDomainEndpointOptions = a})

-- | Specifies the @AdvancedOptions@ for the domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options> for more information.
edcAdvancedOptions :: Lens' ElasticsearchDomainConfig (Maybe AdvancedOptionsStatus)
edcAdvancedOptions = lens _edcAdvancedOptions (\s a -> s {_edcAdvancedOptions = a})

-- | String of format X.Y to specify version for the Elasticsearch domain.
edcElasticsearchVersion :: Lens' ElasticsearchDomainConfig (Maybe ElasticsearchVersionStatus)
edcElasticsearchVersion = lens _edcElasticsearchVersion (\s a -> s {_edcElasticsearchVersion = a})

instance FromJSON ElasticsearchDomainConfig where
  parseJSON =
    withObject
      "ElasticsearchDomainConfig"
      ( \x ->
          ElasticsearchDomainConfig'
            <$> (x .:? "EBSOptions")
            <*> (x .:? "NodeToNodeEncryptionOptions")
            <*> (x .:? "AccessPolicies")
            <*> (x .:? "LogPublishingOptions")
            <*> (x .:? "AdvancedSecurityOptions")
            <*> (x .:? "ElasticsearchClusterConfig")
            <*> (x .:? "SnapshotOptions")
            <*> (x .:? "CognitoOptions")
            <*> (x .:? "EncryptionAtRestOptions")
            <*> (x .:? "VPCOptions")
            <*> (x .:? "DomainEndpointOptions")
            <*> (x .:? "AdvancedOptions")
            <*> (x .:? "ElasticsearchVersion")
      )

instance Hashable ElasticsearchDomainConfig

instance NFData ElasticsearchDomainConfig
