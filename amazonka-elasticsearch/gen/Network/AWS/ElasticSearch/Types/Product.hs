{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.Product where

import Network.AWS.ElasticSearch.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configured access rules for the domain's document and search endpoints, and the current status of those rules.
--
--
--
-- /See:/ 'accessPoliciesStatus' smart constructor.
data AccessPoliciesStatus = AccessPoliciesStatus'
  { _apsOptions :: !Text
  , _apsStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccessPoliciesStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apsOptions' - The access policy configured for the Elasticsearch domain. Access policies may be resource-based, IP-based, or IAM-based. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-access-policies Configuring Access Policies> for more information.
--
-- * 'apsStatus' - The status of the access policy for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
accessPoliciesStatus
    :: Text -- ^ 'apsOptions'
    -> OptionStatus -- ^ 'apsStatus'
    -> AccessPoliciesStatus
accessPoliciesStatus pOptions_ pStatus_ =
  AccessPoliciesStatus' {_apsOptions = pOptions_, _apsStatus = pStatus_}


-- | The access policy configured for the Elasticsearch domain. Access policies may be resource-based, IP-based, or IAM-based. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-access-policies Configuring Access Policies> for more information.
apsOptions :: Lens' AccessPoliciesStatus Text
apsOptions = lens _apsOptions (\ s a -> s{_apsOptions = a})

-- | The status of the access policy for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
apsStatus :: Lens' AccessPoliciesStatus OptionStatus
apsStatus = lens _apsStatus (\ s a -> s{_apsStatus = a})

instance FromJSON AccessPoliciesStatus where
        parseJSON
          = withObject "AccessPoliciesStatus"
              (\ x ->
                 AccessPoliciesStatus' <$>
                   (x .: "Options") <*> (x .: "Status"))

instance Hashable AccessPoliciesStatus where

instance NFData AccessPoliciesStatus where

-- | List of limits that are specific to a given InstanceType and for each of it's @'InstanceRole' @ .
--
--
--
-- /See:/ 'additionalLimit' smart constructor.
data AdditionalLimit = AdditionalLimit'
  { _alLimitName   :: !(Maybe Text)
  , _alLimitValues :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdditionalLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alLimitName' - Name of Additional Limit is specific to a given InstanceType and for each of it's @'InstanceRole' @ etc.  Attributes and their details:      * MaximumNumberOfDataNodesSupportedThis attribute will be present in Master node only to specify how much data nodes upto which given @'ESPartitionInstanceType' @ can support as master node.     * MaximumNumberOfDataNodesWithoutMasterNodeThis attribute will be present in Data node only to specify how much data nodes of given @'ESPartitionInstanceType' @ upto which you don't need any master nodes to govern them.
--
-- * 'alLimitValues' - Value for given @'AdditionalLimit$LimitName' @ .
additionalLimit
    :: AdditionalLimit
additionalLimit =
  AdditionalLimit' {_alLimitName = Nothing, _alLimitValues = Nothing}


-- | Name of Additional Limit is specific to a given InstanceType and for each of it's @'InstanceRole' @ etc.  Attributes and their details:      * MaximumNumberOfDataNodesSupportedThis attribute will be present in Master node only to specify how much data nodes upto which given @'ESPartitionInstanceType' @ can support as master node.     * MaximumNumberOfDataNodesWithoutMasterNodeThis attribute will be present in Data node only to specify how much data nodes of given @'ESPartitionInstanceType' @ upto which you don't need any master nodes to govern them.
alLimitName :: Lens' AdditionalLimit (Maybe Text)
alLimitName = lens _alLimitName (\ s a -> s{_alLimitName = a})

-- | Value for given @'AdditionalLimit$LimitName' @ .
alLimitValues :: Lens' AdditionalLimit [Text]
alLimitValues = lens _alLimitValues (\ s a -> s{_alLimitValues = a}) . _Default . _Coerce

instance FromJSON AdditionalLimit where
        parseJSON
          = withObject "AdditionalLimit"
              (\ x ->
                 AdditionalLimit' <$>
                   (x .:? "LimitName") <*>
                     (x .:? "LimitValues" .!= mempty))

instance Hashable AdditionalLimit where

instance NFData AdditionalLimit where

-- | Status of the advanced options for the specified Elasticsearch domain. Currently, the following advanced options are available:
--
--
--     * Option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.    * Option to specify the percentage of heap space that is allocated to field data. By default, this setting is unbounded.
--
-- For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options> .
--
--
-- /See:/ 'advancedOptionsStatus' smart constructor.
data AdvancedOptionsStatus = AdvancedOptionsStatus'
  { _aosOptions :: !(Map Text Text)
  , _aosStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdvancedOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aosOptions' - Specifies the status of advanced options for the specified Elasticsearch domain.
--
-- * 'aosStatus' - Specifies the status of @OptionStatus@ for advanced options for the specified Elasticsearch domain.
advancedOptionsStatus
    :: OptionStatus -- ^ 'aosStatus'
    -> AdvancedOptionsStatus
advancedOptionsStatus pStatus_ =
  AdvancedOptionsStatus' {_aosOptions = mempty, _aosStatus = pStatus_}


-- | Specifies the status of advanced options for the specified Elasticsearch domain.
aosOptions :: Lens' AdvancedOptionsStatus (HashMap Text Text)
aosOptions = lens _aosOptions (\ s a -> s{_aosOptions = a}) . _Map

-- | Specifies the status of @OptionStatus@ for advanced options for the specified Elasticsearch domain.
aosStatus :: Lens' AdvancedOptionsStatus OptionStatus
aosStatus = lens _aosStatus (\ s a -> s{_aosStatus = a})

instance FromJSON AdvancedOptionsStatus where
        parseJSON
          = withObject "AdvancedOptionsStatus"
              (\ x ->
                 AdvancedOptionsStatus' <$>
                   (x .:? "Options" .!= mempty) <*> (x .: "Status"))

instance Hashable AdvancedOptionsStatus where

instance NFData AdvancedOptionsStatus where

-- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
--
--
-- /See:/ 'cognitoOptions' smart constructor.
data CognitoOptions = CognitoOptions'
  { _coIdentityPoolId :: !(Maybe Text)
  , _coEnabled        :: !(Maybe Bool)
  , _coUserPoolId     :: !(Maybe Text)
  , _coRoleARN        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CognitoOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coIdentityPoolId' - Specifies the Cognito identity pool ID for Kibana authentication.
--
-- * 'coEnabled' - Specifies the option to enable Cognito for Kibana authentication.
--
-- * 'coUserPoolId' - Specifies the Cognito user pool ID for Kibana authentication.
--
-- * 'coRoleARN' - Specifies the role ARN that provides Elasticsearch permissions for accessing Cognito resources.
cognitoOptions
    :: CognitoOptions
cognitoOptions =
  CognitoOptions'
    { _coIdentityPoolId = Nothing
    , _coEnabled = Nothing
    , _coUserPoolId = Nothing
    , _coRoleARN = Nothing
    }


-- | Specifies the Cognito identity pool ID for Kibana authentication.
coIdentityPoolId :: Lens' CognitoOptions (Maybe Text)
coIdentityPoolId = lens _coIdentityPoolId (\ s a -> s{_coIdentityPoolId = a})

-- | Specifies the option to enable Cognito for Kibana authentication.
coEnabled :: Lens' CognitoOptions (Maybe Bool)
coEnabled = lens _coEnabled (\ s a -> s{_coEnabled = a})

-- | Specifies the Cognito user pool ID for Kibana authentication.
coUserPoolId :: Lens' CognitoOptions (Maybe Text)
coUserPoolId = lens _coUserPoolId (\ s a -> s{_coUserPoolId = a})

-- | Specifies the role ARN that provides Elasticsearch permissions for accessing Cognito resources.
coRoleARN :: Lens' CognitoOptions (Maybe Text)
coRoleARN = lens _coRoleARN (\ s a -> s{_coRoleARN = a})

instance FromJSON CognitoOptions where
        parseJSON
          = withObject "CognitoOptions"
              (\ x ->
                 CognitoOptions' <$>
                   (x .:? "IdentityPoolId") <*> (x .:? "Enabled") <*>
                     (x .:? "UserPoolId")
                     <*> (x .:? "RoleArn"))

instance Hashable CognitoOptions where

instance NFData CognitoOptions where

instance ToJSON CognitoOptions where
        toJSON CognitoOptions'{..}
          = object
              (catMaybes
                 [("IdentityPoolId" .=) <$> _coIdentityPoolId,
                  ("Enabled" .=) <$> _coEnabled,
                  ("UserPoolId" .=) <$> _coUserPoolId,
                  ("RoleArn" .=) <$> _coRoleARN])

-- | Status of the Cognito options for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'cognitoOptionsStatus' smart constructor.
data CognitoOptionsStatus = CognitoOptionsStatus'
  { _cosOptions :: !CognitoOptions
  , _cosStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CognitoOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cosOptions' - Specifies the Cognito options for the specified Elasticsearch domain.
--
-- * 'cosStatus' - Specifies the status of the Cognito options for the specified Elasticsearch domain.
cognitoOptionsStatus
    :: CognitoOptions -- ^ 'cosOptions'
    -> OptionStatus -- ^ 'cosStatus'
    -> CognitoOptionsStatus
cognitoOptionsStatus pOptions_ pStatus_ =
  CognitoOptionsStatus' {_cosOptions = pOptions_, _cosStatus = pStatus_}


-- | Specifies the Cognito options for the specified Elasticsearch domain.
cosOptions :: Lens' CognitoOptionsStatus CognitoOptions
cosOptions = lens _cosOptions (\ s a -> s{_cosOptions = a})

-- | Specifies the status of the Cognito options for the specified Elasticsearch domain.
cosStatus :: Lens' CognitoOptionsStatus OptionStatus
cosStatus = lens _cosStatus (\ s a -> s{_cosStatus = a})

instance FromJSON CognitoOptionsStatus where
        parseJSON
          = withObject "CognitoOptionsStatus"
              (\ x ->
                 CognitoOptionsStatus' <$>
                   (x .: "Options") <*> (x .: "Status"))

instance Hashable CognitoOptionsStatus where

instance NFData CognitoOptionsStatus where

-- | /See:/ 'domainInfo' smart constructor.
newtype DomainInfo = DomainInfo'
  { _diDomainName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diDomainName' - Specifies the @DomainName@ .
domainInfo
    :: DomainInfo
domainInfo = DomainInfo' {_diDomainName = Nothing}


-- | Specifies the @DomainName@ .
diDomainName :: Lens' DomainInfo (Maybe Text)
diDomainName = lens _diDomainName (\ s a -> s{_diDomainName = a})

instance FromJSON DomainInfo where
        parseJSON
          = withObject "DomainInfo"
              (\ x -> DomainInfo' <$> (x .:? "DomainName"))

instance Hashable DomainInfo where

instance NFData DomainInfo where

-- | Options to enable, disable, and specify the properties of EBS storage volumes. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> .
--
--
--
-- /See:/ 'ebsOptions' smart constructor.
data EBSOptions = EBSOptions'
  { _eoVolumeSize :: !(Maybe Int)
  , _eoIOPS       :: !(Maybe Int)
  , _eoVolumeType :: !(Maybe VolumeType)
  , _eoEBSEnabled :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EBSOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eoVolumeSize' - Integer to specify the size of an EBS volume.
--
-- * 'eoIOPS' - Specifies the IOPD for a Provisioned IOPS EBS volume (SSD).
--
-- * 'eoVolumeType' - Specifies the volume type for EBS-based storage.
--
-- * 'eoEBSEnabled' - Specifies whether EBS-based storage is enabled.
ebsOptions
    :: EBSOptions
ebsOptions =
  EBSOptions'
    { _eoVolumeSize = Nothing
    , _eoIOPS = Nothing
    , _eoVolumeType = Nothing
    , _eoEBSEnabled = Nothing
    }


-- | Integer to specify the size of an EBS volume.
eoVolumeSize :: Lens' EBSOptions (Maybe Int)
eoVolumeSize = lens _eoVolumeSize (\ s a -> s{_eoVolumeSize = a})

-- | Specifies the IOPD for a Provisioned IOPS EBS volume (SSD).
eoIOPS :: Lens' EBSOptions (Maybe Int)
eoIOPS = lens _eoIOPS (\ s a -> s{_eoIOPS = a})

-- | Specifies the volume type for EBS-based storage.
eoVolumeType :: Lens' EBSOptions (Maybe VolumeType)
eoVolumeType = lens _eoVolumeType (\ s a -> s{_eoVolumeType = a})

-- | Specifies whether EBS-based storage is enabled.
eoEBSEnabled :: Lens' EBSOptions (Maybe Bool)
eoEBSEnabled = lens _eoEBSEnabled (\ s a -> s{_eoEBSEnabled = a})

instance FromJSON EBSOptions where
        parseJSON
          = withObject "EBSOptions"
              (\ x ->
                 EBSOptions' <$>
                   (x .:? "VolumeSize") <*> (x .:? "Iops") <*>
                     (x .:? "VolumeType")
                     <*> (x .:? "EBSEnabled"))

instance Hashable EBSOptions where

instance NFData EBSOptions where

instance ToJSON EBSOptions where
        toJSON EBSOptions'{..}
          = object
              (catMaybes
                 [("VolumeSize" .=) <$> _eoVolumeSize,
                  ("Iops" .=) <$> _eoIOPS,
                  ("VolumeType" .=) <$> _eoVolumeType,
                  ("EBSEnabled" .=) <$> _eoEBSEnabled])

-- | Status of the EBS options for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'ebsOptionsStatus' smart constructor.
data EBSOptionsStatus = EBSOptionsStatus'
  { _eosOptions :: !EBSOptions
  , _eosStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EBSOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eosOptions' - Specifies the EBS options for the specified Elasticsearch domain.
--
-- * 'eosStatus' - Specifies the status of the EBS options for the specified Elasticsearch domain.
ebsOptionsStatus
    :: EBSOptions -- ^ 'eosOptions'
    -> OptionStatus -- ^ 'eosStatus'
    -> EBSOptionsStatus
ebsOptionsStatus pOptions_ pStatus_ =
  EBSOptionsStatus' {_eosOptions = pOptions_, _eosStatus = pStatus_}


-- | Specifies the EBS options for the specified Elasticsearch domain.
eosOptions :: Lens' EBSOptionsStatus EBSOptions
eosOptions = lens _eosOptions (\ s a -> s{_eosOptions = a})

-- | Specifies the status of the EBS options for the specified Elasticsearch domain.
eosStatus :: Lens' EBSOptionsStatus OptionStatus
eosStatus = lens _eosStatus (\ s a -> s{_eosStatus = a})

instance FromJSON EBSOptionsStatus where
        parseJSON
          = withObject "EBSOptionsStatus"
              (\ x ->
                 EBSOptionsStatus' <$>
                   (x .: "Options") <*> (x .: "Status"))

instance Hashable EBSOptionsStatus where

instance NFData EBSOptionsStatus where

-- | Specifies the configuration for the domain cluster, such as the type and number of instances.
--
--
--
-- /See:/ 'elasticsearchClusterConfig' smart constructor.
data ElasticsearchClusterConfig = ElasticsearchClusterConfig'
  { _eccDedicatedMasterCount   :: !(Maybe Int)
  , _eccDedicatedMasterType    :: !(Maybe ESPartitionInstanceType)
  , _eccDedicatedMasterEnabled :: !(Maybe Bool)
  , _eccInstanceCount          :: !(Maybe Int)
  , _eccZoneAwarenessEnabled   :: !(Maybe Bool)
  , _eccInstanceType           :: !(Maybe ESPartitionInstanceType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticsearchClusterConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eccDedicatedMasterCount' - Total number of dedicated master nodes, active and on standby, for the cluster.
--
-- * 'eccDedicatedMasterType' - The instance type for a dedicated master node.
--
-- * 'eccDedicatedMasterEnabled' - A boolean value to indicate whether a dedicated master node is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-dedicatedmasternodes About Dedicated Master Nodes> for more information.
--
-- * 'eccInstanceCount' - The number of instances in the specified domain cluster.
--
-- * 'eccZoneAwarenessEnabled' - A boolean value to indicate whether zone awareness is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-zoneawareness About Zone Awareness> for more information.
--
-- * 'eccInstanceType' - The instance type for an Elasticsearch cluster.
elasticsearchClusterConfig
    :: ElasticsearchClusterConfig
elasticsearchClusterConfig =
  ElasticsearchClusterConfig'
    { _eccDedicatedMasterCount = Nothing
    , _eccDedicatedMasterType = Nothing
    , _eccDedicatedMasterEnabled = Nothing
    , _eccInstanceCount = Nothing
    , _eccZoneAwarenessEnabled = Nothing
    , _eccInstanceType = Nothing
    }


-- | Total number of dedicated master nodes, active and on standby, for the cluster.
eccDedicatedMasterCount :: Lens' ElasticsearchClusterConfig (Maybe Int)
eccDedicatedMasterCount = lens _eccDedicatedMasterCount (\ s a -> s{_eccDedicatedMasterCount = a})

-- | The instance type for a dedicated master node.
eccDedicatedMasterType :: Lens' ElasticsearchClusterConfig (Maybe ESPartitionInstanceType)
eccDedicatedMasterType = lens _eccDedicatedMasterType (\ s a -> s{_eccDedicatedMasterType = a})

-- | A boolean value to indicate whether a dedicated master node is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-dedicatedmasternodes About Dedicated Master Nodes> for more information.
eccDedicatedMasterEnabled :: Lens' ElasticsearchClusterConfig (Maybe Bool)
eccDedicatedMasterEnabled = lens _eccDedicatedMasterEnabled (\ s a -> s{_eccDedicatedMasterEnabled = a})

-- | The number of instances in the specified domain cluster.
eccInstanceCount :: Lens' ElasticsearchClusterConfig (Maybe Int)
eccInstanceCount = lens _eccInstanceCount (\ s a -> s{_eccInstanceCount = a})

-- | A boolean value to indicate whether zone awareness is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-zoneawareness About Zone Awareness> for more information.
eccZoneAwarenessEnabled :: Lens' ElasticsearchClusterConfig (Maybe Bool)
eccZoneAwarenessEnabled = lens _eccZoneAwarenessEnabled (\ s a -> s{_eccZoneAwarenessEnabled = a})

-- | The instance type for an Elasticsearch cluster.
eccInstanceType :: Lens' ElasticsearchClusterConfig (Maybe ESPartitionInstanceType)
eccInstanceType = lens _eccInstanceType (\ s a -> s{_eccInstanceType = a})

instance FromJSON ElasticsearchClusterConfig where
        parseJSON
          = withObject "ElasticsearchClusterConfig"
              (\ x ->
                 ElasticsearchClusterConfig' <$>
                   (x .:? "DedicatedMasterCount") <*>
                     (x .:? "DedicatedMasterType")
                     <*> (x .:? "DedicatedMasterEnabled")
                     <*> (x .:? "InstanceCount")
                     <*> (x .:? "ZoneAwarenessEnabled")
                     <*> (x .:? "InstanceType"))

instance Hashable ElasticsearchClusterConfig where

instance NFData ElasticsearchClusterConfig where

instance ToJSON ElasticsearchClusterConfig where
        toJSON ElasticsearchClusterConfig'{..}
          = object
              (catMaybes
                 [("DedicatedMasterCount" .=) <$>
                    _eccDedicatedMasterCount,
                  ("DedicatedMasterType" .=) <$>
                    _eccDedicatedMasterType,
                  ("DedicatedMasterEnabled" .=) <$>
                    _eccDedicatedMasterEnabled,
                  ("InstanceCount" .=) <$> _eccInstanceCount,
                  ("ZoneAwarenessEnabled" .=) <$>
                    _eccZoneAwarenessEnabled,
                  ("InstanceType" .=) <$> _eccInstanceType])

-- | Specifies the configuration status for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'elasticsearchClusterConfigStatus' smart constructor.
data ElasticsearchClusterConfigStatus = ElasticsearchClusterConfigStatus'
  { _eccsOptions :: !ElasticsearchClusterConfig
  , _eccsStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticsearchClusterConfigStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eccsOptions' - Specifies the cluster configuration for the specified Elasticsearch domain.
--
-- * 'eccsStatus' - Specifies the status of the configuration for the specified Elasticsearch domain.
elasticsearchClusterConfigStatus
    :: ElasticsearchClusterConfig -- ^ 'eccsOptions'
    -> OptionStatus -- ^ 'eccsStatus'
    -> ElasticsearchClusterConfigStatus
elasticsearchClusterConfigStatus pOptions_ pStatus_ =
  ElasticsearchClusterConfigStatus'
    {_eccsOptions = pOptions_, _eccsStatus = pStatus_}


-- | Specifies the cluster configuration for the specified Elasticsearch domain.
eccsOptions :: Lens' ElasticsearchClusterConfigStatus ElasticsearchClusterConfig
eccsOptions = lens _eccsOptions (\ s a -> s{_eccsOptions = a})

-- | Specifies the status of the configuration for the specified Elasticsearch domain.
eccsStatus :: Lens' ElasticsearchClusterConfigStatus OptionStatus
eccsStatus = lens _eccsStatus (\ s a -> s{_eccsStatus = a})

instance FromJSON ElasticsearchClusterConfigStatus
         where
        parseJSON
          = withObject "ElasticsearchClusterConfigStatus"
              (\ x ->
                 ElasticsearchClusterConfigStatus' <$>
                   (x .: "Options") <*> (x .: "Status"))

instance Hashable ElasticsearchClusterConfigStatus
         where

instance NFData ElasticsearchClusterConfigStatus
         where

-- | The configuration of an Elasticsearch domain.
--
--
--
-- /See:/ 'elasticsearchDomainConfig' smart constructor.
data ElasticsearchDomainConfig = ElasticsearchDomainConfig'
  { _edcEBSOptions                 :: !(Maybe EBSOptionsStatus)
  , _edcAccessPolicies             :: !(Maybe AccessPoliciesStatus)
  , _edcLogPublishingOptions       :: !(Maybe LogPublishingOptionsStatus)
  , _edcElasticsearchClusterConfig :: !(Maybe ElasticsearchClusterConfigStatus)
  , _edcSnapshotOptions            :: !(Maybe SnapshotOptionsStatus)
  , _edcCognitoOptions             :: !(Maybe CognitoOptionsStatus)
  , _edcEncryptionAtRestOptions    :: !(Maybe EncryptionAtRestOptionsStatus)
  , _edcVPCOptions                 :: !(Maybe VPCDerivedInfoStatus)
  , _edcAdvancedOptions            :: !(Maybe AdvancedOptionsStatus)
  , _edcElasticsearchVersion       :: !(Maybe ElasticsearchVersionStatus)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticsearchDomainConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edcEBSOptions' - Specifies the @EBSOptions@ for the Elasticsearch domain.
--
-- * 'edcAccessPolicies' - IAM access policy as a JSON-formatted string.
--
-- * 'edcLogPublishingOptions' - Log publishing options for the given domain.
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
-- * 'edcAdvancedOptions' - Specifies the @AdvancedOptions@ for the domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options> for more information.
--
-- * 'edcElasticsearchVersion' - String of format X.Y to specify version for the Elasticsearch domain.
elasticsearchDomainConfig
    :: ElasticsearchDomainConfig
elasticsearchDomainConfig =
  ElasticsearchDomainConfig'
    { _edcEBSOptions = Nothing
    , _edcAccessPolicies = Nothing
    , _edcLogPublishingOptions = Nothing
    , _edcElasticsearchClusterConfig = Nothing
    , _edcSnapshotOptions = Nothing
    , _edcCognitoOptions = Nothing
    , _edcEncryptionAtRestOptions = Nothing
    , _edcVPCOptions = Nothing
    , _edcAdvancedOptions = Nothing
    , _edcElasticsearchVersion = Nothing
    }


-- | Specifies the @EBSOptions@ for the Elasticsearch domain.
edcEBSOptions :: Lens' ElasticsearchDomainConfig (Maybe EBSOptionsStatus)
edcEBSOptions = lens _edcEBSOptions (\ s a -> s{_edcEBSOptions = a})

-- | IAM access policy as a JSON-formatted string.
edcAccessPolicies :: Lens' ElasticsearchDomainConfig (Maybe AccessPoliciesStatus)
edcAccessPolicies = lens _edcAccessPolicies (\ s a -> s{_edcAccessPolicies = a})

-- | Log publishing options for the given domain.
edcLogPublishingOptions :: Lens' ElasticsearchDomainConfig (Maybe LogPublishingOptionsStatus)
edcLogPublishingOptions = lens _edcLogPublishingOptions (\ s a -> s{_edcLogPublishingOptions = a})

-- | Specifies the @ElasticsearchClusterConfig@ for the Elasticsearch domain.
edcElasticsearchClusterConfig :: Lens' ElasticsearchDomainConfig (Maybe ElasticsearchClusterConfigStatus)
edcElasticsearchClusterConfig = lens _edcElasticsearchClusterConfig (\ s a -> s{_edcElasticsearchClusterConfig = a})

-- | Specifies the @SnapshotOptions@ for the Elasticsearch domain.
edcSnapshotOptions :: Lens' ElasticsearchDomainConfig (Maybe SnapshotOptionsStatus)
edcSnapshotOptions = lens _edcSnapshotOptions (\ s a -> s{_edcSnapshotOptions = a})

-- | The @CognitoOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
edcCognitoOptions :: Lens' ElasticsearchDomainConfig (Maybe CognitoOptionsStatus)
edcCognitoOptions = lens _edcCognitoOptions (\ s a -> s{_edcCognitoOptions = a})

-- | Specifies the @EncryptionAtRestOptions@ for the Elasticsearch domain.
edcEncryptionAtRestOptions :: Lens' ElasticsearchDomainConfig (Maybe EncryptionAtRestOptionsStatus)
edcEncryptionAtRestOptions = lens _edcEncryptionAtRestOptions (\ s a -> s{_edcEncryptionAtRestOptions = a})

-- | The @VPCOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
edcVPCOptions :: Lens' ElasticsearchDomainConfig (Maybe VPCDerivedInfoStatus)
edcVPCOptions = lens _edcVPCOptions (\ s a -> s{_edcVPCOptions = a})

-- | Specifies the @AdvancedOptions@ for the domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options> for more information.
edcAdvancedOptions :: Lens' ElasticsearchDomainConfig (Maybe AdvancedOptionsStatus)
edcAdvancedOptions = lens _edcAdvancedOptions (\ s a -> s{_edcAdvancedOptions = a})

-- | String of format X.Y to specify version for the Elasticsearch domain.
edcElasticsearchVersion :: Lens' ElasticsearchDomainConfig (Maybe ElasticsearchVersionStatus)
edcElasticsearchVersion = lens _edcElasticsearchVersion (\ s a -> s{_edcElasticsearchVersion = a})

instance FromJSON ElasticsearchDomainConfig where
        parseJSON
          = withObject "ElasticsearchDomainConfig"
              (\ x ->
                 ElasticsearchDomainConfig' <$>
                   (x .:? "EBSOptions") <*> (x .:? "AccessPolicies") <*>
                     (x .:? "LogPublishingOptions")
                     <*> (x .:? "ElasticsearchClusterConfig")
                     <*> (x .:? "SnapshotOptions")
                     <*> (x .:? "CognitoOptions")
                     <*> (x .:? "EncryptionAtRestOptions")
                     <*> (x .:? "VPCOptions")
                     <*> (x .:? "AdvancedOptions")
                     <*> (x .:? "ElasticsearchVersion"))

instance Hashable ElasticsearchDomainConfig where

instance NFData ElasticsearchDomainConfig where

-- | The current status of an Elasticsearch domain.
--
--
--
-- /See:/ 'elasticsearchDomainStatus' smart constructor.
data ElasticsearchDomainStatus = ElasticsearchDomainStatus'
  { _edsEBSOptions                 :: !(Maybe EBSOptions)
  , _edsAccessPolicies             :: !(Maybe Text)
  , _edsLogPublishingOptions       :: !(Maybe (Map LogType LogPublishingOption))
  , _edsCreated                    :: !(Maybe Bool)
  , _edsSnapshotOptions            :: !(Maybe SnapshotOptions)
  , _edsCognitoOptions             :: !(Maybe CognitoOptions)
  , _edsEncryptionAtRestOptions    :: !(Maybe EncryptionAtRestOptions)
  , _edsDeleted                    :: !(Maybe Bool)
  , _edsVPCOptions                 :: !(Maybe VPCDerivedInfo)
  , _edsEndpoints                  :: !(Maybe (Map Text Text))
  , _edsProcessing                 :: !(Maybe Bool)
  , _edsEndpoint                   :: !(Maybe Text)
  , _edsAdvancedOptions            :: !(Maybe (Map Text Text))
  , _edsElasticsearchVersion       :: !(Maybe Text)
  , _edsDomainId                   :: !Text
  , _edsDomainName                 :: !Text
  , _edsARN                        :: !Text
  , _edsElasticsearchClusterConfig :: !ElasticsearchClusterConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticsearchDomainStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edsEBSOptions' - The @EBSOptions@ for the specified domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> for more information.
--
-- * 'edsAccessPolicies' - IAM access policy as a JSON-formatted string.
--
-- * 'edsLogPublishingOptions' - Log publishing options for the given domain.
--
-- * 'edsCreated' - The domain creation status. @True@ if the creation of an Elasticsearch domain is complete. @False@ if domain creation is still in progress.
--
-- * 'edsSnapshotOptions' - Specifies the status of the @SnapshotOptions@
--
-- * 'edsCognitoOptions' - The @CognitoOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- * 'edsEncryptionAtRestOptions' - Specifies the status of the @EncryptionAtRestOptions@ .
--
-- * 'edsDeleted' - The domain deletion status. @True@ if a delete request has been received for the domain but resource cleanup is still in progress. @False@ if the domain has not been deleted. Once domain deletion is complete, the status of the domain is no longer returned.
--
-- * 'edsVPCOptions' - The @VPCOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
-- * 'edsEndpoints' - Map containing the Elasticsearch domain endpoints used to submit index and search requests. Example @key, value@ : @'vpc','vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com'@ .
--
-- * 'edsProcessing' - The status of the Elasticsearch domain configuration. @True@ if Amazon Elasticsearch Service is processing configuration changes. @False@ if the configuration is active.
--
-- * 'edsEndpoint' - The Elasticsearch domain endpoint that you use to submit index and search requests.
--
-- * 'edsAdvancedOptions' - Specifies the status of the @AdvancedOptions@
--
-- * 'edsElasticsearchVersion' - Undocumented member.
--
-- * 'edsDomainId' - The unique identifier for the specified Elasticsearch domain.
--
-- * 'edsDomainName' - The name of an Elasticsearch domain. Domain names are unique across the domains owned by an account within an AWS region. Domain names start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
--
-- * 'edsARN' - The Amazon resource name (ARN) of an Elasticsearch domain. See <http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html Identifiers for IAM Entities> in /Using AWS Identity and Access Management/ for more information.
--
-- * 'edsElasticsearchClusterConfig' - The type and number of instances in the domain cluster.
elasticsearchDomainStatus
    :: Text -- ^ 'edsDomainId'
    -> Text -- ^ 'edsDomainName'
    -> Text -- ^ 'edsARN'
    -> ElasticsearchClusterConfig -- ^ 'edsElasticsearchClusterConfig'
    -> ElasticsearchDomainStatus
elasticsearchDomainStatus pDomainId_ pDomainName_ pARN_ pElasticsearchClusterConfig_ =
  ElasticsearchDomainStatus'
    { _edsEBSOptions = Nothing
    , _edsAccessPolicies = Nothing
    , _edsLogPublishingOptions = Nothing
    , _edsCreated = Nothing
    , _edsSnapshotOptions = Nothing
    , _edsCognitoOptions = Nothing
    , _edsEncryptionAtRestOptions = Nothing
    , _edsDeleted = Nothing
    , _edsVPCOptions = Nothing
    , _edsEndpoints = Nothing
    , _edsProcessing = Nothing
    , _edsEndpoint = Nothing
    , _edsAdvancedOptions = Nothing
    , _edsElasticsearchVersion = Nothing
    , _edsDomainId = pDomainId_
    , _edsDomainName = pDomainName_
    , _edsARN = pARN_
    , _edsElasticsearchClusterConfig = pElasticsearchClusterConfig_
    }


-- | The @EBSOptions@ for the specified domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> for more information.
edsEBSOptions :: Lens' ElasticsearchDomainStatus (Maybe EBSOptions)
edsEBSOptions = lens _edsEBSOptions (\ s a -> s{_edsEBSOptions = a})

-- | IAM access policy as a JSON-formatted string.
edsAccessPolicies :: Lens' ElasticsearchDomainStatus (Maybe Text)
edsAccessPolicies = lens _edsAccessPolicies (\ s a -> s{_edsAccessPolicies = a})

-- | Log publishing options for the given domain.
edsLogPublishingOptions :: Lens' ElasticsearchDomainStatus (HashMap LogType LogPublishingOption)
edsLogPublishingOptions = lens _edsLogPublishingOptions (\ s a -> s{_edsLogPublishingOptions = a}) . _Default . _Map

-- | The domain creation status. @True@ if the creation of an Elasticsearch domain is complete. @False@ if domain creation is still in progress.
edsCreated :: Lens' ElasticsearchDomainStatus (Maybe Bool)
edsCreated = lens _edsCreated (\ s a -> s{_edsCreated = a})

-- | Specifies the status of the @SnapshotOptions@
edsSnapshotOptions :: Lens' ElasticsearchDomainStatus (Maybe SnapshotOptions)
edsSnapshotOptions = lens _edsSnapshotOptions (\ s a -> s{_edsSnapshotOptions = a})

-- | The @CognitoOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
edsCognitoOptions :: Lens' ElasticsearchDomainStatus (Maybe CognitoOptions)
edsCognitoOptions = lens _edsCognitoOptions (\ s a -> s{_edsCognitoOptions = a})

-- | Specifies the status of the @EncryptionAtRestOptions@ .
edsEncryptionAtRestOptions :: Lens' ElasticsearchDomainStatus (Maybe EncryptionAtRestOptions)
edsEncryptionAtRestOptions = lens _edsEncryptionAtRestOptions (\ s a -> s{_edsEncryptionAtRestOptions = a})

-- | The domain deletion status. @True@ if a delete request has been received for the domain but resource cleanup is still in progress. @False@ if the domain has not been deleted. Once domain deletion is complete, the status of the domain is no longer returned.
edsDeleted :: Lens' ElasticsearchDomainStatus (Maybe Bool)
edsDeleted = lens _edsDeleted (\ s a -> s{_edsDeleted = a})

-- | The @VPCOptions@ for the specified domain. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
edsVPCOptions :: Lens' ElasticsearchDomainStatus (Maybe VPCDerivedInfo)
edsVPCOptions = lens _edsVPCOptions (\ s a -> s{_edsVPCOptions = a})

-- | Map containing the Elasticsearch domain endpoints used to submit index and search requests. Example @key, value@ : @'vpc','vpc-endpoint-h2dsd34efgyghrtguk5gt6j2foh4.us-east-1.es.amazonaws.com'@ .
edsEndpoints :: Lens' ElasticsearchDomainStatus (HashMap Text Text)
edsEndpoints = lens _edsEndpoints (\ s a -> s{_edsEndpoints = a}) . _Default . _Map

-- | The status of the Elasticsearch domain configuration. @True@ if Amazon Elasticsearch Service is processing configuration changes. @False@ if the configuration is active.
edsProcessing :: Lens' ElasticsearchDomainStatus (Maybe Bool)
edsProcessing = lens _edsProcessing (\ s a -> s{_edsProcessing = a})

-- | The Elasticsearch domain endpoint that you use to submit index and search requests.
edsEndpoint :: Lens' ElasticsearchDomainStatus (Maybe Text)
edsEndpoint = lens _edsEndpoint (\ s a -> s{_edsEndpoint = a})

-- | Specifies the status of the @AdvancedOptions@
edsAdvancedOptions :: Lens' ElasticsearchDomainStatus (HashMap Text Text)
edsAdvancedOptions = lens _edsAdvancedOptions (\ s a -> s{_edsAdvancedOptions = a}) . _Default . _Map

-- | Undocumented member.
edsElasticsearchVersion :: Lens' ElasticsearchDomainStatus (Maybe Text)
edsElasticsearchVersion = lens _edsElasticsearchVersion (\ s a -> s{_edsElasticsearchVersion = a})

-- | The unique identifier for the specified Elasticsearch domain.
edsDomainId :: Lens' ElasticsearchDomainStatus Text
edsDomainId = lens _edsDomainId (\ s a -> s{_edsDomainId = a})

-- | The name of an Elasticsearch domain. Domain names are unique across the domains owned by an account within an AWS region. Domain names start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
edsDomainName :: Lens' ElasticsearchDomainStatus Text
edsDomainName = lens _edsDomainName (\ s a -> s{_edsDomainName = a})

-- | The Amazon resource name (ARN) of an Elasticsearch domain. See <http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html Identifiers for IAM Entities> in /Using AWS Identity and Access Management/ for more information.
edsARN :: Lens' ElasticsearchDomainStatus Text
edsARN = lens _edsARN (\ s a -> s{_edsARN = a})

-- | The type and number of instances in the domain cluster.
edsElasticsearchClusterConfig :: Lens' ElasticsearchDomainStatus ElasticsearchClusterConfig
edsElasticsearchClusterConfig = lens _edsElasticsearchClusterConfig (\ s a -> s{_edsElasticsearchClusterConfig = a})

instance FromJSON ElasticsearchDomainStatus where
        parseJSON
          = withObject "ElasticsearchDomainStatus"
              (\ x ->
                 ElasticsearchDomainStatus' <$>
                   (x .:? "EBSOptions") <*> (x .:? "AccessPolicies") <*>
                     (x .:? "LogPublishingOptions" .!= mempty)
                     <*> (x .:? "Created")
                     <*> (x .:? "SnapshotOptions")
                     <*> (x .:? "CognitoOptions")
                     <*> (x .:? "EncryptionAtRestOptions")
                     <*> (x .:? "Deleted")
                     <*> (x .:? "VPCOptions")
                     <*> (x .:? "Endpoints" .!= mempty)
                     <*> (x .:? "Processing")
                     <*> (x .:? "Endpoint")
                     <*> (x .:? "AdvancedOptions" .!= mempty)
                     <*> (x .:? "ElasticsearchVersion")
                     <*> (x .: "DomainId")
                     <*> (x .: "DomainName")
                     <*> (x .: "ARN")
                     <*> (x .: "ElasticsearchClusterConfig"))

instance Hashable ElasticsearchDomainStatus where

instance NFData ElasticsearchDomainStatus where

-- | Status of the Elasticsearch version options for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'elasticsearchVersionStatus' smart constructor.
data ElasticsearchVersionStatus = ElasticsearchVersionStatus'
  { _evsOptions :: !Text
  , _evsStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticsearchVersionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evsOptions' - Specifies the Elasticsearch version for the specified Elasticsearch domain.
--
-- * 'evsStatus' - Specifies the status of the Elasticsearch version options for the specified Elasticsearch domain.
elasticsearchVersionStatus
    :: Text -- ^ 'evsOptions'
    -> OptionStatus -- ^ 'evsStatus'
    -> ElasticsearchVersionStatus
elasticsearchVersionStatus pOptions_ pStatus_ =
  ElasticsearchVersionStatus' {_evsOptions = pOptions_, _evsStatus = pStatus_}


-- | Specifies the Elasticsearch version for the specified Elasticsearch domain.
evsOptions :: Lens' ElasticsearchVersionStatus Text
evsOptions = lens _evsOptions (\ s a -> s{_evsOptions = a})

-- | Specifies the status of the Elasticsearch version options for the specified Elasticsearch domain.
evsStatus :: Lens' ElasticsearchVersionStatus OptionStatus
evsStatus = lens _evsStatus (\ s a -> s{_evsStatus = a})

instance FromJSON ElasticsearchVersionStatus where
        parseJSON
          = withObject "ElasticsearchVersionStatus"
              (\ x ->
                 ElasticsearchVersionStatus' <$>
                   (x .: "Options") <*> (x .: "Status"))

instance Hashable ElasticsearchVersionStatus where

instance NFData ElasticsearchVersionStatus where

-- | Specifies the Encryption At Rest Options.
--
--
--
-- /See:/ 'encryptionAtRestOptions' smart constructor.
data EncryptionAtRestOptions = EncryptionAtRestOptions'
  { _earoEnabled  :: !(Maybe Bool)
  , _earoKMSKeyId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EncryptionAtRestOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'earoEnabled' - Specifies the option to enable Encryption At Rest.
--
-- * 'earoKMSKeyId' - Specifies the KMS Key ID for Encryption At Rest options.
encryptionAtRestOptions
    :: EncryptionAtRestOptions
encryptionAtRestOptions =
  EncryptionAtRestOptions' {_earoEnabled = Nothing, _earoKMSKeyId = Nothing}


-- | Specifies the option to enable Encryption At Rest.
earoEnabled :: Lens' EncryptionAtRestOptions (Maybe Bool)
earoEnabled = lens _earoEnabled (\ s a -> s{_earoEnabled = a})

-- | Specifies the KMS Key ID for Encryption At Rest options.
earoKMSKeyId :: Lens' EncryptionAtRestOptions (Maybe Text)
earoKMSKeyId = lens _earoKMSKeyId (\ s a -> s{_earoKMSKeyId = a})

instance FromJSON EncryptionAtRestOptions where
        parseJSON
          = withObject "EncryptionAtRestOptions"
              (\ x ->
                 EncryptionAtRestOptions' <$>
                   (x .:? "Enabled") <*> (x .:? "KmsKeyId"))

instance Hashable EncryptionAtRestOptions where

instance NFData EncryptionAtRestOptions where

instance ToJSON EncryptionAtRestOptions where
        toJSON EncryptionAtRestOptions'{..}
          = object
              (catMaybes
                 [("Enabled" .=) <$> _earoEnabled,
                  ("KmsKeyId" .=) <$> _earoKMSKeyId])

-- | Status of the Encryption At Rest options for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'encryptionAtRestOptionsStatus' smart constructor.
data EncryptionAtRestOptionsStatus = EncryptionAtRestOptionsStatus'
  { _earosOptions :: !EncryptionAtRestOptions
  , _earosStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EncryptionAtRestOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'earosOptions' - Specifies the Encryption At Rest options for the specified Elasticsearch domain.
--
-- * 'earosStatus' - Specifies the status of the Encryption At Rest options for the specified Elasticsearch domain.
encryptionAtRestOptionsStatus
    :: EncryptionAtRestOptions -- ^ 'earosOptions'
    -> OptionStatus -- ^ 'earosStatus'
    -> EncryptionAtRestOptionsStatus
encryptionAtRestOptionsStatus pOptions_ pStatus_ =
  EncryptionAtRestOptionsStatus'
    {_earosOptions = pOptions_, _earosStatus = pStatus_}


-- | Specifies the Encryption At Rest options for the specified Elasticsearch domain.
earosOptions :: Lens' EncryptionAtRestOptionsStatus EncryptionAtRestOptions
earosOptions = lens _earosOptions (\ s a -> s{_earosOptions = a})

-- | Specifies the status of the Encryption At Rest options for the specified Elasticsearch domain.
earosStatus :: Lens' EncryptionAtRestOptionsStatus OptionStatus
earosStatus = lens _earosStatus (\ s a -> s{_earosStatus = a})

instance FromJSON EncryptionAtRestOptionsStatus where
        parseJSON
          = withObject "EncryptionAtRestOptionsStatus"
              (\ x ->
                 EncryptionAtRestOptionsStatus' <$>
                   (x .: "Options") <*> (x .: "Status"))

instance Hashable EncryptionAtRestOptionsStatus where

instance NFData EncryptionAtRestOptionsStatus where

-- | InstanceCountLimits represents the limits on number of instances that be created in Amazon Elasticsearch for given InstanceType.
--
--
--
-- /See:/ 'instanceCountLimits' smart constructor.
data InstanceCountLimits = InstanceCountLimits'
  { _iclMaximumInstanceCount :: !(Maybe Int)
  , _iclMinimumInstanceCount :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceCountLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iclMaximumInstanceCount' - Undocumented member.
--
-- * 'iclMinimumInstanceCount' - Undocumented member.
instanceCountLimits
    :: InstanceCountLimits
instanceCountLimits =
  InstanceCountLimits'
    {_iclMaximumInstanceCount = Nothing, _iclMinimumInstanceCount = Nothing}


-- | Undocumented member.
iclMaximumInstanceCount :: Lens' InstanceCountLimits (Maybe Int)
iclMaximumInstanceCount = lens _iclMaximumInstanceCount (\ s a -> s{_iclMaximumInstanceCount = a})

-- | Undocumented member.
iclMinimumInstanceCount :: Lens' InstanceCountLimits (Maybe Int)
iclMinimumInstanceCount = lens _iclMinimumInstanceCount (\ s a -> s{_iclMinimumInstanceCount = a})

instance FromJSON InstanceCountLimits where
        parseJSON
          = withObject "InstanceCountLimits"
              (\ x ->
                 InstanceCountLimits' <$>
                   (x .:? "MaximumInstanceCount") <*>
                     (x .:? "MinimumInstanceCount"))

instance Hashable InstanceCountLimits where

instance NFData InstanceCountLimits where

-- | InstanceLimits represents the list of instance related attributes that are available for given InstanceType.
--
--
--
-- /See:/ 'instanceLimits' smart constructor.
newtype InstanceLimits = InstanceLimits'
  { _ilInstanceCountLimits :: Maybe InstanceCountLimits
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilInstanceCountLimits' - Undocumented member.
instanceLimits
    :: InstanceLimits
instanceLimits = InstanceLimits' {_ilInstanceCountLimits = Nothing}


-- | Undocumented member.
ilInstanceCountLimits :: Lens' InstanceLimits (Maybe InstanceCountLimits)
ilInstanceCountLimits = lens _ilInstanceCountLimits (\ s a -> s{_ilInstanceCountLimits = a})

instance FromJSON InstanceLimits where
        parseJSON
          = withObject "InstanceLimits"
              (\ x ->
                 InstanceLimits' <$> (x .:? "InstanceCountLimits"))

instance Hashable InstanceLimits where

instance NFData InstanceLimits where

-- | Limits for given InstanceType and for each of it's role.
--
-- Limits contains following @'StorageTypes,' @ @'InstanceLimits' @ and @'AdditionalLimits' @
--
--
-- /See:/ 'limits' smart constructor.
data Limits = Limits'
  { _lInstanceLimits   :: !(Maybe InstanceLimits)
  , _lAdditionalLimits :: !(Maybe [AdditionalLimit])
  , _lStorageTypes     :: !(Maybe [StorageType])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Limits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lInstanceLimits' - Undocumented member.
--
-- * 'lAdditionalLimits' - List of additional limits that are specific to a given InstanceType and for each of it's @'InstanceRole' @ .
--
-- * 'lStorageTypes' - StorageType represents the list of storage related types and attributes that are available for given InstanceType.
limits
    :: Limits
limits =
  Limits'
    { _lInstanceLimits = Nothing
    , _lAdditionalLimits = Nothing
    , _lStorageTypes = Nothing
    }


-- | Undocumented member.
lInstanceLimits :: Lens' Limits (Maybe InstanceLimits)
lInstanceLimits = lens _lInstanceLimits (\ s a -> s{_lInstanceLimits = a})

-- | List of additional limits that are specific to a given InstanceType and for each of it's @'InstanceRole' @ .
lAdditionalLimits :: Lens' Limits [AdditionalLimit]
lAdditionalLimits = lens _lAdditionalLimits (\ s a -> s{_lAdditionalLimits = a}) . _Default . _Coerce

-- | StorageType represents the list of storage related types and attributes that are available for given InstanceType.
lStorageTypes :: Lens' Limits [StorageType]
lStorageTypes = lens _lStorageTypes (\ s a -> s{_lStorageTypes = a}) . _Default . _Coerce

instance FromJSON Limits where
        parseJSON
          = withObject "Limits"
              (\ x ->
                 Limits' <$>
                   (x .:? "InstanceLimits") <*>
                     (x .:? "AdditionalLimits" .!= mempty)
                     <*> (x .:? "StorageTypes" .!= mempty))

instance Hashable Limits where

instance NFData Limits where

-- | Log Publishing option that is set for given domain.
--
-- Attributes and their details:     * CloudWatchLogsLogGroupArn: ARN of the Cloudwatch log group to which log needs to be published.    * Enabled: Whether the log publishing for given log type is enabled or not
--
--
--
--
-- /See:/ 'logPublishingOption' smart constructor.
data LogPublishingOption = LogPublishingOption'
  { _lpoEnabled                   :: !(Maybe Bool)
  , _lpoCloudWatchLogsLogGroupARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LogPublishingOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpoEnabled' - Specifies whether given log publishing option is enabled or not.
--
-- * 'lpoCloudWatchLogsLogGroupARN' - Undocumented member.
logPublishingOption
    :: LogPublishingOption
logPublishingOption =
  LogPublishingOption'
    {_lpoEnabled = Nothing, _lpoCloudWatchLogsLogGroupARN = Nothing}


-- | Specifies whether given log publishing option is enabled or not.
lpoEnabled :: Lens' LogPublishingOption (Maybe Bool)
lpoEnabled = lens _lpoEnabled (\ s a -> s{_lpoEnabled = a})

-- | Undocumented member.
lpoCloudWatchLogsLogGroupARN :: Lens' LogPublishingOption (Maybe Text)
lpoCloudWatchLogsLogGroupARN = lens _lpoCloudWatchLogsLogGroupARN (\ s a -> s{_lpoCloudWatchLogsLogGroupARN = a})

instance FromJSON LogPublishingOption where
        parseJSON
          = withObject "LogPublishingOption"
              (\ x ->
                 LogPublishingOption' <$>
                   (x .:? "Enabled") <*>
                     (x .:? "CloudWatchLogsLogGroupArn"))

instance Hashable LogPublishingOption where

instance NFData LogPublishingOption where

instance ToJSON LogPublishingOption where
        toJSON LogPublishingOption'{..}
          = object
              (catMaybes
                 [("Enabled" .=) <$> _lpoEnabled,
                  ("CloudWatchLogsLogGroupArn" .=) <$>
                    _lpoCloudWatchLogsLogGroupARN])

-- | The configured log publishing options for the domain and their current status.
--
--
--
-- /See:/ 'logPublishingOptionsStatus' smart constructor.
data LogPublishingOptionsStatus = LogPublishingOptionsStatus'
  { _lposStatus  :: !(Maybe OptionStatus)
  , _lposOptions :: !(Maybe (Map LogType LogPublishingOption))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LogPublishingOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lposStatus' - The status of the log publishing options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
--
-- * 'lposOptions' - The log publishing options configured for the Elasticsearch domain.
logPublishingOptionsStatus
    :: LogPublishingOptionsStatus
logPublishingOptionsStatus =
  LogPublishingOptionsStatus' {_lposStatus = Nothing, _lposOptions = Nothing}


-- | The status of the log publishing options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
lposStatus :: Lens' LogPublishingOptionsStatus (Maybe OptionStatus)
lposStatus = lens _lposStatus (\ s a -> s{_lposStatus = a})

-- | The log publishing options configured for the Elasticsearch domain.
lposOptions :: Lens' LogPublishingOptionsStatus (HashMap LogType LogPublishingOption)
lposOptions = lens _lposOptions (\ s a -> s{_lposOptions = a}) . _Default . _Map

instance FromJSON LogPublishingOptionsStatus where
        parseJSON
          = withObject "LogPublishingOptionsStatus"
              (\ x ->
                 LogPublishingOptionsStatus' <$>
                   (x .:? "Status") <*> (x .:? "Options" .!= mempty))

instance Hashable LogPublishingOptionsStatus where

instance NFData LogPublishingOptionsStatus where

-- | Provides the current status of the entity.
--
--
--
-- /See:/ 'optionStatus' smart constructor.
data OptionStatus = OptionStatus'
  { _osPendingDeletion :: !(Maybe Bool)
  , _osUpdateVersion   :: !(Maybe Nat)
  , _osCreationDate    :: !POSIX
  , _osUpdateDate      :: !POSIX
  , _osState           :: !OptionState
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OptionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osPendingDeletion' - Indicates whether the Elasticsearch domain is being deleted.
--
-- * 'osUpdateVersion' - Specifies the latest version for the entity.
--
-- * 'osCreationDate' - Timestamp which tells the creation date for the entity.
--
-- * 'osUpdateDate' - Timestamp which tells the last updated time for the entity.
--
-- * 'osState' - Provides the @OptionState@ for the Elasticsearch domain.
optionStatus
    :: UTCTime -- ^ 'osCreationDate'
    -> UTCTime -- ^ 'osUpdateDate'
    -> OptionState -- ^ 'osState'
    -> OptionStatus
optionStatus pCreationDate_ pUpdateDate_ pState_ =
  OptionStatus'
    { _osPendingDeletion = Nothing
    , _osUpdateVersion = Nothing
    , _osCreationDate = _Time # pCreationDate_
    , _osUpdateDate = _Time # pUpdateDate_
    , _osState = pState_
    }


-- | Indicates whether the Elasticsearch domain is being deleted.
osPendingDeletion :: Lens' OptionStatus (Maybe Bool)
osPendingDeletion = lens _osPendingDeletion (\ s a -> s{_osPendingDeletion = a})

-- | Specifies the latest version for the entity.
osUpdateVersion :: Lens' OptionStatus (Maybe Natural)
osUpdateVersion = lens _osUpdateVersion (\ s a -> s{_osUpdateVersion = a}) . mapping _Nat

-- | Timestamp which tells the creation date for the entity.
osCreationDate :: Lens' OptionStatus UTCTime
osCreationDate = lens _osCreationDate (\ s a -> s{_osCreationDate = a}) . _Time

-- | Timestamp which tells the last updated time for the entity.
osUpdateDate :: Lens' OptionStatus UTCTime
osUpdateDate = lens _osUpdateDate (\ s a -> s{_osUpdateDate = a}) . _Time

-- | Provides the @OptionState@ for the Elasticsearch domain.
osState :: Lens' OptionStatus OptionState
osState = lens _osState (\ s a -> s{_osState = a})

instance FromJSON OptionStatus where
        parseJSON
          = withObject "OptionStatus"
              (\ x ->
                 OptionStatus' <$>
                   (x .:? "PendingDeletion") <*> (x .:? "UpdateVersion")
                     <*> (x .: "CreationDate")
                     <*> (x .: "UpdateDate")
                     <*> (x .: "State"))

instance Hashable OptionStatus where

instance NFData OptionStatus where

-- | Contains the specific price and frequency of a recurring charges for a reserved Elasticsearch instance, or for a reserved Elasticsearch instance offering.
--
--
--
-- /See:/ 'recurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { _rcRecurringChargeFrequency :: !(Maybe Text)
  , _rcRecurringChargeAmount    :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRecurringChargeFrequency' - The frequency of the recurring charge.
--
-- * 'rcRecurringChargeAmount' - The monetary amount of the recurring charge.
recurringCharge
    :: RecurringCharge
recurringCharge =
  RecurringCharge'
    {_rcRecurringChargeFrequency = Nothing, _rcRecurringChargeAmount = Nothing}


-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency = lens _rcRecurringChargeFrequency (\ s a -> s{_rcRecurringChargeFrequency = a})

-- | The monetary amount of the recurring charge.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\ s a -> s{_rcRecurringChargeAmount = a})

instance FromJSON RecurringCharge where
        parseJSON
          = withObject "RecurringCharge"
              (\ x ->
                 RecurringCharge' <$>
                   (x .:? "RecurringChargeFrequency") <*>
                     (x .:? "RecurringChargeAmount"))

instance Hashable RecurringCharge where

instance NFData RecurringCharge where

-- | Details of a reserved Elasticsearch instance.
--
--
--
-- /See:/ 'reservedElasticsearchInstance' smart constructor.
data ReservedElasticsearchInstance = ReservedElasticsearchInstance'
  { _reiState :: !(Maybe Text)
  , _reiCurrencyCode :: !(Maybe Text)
  , _reiStartTime :: !(Maybe POSIX)
  , _reiReservedElasticsearchInstanceOfferingId :: !(Maybe Text)
  , _reiReservedElasticsearchInstanceId :: !(Maybe Text)
  , _reiElasticsearchInstanceCount :: !(Maybe Int)
  , _reiReservationName :: !(Maybe Text)
  , _reiElasticsearchInstanceType :: !(Maybe ESPartitionInstanceType)
  , _reiRecurringCharges :: !(Maybe [RecurringCharge])
  , _reiUsagePrice :: !(Maybe Double)
  , _reiFixedPrice :: !(Maybe Double)
  , _reiDuration :: !(Maybe Int)
  , _reiPaymentOption :: !(Maybe ReservedElasticsearchInstancePaymentOption)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedElasticsearchInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reiState' - The state of the reserved Elasticsearch instance.
--
-- * 'reiCurrencyCode' - The currency code for the reserved Elasticsearch instance offering.
--
-- * 'reiStartTime' - The time the reservation started.
--
-- * 'reiReservedElasticsearchInstanceOfferingId' - The offering identifier.
--
-- * 'reiReservedElasticsearchInstanceId' - The unique identifier for the reservation.
--
-- * 'reiElasticsearchInstanceCount' - The number of Elasticsearch instances that have been reserved.
--
-- * 'reiReservationName' - The customer-specified identifier to track this reservation.
--
-- * 'reiElasticsearchInstanceType' - The Elasticsearch instance type offered by the reserved instance offering.
--
-- * 'reiRecurringCharges' - The charge to your account regardless of whether you are creating any domains using the instance offering.
--
-- * 'reiUsagePrice' - The rate you are charged for each hour for the domain that is using this reserved instance.
--
-- * 'reiFixedPrice' - The upfront fixed charge you will paid to purchase the specific reserved Elasticsearch instance offering.
--
-- * 'reiDuration' - The duration, in seconds, for which the Elasticsearch instance is reserved.
--
-- * 'reiPaymentOption' - The payment option as defined in the reserved Elasticsearch instance offering.
reservedElasticsearchInstance
    :: ReservedElasticsearchInstance
reservedElasticsearchInstance =
  ReservedElasticsearchInstance'
    { _reiState = Nothing
    , _reiCurrencyCode = Nothing
    , _reiStartTime = Nothing
    , _reiReservedElasticsearchInstanceOfferingId = Nothing
    , _reiReservedElasticsearchInstanceId = Nothing
    , _reiElasticsearchInstanceCount = Nothing
    , _reiReservationName = Nothing
    , _reiElasticsearchInstanceType = Nothing
    , _reiRecurringCharges = Nothing
    , _reiUsagePrice = Nothing
    , _reiFixedPrice = Nothing
    , _reiDuration = Nothing
    , _reiPaymentOption = Nothing
    }


-- | The state of the reserved Elasticsearch instance.
reiState :: Lens' ReservedElasticsearchInstance (Maybe Text)
reiState = lens _reiState (\ s a -> s{_reiState = a})

-- | The currency code for the reserved Elasticsearch instance offering.
reiCurrencyCode :: Lens' ReservedElasticsearchInstance (Maybe Text)
reiCurrencyCode = lens _reiCurrencyCode (\ s a -> s{_reiCurrencyCode = a})

-- | The time the reservation started.
reiStartTime :: Lens' ReservedElasticsearchInstance (Maybe UTCTime)
reiStartTime = lens _reiStartTime (\ s a -> s{_reiStartTime = a}) . mapping _Time

-- | The offering identifier.
reiReservedElasticsearchInstanceOfferingId :: Lens' ReservedElasticsearchInstance (Maybe Text)
reiReservedElasticsearchInstanceOfferingId = lens _reiReservedElasticsearchInstanceOfferingId (\ s a -> s{_reiReservedElasticsearchInstanceOfferingId = a})

-- | The unique identifier for the reservation.
reiReservedElasticsearchInstanceId :: Lens' ReservedElasticsearchInstance (Maybe Text)
reiReservedElasticsearchInstanceId = lens _reiReservedElasticsearchInstanceId (\ s a -> s{_reiReservedElasticsearchInstanceId = a})

-- | The number of Elasticsearch instances that have been reserved.
reiElasticsearchInstanceCount :: Lens' ReservedElasticsearchInstance (Maybe Int)
reiElasticsearchInstanceCount = lens _reiElasticsearchInstanceCount (\ s a -> s{_reiElasticsearchInstanceCount = a})

-- | The customer-specified identifier to track this reservation.
reiReservationName :: Lens' ReservedElasticsearchInstance (Maybe Text)
reiReservationName = lens _reiReservationName (\ s a -> s{_reiReservationName = a})

-- | The Elasticsearch instance type offered by the reserved instance offering.
reiElasticsearchInstanceType :: Lens' ReservedElasticsearchInstance (Maybe ESPartitionInstanceType)
reiElasticsearchInstanceType = lens _reiElasticsearchInstanceType (\ s a -> s{_reiElasticsearchInstanceType = a})

-- | The charge to your account regardless of whether you are creating any domains using the instance offering.
reiRecurringCharges :: Lens' ReservedElasticsearchInstance [RecurringCharge]
reiRecurringCharges = lens _reiRecurringCharges (\ s a -> s{_reiRecurringCharges = a}) . _Default . _Coerce

-- | The rate you are charged for each hour for the domain that is using this reserved instance.
reiUsagePrice :: Lens' ReservedElasticsearchInstance (Maybe Double)
reiUsagePrice = lens _reiUsagePrice (\ s a -> s{_reiUsagePrice = a})

-- | The upfront fixed charge you will paid to purchase the specific reserved Elasticsearch instance offering.
reiFixedPrice :: Lens' ReservedElasticsearchInstance (Maybe Double)
reiFixedPrice = lens _reiFixedPrice (\ s a -> s{_reiFixedPrice = a})

-- | The duration, in seconds, for which the Elasticsearch instance is reserved.
reiDuration :: Lens' ReservedElasticsearchInstance (Maybe Int)
reiDuration = lens _reiDuration (\ s a -> s{_reiDuration = a})

-- | The payment option as defined in the reserved Elasticsearch instance offering.
reiPaymentOption :: Lens' ReservedElasticsearchInstance (Maybe ReservedElasticsearchInstancePaymentOption)
reiPaymentOption = lens _reiPaymentOption (\ s a -> s{_reiPaymentOption = a})

instance FromJSON ReservedElasticsearchInstance where
        parseJSON
          = withObject "ReservedElasticsearchInstance"
              (\ x ->
                 ReservedElasticsearchInstance' <$>
                   (x .:? "State") <*> (x .:? "CurrencyCode") <*>
                     (x .:? "StartTime")
                     <*> (x .:? "ReservedElasticsearchInstanceOfferingId")
                     <*> (x .:? "ReservedElasticsearchInstanceId")
                     <*> (x .:? "ElasticsearchInstanceCount")
                     <*> (x .:? "ReservationName")
                     <*> (x .:? "ElasticsearchInstanceType")
                     <*> (x .:? "RecurringCharges" .!= mempty)
                     <*> (x .:? "UsagePrice")
                     <*> (x .:? "FixedPrice")
                     <*> (x .:? "Duration")
                     <*> (x .:? "PaymentOption"))

instance Hashable ReservedElasticsearchInstance where

instance NFData ReservedElasticsearchInstance where

-- | Details of a reserved Elasticsearch instance offering.
--
--
--
-- /See:/ 'reservedElasticsearchInstanceOffering' smart constructor.
data ReservedElasticsearchInstanceOffering = ReservedElasticsearchInstanceOffering'
  { _reioCurrencyCode :: !(Maybe Text)
  , _reioReservedElasticsearchInstanceOfferingId :: !(Maybe Text)
  , _reioElasticsearchInstanceType :: !(Maybe ESPartitionInstanceType)
  , _reioRecurringCharges :: !(Maybe [RecurringCharge])
  , _reioUsagePrice :: !(Maybe Double)
  , _reioFixedPrice :: !(Maybe Double)
  , _reioDuration :: !(Maybe Int)
  , _reioPaymentOption :: !(Maybe ReservedElasticsearchInstancePaymentOption)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedElasticsearchInstanceOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reioCurrencyCode' - The currency code for the reserved Elasticsearch instance offering.
--
-- * 'reioReservedElasticsearchInstanceOfferingId' - The Elasticsearch reserved instance offering identifier.
--
-- * 'reioElasticsearchInstanceType' - The Elasticsearch instance type offered by the reserved instance offering.
--
-- * 'reioRecurringCharges' - The charge to your account regardless of whether you are creating any domains using the instance offering.
--
-- * 'reioUsagePrice' - The rate you are charged for each hour the domain that is using the offering is running.
--
-- * 'reioFixedPrice' - The upfront fixed charge you will pay to purchase the specific reserved Elasticsearch instance offering.
--
-- * 'reioDuration' - The duration, in seconds, for which the offering will reserve the Elasticsearch instance.
--
-- * 'reioPaymentOption' - Payment option for the reserved Elasticsearch instance offering
reservedElasticsearchInstanceOffering
    :: ReservedElasticsearchInstanceOffering
reservedElasticsearchInstanceOffering =
  ReservedElasticsearchInstanceOffering'
    { _reioCurrencyCode = Nothing
    , _reioReservedElasticsearchInstanceOfferingId = Nothing
    , _reioElasticsearchInstanceType = Nothing
    , _reioRecurringCharges = Nothing
    , _reioUsagePrice = Nothing
    , _reioFixedPrice = Nothing
    , _reioDuration = Nothing
    , _reioPaymentOption = Nothing
    }


-- | The currency code for the reserved Elasticsearch instance offering.
reioCurrencyCode :: Lens' ReservedElasticsearchInstanceOffering (Maybe Text)
reioCurrencyCode = lens _reioCurrencyCode (\ s a -> s{_reioCurrencyCode = a})

-- | The Elasticsearch reserved instance offering identifier.
reioReservedElasticsearchInstanceOfferingId :: Lens' ReservedElasticsearchInstanceOffering (Maybe Text)
reioReservedElasticsearchInstanceOfferingId = lens _reioReservedElasticsearchInstanceOfferingId (\ s a -> s{_reioReservedElasticsearchInstanceOfferingId = a})

-- | The Elasticsearch instance type offered by the reserved instance offering.
reioElasticsearchInstanceType :: Lens' ReservedElasticsearchInstanceOffering (Maybe ESPartitionInstanceType)
reioElasticsearchInstanceType = lens _reioElasticsearchInstanceType (\ s a -> s{_reioElasticsearchInstanceType = a})

-- | The charge to your account regardless of whether you are creating any domains using the instance offering.
reioRecurringCharges :: Lens' ReservedElasticsearchInstanceOffering [RecurringCharge]
reioRecurringCharges = lens _reioRecurringCharges (\ s a -> s{_reioRecurringCharges = a}) . _Default . _Coerce

-- | The rate you are charged for each hour the domain that is using the offering is running.
reioUsagePrice :: Lens' ReservedElasticsearchInstanceOffering (Maybe Double)
reioUsagePrice = lens _reioUsagePrice (\ s a -> s{_reioUsagePrice = a})

-- | The upfront fixed charge you will pay to purchase the specific reserved Elasticsearch instance offering.
reioFixedPrice :: Lens' ReservedElasticsearchInstanceOffering (Maybe Double)
reioFixedPrice = lens _reioFixedPrice (\ s a -> s{_reioFixedPrice = a})

-- | The duration, in seconds, for which the offering will reserve the Elasticsearch instance.
reioDuration :: Lens' ReservedElasticsearchInstanceOffering (Maybe Int)
reioDuration = lens _reioDuration (\ s a -> s{_reioDuration = a})

-- | Payment option for the reserved Elasticsearch instance offering
reioPaymentOption :: Lens' ReservedElasticsearchInstanceOffering (Maybe ReservedElasticsearchInstancePaymentOption)
reioPaymentOption = lens _reioPaymentOption (\ s a -> s{_reioPaymentOption = a})

instance FromJSON
           ReservedElasticsearchInstanceOffering
         where
        parseJSON
          = withObject "ReservedElasticsearchInstanceOffering"
              (\ x ->
                 ReservedElasticsearchInstanceOffering' <$>
                   (x .:? "CurrencyCode") <*>
                     (x .:? "ReservedElasticsearchInstanceOfferingId")
                     <*> (x .:? "ElasticsearchInstanceType")
                     <*> (x .:? "RecurringCharges" .!= mempty)
                     <*> (x .:? "UsagePrice")
                     <*> (x .:? "FixedPrice")
                     <*> (x .:? "Duration")
                     <*> (x .:? "PaymentOption"))

instance Hashable
           ReservedElasticsearchInstanceOffering
         where

instance NFData ReservedElasticsearchInstanceOffering
         where

-- | Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is @0@ hours.
--
--
--
-- /See:/ 'snapshotOptions' smart constructor.
newtype SnapshotOptions = SnapshotOptions'
  { _soAutomatedSnapshotStartHour :: Maybe Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SnapshotOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'soAutomatedSnapshotStartHour' - Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is @0@ hours.
snapshotOptions
    :: SnapshotOptions
snapshotOptions = SnapshotOptions' {_soAutomatedSnapshotStartHour = Nothing}


-- | Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is @0@ hours.
soAutomatedSnapshotStartHour :: Lens' SnapshotOptions (Maybe Int)
soAutomatedSnapshotStartHour = lens _soAutomatedSnapshotStartHour (\ s a -> s{_soAutomatedSnapshotStartHour = a})

instance FromJSON SnapshotOptions where
        parseJSON
          = withObject "SnapshotOptions"
              (\ x ->
                 SnapshotOptions' <$>
                   (x .:? "AutomatedSnapshotStartHour"))

instance Hashable SnapshotOptions where

instance NFData SnapshotOptions where

instance ToJSON SnapshotOptions where
        toJSON SnapshotOptions'{..}
          = object
              (catMaybes
                 [("AutomatedSnapshotStartHour" .=) <$>
                    _soAutomatedSnapshotStartHour])

-- | Status of a daily automated snapshot.
--
--
--
-- /See:/ 'snapshotOptionsStatus' smart constructor.
data SnapshotOptionsStatus = SnapshotOptionsStatus'
  { _sosOptions :: !SnapshotOptions
  , _sosStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SnapshotOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sosOptions' - Specifies the daily snapshot options specified for the Elasticsearch domain.
--
-- * 'sosStatus' - Specifies the status of a daily automated snapshot.
snapshotOptionsStatus
    :: SnapshotOptions -- ^ 'sosOptions'
    -> OptionStatus -- ^ 'sosStatus'
    -> SnapshotOptionsStatus
snapshotOptionsStatus pOptions_ pStatus_ =
  SnapshotOptionsStatus' {_sosOptions = pOptions_, _sosStatus = pStatus_}


-- | Specifies the daily snapshot options specified for the Elasticsearch domain.
sosOptions :: Lens' SnapshotOptionsStatus SnapshotOptions
sosOptions = lens _sosOptions (\ s a -> s{_sosOptions = a})

-- | Specifies the status of a daily automated snapshot.
sosStatus :: Lens' SnapshotOptionsStatus OptionStatus
sosStatus = lens _sosStatus (\ s a -> s{_sosStatus = a})

instance FromJSON SnapshotOptionsStatus where
        parseJSON
          = withObject "SnapshotOptionsStatus"
              (\ x ->
                 SnapshotOptionsStatus' <$>
                   (x .: "Options") <*> (x .: "Status"))

instance Hashable SnapshotOptionsStatus where

instance NFData SnapshotOptionsStatus where

-- | StorageTypes represents the list of storage related types and their attributes that are available for given InstanceType.
--
--
--
-- /See:/ 'storageType' smart constructor.
data StorageType = StorageType'
  { _stStorageTypeLimits  :: !(Maybe [StorageTypeLimit])
  , _stStorageSubTypeName :: !(Maybe Text)
  , _stStorageTypeName    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StorageType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stStorageTypeLimits' - List of limits that are applicable for given storage type.
--
-- * 'stStorageSubTypeName' - Undocumented member.
--
-- * 'stStorageTypeName' - Undocumented member.
storageType
    :: StorageType
storageType =
  StorageType'
    { _stStorageTypeLimits = Nothing
    , _stStorageSubTypeName = Nothing
    , _stStorageTypeName = Nothing
    }


-- | List of limits that are applicable for given storage type.
stStorageTypeLimits :: Lens' StorageType [StorageTypeLimit]
stStorageTypeLimits = lens _stStorageTypeLimits (\ s a -> s{_stStorageTypeLimits = a}) . _Default . _Coerce

-- | Undocumented member.
stStorageSubTypeName :: Lens' StorageType (Maybe Text)
stStorageSubTypeName = lens _stStorageSubTypeName (\ s a -> s{_stStorageSubTypeName = a})

-- | Undocumented member.
stStorageTypeName :: Lens' StorageType (Maybe Text)
stStorageTypeName = lens _stStorageTypeName (\ s a -> s{_stStorageTypeName = a})

instance FromJSON StorageType where
        parseJSON
          = withObject "StorageType"
              (\ x ->
                 StorageType' <$>
                   (x .:? "StorageTypeLimits" .!= mempty) <*>
                     (x .:? "StorageSubTypeName")
                     <*> (x .:? "StorageTypeName"))

instance Hashable StorageType where

instance NFData StorageType where

-- | Limits that are applicable for given storage type.
--
--
--
-- /See:/ 'storageTypeLimit' smart constructor.
data StorageTypeLimit = StorageTypeLimit'
  { _stlLimitName   :: !(Maybe Text)
  , _stlLimitValues :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StorageTypeLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stlLimitName' - Name of storage limits that are applicable for given storage type. If @'StorageType' @ is ebs, following storage options are applicable     * MinimumVolumeSizeMinimum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.     * MaximumVolumeSizeMaximum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.     * MaximumIopsMaximum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.     * MinimumIopsMinimum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.
--
-- * 'stlLimitValues' - Values for the @'StorageTypeLimit$LimitName' @ .
storageTypeLimit
    :: StorageTypeLimit
storageTypeLimit =
  StorageTypeLimit' {_stlLimitName = Nothing, _stlLimitValues = Nothing}


-- | Name of storage limits that are applicable for given storage type. If @'StorageType' @ is ebs, following storage options are applicable     * MinimumVolumeSizeMinimum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.     * MaximumVolumeSizeMaximum amount of volume size that is applicable for given storage type.It can be empty if it is not applicable.     * MaximumIopsMaximum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.     * MinimumIopsMinimum amount of Iops that is applicable for given storage type.It can be empty if it is not applicable.
stlLimitName :: Lens' StorageTypeLimit (Maybe Text)
stlLimitName = lens _stlLimitName (\ s a -> s{_stlLimitName = a})

-- | Values for the @'StorageTypeLimit$LimitName' @ .
stlLimitValues :: Lens' StorageTypeLimit [Text]
stlLimitValues = lens _stlLimitValues (\ s a -> s{_stlLimitValues = a}) . _Default . _Coerce

instance FromJSON StorageTypeLimit where
        parseJSON
          = withObject "StorageTypeLimit"
              (\ x ->
                 StorageTypeLimit' <$>
                   (x .:? "LimitName") <*>
                     (x .:? "LimitValues" .!= mempty))

instance Hashable StorageTypeLimit where

instance NFData StorageTypeLimit where

-- | Specifies a key value pair for a resource tag.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - Specifies the @TagKey@ , the name of the tag. Tag keys must be unique for the Elasticsearch domain to which they are attached.
--
-- * 'tagValue' - Specifies the @TagValue@ , the value assigned to the corresponding tag key. Tag values can be null and do not have to be unique in a tag set. For example, you can have a key value pair in a tag set of @project : Trinity@ and @cost-center : Trinity@
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | Specifies the @TagKey@ , the name of the tag. Tag keys must be unique for the Elasticsearch domain to which they are attached.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | Specifies the @TagValue@ , the value assigned to the corresponding tag key. Tag values can be null and do not have to be unique in a tag set. For example, you can have a key value pair in a tag set of @project : Trinity@ and @cost-center : Trinity@
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
--
--
-- /See:/ 'vpcDerivedInfo' smart constructor.
data VPCDerivedInfo = VPCDerivedInfo'
  { _vdiSecurityGroupIds  :: !(Maybe [Text])
  , _vdiSubnetIds         :: !(Maybe [Text])
  , _vdiVPCId             :: !(Maybe Text)
  , _vdiAvailabilityZones :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCDerivedInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdiSecurityGroupIds' - Specifies the security groups for VPC endpoint.
--
-- * 'vdiSubnetIds' - Specifies the subnets for VPC endpoint.
--
-- * 'vdiVPCId' - The VPC Id for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
--
-- * 'vdiAvailabilityZones' - The availability zones for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
vpcDerivedInfo
    :: VPCDerivedInfo
vpcDerivedInfo =
  VPCDerivedInfo'
    { _vdiSecurityGroupIds = Nothing
    , _vdiSubnetIds = Nothing
    , _vdiVPCId = Nothing
    , _vdiAvailabilityZones = Nothing
    }


-- | Specifies the security groups for VPC endpoint.
vdiSecurityGroupIds :: Lens' VPCDerivedInfo [Text]
vdiSecurityGroupIds = lens _vdiSecurityGroupIds (\ s a -> s{_vdiSecurityGroupIds = a}) . _Default . _Coerce

-- | Specifies the subnets for VPC endpoint.
vdiSubnetIds :: Lens' VPCDerivedInfo [Text]
vdiSubnetIds = lens _vdiSubnetIds (\ s a -> s{_vdiSubnetIds = a}) . _Default . _Coerce

-- | The VPC Id for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
vdiVPCId :: Lens' VPCDerivedInfo (Maybe Text)
vdiVPCId = lens _vdiVPCId (\ s a -> s{_vdiVPCId = a})

-- | The availability zones for the Elasticsearch domain. Exists only if the domain was created with VPCOptions.
vdiAvailabilityZones :: Lens' VPCDerivedInfo [Text]
vdiAvailabilityZones = lens _vdiAvailabilityZones (\ s a -> s{_vdiAvailabilityZones = a}) . _Default . _Coerce

instance FromJSON VPCDerivedInfo where
        parseJSON
          = withObject "VPCDerivedInfo"
              (\ x ->
                 VPCDerivedInfo' <$>
                   (x .:? "SecurityGroupIds" .!= mempty) <*>
                     (x .:? "SubnetIds" .!= mempty)
                     <*> (x .:? "VPCId")
                     <*> (x .:? "AvailabilityZones" .!= mempty))

instance Hashable VPCDerivedInfo where

instance NFData VPCDerivedInfo where

-- | Status of the VPC options for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'vpcDerivedInfoStatus' smart constructor.
data VPCDerivedInfoStatus = VPCDerivedInfoStatus'
  { _vdisOptions :: !VPCDerivedInfo
  , _vdisStatus  :: !OptionStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCDerivedInfoStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdisOptions' - Specifies the VPC options for the specified Elasticsearch domain.
--
-- * 'vdisStatus' - Specifies the status of the VPC options for the specified Elasticsearch domain.
vpcDerivedInfoStatus
    :: VPCDerivedInfo -- ^ 'vdisOptions'
    -> OptionStatus -- ^ 'vdisStatus'
    -> VPCDerivedInfoStatus
vpcDerivedInfoStatus pOptions_ pStatus_ =
  VPCDerivedInfoStatus' {_vdisOptions = pOptions_, _vdisStatus = pStatus_}


-- | Specifies the VPC options for the specified Elasticsearch domain.
vdisOptions :: Lens' VPCDerivedInfoStatus VPCDerivedInfo
vdisOptions = lens _vdisOptions (\ s a -> s{_vdisOptions = a})

-- | Specifies the status of the VPC options for the specified Elasticsearch domain.
vdisStatus :: Lens' VPCDerivedInfoStatus OptionStatus
vdisStatus = lens _vdisStatus (\ s a -> s{_vdisStatus = a})

instance FromJSON VPCDerivedInfoStatus where
        parseJSON
          = withObject "VPCDerivedInfoStatus"
              (\ x ->
                 VPCDerivedInfoStatus' <$>
                   (x .: "Options") <*> (x .: "Status"))

instance Hashable VPCDerivedInfoStatus where

instance NFData VPCDerivedInfoStatus where

-- | Options to specify the subnets and security groups for VPC endpoint. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-vpc.html VPC Endpoints for Amazon Elasticsearch Service Domains> .
--
--
--
-- /See:/ 'vpcOptions' smart constructor.
data VPCOptions = VPCOptions'
  { _voSecurityGroupIds :: !(Maybe [Text])
  , _voSubnetIds        :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'voSecurityGroupIds' - Specifies the security groups for VPC endpoint.
--
-- * 'voSubnetIds' - Specifies the subnets for VPC endpoint.
vpcOptions
    :: VPCOptions
vpcOptions = VPCOptions' {_voSecurityGroupIds = Nothing, _voSubnetIds = Nothing}


-- | Specifies the security groups for VPC endpoint.
voSecurityGroupIds :: Lens' VPCOptions [Text]
voSecurityGroupIds = lens _voSecurityGroupIds (\ s a -> s{_voSecurityGroupIds = a}) . _Default . _Coerce

-- | Specifies the subnets for VPC endpoint.
voSubnetIds :: Lens' VPCOptions [Text]
voSubnetIds = lens _voSubnetIds (\ s a -> s{_voSubnetIds = a}) . _Default . _Coerce

instance Hashable VPCOptions where

instance NFData VPCOptions where

instance ToJSON VPCOptions where
        toJSON VPCOptions'{..}
          = object
              (catMaybes
                 [("SecurityGroupIds" .=) <$> _voSecurityGroupIds,
                  ("SubnetIds" .=) <$> _voSubnetIds])
