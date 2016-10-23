{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.Product where

import           Network.AWS.ElasticSearch.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | The configured access rules for the domain\'s document and search endpoints, and the current status of those rules.
--
-- /See:/ 'accessPoliciesStatus' smart constructor.
data AccessPoliciesStatus = AccessPoliciesStatus'
    { _apsOptions :: !Text
    , _apsStatus  :: !OptionStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccessPoliciesStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apsOptions'
--
-- * 'apsStatus'
accessPoliciesStatus
    :: Text -- ^ 'apsOptions'
    -> OptionStatus -- ^ 'apsStatus'
    -> AccessPoliciesStatus
accessPoliciesStatus pOptions_ pStatus_ =
    AccessPoliciesStatus'
    { _apsOptions = pOptions_
    , _apsStatus = pStatus_
    }

-- | The access policy configured for the Elasticsearch domain. Access policies may be resource-based, IP-based, or IAM-based. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-access-policies Configuring Access Policies>for more information.
apsOptions :: Lens' AccessPoliciesStatus Text
apsOptions = lens _apsOptions (\ s a -> s{_apsOptions = a});

-- | The status of the access policy for the Elasticsearch domain. See 'OptionStatus' for the status information that\'s included.
apsStatus :: Lens' AccessPoliciesStatus OptionStatus
apsStatus = lens _apsStatus (\ s a -> s{_apsStatus = a});

instance FromJSON AccessPoliciesStatus where
        parseJSON
          = withObject "AccessPoliciesStatus"
              (\ x ->
                 AccessPoliciesStatus' <$>
                   (x .: "Options") <*> (x .: "Status"))

instance Hashable AccessPoliciesStatus

instance NFData AccessPoliciesStatus

-- | Status of the advanced options for the specified Elasticsearch domain. Currently, the following advanced options are available:
--
-- -   Option to allow references to indices in an HTTP request body. Must be 'false' when configuring access to individual sub-resources. By default, the value is 'true'. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
-- -   Option to specify the percentage of heap space that is allocated to field data. By default, this setting is unbounded.
--
-- For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options>.
--
-- /See:/ 'advancedOptionsStatus' smart constructor.
data AdvancedOptionsStatus = AdvancedOptionsStatus'
    { _aosOptions :: !(Map Text Text)
    , _aosStatus  :: !OptionStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AdvancedOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aosOptions'
--
-- * 'aosStatus'
advancedOptionsStatus
    :: OptionStatus -- ^ 'aosStatus'
    -> AdvancedOptionsStatus
advancedOptionsStatus pStatus_ =
    AdvancedOptionsStatus'
    { _aosOptions = mempty
    , _aosStatus = pStatus_
    }

-- | Specifies the status of advanced options for the specified Elasticsearch domain.
aosOptions :: Lens' AdvancedOptionsStatus (HashMap Text Text)
aosOptions = lens _aosOptions (\ s a -> s{_aosOptions = a}) . _Map;

-- | Specifies the status of 'OptionStatus' for advanced options for the specified Elasticsearch domain.
aosStatus :: Lens' AdvancedOptionsStatus OptionStatus
aosStatus = lens _aosStatus (\ s a -> s{_aosStatus = a});

instance FromJSON AdvancedOptionsStatus where
        parseJSON
          = withObject "AdvancedOptionsStatus"
              (\ x ->
                 AdvancedOptionsStatus' <$>
                   (x .:? "Options" .!= mempty) <*> (x .: "Status"))

instance Hashable AdvancedOptionsStatus

instance NFData AdvancedOptionsStatus

-- | /See:/ 'domainInfo' smart constructor.
newtype DomainInfo = DomainInfo'
    { _diDomainName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DomainInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diDomainName'
domainInfo
    :: DomainInfo
domainInfo =
    DomainInfo'
    { _diDomainName = Nothing
    }

-- | Specifies the 'DomainName'.
diDomainName :: Lens' DomainInfo (Maybe Text)
diDomainName = lens _diDomainName (\ s a -> s{_diDomainName = a});

instance FromJSON DomainInfo where
        parseJSON
          = withObject "DomainInfo"
              (\ x -> DomainInfo' <$> (x .:? "DomainName"))

instance Hashable DomainInfo

instance NFData DomainInfo

-- | Options to enable, disable, and specify the properties of EBS storage volumes. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage>.
--
-- /See:/ 'ebsOptions' smart constructor.
data EBSOptions = EBSOptions'
    { _eoVolumeSize :: !(Maybe Int)
    , _eoIOPS       :: !(Maybe Int)
    , _eoVolumeType :: !(Maybe VolumeType)
    , _eoEBSEnabled :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EBSOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eoVolumeSize'
--
-- * 'eoIOPS'
--
-- * 'eoVolumeType'
--
-- * 'eoEBSEnabled'
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
eoVolumeSize = lens _eoVolumeSize (\ s a -> s{_eoVolumeSize = a});

-- | Specifies the IOPD for a Provisioned IOPS EBS volume (SSD).
eoIOPS :: Lens' EBSOptions (Maybe Int)
eoIOPS = lens _eoIOPS (\ s a -> s{_eoIOPS = a});

-- | Specifies the volume type for EBS-based storage.
eoVolumeType :: Lens' EBSOptions (Maybe VolumeType)
eoVolumeType = lens _eoVolumeType (\ s a -> s{_eoVolumeType = a});

-- | Specifies whether EBS-based storage is enabled.
eoEBSEnabled :: Lens' EBSOptions (Maybe Bool)
eoEBSEnabled = lens _eoEBSEnabled (\ s a -> s{_eoEBSEnabled = a});

instance FromJSON EBSOptions where
        parseJSON
          = withObject "EBSOptions"
              (\ x ->
                 EBSOptions' <$>
                   (x .:? "VolumeSize") <*> (x .:? "Iops") <*>
                     (x .:? "VolumeType")
                     <*> (x .:? "EBSEnabled"))

instance Hashable EBSOptions

instance NFData EBSOptions

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
-- /See:/ 'ebsOptionsStatus' smart constructor.
data EBSOptionsStatus = EBSOptionsStatus'
    { _eosOptions :: !EBSOptions
    , _eosStatus  :: !OptionStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EBSOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eosOptions'
--
-- * 'eosStatus'
ebsOptionsStatus
    :: EBSOptions -- ^ 'eosOptions'
    -> OptionStatus -- ^ 'eosStatus'
    -> EBSOptionsStatus
ebsOptionsStatus pOptions_ pStatus_ =
    EBSOptionsStatus'
    { _eosOptions = pOptions_
    , _eosStatus = pStatus_
    }

-- | Specifies the EBS options for the specified Elasticsearch domain.
eosOptions :: Lens' EBSOptionsStatus EBSOptions
eosOptions = lens _eosOptions (\ s a -> s{_eosOptions = a});

-- | Specifies the status of the EBS options for the specified Elasticsearch domain.
eosStatus :: Lens' EBSOptionsStatus OptionStatus
eosStatus = lens _eosStatus (\ s a -> s{_eosStatus = a});

instance FromJSON EBSOptionsStatus where
        parseJSON
          = withObject "EBSOptionsStatus"
              (\ x ->
                 EBSOptionsStatus' <$>
                   (x .: "Options") <*> (x .: "Status"))

instance Hashable EBSOptionsStatus

instance NFData EBSOptionsStatus

-- | Specifies the configuration for the domain cluster, such as the type and number of instances.
--
-- /See:/ 'elasticsearchClusterConfig' smart constructor.
data ElasticsearchClusterConfig = ElasticsearchClusterConfig'
    { _eccDedicatedMasterCount   :: !(Maybe Int)
    , _eccDedicatedMasterType    :: !(Maybe ESPartitionInstanceType)
    , _eccDedicatedMasterEnabled :: !(Maybe Bool)
    , _eccInstanceCount          :: !(Maybe Int)
    , _eccZoneAwarenessEnabled   :: !(Maybe Bool)
    , _eccInstanceType           :: !(Maybe ESPartitionInstanceType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ElasticsearchClusterConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eccDedicatedMasterCount'
--
-- * 'eccDedicatedMasterType'
--
-- * 'eccDedicatedMasterEnabled'
--
-- * 'eccInstanceCount'
--
-- * 'eccZoneAwarenessEnabled'
--
-- * 'eccInstanceType'
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
eccDedicatedMasterCount = lens _eccDedicatedMasterCount (\ s a -> s{_eccDedicatedMasterCount = a});

-- | The instance type for a dedicated master node.
eccDedicatedMasterType :: Lens' ElasticsearchClusterConfig (Maybe ESPartitionInstanceType)
eccDedicatedMasterType = lens _eccDedicatedMasterType (\ s a -> s{_eccDedicatedMasterType = a});

-- | A boolean value to indicate whether a dedicated master node is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-dedicatedmasternodes About Dedicated Master Nodes> for more information.
eccDedicatedMasterEnabled :: Lens' ElasticsearchClusterConfig (Maybe Bool)
eccDedicatedMasterEnabled = lens _eccDedicatedMasterEnabled (\ s a -> s{_eccDedicatedMasterEnabled = a});

-- | The number of instances in the specified domain cluster.
eccInstanceCount :: Lens' ElasticsearchClusterConfig (Maybe Int)
eccInstanceCount = lens _eccInstanceCount (\ s a -> s{_eccInstanceCount = a});

-- | A boolean value to indicate whether zone awareness is enabled. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-zoneawareness About Zone Awareness> for more information.
eccZoneAwarenessEnabled :: Lens' ElasticsearchClusterConfig (Maybe Bool)
eccZoneAwarenessEnabled = lens _eccZoneAwarenessEnabled (\ s a -> s{_eccZoneAwarenessEnabled = a});

-- | The instance type for an Elasticsearch cluster.
eccInstanceType :: Lens' ElasticsearchClusterConfig (Maybe ESPartitionInstanceType)
eccInstanceType = lens _eccInstanceType (\ s a -> s{_eccInstanceType = a});

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

instance Hashable ElasticsearchClusterConfig

instance NFData ElasticsearchClusterConfig

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
-- /See:/ 'elasticsearchClusterConfigStatus' smart constructor.
data ElasticsearchClusterConfigStatus = ElasticsearchClusterConfigStatus'
    { _eccsOptions :: !ElasticsearchClusterConfig
    , _eccsStatus  :: !OptionStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ElasticsearchClusterConfigStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eccsOptions'
--
-- * 'eccsStatus'
elasticsearchClusterConfigStatus
    :: ElasticsearchClusterConfig -- ^ 'eccsOptions'
    -> OptionStatus -- ^ 'eccsStatus'
    -> ElasticsearchClusterConfigStatus
elasticsearchClusterConfigStatus pOptions_ pStatus_ =
    ElasticsearchClusterConfigStatus'
    { _eccsOptions = pOptions_
    , _eccsStatus = pStatus_
    }

-- | Specifies the cluster configuration for the specified Elasticsearch domain.
eccsOptions :: Lens' ElasticsearchClusterConfigStatus ElasticsearchClusterConfig
eccsOptions = lens _eccsOptions (\ s a -> s{_eccsOptions = a});

-- | Specifies the status of the configuration for the specified Elasticsearch domain.
eccsStatus :: Lens' ElasticsearchClusterConfigStatus OptionStatus
eccsStatus = lens _eccsStatus (\ s a -> s{_eccsStatus = a});

instance FromJSON ElasticsearchClusterConfigStatus
         where
        parseJSON
          = withObject "ElasticsearchClusterConfigStatus"
              (\ x ->
                 ElasticsearchClusterConfigStatus' <$>
                   (x .: "Options") <*> (x .: "Status"))

instance Hashable ElasticsearchClusterConfigStatus

instance NFData ElasticsearchClusterConfigStatus

-- | The configuration of an Elasticsearch domain.
--
-- /See:/ 'elasticsearchDomainConfig' smart constructor.
data ElasticsearchDomainConfig = ElasticsearchDomainConfig'
    { _edcEBSOptions                 :: !(Maybe EBSOptionsStatus)
    , _edcAccessPolicies             :: !(Maybe AccessPoliciesStatus)
    , _edcElasticsearchClusterConfig :: !(Maybe ElasticsearchClusterConfigStatus)
    , _edcSnapshotOptions            :: !(Maybe SnapshotOptionsStatus)
    , _edcAdvancedOptions            :: !(Maybe AdvancedOptionsStatus)
    , _edcElasticsearchVersion       :: !(Maybe ElasticsearchVersionStatus)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ElasticsearchDomainConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edcEBSOptions'
--
-- * 'edcAccessPolicies'
--
-- * 'edcElasticsearchClusterConfig'
--
-- * 'edcSnapshotOptions'
--
-- * 'edcAdvancedOptions'
--
-- * 'edcElasticsearchVersion'
elasticsearchDomainConfig
    :: ElasticsearchDomainConfig
elasticsearchDomainConfig =
    ElasticsearchDomainConfig'
    { _edcEBSOptions = Nothing
    , _edcAccessPolicies = Nothing
    , _edcElasticsearchClusterConfig = Nothing
    , _edcSnapshotOptions = Nothing
    , _edcAdvancedOptions = Nothing
    , _edcElasticsearchVersion = Nothing
    }

-- | Specifies the 'EBSOptions' for the Elasticsearch domain.
edcEBSOptions :: Lens' ElasticsearchDomainConfig (Maybe EBSOptionsStatus)
edcEBSOptions = lens _edcEBSOptions (\ s a -> s{_edcEBSOptions = a});

-- | IAM access policy as a JSON-formatted string.
edcAccessPolicies :: Lens' ElasticsearchDomainConfig (Maybe AccessPoliciesStatus)
edcAccessPolicies = lens _edcAccessPolicies (\ s a -> s{_edcAccessPolicies = a});

-- | Specifies the 'ElasticsearchClusterConfig' for the Elasticsearch domain.
edcElasticsearchClusterConfig :: Lens' ElasticsearchDomainConfig (Maybe ElasticsearchClusterConfigStatus)
edcElasticsearchClusterConfig = lens _edcElasticsearchClusterConfig (\ s a -> s{_edcElasticsearchClusterConfig = a});

-- | Specifies the 'SnapshotOptions' for the Elasticsearch domain.
edcSnapshotOptions :: Lens' ElasticsearchDomainConfig (Maybe SnapshotOptionsStatus)
edcSnapshotOptions = lens _edcSnapshotOptions (\ s a -> s{_edcSnapshotOptions = a});

-- | Specifies the 'AdvancedOptions' for the domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options> for more information.
edcAdvancedOptions :: Lens' ElasticsearchDomainConfig (Maybe AdvancedOptionsStatus)
edcAdvancedOptions = lens _edcAdvancedOptions (\ s a -> s{_edcAdvancedOptions = a});

-- | String of format X.Y to specify version for the Elasticsearch domain.
edcElasticsearchVersion :: Lens' ElasticsearchDomainConfig (Maybe ElasticsearchVersionStatus)
edcElasticsearchVersion = lens _edcElasticsearchVersion (\ s a -> s{_edcElasticsearchVersion = a});

instance FromJSON ElasticsearchDomainConfig where
        parseJSON
          = withObject "ElasticsearchDomainConfig"
              (\ x ->
                 ElasticsearchDomainConfig' <$>
                   (x .:? "EBSOptions") <*> (x .:? "AccessPolicies") <*>
                     (x .:? "ElasticsearchClusterConfig")
                     <*> (x .:? "SnapshotOptions")
                     <*> (x .:? "AdvancedOptions")
                     <*> (x .:? "ElasticsearchVersion"))

instance Hashable ElasticsearchDomainConfig

instance NFData ElasticsearchDomainConfig

-- | The current status of an Elasticsearch domain.
--
-- /See:/ 'elasticsearchDomainStatus' smart constructor.
data ElasticsearchDomainStatus = ElasticsearchDomainStatus'
    { _edsEBSOptions                 :: !(Maybe EBSOptions)
    , _edsAccessPolicies             :: !(Maybe Text)
    , _edsCreated                    :: !(Maybe Bool)
    , _edsSnapshotOptions            :: !(Maybe SnapshotOptions)
    , _edsDeleted                    :: !(Maybe Bool)
    , _edsProcessing                 :: !(Maybe Bool)
    , _edsEndpoint                   :: !(Maybe Text)
    , _edsAdvancedOptions            :: !(Maybe (Map Text Text))
    , _edsElasticsearchVersion       :: !(Maybe Text)
    , _edsDomainId                   :: !Text
    , _edsDomainName                 :: !Text
    , _edsARN                        :: !Text
    , _edsElasticsearchClusterConfig :: !ElasticsearchClusterConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ElasticsearchDomainStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edsEBSOptions'
--
-- * 'edsAccessPolicies'
--
-- * 'edsCreated'
--
-- * 'edsSnapshotOptions'
--
-- * 'edsDeleted'
--
-- * 'edsProcessing'
--
-- * 'edsEndpoint'
--
-- * 'edsAdvancedOptions'
--
-- * 'edsElasticsearchVersion'
--
-- * 'edsDomainId'
--
-- * 'edsDomainName'
--
-- * 'edsARN'
--
-- * 'edsElasticsearchClusterConfig'
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
    , _edsCreated = Nothing
    , _edsSnapshotOptions = Nothing
    , _edsDeleted = Nothing
    , _edsProcessing = Nothing
    , _edsEndpoint = Nothing
    , _edsAdvancedOptions = Nothing
    , _edsElasticsearchVersion = Nothing
    , _edsDomainId = pDomainId_
    , _edsDomainName = pDomainName_
    , _edsARN = pARN_
    , _edsElasticsearchClusterConfig = pElasticsearchClusterConfig_
    }

-- | The 'EBSOptions' for the specified domain. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> for more information.
edsEBSOptions :: Lens' ElasticsearchDomainStatus (Maybe EBSOptions)
edsEBSOptions = lens _edsEBSOptions (\ s a -> s{_edsEBSOptions = a});

-- | IAM access policy as a JSON-formatted string.
edsAccessPolicies :: Lens' ElasticsearchDomainStatus (Maybe Text)
edsAccessPolicies = lens _edsAccessPolicies (\ s a -> s{_edsAccessPolicies = a});

-- | The domain creation status. 'True' if the creation of an Elasticsearch domain is complete. 'False' if domain creation is still in progress.
edsCreated :: Lens' ElasticsearchDomainStatus (Maybe Bool)
edsCreated = lens _edsCreated (\ s a -> s{_edsCreated = a});

-- | Specifies the status of the 'SnapshotOptions'
edsSnapshotOptions :: Lens' ElasticsearchDomainStatus (Maybe SnapshotOptions)
edsSnapshotOptions = lens _edsSnapshotOptions (\ s a -> s{_edsSnapshotOptions = a});

-- | The domain deletion status. 'True' if a delete request has been received for the domain but resource cleanup is still in progress. 'False' if the domain has not been deleted. Once domain deletion is complete, the status of the domain is no longer returned.
edsDeleted :: Lens' ElasticsearchDomainStatus (Maybe Bool)
edsDeleted = lens _edsDeleted (\ s a -> s{_edsDeleted = a});

-- | The status of the Elasticsearch domain configuration. 'True' if Amazon Elasticsearch Service is processing configuration changes. 'False' if the configuration is active.
edsProcessing :: Lens' ElasticsearchDomainStatus (Maybe Bool)
edsProcessing = lens _edsProcessing (\ s a -> s{_edsProcessing = a});

-- | The Elasticsearch domain endpoint that you use to submit index and search requests.
edsEndpoint :: Lens' ElasticsearchDomainStatus (Maybe Text)
edsEndpoint = lens _edsEndpoint (\ s a -> s{_edsEndpoint = a});

-- | Specifies the status of the 'AdvancedOptions'
edsAdvancedOptions :: Lens' ElasticsearchDomainStatus (HashMap Text Text)
edsAdvancedOptions = lens _edsAdvancedOptions (\ s a -> s{_edsAdvancedOptions = a}) . _Default . _Map;

-- | Undocumented member.
edsElasticsearchVersion :: Lens' ElasticsearchDomainStatus (Maybe Text)
edsElasticsearchVersion = lens _edsElasticsearchVersion (\ s a -> s{_edsElasticsearchVersion = a});

-- | The unique identifier for the specified Elasticsearch domain.
edsDomainId :: Lens' ElasticsearchDomainStatus Text
edsDomainId = lens _edsDomainId (\ s a -> s{_edsDomainId = a});

-- | The name of an Elasticsearch domain. Domain names are unique across the domains owned by an account within an AWS region. Domain names start with a letter or number and can contain the following characters: a-z (lowercase), 0-9, and - (hyphen).
edsDomainName :: Lens' ElasticsearchDomainStatus Text
edsDomainName = lens _edsDomainName (\ s a -> s{_edsDomainName = a});

-- | The Amazon resource name (ARN) of an Elasticsearch domain. See <http://docs.aws.amazon.com/IAM/latest/UserGuide/index.html?Using_Identifiers.html Identifiers for IAM Entities> in /Using AWS Identity and Access Management/ for more information.
edsARN :: Lens' ElasticsearchDomainStatus Text
edsARN = lens _edsARN (\ s a -> s{_edsARN = a});

-- | The type and number of instances in the domain cluster.
edsElasticsearchClusterConfig :: Lens' ElasticsearchDomainStatus ElasticsearchClusterConfig
edsElasticsearchClusterConfig = lens _edsElasticsearchClusterConfig (\ s a -> s{_edsElasticsearchClusterConfig = a});

instance FromJSON ElasticsearchDomainStatus where
        parseJSON
          = withObject "ElasticsearchDomainStatus"
              (\ x ->
                 ElasticsearchDomainStatus' <$>
                   (x .:? "EBSOptions") <*> (x .:? "AccessPolicies") <*>
                     (x .:? "Created")
                     <*> (x .:? "SnapshotOptions")
                     <*> (x .:? "Deleted")
                     <*> (x .:? "Processing")
                     <*> (x .:? "Endpoint")
                     <*> (x .:? "AdvancedOptions" .!= mempty)
                     <*> (x .:? "ElasticsearchVersion")
                     <*> (x .: "DomainId")
                     <*> (x .: "DomainName")
                     <*> (x .: "ARN")
                     <*> (x .: "ElasticsearchClusterConfig"))

instance Hashable ElasticsearchDomainStatus

instance NFData ElasticsearchDomainStatus

-- | Status of the Elasticsearch version options for the specified Elasticsearch domain.
--
-- /See:/ 'elasticsearchVersionStatus' smart constructor.
data ElasticsearchVersionStatus = ElasticsearchVersionStatus'
    { _evsOptions :: !Text
    , _evsStatus  :: !OptionStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ElasticsearchVersionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evsOptions'
--
-- * 'evsStatus'
elasticsearchVersionStatus
    :: Text -- ^ 'evsOptions'
    -> OptionStatus -- ^ 'evsStatus'
    -> ElasticsearchVersionStatus
elasticsearchVersionStatus pOptions_ pStatus_ =
    ElasticsearchVersionStatus'
    { _evsOptions = pOptions_
    , _evsStatus = pStatus_
    }

-- | Specifies the Elasticsearch version for the specified Elasticsearch domain.
evsOptions :: Lens' ElasticsearchVersionStatus Text
evsOptions = lens _evsOptions (\ s a -> s{_evsOptions = a});

-- | Specifies the status of the Elasticsearch version options for the specified Elasticsearch domain.
evsStatus :: Lens' ElasticsearchVersionStatus OptionStatus
evsStatus = lens _evsStatus (\ s a -> s{_evsStatus = a});

instance FromJSON ElasticsearchVersionStatus where
        parseJSON
          = withObject "ElasticsearchVersionStatus"
              (\ x ->
                 ElasticsearchVersionStatus' <$>
                   (x .: "Options") <*> (x .: "Status"))

instance Hashable ElasticsearchVersionStatus

instance NFData ElasticsearchVersionStatus

-- | Provides the current status of the entity.
--
-- /See:/ 'optionStatus' smart constructor.
data OptionStatus = OptionStatus'
    { _osPendingDeletion :: !(Maybe Bool)
    , _osUpdateVersion   :: !(Maybe Nat)
    , _osCreationDate    :: !POSIX
    , _osUpdateDate      :: !POSIX
    , _osState           :: !OptionState
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OptionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osPendingDeletion'
--
-- * 'osUpdateVersion'
--
-- * 'osCreationDate'
--
-- * 'osUpdateDate'
--
-- * 'osState'
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
osPendingDeletion = lens _osPendingDeletion (\ s a -> s{_osPendingDeletion = a});

-- | Specifies the latest version for the entity.
osUpdateVersion :: Lens' OptionStatus (Maybe Natural)
osUpdateVersion = lens _osUpdateVersion (\ s a -> s{_osUpdateVersion = a}) . mapping _Nat;

-- | Timestamp which tells the creation date for the entity.
osCreationDate :: Lens' OptionStatus UTCTime
osCreationDate = lens _osCreationDate (\ s a -> s{_osCreationDate = a}) . _Time;

-- | Timestamp which tells the last updated time for the entity.
osUpdateDate :: Lens' OptionStatus UTCTime
osUpdateDate = lens _osUpdateDate (\ s a -> s{_osUpdateDate = a}) . _Time;

-- | Provides the 'OptionState' for the Elasticsearch domain.
osState :: Lens' OptionStatus OptionState
osState = lens _osState (\ s a -> s{_osState = a});

instance FromJSON OptionStatus where
        parseJSON
          = withObject "OptionStatus"
              (\ x ->
                 OptionStatus' <$>
                   (x .:? "PendingDeletion") <*> (x .:? "UpdateVersion")
                     <*> (x .: "CreationDate")
                     <*> (x .: "UpdateDate")
                     <*> (x .: "State"))

instance Hashable OptionStatus

instance NFData OptionStatus

-- | Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is '0' hours.
--
-- /See:/ 'snapshotOptions' smart constructor.
newtype SnapshotOptions = SnapshotOptions'
    { _soAutomatedSnapshotStartHour :: Maybe Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SnapshotOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'soAutomatedSnapshotStartHour'
snapshotOptions
    :: SnapshotOptions
snapshotOptions =
    SnapshotOptions'
    { _soAutomatedSnapshotStartHour = Nothing
    }

-- | Specifies the time, in UTC format, when the service takes a daily automated snapshot of the specified Elasticsearch domain. Default value is '0' hours.
soAutomatedSnapshotStartHour :: Lens' SnapshotOptions (Maybe Int)
soAutomatedSnapshotStartHour = lens _soAutomatedSnapshotStartHour (\ s a -> s{_soAutomatedSnapshotStartHour = a});

instance FromJSON SnapshotOptions where
        parseJSON
          = withObject "SnapshotOptions"
              (\ x ->
                 SnapshotOptions' <$>
                   (x .:? "AutomatedSnapshotStartHour"))

instance Hashable SnapshotOptions

instance NFData SnapshotOptions

instance ToJSON SnapshotOptions where
        toJSON SnapshotOptions'{..}
          = object
              (catMaybes
                 [("AutomatedSnapshotStartHour" .=) <$>
                    _soAutomatedSnapshotStartHour])

-- | Status of a daily automated snapshot.
--
-- /See:/ 'snapshotOptionsStatus' smart constructor.
data SnapshotOptionsStatus = SnapshotOptionsStatus'
    { _sosOptions :: !SnapshotOptions
    , _sosStatus  :: !OptionStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SnapshotOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sosOptions'
--
-- * 'sosStatus'
snapshotOptionsStatus
    :: SnapshotOptions -- ^ 'sosOptions'
    -> OptionStatus -- ^ 'sosStatus'
    -> SnapshotOptionsStatus
snapshotOptionsStatus pOptions_ pStatus_ =
    SnapshotOptionsStatus'
    { _sosOptions = pOptions_
    , _sosStatus = pStatus_
    }

-- | Specifies the daily snapshot options specified for the Elasticsearch domain.
sosOptions :: Lens' SnapshotOptionsStatus SnapshotOptions
sosOptions = lens _sosOptions (\ s a -> s{_sosOptions = a});

-- | Specifies the status of a daily automated snapshot.
sosStatus :: Lens' SnapshotOptionsStatus OptionStatus
sosStatus = lens _sosStatus (\ s a -> s{_sosStatus = a});

instance FromJSON SnapshotOptionsStatus where
        parseJSON
          = withObject "SnapshotOptionsStatus"
              (\ x ->
                 SnapshotOptionsStatus' <$>
                   (x .: "Options") <*> (x .: "Status"))

instance Hashable SnapshotOptionsStatus

instance NFData SnapshotOptionsStatus

-- | Specifies a key value pair for a resource tag.
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagKey   :: !Text
    , _tagValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey'
--
-- * 'tagValue'
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ =
    Tag'
    { _tagKey = pKey_
    , _tagValue = pValue_
    }

-- | Specifies the 'TagKey', the name of the tag. Tag keys must be unique for the Elasticsearch domain to which they are attached.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

-- | Specifies the 'TagValue', the value assigned to the corresponding tag key. Tag values can be null and do not have to be unique in a tag set. For example, you can have a key value pair in a tag set of 'project : Trinity' and 'cost-center : Trinity'
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag

instance NFData Tag

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])
