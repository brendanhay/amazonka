{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.Sum

-- | Information about the application.
--
--
--
-- /See:/ 'appSummary' smart constructor.
data AppSummary = AppSummary'
  { _asCreationTime             :: !(Maybe POSIX)
  , _asTotalServers             :: !(Maybe Int)
  , _asStatus                   :: !(Maybe AppStatus)
  , _asLaunchDetails            :: !(Maybe LaunchDetails)
  , _asLaunchStatusMessage      :: !(Maybe Text)
  , _asReplicationStatusMessage :: !(Maybe Text)
  , _asTotalServerGroups        :: !(Maybe Int)
  , _asRoleName                 :: !(Maybe Text)
  , _asLaunchStatus             :: !(Maybe AppLaunchStatus)
  , _asAppId                    :: !(Maybe Text)
  , _asName                     :: !(Maybe Text)
  , _asStatusMessage            :: !(Maybe Text)
  , _asLatestReplicationTime    :: !(Maybe POSIX)
  , _asReplicationStatus        :: !(Maybe AppReplicationStatus)
  , _asLastModified             :: !(Maybe POSIX)
  , _asDescription              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AppSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asCreationTime' - Time of creation of this application.
--
-- * 'asTotalServers' - Number of servers present in the application.
--
-- * 'asStatus' - Status of the application.
--
-- * 'asLaunchDetails' - Details about the latest launch of the application.
--
-- * 'asLaunchStatusMessage' - A message related to the launch status of the application.
--
-- * 'asReplicationStatusMessage' - A message related to the replication status of the application.
--
-- * 'asTotalServerGroups' - Number of server groups present in the application.
--
-- * 'asRoleName' - Name of the service role in the customer's account used by AWS SMS.
--
-- * 'asLaunchStatus' - Launch status of the application.
--
-- * 'asAppId' - Unique ID of the application.
--
-- * 'asName' - Name of the application.
--
-- * 'asStatusMessage' - A message related to the status of the application
--
-- * 'asLatestReplicationTime' - Timestamp of the application's most recent successful replication.
--
-- * 'asReplicationStatus' - Replication status of the application.
--
-- * 'asLastModified' - Timestamp of the application's creation.
--
-- * 'asDescription' - Description of the application.
appSummary
    :: AppSummary
appSummary =
  AppSummary'
    { _asCreationTime = Nothing
    , _asTotalServers = Nothing
    , _asStatus = Nothing
    , _asLaunchDetails = Nothing
    , _asLaunchStatusMessage = Nothing
    , _asReplicationStatusMessage = Nothing
    , _asTotalServerGroups = Nothing
    , _asRoleName = Nothing
    , _asLaunchStatus = Nothing
    , _asAppId = Nothing
    , _asName = Nothing
    , _asStatusMessage = Nothing
    , _asLatestReplicationTime = Nothing
    , _asReplicationStatus = Nothing
    , _asLastModified = Nothing
    , _asDescription = Nothing
    }


-- | Time of creation of this application.
asCreationTime :: Lens' AppSummary (Maybe UTCTime)
asCreationTime = lens _asCreationTime (\ s a -> s{_asCreationTime = a}) . mapping _Time

-- | Number of servers present in the application.
asTotalServers :: Lens' AppSummary (Maybe Int)
asTotalServers = lens _asTotalServers (\ s a -> s{_asTotalServers = a})

-- | Status of the application.
asStatus :: Lens' AppSummary (Maybe AppStatus)
asStatus = lens _asStatus (\ s a -> s{_asStatus = a})

-- | Details about the latest launch of the application.
asLaunchDetails :: Lens' AppSummary (Maybe LaunchDetails)
asLaunchDetails = lens _asLaunchDetails (\ s a -> s{_asLaunchDetails = a})

-- | A message related to the launch status of the application.
asLaunchStatusMessage :: Lens' AppSummary (Maybe Text)
asLaunchStatusMessage = lens _asLaunchStatusMessage (\ s a -> s{_asLaunchStatusMessage = a})

-- | A message related to the replication status of the application.
asReplicationStatusMessage :: Lens' AppSummary (Maybe Text)
asReplicationStatusMessage = lens _asReplicationStatusMessage (\ s a -> s{_asReplicationStatusMessage = a})

-- | Number of server groups present in the application.
asTotalServerGroups :: Lens' AppSummary (Maybe Int)
asTotalServerGroups = lens _asTotalServerGroups (\ s a -> s{_asTotalServerGroups = a})

-- | Name of the service role in the customer's account used by AWS SMS.
asRoleName :: Lens' AppSummary (Maybe Text)
asRoleName = lens _asRoleName (\ s a -> s{_asRoleName = a})

-- | Launch status of the application.
asLaunchStatus :: Lens' AppSummary (Maybe AppLaunchStatus)
asLaunchStatus = lens _asLaunchStatus (\ s a -> s{_asLaunchStatus = a})

-- | Unique ID of the application.
asAppId :: Lens' AppSummary (Maybe Text)
asAppId = lens _asAppId (\ s a -> s{_asAppId = a})

-- | Name of the application.
asName :: Lens' AppSummary (Maybe Text)
asName = lens _asName (\ s a -> s{_asName = a})

-- | A message related to the status of the application
asStatusMessage :: Lens' AppSummary (Maybe Text)
asStatusMessage = lens _asStatusMessage (\ s a -> s{_asStatusMessage = a})

-- | Timestamp of the application's most recent successful replication.
asLatestReplicationTime :: Lens' AppSummary (Maybe UTCTime)
asLatestReplicationTime = lens _asLatestReplicationTime (\ s a -> s{_asLatestReplicationTime = a}) . mapping _Time

-- | Replication status of the application.
asReplicationStatus :: Lens' AppSummary (Maybe AppReplicationStatus)
asReplicationStatus = lens _asReplicationStatus (\ s a -> s{_asReplicationStatus = a})

-- | Timestamp of the application's creation.
asLastModified :: Lens' AppSummary (Maybe UTCTime)
asLastModified = lens _asLastModified (\ s a -> s{_asLastModified = a}) . mapping _Time

-- | Description of the application.
asDescription :: Lens' AppSummary (Maybe Text)
asDescription = lens _asDescription (\ s a -> s{_asDescription = a})

instance FromJSON AppSummary where
        parseJSON
          = withObject "AppSummary"
              (\ x ->
                 AppSummary' <$>
                   (x .:? "creationTime") <*> (x .:? "totalServers") <*>
                     (x .:? "status")
                     <*> (x .:? "launchDetails")
                     <*> (x .:? "launchStatusMessage")
                     <*> (x .:? "replicationStatusMessage")
                     <*> (x .:? "totalServerGroups")
                     <*> (x .:? "roleName")
                     <*> (x .:? "launchStatus")
                     <*> (x .:? "appId")
                     <*> (x .:? "name")
                     <*> (x .:? "statusMessage")
                     <*> (x .:? "latestReplicationTime")
                     <*> (x .:? "replicationStatus")
                     <*> (x .:? "lastModified")
                     <*> (x .:? "description"))

instance Hashable AppSummary where

instance NFData AppSummary where

-- | Represents a connector.
--
--
--
-- /See:/ 'connector' smart constructor.
data Connector = Connector'
  { _cStatus         :: !(Maybe ConnectorStatus)
  , _cVmManagerName  :: !(Maybe Text)
  , _cIpAddress      :: !(Maybe Text)
  , _cVmManagerId    :: !(Maybe Text)
  , _cVmManagerType  :: !(Maybe VMManagerType)
  , _cConnectorId    :: !(Maybe Text)
  , _cAssociatedOn   :: !(Maybe POSIX)
  , _cMacAddress     :: !(Maybe Text)
  , _cVersion        :: !(Maybe Text)
  , _cCapabilityList :: !(Maybe [ConnectorCapability])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Connector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The status of the connector.
--
-- * 'cVmManagerName' - The name of the VM manager.
--
-- * 'cIpAddress' - The IP address of the connector.
--
-- * 'cVmManagerId' - The identifier of the VM manager.
--
-- * 'cVmManagerType' - The VM management product.
--
-- * 'cConnectorId' - The identifier of the connector.
--
-- * 'cAssociatedOn' - The time the connector was associated.
--
-- * 'cMacAddress' - The MAC address of the connector.
--
-- * 'cVersion' - The connector version.
--
-- * 'cCapabilityList' - The capabilities of the connector.
connector
    :: Connector
connector =
  Connector'
    { _cStatus = Nothing
    , _cVmManagerName = Nothing
    , _cIpAddress = Nothing
    , _cVmManagerId = Nothing
    , _cVmManagerType = Nothing
    , _cConnectorId = Nothing
    , _cAssociatedOn = Nothing
    , _cMacAddress = Nothing
    , _cVersion = Nothing
    , _cCapabilityList = Nothing
    }


-- | The status of the connector.
cStatus :: Lens' Connector (Maybe ConnectorStatus)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a})

-- | The name of the VM manager.
cVmManagerName :: Lens' Connector (Maybe Text)
cVmManagerName = lens _cVmManagerName (\ s a -> s{_cVmManagerName = a})

-- | The IP address of the connector.
cIpAddress :: Lens' Connector (Maybe Text)
cIpAddress = lens _cIpAddress (\ s a -> s{_cIpAddress = a})

-- | The identifier of the VM manager.
cVmManagerId :: Lens' Connector (Maybe Text)
cVmManagerId = lens _cVmManagerId (\ s a -> s{_cVmManagerId = a})

-- | The VM management product.
cVmManagerType :: Lens' Connector (Maybe VMManagerType)
cVmManagerType = lens _cVmManagerType (\ s a -> s{_cVmManagerType = a})

-- | The identifier of the connector.
cConnectorId :: Lens' Connector (Maybe Text)
cConnectorId = lens _cConnectorId (\ s a -> s{_cConnectorId = a})

-- | The time the connector was associated.
cAssociatedOn :: Lens' Connector (Maybe UTCTime)
cAssociatedOn = lens _cAssociatedOn (\ s a -> s{_cAssociatedOn = a}) . mapping _Time

-- | The MAC address of the connector.
cMacAddress :: Lens' Connector (Maybe Text)
cMacAddress = lens _cMacAddress (\ s a -> s{_cMacAddress = a})

-- | The connector version.
cVersion :: Lens' Connector (Maybe Text)
cVersion = lens _cVersion (\ s a -> s{_cVersion = a})

-- | The capabilities of the connector.
cCapabilityList :: Lens' Connector [ConnectorCapability]
cCapabilityList = lens _cCapabilityList (\ s a -> s{_cCapabilityList = a}) . _Default . _Coerce

instance FromJSON Connector where
        parseJSON
          = withObject "Connector"
              (\ x ->
                 Connector' <$>
                   (x .:? "status") <*> (x .:? "vmManagerName") <*>
                     (x .:? "ipAddress")
                     <*> (x .:? "vmManagerId")
                     <*> (x .:? "vmManagerType")
                     <*> (x .:? "connectorId")
                     <*> (x .:? "associatedOn")
                     <*> (x .:? "macAddress")
                     <*> (x .:? "version")
                     <*> (x .:? "capabilityList" .!= mempty))

instance Hashable Connector where

instance NFData Connector where

-- | Details about the latest launch of an application.
--
--
--
-- /See:/ 'launchDetails' smart constructor.
data LaunchDetails = LaunchDetails'
  { _ldStackId          :: !(Maybe Text)
  , _ldLatestLaunchTime :: !(Maybe POSIX)
  , _ldStackName        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldStackId' - Identifier of the latest stack launched for this application.
--
-- * 'ldLatestLaunchTime' - Latest time this application was launched successfully.
--
-- * 'ldStackName' - Name of the latest stack launched for this application.
launchDetails
    :: LaunchDetails
launchDetails =
  LaunchDetails'
    { _ldStackId = Nothing
    , _ldLatestLaunchTime = Nothing
    , _ldStackName = Nothing
    }


-- | Identifier of the latest stack launched for this application.
ldStackId :: Lens' LaunchDetails (Maybe Text)
ldStackId = lens _ldStackId (\ s a -> s{_ldStackId = a})

-- | Latest time this application was launched successfully.
ldLatestLaunchTime :: Lens' LaunchDetails (Maybe UTCTime)
ldLatestLaunchTime = lens _ldLatestLaunchTime (\ s a -> s{_ldLatestLaunchTime = a}) . mapping _Time

-- | Name of the latest stack launched for this application.
ldStackName :: Lens' LaunchDetails (Maybe Text)
ldStackName = lens _ldStackName (\ s a -> s{_ldStackName = a})

instance FromJSON LaunchDetails where
        parseJSON
          = withObject "LaunchDetails"
              (\ x ->
                 LaunchDetails' <$>
                   (x .:? "stackId") <*> (x .:? "latestLaunchTime") <*>
                     (x .:? "stackName"))

instance Hashable LaunchDetails where

instance NFData LaunchDetails where

-- | Represents a replication job.
--
--
--
-- /See:/ 'replicationJob' smart constructor.
data ReplicationJob = ReplicationJob'
  { _rjFrequency                   :: !(Maybe Int)
  , _rjNumberOfRecentAMIsToKeep    :: !(Maybe Int)
  , _rjState                       :: !(Maybe ReplicationJobState)
  , _rjServerType                  :: !(Maybe ServerType)
  , _rjServerId                    :: !(Maybe Text)
  , _rjLicenseType                 :: !(Maybe LicenseType)
  , _rjRoleName                    :: !(Maybe Text)
  , _rjVmServer                    :: !(Maybe VMServer)
  , _rjEncrypted                   :: !(Maybe Bool)
  , _rjReplicationJobId            :: !(Maybe Text)
  , _rjReplicationRunList          :: !(Maybe [ReplicationRun])
  , _rjNextReplicationRunStartTime :: !(Maybe POSIX)
  , _rjStatusMessage               :: !(Maybe Text)
  , _rjKmsKeyId                    :: !(Maybe Text)
  , _rjLatestAMIId                 :: !(Maybe Text)
  , _rjSeedReplicationTime         :: !(Maybe POSIX)
  , _rjRunOnce                     :: !(Maybe Bool)
  , _rjDescription                 :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rjFrequency' - The time between consecutive replication runs, in hours.
--
-- * 'rjNumberOfRecentAMIsToKeep' - Number of recent AMIs to keep in the customer's account for a replication job. By default the value is set to zero, meaning that all AMIs are kept.
--
-- * 'rjState' - The state of the replication job.
--
-- * 'rjServerType' - The type of server.
--
-- * 'rjServerId' - The identifier of the server.
--
-- * 'rjLicenseType' - The license type to be used for the AMI created by a successful replication run.
--
-- * 'rjRoleName' - The name of the IAM role to be used by the Server Migration Service.
--
-- * 'rjVmServer' - Information about the VM server.
--
-- * 'rjEncrypted' - Whether the replication job should produce encrypted AMIs or not. See also @KmsKeyId@ below.
--
-- * 'rjReplicationJobId' - The identifier of the replication job.
--
-- * 'rjReplicationRunList' - Information about the replication runs.
--
-- * 'rjNextReplicationRunStartTime' - The start time of the next replication run.
--
-- * 'rjStatusMessage' - The description of the current status of the replication job.
--
-- * 'rjKmsKeyId' - KMS key ID for replication jobs that produce encrypted AMIs. Can be any of the following:      * KMS key ID     * KMS key alias     * ARN referring to KMS key ID     * ARN referring to KMS key alias If encrypted is /true/ but a KMS key id is not specified, the customer's default KMS key for EBS is used.
--
-- * 'rjLatestAMIId' - The ID of the latest Amazon Machine Image (AMI).
--
-- * 'rjSeedReplicationTime' - The seed replication time.
--
-- * 'rjRunOnce' -
--
-- * 'rjDescription' - The description of the replication job.
replicationJob
    :: ReplicationJob
replicationJob =
  ReplicationJob'
    { _rjFrequency = Nothing
    , _rjNumberOfRecentAMIsToKeep = Nothing
    , _rjState = Nothing
    , _rjServerType = Nothing
    , _rjServerId = Nothing
    , _rjLicenseType = Nothing
    , _rjRoleName = Nothing
    , _rjVmServer = Nothing
    , _rjEncrypted = Nothing
    , _rjReplicationJobId = Nothing
    , _rjReplicationRunList = Nothing
    , _rjNextReplicationRunStartTime = Nothing
    , _rjStatusMessage = Nothing
    , _rjKmsKeyId = Nothing
    , _rjLatestAMIId = Nothing
    , _rjSeedReplicationTime = Nothing
    , _rjRunOnce = Nothing
    , _rjDescription = Nothing
    }


-- | The time between consecutive replication runs, in hours.
rjFrequency :: Lens' ReplicationJob (Maybe Int)
rjFrequency = lens _rjFrequency (\ s a -> s{_rjFrequency = a})

-- | Number of recent AMIs to keep in the customer's account for a replication job. By default the value is set to zero, meaning that all AMIs are kept.
rjNumberOfRecentAMIsToKeep :: Lens' ReplicationJob (Maybe Int)
rjNumberOfRecentAMIsToKeep = lens _rjNumberOfRecentAMIsToKeep (\ s a -> s{_rjNumberOfRecentAMIsToKeep = a})

-- | The state of the replication job.
rjState :: Lens' ReplicationJob (Maybe ReplicationJobState)
rjState = lens _rjState (\ s a -> s{_rjState = a})

-- | The type of server.
rjServerType :: Lens' ReplicationJob (Maybe ServerType)
rjServerType = lens _rjServerType (\ s a -> s{_rjServerType = a})

-- | The identifier of the server.
rjServerId :: Lens' ReplicationJob (Maybe Text)
rjServerId = lens _rjServerId (\ s a -> s{_rjServerId = a})

-- | The license type to be used for the AMI created by a successful replication run.
rjLicenseType :: Lens' ReplicationJob (Maybe LicenseType)
rjLicenseType = lens _rjLicenseType (\ s a -> s{_rjLicenseType = a})

-- | The name of the IAM role to be used by the Server Migration Service.
rjRoleName :: Lens' ReplicationJob (Maybe Text)
rjRoleName = lens _rjRoleName (\ s a -> s{_rjRoleName = a})

-- | Information about the VM server.
rjVmServer :: Lens' ReplicationJob (Maybe VMServer)
rjVmServer = lens _rjVmServer (\ s a -> s{_rjVmServer = a})

-- | Whether the replication job should produce encrypted AMIs or not. See also @KmsKeyId@ below.
rjEncrypted :: Lens' ReplicationJob (Maybe Bool)
rjEncrypted = lens _rjEncrypted (\ s a -> s{_rjEncrypted = a})

-- | The identifier of the replication job.
rjReplicationJobId :: Lens' ReplicationJob (Maybe Text)
rjReplicationJobId = lens _rjReplicationJobId (\ s a -> s{_rjReplicationJobId = a})

-- | Information about the replication runs.
rjReplicationRunList :: Lens' ReplicationJob [ReplicationRun]
rjReplicationRunList = lens _rjReplicationRunList (\ s a -> s{_rjReplicationRunList = a}) . _Default . _Coerce

-- | The start time of the next replication run.
rjNextReplicationRunStartTime :: Lens' ReplicationJob (Maybe UTCTime)
rjNextReplicationRunStartTime = lens _rjNextReplicationRunStartTime (\ s a -> s{_rjNextReplicationRunStartTime = a}) . mapping _Time

-- | The description of the current status of the replication job.
rjStatusMessage :: Lens' ReplicationJob (Maybe Text)
rjStatusMessage = lens _rjStatusMessage (\ s a -> s{_rjStatusMessage = a})

-- | KMS key ID for replication jobs that produce encrypted AMIs. Can be any of the following:      * KMS key ID     * KMS key alias     * ARN referring to KMS key ID     * ARN referring to KMS key alias If encrypted is /true/ but a KMS key id is not specified, the customer's default KMS key for EBS is used.
rjKmsKeyId :: Lens' ReplicationJob (Maybe Text)
rjKmsKeyId = lens _rjKmsKeyId (\ s a -> s{_rjKmsKeyId = a})

-- | The ID of the latest Amazon Machine Image (AMI).
rjLatestAMIId :: Lens' ReplicationJob (Maybe Text)
rjLatestAMIId = lens _rjLatestAMIId (\ s a -> s{_rjLatestAMIId = a})

-- | The seed replication time.
rjSeedReplicationTime :: Lens' ReplicationJob (Maybe UTCTime)
rjSeedReplicationTime = lens _rjSeedReplicationTime (\ s a -> s{_rjSeedReplicationTime = a}) . mapping _Time

-- |
rjRunOnce :: Lens' ReplicationJob (Maybe Bool)
rjRunOnce = lens _rjRunOnce (\ s a -> s{_rjRunOnce = a})

-- | The description of the replication job.
rjDescription :: Lens' ReplicationJob (Maybe Text)
rjDescription = lens _rjDescription (\ s a -> s{_rjDescription = a})

instance FromJSON ReplicationJob where
        parseJSON
          = withObject "ReplicationJob"
              (\ x ->
                 ReplicationJob' <$>
                   (x .:? "frequency") <*>
                     (x .:? "numberOfRecentAmisToKeep")
                     <*> (x .:? "state")
                     <*> (x .:? "serverType")
                     <*> (x .:? "serverId")
                     <*> (x .:? "licenseType")
                     <*> (x .:? "roleName")
                     <*> (x .:? "vmServer")
                     <*> (x .:? "encrypted")
                     <*> (x .:? "replicationJobId")
                     <*> (x .:? "replicationRunList" .!= mempty)
                     <*> (x .:? "nextReplicationRunStartTime")
                     <*> (x .:? "statusMessage")
                     <*> (x .:? "kmsKeyId")
                     <*> (x .:? "latestAmiId")
                     <*> (x .:? "seedReplicationTime")
                     <*> (x .:? "runOnce")
                     <*> (x .:? "description"))

instance Hashable ReplicationJob where

instance NFData ReplicationJob where

-- | Represents a replication run.
--
--
--
-- /See:/ 'replicationRun' smart constructor.
data ReplicationRun = ReplicationRun'
  { _rrState              :: !(Maybe ReplicationRunState)
  , _rrReplicationRunId   :: !(Maybe Text)
  , _rrEncrypted          :: !(Maybe Bool)
  , _rrStageDetails       :: !(Maybe ReplicationRunStageDetails)
  , _rrScheduledStartTime :: !(Maybe POSIX)
  , _rrStatusMessage      :: !(Maybe Text)
  , _rrKmsKeyId           :: !(Maybe Text)
  , _rrCompletedTime      :: !(Maybe POSIX)
  , _rrAmiId              :: !(Maybe Text)
  , _rrType               :: !(Maybe ReplicationRunType)
  , _rrDescription        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrState' - The state of the replication run.
--
-- * 'rrReplicationRunId' - The identifier of the replication run.
--
-- * 'rrEncrypted' - Whether the replication run should produce encrypted AMI or not. See also @KmsKeyId@ below.
--
-- * 'rrStageDetails' - Details of the current stage of the replication run.
--
-- * 'rrScheduledStartTime' - The start time of the next replication run.
--
-- * 'rrStatusMessage' - The description of the current status of the replication job.
--
-- * 'rrKmsKeyId' - KMS key ID for replication jobs that produce encrypted AMIs. Can be any of the following:      * KMS key ID     * KMS key alias     * ARN referring to KMS key ID     * ARN referring to KMS key alias If encrypted is /true/ but a KMS key id is not specified, the customer's default KMS key for EBS is used.
--
-- * 'rrCompletedTime' - The completion time of the last replication run.
--
-- * 'rrAmiId' - The identifier of the Amazon Machine Image (AMI) from the replication run.
--
-- * 'rrType' - The type of replication run.
--
-- * 'rrDescription' - The description of the replication run.
replicationRun
    :: ReplicationRun
replicationRun =
  ReplicationRun'
    { _rrState = Nothing
    , _rrReplicationRunId = Nothing
    , _rrEncrypted = Nothing
    , _rrStageDetails = Nothing
    , _rrScheduledStartTime = Nothing
    , _rrStatusMessage = Nothing
    , _rrKmsKeyId = Nothing
    , _rrCompletedTime = Nothing
    , _rrAmiId = Nothing
    , _rrType = Nothing
    , _rrDescription = Nothing
    }


-- | The state of the replication run.
rrState :: Lens' ReplicationRun (Maybe ReplicationRunState)
rrState = lens _rrState (\ s a -> s{_rrState = a})

-- | The identifier of the replication run.
rrReplicationRunId :: Lens' ReplicationRun (Maybe Text)
rrReplicationRunId = lens _rrReplicationRunId (\ s a -> s{_rrReplicationRunId = a})

-- | Whether the replication run should produce encrypted AMI or not. See also @KmsKeyId@ below.
rrEncrypted :: Lens' ReplicationRun (Maybe Bool)
rrEncrypted = lens _rrEncrypted (\ s a -> s{_rrEncrypted = a})

-- | Details of the current stage of the replication run.
rrStageDetails :: Lens' ReplicationRun (Maybe ReplicationRunStageDetails)
rrStageDetails = lens _rrStageDetails (\ s a -> s{_rrStageDetails = a})

-- | The start time of the next replication run.
rrScheduledStartTime :: Lens' ReplicationRun (Maybe UTCTime)
rrScheduledStartTime = lens _rrScheduledStartTime (\ s a -> s{_rrScheduledStartTime = a}) . mapping _Time

-- | The description of the current status of the replication job.
rrStatusMessage :: Lens' ReplicationRun (Maybe Text)
rrStatusMessage = lens _rrStatusMessage (\ s a -> s{_rrStatusMessage = a})

-- | KMS key ID for replication jobs that produce encrypted AMIs. Can be any of the following:      * KMS key ID     * KMS key alias     * ARN referring to KMS key ID     * ARN referring to KMS key alias If encrypted is /true/ but a KMS key id is not specified, the customer's default KMS key for EBS is used.
rrKmsKeyId :: Lens' ReplicationRun (Maybe Text)
rrKmsKeyId = lens _rrKmsKeyId (\ s a -> s{_rrKmsKeyId = a})

-- | The completion time of the last replication run.
rrCompletedTime :: Lens' ReplicationRun (Maybe UTCTime)
rrCompletedTime = lens _rrCompletedTime (\ s a -> s{_rrCompletedTime = a}) . mapping _Time

-- | The identifier of the Amazon Machine Image (AMI) from the replication run.
rrAmiId :: Lens' ReplicationRun (Maybe Text)
rrAmiId = lens _rrAmiId (\ s a -> s{_rrAmiId = a})

-- | The type of replication run.
rrType :: Lens' ReplicationRun (Maybe ReplicationRunType)
rrType = lens _rrType (\ s a -> s{_rrType = a})

-- | The description of the replication run.
rrDescription :: Lens' ReplicationRun (Maybe Text)
rrDescription = lens _rrDescription (\ s a -> s{_rrDescription = a})

instance FromJSON ReplicationRun where
        parseJSON
          = withObject "ReplicationRun"
              (\ x ->
                 ReplicationRun' <$>
                   (x .:? "state") <*> (x .:? "replicationRunId") <*>
                     (x .:? "encrypted")
                     <*> (x .:? "stageDetails")
                     <*> (x .:? "scheduledStartTime")
                     <*> (x .:? "statusMessage")
                     <*> (x .:? "kmsKeyId")
                     <*> (x .:? "completedTime")
                     <*> (x .:? "amiId")
                     <*> (x .:? "type")
                     <*> (x .:? "description"))

instance Hashable ReplicationRun where

instance NFData ReplicationRun where

-- | Details of the current stage of a replication run.
--
--
--
-- /See:/ 'replicationRunStageDetails' smart constructor.
data ReplicationRunStageDetails = ReplicationRunStageDetails'
  { _rrsdStage         :: !(Maybe Text)
  , _rrsdStageProgress :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationRunStageDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsdStage' - String describing the current stage of a replication run.
--
-- * 'rrsdStageProgress' - String describing the progress of the current stage of a replication run.
replicationRunStageDetails
    :: ReplicationRunStageDetails
replicationRunStageDetails =
  ReplicationRunStageDetails'
    {_rrsdStage = Nothing, _rrsdStageProgress = Nothing}


-- | String describing the current stage of a replication run.
rrsdStage :: Lens' ReplicationRunStageDetails (Maybe Text)
rrsdStage = lens _rrsdStage (\ s a -> s{_rrsdStage = a})

-- | String describing the progress of the current stage of a replication run.
rrsdStageProgress :: Lens' ReplicationRunStageDetails (Maybe Text)
rrsdStageProgress = lens _rrsdStageProgress (\ s a -> s{_rrsdStageProgress = a})

instance FromJSON ReplicationRunStageDetails where
        parseJSON
          = withObject "ReplicationRunStageDetails"
              (\ x ->
                 ReplicationRunStageDetails' <$>
                   (x .:? "stage") <*> (x .:? "stageProgress"))

instance Hashable ReplicationRunStageDetails where

instance NFData ReplicationRunStageDetails where

-- | Location of the Amazon S3 object in the customer's account.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slBucket :: !(Maybe Text)
  , _slKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slBucket' - Amazon S3 bucket name.
--
-- * 'slKey' - Amazon S3 bucket key.
s3Location
    :: S3Location
s3Location = S3Location' {_slBucket = Nothing, _slKey = Nothing}


-- | Amazon S3 bucket name.
slBucket :: Lens' S3Location (Maybe Text)
slBucket = lens _slBucket (\ s a -> s{_slBucket = a})

-- | Amazon S3 bucket key.
slKey :: Lens' S3Location (Maybe Text)
slKey = lens _slKey (\ s a -> s{_slKey = a})

instance FromJSON S3Location where
        parseJSON
          = withObject "S3Location"
              (\ x ->
                 S3Location' <$> (x .:? "bucket") <*> (x .:? "key"))

instance Hashable S3Location where

instance NFData S3Location where

instance ToJSON S3Location where
        toJSON S3Location'{..}
          = object
              (catMaybes
                 [("bucket" .=) <$> _slBucket, ("key" .=) <$> _slKey])

-- | Represents a server.
--
--
--
-- /See:/ 'server' smart constructor.
data Server = Server'
  { _sServerType               :: !(Maybe ServerType)
  , _sServerId                 :: !(Maybe Text)
  , _sReplicationJobTerminated :: !(Maybe Bool)
  , _sVmServer                 :: !(Maybe VMServer)
  , _sReplicationJobId         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Server' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sServerType' - The type of server.
--
-- * 'sServerId' - The identifier of the server.
--
-- * 'sReplicationJobTerminated' - Indicates whether the replication job is deleted or failed.
--
-- * 'sVmServer' - Information about the VM server.
--
-- * 'sReplicationJobId' - The identifier of the replication job.
server
    :: Server
server =
  Server'
    { _sServerType = Nothing
    , _sServerId = Nothing
    , _sReplicationJobTerminated = Nothing
    , _sVmServer = Nothing
    , _sReplicationJobId = Nothing
    }


-- | The type of server.
sServerType :: Lens' Server (Maybe ServerType)
sServerType = lens _sServerType (\ s a -> s{_sServerType = a})

-- | The identifier of the server.
sServerId :: Lens' Server (Maybe Text)
sServerId = lens _sServerId (\ s a -> s{_sServerId = a})

-- | Indicates whether the replication job is deleted or failed.
sReplicationJobTerminated :: Lens' Server (Maybe Bool)
sReplicationJobTerminated = lens _sReplicationJobTerminated (\ s a -> s{_sReplicationJobTerminated = a})

-- | Information about the VM server.
sVmServer :: Lens' Server (Maybe VMServer)
sVmServer = lens _sVmServer (\ s a -> s{_sVmServer = a})

-- | The identifier of the replication job.
sReplicationJobId :: Lens' Server (Maybe Text)
sReplicationJobId = lens _sReplicationJobId (\ s a -> s{_sReplicationJobId = a})

instance FromJSON Server where
        parseJSON
          = withObject "Server"
              (\ x ->
                 Server' <$>
                   (x .:? "serverType") <*> (x .:? "serverId") <*>
                     (x .:? "replicationJobTerminated")
                     <*> (x .:? "vmServer")
                     <*> (x .:? "replicationJobId"))

instance Hashable Server where

instance NFData Server where

instance ToJSON Server where
        toJSON Server'{..}
          = object
              (catMaybes
                 [("serverType" .=) <$> _sServerType,
                  ("serverId" .=) <$> _sServerId,
                  ("replicationJobTerminated" .=) <$>
                    _sReplicationJobTerminated,
                  ("vmServer" .=) <$> _sVmServer,
                  ("replicationJobId" .=) <$> _sReplicationJobId])

-- | A logical grouping of servers.
--
--
--
-- /See:/ 'serverGroup' smart constructor.
data ServerGroup = ServerGroup'
  { _sgServerList    :: !(Maybe [Server])
  , _sgName          :: !(Maybe Text)
  , _sgServerGroupId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgServerList' - List of servers belonging to a server group.
--
-- * 'sgName' - Name of a server group.
--
-- * 'sgServerGroupId' - Identifier of a server group.
serverGroup
    :: ServerGroup
serverGroup =
  ServerGroup'
    {_sgServerList = Nothing, _sgName = Nothing, _sgServerGroupId = Nothing}


-- | List of servers belonging to a server group.
sgServerList :: Lens' ServerGroup [Server]
sgServerList = lens _sgServerList (\ s a -> s{_sgServerList = a}) . _Default . _Coerce

-- | Name of a server group.
sgName :: Lens' ServerGroup (Maybe Text)
sgName = lens _sgName (\ s a -> s{_sgName = a})

-- | Identifier of a server group.
sgServerGroupId :: Lens' ServerGroup (Maybe Text)
sgServerGroupId = lens _sgServerGroupId (\ s a -> s{_sgServerGroupId = a})

instance FromJSON ServerGroup where
        parseJSON
          = withObject "ServerGroup"
              (\ x ->
                 ServerGroup' <$>
                   (x .:? "serverList" .!= mempty) <*> (x .:? "name")
                     <*> (x .:? "serverGroupId"))

instance Hashable ServerGroup where

instance NFData ServerGroup where

instance ToJSON ServerGroup where
        toJSON ServerGroup'{..}
          = object
              (catMaybes
                 [("serverList" .=) <$> _sgServerList,
                  ("name" .=) <$> _sgName,
                  ("serverGroupId" .=) <$> _sgServerGroupId])

-- | Launch configuration for a server group.
--
--
--
-- /See:/ 'serverGroupLaunchConfiguration' smart constructor.
data ServerGroupLaunchConfiguration = ServerGroupLaunchConfiguration'
  { _sglcServerGroupId              :: !(Maybe Text)
  , _sglcLaunchOrder                :: !(Maybe Int)
  , _sglcServerLaunchConfigurations :: !(Maybe [ServerLaunchConfiguration])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerGroupLaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sglcServerGroupId' - Identifier of the server group the launch configuration is associated with.
--
-- * 'sglcLaunchOrder' - Launch order of servers in the server group.
--
-- * 'sglcServerLaunchConfigurations' - Launch configuration for servers in the server group.
serverGroupLaunchConfiguration
    :: ServerGroupLaunchConfiguration
serverGroupLaunchConfiguration =
  ServerGroupLaunchConfiguration'
    { _sglcServerGroupId = Nothing
    , _sglcLaunchOrder = Nothing
    , _sglcServerLaunchConfigurations = Nothing
    }


-- | Identifier of the server group the launch configuration is associated with.
sglcServerGroupId :: Lens' ServerGroupLaunchConfiguration (Maybe Text)
sglcServerGroupId = lens _sglcServerGroupId (\ s a -> s{_sglcServerGroupId = a})

-- | Launch order of servers in the server group.
sglcLaunchOrder :: Lens' ServerGroupLaunchConfiguration (Maybe Int)
sglcLaunchOrder = lens _sglcLaunchOrder (\ s a -> s{_sglcLaunchOrder = a})

-- | Launch configuration for servers in the server group.
sglcServerLaunchConfigurations :: Lens' ServerGroupLaunchConfiguration [ServerLaunchConfiguration]
sglcServerLaunchConfigurations = lens _sglcServerLaunchConfigurations (\ s a -> s{_sglcServerLaunchConfigurations = a}) . _Default . _Coerce

instance FromJSON ServerGroupLaunchConfiguration
         where
        parseJSON
          = withObject "ServerGroupLaunchConfiguration"
              (\ x ->
                 ServerGroupLaunchConfiguration' <$>
                   (x .:? "serverGroupId") <*> (x .:? "launchOrder") <*>
                     (x .:? "serverLaunchConfigurations" .!= mempty))

instance Hashable ServerGroupLaunchConfiguration
         where

instance NFData ServerGroupLaunchConfiguration where

instance ToJSON ServerGroupLaunchConfiguration where
        toJSON ServerGroupLaunchConfiguration'{..}
          = object
              (catMaybes
                 [("serverGroupId" .=) <$> _sglcServerGroupId,
                  ("launchOrder" .=) <$> _sglcLaunchOrder,
                  ("serverLaunchConfigurations" .=) <$>
                    _sglcServerLaunchConfigurations])

-- | Replication configuration for a server group.
--
--
--
-- /See:/ 'serverGroupReplicationConfiguration' smart constructor.
data ServerGroupReplicationConfiguration = ServerGroupReplicationConfiguration'
  { _sgrcServerGroupId :: !(Maybe Text)
  , _sgrcServerReplicationConfigurations :: !(Maybe [ServerReplicationConfiguration])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerGroupReplicationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgrcServerGroupId' - Identifier of the server group this replication configuration is associated with.
--
-- * 'sgrcServerReplicationConfigurations' - Replication configuration for servers in the server group.
serverGroupReplicationConfiguration
    :: ServerGroupReplicationConfiguration
serverGroupReplicationConfiguration =
  ServerGroupReplicationConfiguration'
    { _sgrcServerGroupId = Nothing
    , _sgrcServerReplicationConfigurations = Nothing
    }


-- | Identifier of the server group this replication configuration is associated with.
sgrcServerGroupId :: Lens' ServerGroupReplicationConfiguration (Maybe Text)
sgrcServerGroupId = lens _sgrcServerGroupId (\ s a -> s{_sgrcServerGroupId = a})

-- | Replication configuration for servers in the server group.
sgrcServerReplicationConfigurations :: Lens' ServerGroupReplicationConfiguration [ServerReplicationConfiguration]
sgrcServerReplicationConfigurations = lens _sgrcServerReplicationConfigurations (\ s a -> s{_sgrcServerReplicationConfigurations = a}) . _Default . _Coerce

instance FromJSON ServerGroupReplicationConfiguration
         where
        parseJSON
          = withObject "ServerGroupReplicationConfiguration"
              (\ x ->
                 ServerGroupReplicationConfiguration' <$>
                   (x .:? "serverGroupId") <*>
                     (x .:? "serverReplicationConfigurations" .!= mempty))

instance Hashable ServerGroupReplicationConfiguration
         where

instance NFData ServerGroupReplicationConfiguration
         where

instance ToJSON ServerGroupReplicationConfiguration
         where
        toJSON ServerGroupReplicationConfiguration'{..}
          = object
              (catMaybes
                 [("serverGroupId" .=) <$> _sgrcServerGroupId,
                  ("serverReplicationConfigurations" .=) <$>
                    _sgrcServerReplicationConfigurations])

-- | Launch configuration for a server.
--
--
--
-- /See:/ 'serverLaunchConfiguration' smart constructor.
data ServerLaunchConfiguration = ServerLaunchConfiguration'
  { _slcEc2KeyName               :: !(Maybe Text)
  , _slcAssociatePublicIPAddress :: !(Maybe Bool)
  , _slcSubnet                   :: !(Maybe Text)
  , _slcLogicalId                :: !(Maybe Text)
  , _slcSecurityGroup            :: !(Maybe Text)
  , _slcUserData                 :: !(Maybe UserData)
  , _slcInstanceType             :: !(Maybe Text)
  , _slcServer                   :: !(Maybe Server)
  , _slcVpc                      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerLaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slcEc2KeyName' - Name of the EC2 SSH Key to be used for connecting to the launched server.
--
-- * 'slcAssociatePublicIPAddress' - If true, a publicly accessible IP address is created when launching the server.
--
-- * 'slcSubnet' - Identifier of the subnet the server should be launched into.
--
-- * 'slcLogicalId' - Logical ID of the server in the Amazon CloudFormation template.
--
-- * 'slcSecurityGroup' - Identifier of the security group that applies to the launched server.
--
-- * 'slcUserData' - Location of the user-data script to be executed when launching the server.
--
-- * 'slcInstanceType' - Instance type to be used for launching the server.
--
-- * 'slcServer' - Identifier of the server the launch configuration is associated with.
--
-- * 'slcVpc' - Identifier of the VPC the server should be launched into.
serverLaunchConfiguration
    :: ServerLaunchConfiguration
serverLaunchConfiguration =
  ServerLaunchConfiguration'
    { _slcEc2KeyName = Nothing
    , _slcAssociatePublicIPAddress = Nothing
    , _slcSubnet = Nothing
    , _slcLogicalId = Nothing
    , _slcSecurityGroup = Nothing
    , _slcUserData = Nothing
    , _slcInstanceType = Nothing
    , _slcServer = Nothing
    , _slcVpc = Nothing
    }


-- | Name of the EC2 SSH Key to be used for connecting to the launched server.
slcEc2KeyName :: Lens' ServerLaunchConfiguration (Maybe Text)
slcEc2KeyName = lens _slcEc2KeyName (\ s a -> s{_slcEc2KeyName = a})

-- | If true, a publicly accessible IP address is created when launching the server.
slcAssociatePublicIPAddress :: Lens' ServerLaunchConfiguration (Maybe Bool)
slcAssociatePublicIPAddress = lens _slcAssociatePublicIPAddress (\ s a -> s{_slcAssociatePublicIPAddress = a})

-- | Identifier of the subnet the server should be launched into.
slcSubnet :: Lens' ServerLaunchConfiguration (Maybe Text)
slcSubnet = lens _slcSubnet (\ s a -> s{_slcSubnet = a})

-- | Logical ID of the server in the Amazon CloudFormation template.
slcLogicalId :: Lens' ServerLaunchConfiguration (Maybe Text)
slcLogicalId = lens _slcLogicalId (\ s a -> s{_slcLogicalId = a})

-- | Identifier of the security group that applies to the launched server.
slcSecurityGroup :: Lens' ServerLaunchConfiguration (Maybe Text)
slcSecurityGroup = lens _slcSecurityGroup (\ s a -> s{_slcSecurityGroup = a})

-- | Location of the user-data script to be executed when launching the server.
slcUserData :: Lens' ServerLaunchConfiguration (Maybe UserData)
slcUserData = lens _slcUserData (\ s a -> s{_slcUserData = a})

-- | Instance type to be used for launching the server.
slcInstanceType :: Lens' ServerLaunchConfiguration (Maybe Text)
slcInstanceType = lens _slcInstanceType (\ s a -> s{_slcInstanceType = a})

-- | Identifier of the server the launch configuration is associated with.
slcServer :: Lens' ServerLaunchConfiguration (Maybe Server)
slcServer = lens _slcServer (\ s a -> s{_slcServer = a})

-- | Identifier of the VPC the server should be launched into.
slcVpc :: Lens' ServerLaunchConfiguration (Maybe Text)
slcVpc = lens _slcVpc (\ s a -> s{_slcVpc = a})

instance FromJSON ServerLaunchConfiguration where
        parseJSON
          = withObject "ServerLaunchConfiguration"
              (\ x ->
                 ServerLaunchConfiguration' <$>
                   (x .:? "ec2KeyName") <*>
                     (x .:? "associatePublicIpAddress")
                     <*> (x .:? "subnet")
                     <*> (x .:? "logicalId")
                     <*> (x .:? "securityGroup")
                     <*> (x .:? "userData")
                     <*> (x .:? "instanceType")
                     <*> (x .:? "server")
                     <*> (x .:? "vpc"))

instance Hashable ServerLaunchConfiguration where

instance NFData ServerLaunchConfiguration where

instance ToJSON ServerLaunchConfiguration where
        toJSON ServerLaunchConfiguration'{..}
          = object
              (catMaybes
                 [("ec2KeyName" .=) <$> _slcEc2KeyName,
                  ("associatePublicIpAddress" .=) <$>
                    _slcAssociatePublicIPAddress,
                  ("subnet" .=) <$> _slcSubnet,
                  ("logicalId" .=) <$> _slcLogicalId,
                  ("securityGroup" .=) <$> _slcSecurityGroup,
                  ("userData" .=) <$> _slcUserData,
                  ("instanceType" .=) <$> _slcInstanceType,
                  ("server" .=) <$> _slcServer,
                  ("vpc" .=) <$> _slcVpc])

-- | Replication configuration of a server.
--
--
--
-- /See:/ 'serverReplicationConfiguration' smart constructor.
data ServerReplicationConfiguration = ServerReplicationConfiguration'
  { _srcServerReplicationParameters :: !(Maybe ServerReplicationParameters)
  , _srcServer                      :: !(Maybe Server)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerReplicationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srcServerReplicationParameters' - Parameters for replicating the server.
--
-- * 'srcServer' - Identifier of the server this replication configuration is associated with.
serverReplicationConfiguration
    :: ServerReplicationConfiguration
serverReplicationConfiguration =
  ServerReplicationConfiguration'
    {_srcServerReplicationParameters = Nothing, _srcServer = Nothing}


-- | Parameters for replicating the server.
srcServerReplicationParameters :: Lens' ServerReplicationConfiguration (Maybe ServerReplicationParameters)
srcServerReplicationParameters = lens _srcServerReplicationParameters (\ s a -> s{_srcServerReplicationParameters = a})

-- | Identifier of the server this replication configuration is associated with.
srcServer :: Lens' ServerReplicationConfiguration (Maybe Server)
srcServer = lens _srcServer (\ s a -> s{_srcServer = a})

instance FromJSON ServerReplicationConfiguration
         where
        parseJSON
          = withObject "ServerReplicationConfiguration"
              (\ x ->
                 ServerReplicationConfiguration' <$>
                   (x .:? "serverReplicationParameters") <*>
                     (x .:? "server"))

instance Hashable ServerReplicationConfiguration
         where

instance NFData ServerReplicationConfiguration where

instance ToJSON ServerReplicationConfiguration where
        toJSON ServerReplicationConfiguration'{..}
          = object
              (catMaybes
                 [("serverReplicationParameters" .=) <$>
                    _srcServerReplicationParameters,
                  ("server" .=) <$> _srcServer])

-- | Replication parameters for replicating a server.
--
--
--
-- /See:/ 'serverReplicationParameters' smart constructor.
data ServerReplicationParameters = ServerReplicationParameters'
  { _srpFrequency                :: !(Maybe Int)
  , _srpNumberOfRecentAMIsToKeep :: !(Maybe Int)
  , _srpSeedTime                 :: !(Maybe POSIX)
  , _srpLicenseType              :: !(Maybe LicenseType)
  , _srpEncrypted                :: !(Maybe Bool)
  , _srpKmsKeyId                 :: !(Maybe Text)
  , _srpRunOnce                  :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerReplicationParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srpFrequency' - Frequency of creating replication jobs for the server.
--
-- * 'srpNumberOfRecentAMIsToKeep' - Number of recent AMIs to keep when creating a replication job for this server.
--
-- * 'srpSeedTime' - Seed time for creating a replication job for the server.
--
-- * 'srpLicenseType' - License type for creating a replication job for the server.
--
-- * 'srpEncrypted' - When true, the replication job produces encrypted AMIs. See also @KmsKeyId@ below.
--
-- * 'srpKmsKeyId' -  KMS key ID for replication jobs that produce encrypted AMIs. Can be any of the following:      * KMS key ID     * KMS key alias     * ARN referring to KMS key ID     * ARN referring to KMS key alias If encrypted is /true/ but a KMS key id is not specified, the customer's default KMS key for EBS is used.
--
-- * 'srpRunOnce' -
serverReplicationParameters
    :: ServerReplicationParameters
serverReplicationParameters =
  ServerReplicationParameters'
    { _srpFrequency = Nothing
    , _srpNumberOfRecentAMIsToKeep = Nothing
    , _srpSeedTime = Nothing
    , _srpLicenseType = Nothing
    , _srpEncrypted = Nothing
    , _srpKmsKeyId = Nothing
    , _srpRunOnce = Nothing
    }


-- | Frequency of creating replication jobs for the server.
srpFrequency :: Lens' ServerReplicationParameters (Maybe Int)
srpFrequency = lens _srpFrequency (\ s a -> s{_srpFrequency = a})

-- | Number of recent AMIs to keep when creating a replication job for this server.
srpNumberOfRecentAMIsToKeep :: Lens' ServerReplicationParameters (Maybe Int)
srpNumberOfRecentAMIsToKeep = lens _srpNumberOfRecentAMIsToKeep (\ s a -> s{_srpNumberOfRecentAMIsToKeep = a})

-- | Seed time for creating a replication job for the server.
srpSeedTime :: Lens' ServerReplicationParameters (Maybe UTCTime)
srpSeedTime = lens _srpSeedTime (\ s a -> s{_srpSeedTime = a}) . mapping _Time

-- | License type for creating a replication job for the server.
srpLicenseType :: Lens' ServerReplicationParameters (Maybe LicenseType)
srpLicenseType = lens _srpLicenseType (\ s a -> s{_srpLicenseType = a})

-- | When true, the replication job produces encrypted AMIs. See also @KmsKeyId@ below.
srpEncrypted :: Lens' ServerReplicationParameters (Maybe Bool)
srpEncrypted = lens _srpEncrypted (\ s a -> s{_srpEncrypted = a})

-- |  KMS key ID for replication jobs that produce encrypted AMIs. Can be any of the following:      * KMS key ID     * KMS key alias     * ARN referring to KMS key ID     * ARN referring to KMS key alias If encrypted is /true/ but a KMS key id is not specified, the customer's default KMS key for EBS is used.
srpKmsKeyId :: Lens' ServerReplicationParameters (Maybe Text)
srpKmsKeyId = lens _srpKmsKeyId (\ s a -> s{_srpKmsKeyId = a})

-- |
srpRunOnce :: Lens' ServerReplicationParameters (Maybe Bool)
srpRunOnce = lens _srpRunOnce (\ s a -> s{_srpRunOnce = a})

instance FromJSON ServerReplicationParameters where
        parseJSON
          = withObject "ServerReplicationParameters"
              (\ x ->
                 ServerReplicationParameters' <$>
                   (x .:? "frequency") <*>
                     (x .:? "numberOfRecentAmisToKeep")
                     <*> (x .:? "seedTime")
                     <*> (x .:? "licenseType")
                     <*> (x .:? "encrypted")
                     <*> (x .:? "kmsKeyId")
                     <*> (x .:? "runOnce"))

instance Hashable ServerReplicationParameters where

instance NFData ServerReplicationParameters where

instance ToJSON ServerReplicationParameters where
        toJSON ServerReplicationParameters'{..}
          = object
              (catMaybes
                 [("frequency" .=) <$> _srpFrequency,
                  ("numberOfRecentAmisToKeep" .=) <$>
                    _srpNumberOfRecentAMIsToKeep,
                  ("seedTime" .=) <$> _srpSeedTime,
                  ("licenseType" .=) <$> _srpLicenseType,
                  ("encrypted" .=) <$> _srpEncrypted,
                  ("kmsKeyId" .=) <$> _srpKmsKeyId,
                  ("runOnce" .=) <$> _srpRunOnce])

-- | A label that can be assigned to an application.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - Tag value.
--
-- * 'tagKey' - Tag key.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | Tag value.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | Tag key.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "value") <*> (x .:? "key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _tagValue, ("key" .=) <$> _tagKey])

-- | A script that runs on first launch of an Amazon EC2 instance. Used for configuring the server during launch.
--
--
--
-- /See:/ 'userData' smart constructor.
newtype UserData = UserData'
  { _udS3Location :: Maybe S3Location
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udS3Location' - Amazon S3 location of the user-data script.
userData
    :: UserData
userData = UserData' {_udS3Location = Nothing}


-- | Amazon S3 location of the user-data script.
udS3Location :: Lens' UserData (Maybe S3Location)
udS3Location = lens _udS3Location (\ s a -> s{_udS3Location = a})

instance FromJSON UserData where
        parseJSON
          = withObject "UserData"
              (\ x -> UserData' <$> (x .:? "s3Location"))

instance Hashable UserData where

instance NFData UserData where

instance ToJSON UserData where
        toJSON UserData'{..}
          = object
              (catMaybes [("s3Location" .=) <$> _udS3Location])

-- | Represents a VM server.
--
--
--
-- /See:/ 'vMServer' smart constructor.
data VMServer = VMServer'
  { _vmsVmManagerName   :: !(Maybe Text)
  , _vmsVmManagerType   :: !(Maybe VMManagerType)
  , _vmsVmServerAddress :: !(Maybe VMServerAddress)
  , _vmsVmName          :: !(Maybe Text)
  , _vmsVmPath          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VMServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmsVmManagerName' - The name of the VM manager.
--
-- * 'vmsVmManagerType' - The type of VM management product.
--
-- * 'vmsVmServerAddress' - Information about the VM server location.
--
-- * 'vmsVmName' - The name of the VM.
--
-- * 'vmsVmPath' - The VM folder path in the vCenter Server virtual machine inventory tree.
vMServer
    :: VMServer
vMServer =
  VMServer'
    { _vmsVmManagerName = Nothing
    , _vmsVmManagerType = Nothing
    , _vmsVmServerAddress = Nothing
    , _vmsVmName = Nothing
    , _vmsVmPath = Nothing
    }


-- | The name of the VM manager.
vmsVmManagerName :: Lens' VMServer (Maybe Text)
vmsVmManagerName = lens _vmsVmManagerName (\ s a -> s{_vmsVmManagerName = a})

-- | The type of VM management product.
vmsVmManagerType :: Lens' VMServer (Maybe VMManagerType)
vmsVmManagerType = lens _vmsVmManagerType (\ s a -> s{_vmsVmManagerType = a})

-- | Information about the VM server location.
vmsVmServerAddress :: Lens' VMServer (Maybe VMServerAddress)
vmsVmServerAddress = lens _vmsVmServerAddress (\ s a -> s{_vmsVmServerAddress = a})

-- | The name of the VM.
vmsVmName :: Lens' VMServer (Maybe Text)
vmsVmName = lens _vmsVmName (\ s a -> s{_vmsVmName = a})

-- | The VM folder path in the vCenter Server virtual machine inventory tree.
vmsVmPath :: Lens' VMServer (Maybe Text)
vmsVmPath = lens _vmsVmPath (\ s a -> s{_vmsVmPath = a})

instance FromJSON VMServer where
        parseJSON
          = withObject "VMServer"
              (\ x ->
                 VMServer' <$>
                   (x .:? "vmManagerName") <*> (x .:? "vmManagerType")
                     <*> (x .:? "vmServerAddress")
                     <*> (x .:? "vmName")
                     <*> (x .:? "vmPath"))

instance Hashable VMServer where

instance NFData VMServer where

instance ToJSON VMServer where
        toJSON VMServer'{..}
          = object
              (catMaybes
                 [("vmManagerName" .=) <$> _vmsVmManagerName,
                  ("vmManagerType" .=) <$> _vmsVmManagerType,
                  ("vmServerAddress" .=) <$> _vmsVmServerAddress,
                  ("vmName" .=) <$> _vmsVmName,
                  ("vmPath" .=) <$> _vmsVmPath])

-- | Represents a VM server location.
--
--
--
-- /See:/ 'vMServerAddress' smart constructor.
data VMServerAddress = VMServerAddress'
  { _vmsaVmManagerId :: !(Maybe Text)
  , _vmsaVmId        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VMServerAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmsaVmManagerId' - The identifier of the VM manager.
--
-- * 'vmsaVmId' - The identifier of the VM.
vMServerAddress
    :: VMServerAddress
vMServerAddress =
  VMServerAddress' {_vmsaVmManagerId = Nothing, _vmsaVmId = Nothing}


-- | The identifier of the VM manager.
vmsaVmManagerId :: Lens' VMServerAddress (Maybe Text)
vmsaVmManagerId = lens _vmsaVmManagerId (\ s a -> s{_vmsaVmManagerId = a})

-- | The identifier of the VM.
vmsaVmId :: Lens' VMServerAddress (Maybe Text)
vmsaVmId = lens _vmsaVmId (\ s a -> s{_vmsaVmId = a})

instance FromJSON VMServerAddress where
        parseJSON
          = withObject "VMServerAddress"
              (\ x ->
                 VMServerAddress' <$>
                   (x .:? "vmManagerId") <*> (x .:? "vmId"))

instance Hashable VMServerAddress where

instance NFData VMServerAddress where

instance ToJSON VMServerAddress where
        toJSON VMServerAddress'{..}
          = object
              (catMaybes
                 [("vmManagerId" .=) <$> _vmsaVmManagerId,
                  ("vmId" .=) <$> _vmsaVmId])
