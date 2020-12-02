{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationJob where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.LicenseType
import Network.AWS.SMS.Types.ReplicationJobState
import Network.AWS.SMS.Types.ReplicationRun
import Network.AWS.SMS.Types.ServerType
import Network.AWS.SMS.Types.VMServer

-- | Represents a replication job.
--
--
--
-- /See:/ 'replicationJob' smart constructor.
data ReplicationJob = ReplicationJob'
  { _rjFrequency :: !(Maybe Int),
    _rjNumberOfRecentAMIsToKeep :: !(Maybe Int),
    _rjState :: !(Maybe ReplicationJobState),
    _rjServerType :: !(Maybe ServerType),
    _rjServerId :: !(Maybe Text),
    _rjLicenseType :: !(Maybe LicenseType),
    _rjRoleName :: !(Maybe Text),
    _rjVmServer :: !(Maybe VMServer),
    _rjEncrypted :: !(Maybe Bool),
    _rjReplicationJobId :: !(Maybe Text),
    _rjReplicationRunList :: !(Maybe [ReplicationRun]),
    _rjNextReplicationRunStartTime :: !(Maybe POSIX),
    _rjStatusMessage :: !(Maybe Text),
    _rjKmsKeyId :: !(Maybe Text),
    _rjLatestAMIId :: !(Maybe Text),
    _rjSeedReplicationTime :: !(Maybe POSIX),
    _rjRunOnce :: !(Maybe Bool),
    _rjDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rjFrequency' - The time between consecutive replication runs, in hours.
--
-- * 'rjNumberOfRecentAMIsToKeep' - The number of recent AMIs to keep in the customer's account for a replication job. By default, the value is set to zero, meaning that all AMIs are kept.
--
-- * 'rjState' - The state of the replication job.
--
-- * 'rjServerType' - The type of server.
--
-- * 'rjServerId' - The ID of the server.
--
-- * 'rjLicenseType' - The license type to be used for the AMI created by a successful replication run.
--
-- * 'rjRoleName' - The name of the IAM role to be used by AWS SMS.
--
-- * 'rjVmServer' - Information about the VM server.
--
-- * 'rjEncrypted' - Indicates whether the replication job should produce encrypted AMIs.
--
-- * 'rjReplicationJobId' - The ID of the replication job.
--
-- * 'rjReplicationRunList' - Information about the replication runs.
--
-- * 'rjNextReplicationRunStartTime' - The start time of the next replication run.
--
-- * 'rjStatusMessage' - The description of the current status of the replication job.
--
-- * 'rjKmsKeyId' - The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:      * KMS key ID     * KMS key alias     * ARN referring to the KMS key ID     * ARN referring to the KMS key alias If encrypted is enabled but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
--
-- * 'rjLatestAMIId' - The ID of the latest Amazon Machine Image (AMI).
--
-- * 'rjSeedReplicationTime' - The seed replication time.
--
-- * 'rjRunOnce' - Indicates whether to run the replication job one time.
--
-- * 'rjDescription' - The description of the replication job.
replicationJob ::
  ReplicationJob
replicationJob =
  ReplicationJob'
    { _rjFrequency = Nothing,
      _rjNumberOfRecentAMIsToKeep = Nothing,
      _rjState = Nothing,
      _rjServerType = Nothing,
      _rjServerId = Nothing,
      _rjLicenseType = Nothing,
      _rjRoleName = Nothing,
      _rjVmServer = Nothing,
      _rjEncrypted = Nothing,
      _rjReplicationJobId = Nothing,
      _rjReplicationRunList = Nothing,
      _rjNextReplicationRunStartTime = Nothing,
      _rjStatusMessage = Nothing,
      _rjKmsKeyId = Nothing,
      _rjLatestAMIId = Nothing,
      _rjSeedReplicationTime = Nothing,
      _rjRunOnce = Nothing,
      _rjDescription = Nothing
    }

-- | The time between consecutive replication runs, in hours.
rjFrequency :: Lens' ReplicationJob (Maybe Int)
rjFrequency = lens _rjFrequency (\s a -> s {_rjFrequency = a})

-- | The number of recent AMIs to keep in the customer's account for a replication job. By default, the value is set to zero, meaning that all AMIs are kept.
rjNumberOfRecentAMIsToKeep :: Lens' ReplicationJob (Maybe Int)
rjNumberOfRecentAMIsToKeep = lens _rjNumberOfRecentAMIsToKeep (\s a -> s {_rjNumberOfRecentAMIsToKeep = a})

-- | The state of the replication job.
rjState :: Lens' ReplicationJob (Maybe ReplicationJobState)
rjState = lens _rjState (\s a -> s {_rjState = a})

-- | The type of server.
rjServerType :: Lens' ReplicationJob (Maybe ServerType)
rjServerType = lens _rjServerType (\s a -> s {_rjServerType = a})

-- | The ID of the server.
rjServerId :: Lens' ReplicationJob (Maybe Text)
rjServerId = lens _rjServerId (\s a -> s {_rjServerId = a})

-- | The license type to be used for the AMI created by a successful replication run.
rjLicenseType :: Lens' ReplicationJob (Maybe LicenseType)
rjLicenseType = lens _rjLicenseType (\s a -> s {_rjLicenseType = a})

-- | The name of the IAM role to be used by AWS SMS.
rjRoleName :: Lens' ReplicationJob (Maybe Text)
rjRoleName = lens _rjRoleName (\s a -> s {_rjRoleName = a})

-- | Information about the VM server.
rjVmServer :: Lens' ReplicationJob (Maybe VMServer)
rjVmServer = lens _rjVmServer (\s a -> s {_rjVmServer = a})

-- | Indicates whether the replication job should produce encrypted AMIs.
rjEncrypted :: Lens' ReplicationJob (Maybe Bool)
rjEncrypted = lens _rjEncrypted (\s a -> s {_rjEncrypted = a})

-- | The ID of the replication job.
rjReplicationJobId :: Lens' ReplicationJob (Maybe Text)
rjReplicationJobId = lens _rjReplicationJobId (\s a -> s {_rjReplicationJobId = a})

-- | Information about the replication runs.
rjReplicationRunList :: Lens' ReplicationJob [ReplicationRun]
rjReplicationRunList = lens _rjReplicationRunList (\s a -> s {_rjReplicationRunList = a}) . _Default . _Coerce

-- | The start time of the next replication run.
rjNextReplicationRunStartTime :: Lens' ReplicationJob (Maybe UTCTime)
rjNextReplicationRunStartTime = lens _rjNextReplicationRunStartTime (\s a -> s {_rjNextReplicationRunStartTime = a}) . mapping _Time

-- | The description of the current status of the replication job.
rjStatusMessage :: Lens' ReplicationJob (Maybe Text)
rjStatusMessage = lens _rjStatusMessage (\s a -> s {_rjStatusMessage = a})

-- | The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:      * KMS key ID     * KMS key alias     * ARN referring to the KMS key ID     * ARN referring to the KMS key alias If encrypted is enabled but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
rjKmsKeyId :: Lens' ReplicationJob (Maybe Text)
rjKmsKeyId = lens _rjKmsKeyId (\s a -> s {_rjKmsKeyId = a})

-- | The ID of the latest Amazon Machine Image (AMI).
rjLatestAMIId :: Lens' ReplicationJob (Maybe Text)
rjLatestAMIId = lens _rjLatestAMIId (\s a -> s {_rjLatestAMIId = a})

-- | The seed replication time.
rjSeedReplicationTime :: Lens' ReplicationJob (Maybe UTCTime)
rjSeedReplicationTime = lens _rjSeedReplicationTime (\s a -> s {_rjSeedReplicationTime = a}) . mapping _Time

-- | Indicates whether to run the replication job one time.
rjRunOnce :: Lens' ReplicationJob (Maybe Bool)
rjRunOnce = lens _rjRunOnce (\s a -> s {_rjRunOnce = a})

-- | The description of the replication job.
rjDescription :: Lens' ReplicationJob (Maybe Text)
rjDescription = lens _rjDescription (\s a -> s {_rjDescription = a})

instance FromJSON ReplicationJob where
  parseJSON =
    withObject
      "ReplicationJob"
      ( \x ->
          ReplicationJob'
            <$> (x .:? "frequency")
            <*> (x .:? "numberOfRecentAmisToKeep")
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
            <*> (x .:? "description")
      )

instance Hashable ReplicationJob

instance NFData ReplicationJob
