{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationRun where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.ReplicationRunStageDetails
import Network.AWS.SMS.Types.ReplicationRunState
import Network.AWS.SMS.Types.ReplicationRunType

-- | Represents a replication run.
--
--
--
-- /See:/ 'replicationRun' smart constructor.
data ReplicationRun = ReplicationRun'
  { _rrState ::
      !(Maybe ReplicationRunState),
    _rrReplicationRunId :: !(Maybe Text),
    _rrEncrypted :: !(Maybe Bool),
    _rrStageDetails :: !(Maybe ReplicationRunStageDetails),
    _rrScheduledStartTime :: !(Maybe POSIX),
    _rrStatusMessage :: !(Maybe Text),
    _rrKmsKeyId :: !(Maybe Text),
    _rrCompletedTime :: !(Maybe POSIX),
    _rrAmiId :: !(Maybe Text),
    _rrType :: !(Maybe ReplicationRunType),
    _rrDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrState' - The state of the replication run.
--
-- * 'rrReplicationRunId' - The ID of the replication run.
--
-- * 'rrEncrypted' - Indicates whether the replication run should produce an encrypted AMI.
--
-- * 'rrStageDetails' - Details about the current stage of the replication run.
--
-- * 'rrScheduledStartTime' - The start time of the next replication run.
--
-- * 'rrStatusMessage' - The description of the current status of the replication job.
--
-- * 'rrKmsKeyId' - The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:     * KMS key ID     * KMS key alias     * ARN referring to the KMS key ID     * ARN referring to the KMS key alias If encrypted is /true/ but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
--
-- * 'rrCompletedTime' - The completion time of the last replication run.
--
-- * 'rrAmiId' - The ID of the Amazon Machine Image (AMI) from the replication run.
--
-- * 'rrType' - The type of replication run.
--
-- * 'rrDescription' - The description of the replication run.
replicationRun ::
  ReplicationRun
replicationRun =
  ReplicationRun'
    { _rrState = Nothing,
      _rrReplicationRunId = Nothing,
      _rrEncrypted = Nothing,
      _rrStageDetails = Nothing,
      _rrScheduledStartTime = Nothing,
      _rrStatusMessage = Nothing,
      _rrKmsKeyId = Nothing,
      _rrCompletedTime = Nothing,
      _rrAmiId = Nothing,
      _rrType = Nothing,
      _rrDescription = Nothing
    }

-- | The state of the replication run.
rrState :: Lens' ReplicationRun (Maybe ReplicationRunState)
rrState = lens _rrState (\s a -> s {_rrState = a})

-- | The ID of the replication run.
rrReplicationRunId :: Lens' ReplicationRun (Maybe Text)
rrReplicationRunId = lens _rrReplicationRunId (\s a -> s {_rrReplicationRunId = a})

-- | Indicates whether the replication run should produce an encrypted AMI.
rrEncrypted :: Lens' ReplicationRun (Maybe Bool)
rrEncrypted = lens _rrEncrypted (\s a -> s {_rrEncrypted = a})

-- | Details about the current stage of the replication run.
rrStageDetails :: Lens' ReplicationRun (Maybe ReplicationRunStageDetails)
rrStageDetails = lens _rrStageDetails (\s a -> s {_rrStageDetails = a})

-- | The start time of the next replication run.
rrScheduledStartTime :: Lens' ReplicationRun (Maybe UTCTime)
rrScheduledStartTime = lens _rrScheduledStartTime (\s a -> s {_rrScheduledStartTime = a}) . mapping _Time

-- | The description of the current status of the replication job.
rrStatusMessage :: Lens' ReplicationRun (Maybe Text)
rrStatusMessage = lens _rrStatusMessage (\s a -> s {_rrStatusMessage = a})

-- | The ID of the KMS key for replication jobs that produce encrypted AMIs. This value can be any of the following:     * KMS key ID     * KMS key alias     * ARN referring to the KMS key ID     * ARN referring to the KMS key alias If encrypted is /true/ but a KMS key ID is not specified, the customer's default KMS key for Amazon EBS is used.
rrKmsKeyId :: Lens' ReplicationRun (Maybe Text)
rrKmsKeyId = lens _rrKmsKeyId (\s a -> s {_rrKmsKeyId = a})

-- | The completion time of the last replication run.
rrCompletedTime :: Lens' ReplicationRun (Maybe UTCTime)
rrCompletedTime = lens _rrCompletedTime (\s a -> s {_rrCompletedTime = a}) . mapping _Time

-- | The ID of the Amazon Machine Image (AMI) from the replication run.
rrAmiId :: Lens' ReplicationRun (Maybe Text)
rrAmiId = lens _rrAmiId (\s a -> s {_rrAmiId = a})

-- | The type of replication run.
rrType :: Lens' ReplicationRun (Maybe ReplicationRunType)
rrType = lens _rrType (\s a -> s {_rrType = a})

-- | The description of the replication run.
rrDescription :: Lens' ReplicationRun (Maybe Text)
rrDescription = lens _rrDescription (\s a -> s {_rrDescription = a})

instance FromJSON ReplicationRun where
  parseJSON =
    withObject
      "ReplicationRun"
      ( \x ->
          ReplicationRun'
            <$> (x .:? "state")
            <*> (x .:? "replicationRunId")
            <*> (x .:? "encrypted")
            <*> (x .:? "stageDetails")
            <*> (x .:? "scheduledStartTime")
            <*> (x .:? "statusMessage")
            <*> (x .:? "kmsKeyId")
            <*> (x .:? "completedTime")
            <*> (x .:? "amiId")
            <*> (x .:? "type")
            <*> (x .:? "description")
      )

instance Hashable ReplicationRun

instance NFData ReplicationRun
