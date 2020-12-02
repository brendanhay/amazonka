{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskAssessmentResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTaskAssessmentResult where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The task assessment report in JSON format.
--
--
--
-- /See:/ 'replicationTaskAssessmentResult' smart constructor.
data ReplicationTaskAssessmentResult = ReplicationTaskAssessmentResult'
  { _rAssessmentResults ::
      !(Maybe Text),
    _rAssessmentResultsFile ::
      !(Maybe Text),
    _rReplicationTaskIdentifier ::
      !(Maybe Text),
    _rAssessmentStatus ::
      !(Maybe Text),
    _rS3ObjectURL ::
      !(Maybe Text),
    _rReplicationTaskLastAssessmentDate ::
      !(Maybe POSIX),
    _rReplicationTaskARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationTaskAssessmentResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rAssessmentResults' - The task assessment results in JSON format.
--
-- * 'rAssessmentResultsFile' - The file containing the results of the task assessment.
--
-- * 'rReplicationTaskIdentifier' - The replication task identifier of the task on which the task assessment was run.
--
-- * 'rAssessmentStatus' - The status of the task assessment.
--
-- * 'rS3ObjectURL' - The URL of the S3 object containing the task assessment results.
--
-- * 'rReplicationTaskLastAssessmentDate' - The date the task assessment was completed.
--
-- * 'rReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
replicationTaskAssessmentResult ::
  ReplicationTaskAssessmentResult
replicationTaskAssessmentResult =
  ReplicationTaskAssessmentResult'
    { _rAssessmentResults = Nothing,
      _rAssessmentResultsFile = Nothing,
      _rReplicationTaskIdentifier = Nothing,
      _rAssessmentStatus = Nothing,
      _rS3ObjectURL = Nothing,
      _rReplicationTaskLastAssessmentDate = Nothing,
      _rReplicationTaskARN = Nothing
    }

-- | The task assessment results in JSON format.
rAssessmentResults :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rAssessmentResults = lens _rAssessmentResults (\s a -> s {_rAssessmentResults = a})

-- | The file containing the results of the task assessment.
rAssessmentResultsFile :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rAssessmentResultsFile = lens _rAssessmentResultsFile (\s a -> s {_rAssessmentResultsFile = a})

-- | The replication task identifier of the task on which the task assessment was run.
rReplicationTaskIdentifier :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rReplicationTaskIdentifier = lens _rReplicationTaskIdentifier (\s a -> s {_rReplicationTaskIdentifier = a})

-- | The status of the task assessment.
rAssessmentStatus :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rAssessmentStatus = lens _rAssessmentStatus (\s a -> s {_rAssessmentStatus = a})

-- | The URL of the S3 object containing the task assessment results.
rS3ObjectURL :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rS3ObjectURL = lens _rS3ObjectURL (\s a -> s {_rS3ObjectURL = a})

-- | The date the task assessment was completed.
rReplicationTaskLastAssessmentDate :: Lens' ReplicationTaskAssessmentResult (Maybe UTCTime)
rReplicationTaskLastAssessmentDate = lens _rReplicationTaskLastAssessmentDate (\s a -> s {_rReplicationTaskLastAssessmentDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the replication task.
rReplicationTaskARN :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rReplicationTaskARN = lens _rReplicationTaskARN (\s a -> s {_rReplicationTaskARN = a})

instance FromJSON ReplicationTaskAssessmentResult where
  parseJSON =
    withObject
      "ReplicationTaskAssessmentResult"
      ( \x ->
          ReplicationTaskAssessmentResult'
            <$> (x .:? "AssessmentResults")
            <*> (x .:? "AssessmentResultsFile")
            <*> (x .:? "ReplicationTaskIdentifier")
            <*> (x .:? "AssessmentStatus")
            <*> (x .:? "S3ObjectUrl")
            <*> (x .:? "ReplicationTaskLastAssessmentDate")
            <*> (x .:? "ReplicationTaskArn")
      )

instance Hashable ReplicationTaskAssessmentResult

instance NFData ReplicationTaskAssessmentResult
