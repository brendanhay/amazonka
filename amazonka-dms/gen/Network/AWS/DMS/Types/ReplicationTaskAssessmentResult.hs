{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskAssessmentResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTaskAssessmentResult where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The task assessment report in JSON format.
--
-- /See:/ 'newReplicationTaskAssessmentResult' smart constructor.
data ReplicationTaskAssessmentResult = ReplicationTaskAssessmentResult'
  { -- | The URL of the S3 object containing the task assessment results.
    --
    -- The response object only contains this field if you provide
    -- DescribeReplicationTaskAssessmentResultsMessage$ReplicationTaskArn in
    -- the request.
    s3ObjectUrl :: Prelude.Maybe Prelude.Text,
    -- | The status of the task assessment.
    assessmentStatus :: Prelude.Maybe Prelude.Text,
    -- | The task assessment results in JSON format.
    --
    -- The response object only contains this field if you provide
    -- DescribeReplicationTaskAssessmentResultsMessage$ReplicationTaskArn in
    -- the request.
    assessmentResults :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Prelude.Maybe Prelude.Text,
    -- | The date the task assessment was completed.
    replicationTaskLastAssessmentDate :: Prelude.Maybe Core.POSIX,
    -- | The replication task identifier of the task on which the task assessment
    -- was run.
    replicationTaskIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The file containing the results of the task assessment.
    assessmentResultsFile :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationTaskAssessmentResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ObjectUrl', 'replicationTaskAssessmentResult_s3ObjectUrl' - The URL of the S3 object containing the task assessment results.
--
-- The response object only contains this field if you provide
-- DescribeReplicationTaskAssessmentResultsMessage$ReplicationTaskArn in
-- the request.
--
-- 'assessmentStatus', 'replicationTaskAssessmentResult_assessmentStatus' - The status of the task assessment.
--
-- 'assessmentResults', 'replicationTaskAssessmentResult_assessmentResults' - The task assessment results in JSON format.
--
-- The response object only contains this field if you provide
-- DescribeReplicationTaskAssessmentResultsMessage$ReplicationTaskArn in
-- the request.
--
-- 'replicationTaskArn', 'replicationTaskAssessmentResult_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task.
--
-- 'replicationTaskLastAssessmentDate', 'replicationTaskAssessmentResult_replicationTaskLastAssessmentDate' - The date the task assessment was completed.
--
-- 'replicationTaskIdentifier', 'replicationTaskAssessmentResult_replicationTaskIdentifier' - The replication task identifier of the task on which the task assessment
-- was run.
--
-- 'assessmentResultsFile', 'replicationTaskAssessmentResult_assessmentResultsFile' - The file containing the results of the task assessment.
newReplicationTaskAssessmentResult ::
  ReplicationTaskAssessmentResult
newReplicationTaskAssessmentResult =
  ReplicationTaskAssessmentResult'
    { s3ObjectUrl =
        Prelude.Nothing,
      assessmentStatus = Prelude.Nothing,
      assessmentResults = Prelude.Nothing,
      replicationTaskArn = Prelude.Nothing,
      replicationTaskLastAssessmentDate =
        Prelude.Nothing,
      replicationTaskIdentifier =
        Prelude.Nothing,
      assessmentResultsFile = Prelude.Nothing
    }

-- | The URL of the S3 object containing the task assessment results.
--
-- The response object only contains this field if you provide
-- DescribeReplicationTaskAssessmentResultsMessage$ReplicationTaskArn in
-- the request.
replicationTaskAssessmentResult_s3ObjectUrl :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentResult_s3ObjectUrl = Lens.lens (\ReplicationTaskAssessmentResult' {s3ObjectUrl} -> s3ObjectUrl) (\s@ReplicationTaskAssessmentResult' {} a -> s {s3ObjectUrl = a} :: ReplicationTaskAssessmentResult)

-- | The status of the task assessment.
replicationTaskAssessmentResult_assessmentStatus :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentResult_assessmentStatus = Lens.lens (\ReplicationTaskAssessmentResult' {assessmentStatus} -> assessmentStatus) (\s@ReplicationTaskAssessmentResult' {} a -> s {assessmentStatus = a} :: ReplicationTaskAssessmentResult)

-- | The task assessment results in JSON format.
--
-- The response object only contains this field if you provide
-- DescribeReplicationTaskAssessmentResultsMessage$ReplicationTaskArn in
-- the request.
replicationTaskAssessmentResult_assessmentResults :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentResult_assessmentResults = Lens.lens (\ReplicationTaskAssessmentResult' {assessmentResults} -> assessmentResults) (\s@ReplicationTaskAssessmentResult' {} a -> s {assessmentResults = a} :: ReplicationTaskAssessmentResult)

-- | The Amazon Resource Name (ARN) of the replication task.
replicationTaskAssessmentResult_replicationTaskArn :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentResult_replicationTaskArn = Lens.lens (\ReplicationTaskAssessmentResult' {replicationTaskArn} -> replicationTaskArn) (\s@ReplicationTaskAssessmentResult' {} a -> s {replicationTaskArn = a} :: ReplicationTaskAssessmentResult)

-- | The date the task assessment was completed.
replicationTaskAssessmentResult_replicationTaskLastAssessmentDate :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.UTCTime)
replicationTaskAssessmentResult_replicationTaskLastAssessmentDate = Lens.lens (\ReplicationTaskAssessmentResult' {replicationTaskLastAssessmentDate} -> replicationTaskLastAssessmentDate) (\s@ReplicationTaskAssessmentResult' {} a -> s {replicationTaskLastAssessmentDate = a} :: ReplicationTaskAssessmentResult) Prelude.. Lens.mapping Core._Time

-- | The replication task identifier of the task on which the task assessment
-- was run.
replicationTaskAssessmentResult_replicationTaskIdentifier :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentResult_replicationTaskIdentifier = Lens.lens (\ReplicationTaskAssessmentResult' {replicationTaskIdentifier} -> replicationTaskIdentifier) (\s@ReplicationTaskAssessmentResult' {} a -> s {replicationTaskIdentifier = a} :: ReplicationTaskAssessmentResult)

-- | The file containing the results of the task assessment.
replicationTaskAssessmentResult_assessmentResultsFile :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentResult_assessmentResultsFile = Lens.lens (\ReplicationTaskAssessmentResult' {assessmentResultsFile} -> assessmentResultsFile) (\s@ReplicationTaskAssessmentResult' {} a -> s {assessmentResultsFile = a} :: ReplicationTaskAssessmentResult)

instance
  Core.FromJSON
    ReplicationTaskAssessmentResult
  where
  parseJSON =
    Core.withObject
      "ReplicationTaskAssessmentResult"
      ( \x ->
          ReplicationTaskAssessmentResult'
            Prelude.<$> (x Core..:? "S3ObjectUrl")
            Prelude.<*> (x Core..:? "AssessmentStatus")
            Prelude.<*> (x Core..:? "AssessmentResults")
            Prelude.<*> (x Core..:? "ReplicationTaskArn")
            Prelude.<*> (x Core..:? "ReplicationTaskLastAssessmentDate")
            Prelude.<*> (x Core..:? "ReplicationTaskIdentifier")
            Prelude.<*> (x Core..:? "AssessmentResultsFile")
      )

instance
  Prelude.Hashable
    ReplicationTaskAssessmentResult

instance
  Prelude.NFData
    ReplicationTaskAssessmentResult
