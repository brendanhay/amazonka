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
-- Module      : Amazonka.DMS.Types.ReplicationTaskAssessmentResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.ReplicationTaskAssessmentResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The task assessment report in JSON format.
--
-- /See:/ 'newReplicationTaskAssessmentResult' smart constructor.
data ReplicationTaskAssessmentResult = ReplicationTaskAssessmentResult'
  { -- | The task assessment results in JSON format.
    --
    -- The response object only contains this field if you provide
    -- DescribeReplicationTaskAssessmentResultsMessage$ReplicationTaskArn in
    -- the request.
    assessmentResults :: Prelude.Maybe Prelude.Text,
    -- | The file containing the results of the task assessment.
    assessmentResultsFile :: Prelude.Maybe Prelude.Text,
    -- | The status of the task assessment.
    assessmentStatus :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Prelude.Maybe Prelude.Text,
    -- | The replication task identifier of the task on which the task assessment
    -- was run.
    replicationTaskIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The date the task assessment was completed.
    replicationTaskLastAssessmentDate :: Prelude.Maybe Data.POSIX,
    -- | The URL of the S3 object containing the task assessment results.
    --
    -- The response object only contains this field if you provide
    -- DescribeReplicationTaskAssessmentResultsMessage$ReplicationTaskArn in
    -- the request.
    s3ObjectUrl :: Prelude.Maybe Prelude.Text
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
-- 'assessmentResults', 'replicationTaskAssessmentResult_assessmentResults' - The task assessment results in JSON format.
--
-- The response object only contains this field if you provide
-- DescribeReplicationTaskAssessmentResultsMessage$ReplicationTaskArn in
-- the request.
--
-- 'assessmentResultsFile', 'replicationTaskAssessmentResult_assessmentResultsFile' - The file containing the results of the task assessment.
--
-- 'assessmentStatus', 'replicationTaskAssessmentResult_assessmentStatus' - The status of the task assessment.
--
-- 'replicationTaskArn', 'replicationTaskAssessmentResult_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task.
--
-- 'replicationTaskIdentifier', 'replicationTaskAssessmentResult_replicationTaskIdentifier' - The replication task identifier of the task on which the task assessment
-- was run.
--
-- 'replicationTaskLastAssessmentDate', 'replicationTaskAssessmentResult_replicationTaskLastAssessmentDate' - The date the task assessment was completed.
--
-- 's3ObjectUrl', 'replicationTaskAssessmentResult_s3ObjectUrl' - The URL of the S3 object containing the task assessment results.
--
-- The response object only contains this field if you provide
-- DescribeReplicationTaskAssessmentResultsMessage$ReplicationTaskArn in
-- the request.
newReplicationTaskAssessmentResult ::
  ReplicationTaskAssessmentResult
newReplicationTaskAssessmentResult =
  ReplicationTaskAssessmentResult'
    { assessmentResults =
        Prelude.Nothing,
      assessmentResultsFile = Prelude.Nothing,
      assessmentStatus = Prelude.Nothing,
      replicationTaskArn = Prelude.Nothing,
      replicationTaskIdentifier =
        Prelude.Nothing,
      replicationTaskLastAssessmentDate =
        Prelude.Nothing,
      s3ObjectUrl = Prelude.Nothing
    }

-- | The task assessment results in JSON format.
--
-- The response object only contains this field if you provide
-- DescribeReplicationTaskAssessmentResultsMessage$ReplicationTaskArn in
-- the request.
replicationTaskAssessmentResult_assessmentResults :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentResult_assessmentResults = Lens.lens (\ReplicationTaskAssessmentResult' {assessmentResults} -> assessmentResults) (\s@ReplicationTaskAssessmentResult' {} a -> s {assessmentResults = a} :: ReplicationTaskAssessmentResult)

-- | The file containing the results of the task assessment.
replicationTaskAssessmentResult_assessmentResultsFile :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentResult_assessmentResultsFile = Lens.lens (\ReplicationTaskAssessmentResult' {assessmentResultsFile} -> assessmentResultsFile) (\s@ReplicationTaskAssessmentResult' {} a -> s {assessmentResultsFile = a} :: ReplicationTaskAssessmentResult)

-- | The status of the task assessment.
replicationTaskAssessmentResult_assessmentStatus :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentResult_assessmentStatus = Lens.lens (\ReplicationTaskAssessmentResult' {assessmentStatus} -> assessmentStatus) (\s@ReplicationTaskAssessmentResult' {} a -> s {assessmentStatus = a} :: ReplicationTaskAssessmentResult)

-- | The Amazon Resource Name (ARN) of the replication task.
replicationTaskAssessmentResult_replicationTaskArn :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentResult_replicationTaskArn = Lens.lens (\ReplicationTaskAssessmentResult' {replicationTaskArn} -> replicationTaskArn) (\s@ReplicationTaskAssessmentResult' {} a -> s {replicationTaskArn = a} :: ReplicationTaskAssessmentResult)

-- | The replication task identifier of the task on which the task assessment
-- was run.
replicationTaskAssessmentResult_replicationTaskIdentifier :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentResult_replicationTaskIdentifier = Lens.lens (\ReplicationTaskAssessmentResult' {replicationTaskIdentifier} -> replicationTaskIdentifier) (\s@ReplicationTaskAssessmentResult' {} a -> s {replicationTaskIdentifier = a} :: ReplicationTaskAssessmentResult)

-- | The date the task assessment was completed.
replicationTaskAssessmentResult_replicationTaskLastAssessmentDate :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.UTCTime)
replicationTaskAssessmentResult_replicationTaskLastAssessmentDate = Lens.lens (\ReplicationTaskAssessmentResult' {replicationTaskLastAssessmentDate} -> replicationTaskLastAssessmentDate) (\s@ReplicationTaskAssessmentResult' {} a -> s {replicationTaskLastAssessmentDate = a} :: ReplicationTaskAssessmentResult) Prelude.. Lens.mapping Data._Time

-- | The URL of the S3 object containing the task assessment results.
--
-- The response object only contains this field if you provide
-- DescribeReplicationTaskAssessmentResultsMessage$ReplicationTaskArn in
-- the request.
replicationTaskAssessmentResult_s3ObjectUrl :: Lens.Lens' ReplicationTaskAssessmentResult (Prelude.Maybe Prelude.Text)
replicationTaskAssessmentResult_s3ObjectUrl = Lens.lens (\ReplicationTaskAssessmentResult' {s3ObjectUrl} -> s3ObjectUrl) (\s@ReplicationTaskAssessmentResult' {} a -> s {s3ObjectUrl = a} :: ReplicationTaskAssessmentResult)

instance
  Data.FromJSON
    ReplicationTaskAssessmentResult
  where
  parseJSON =
    Data.withObject
      "ReplicationTaskAssessmentResult"
      ( \x ->
          ReplicationTaskAssessmentResult'
            Prelude.<$> (x Data..:? "AssessmentResults")
            Prelude.<*> (x Data..:? "AssessmentResultsFile")
            Prelude.<*> (x Data..:? "AssessmentStatus")
            Prelude.<*> (x Data..:? "ReplicationTaskArn")
            Prelude.<*> (x Data..:? "ReplicationTaskIdentifier")
            Prelude.<*> (x Data..:? "ReplicationTaskLastAssessmentDate")
            Prelude.<*> (x Data..:? "S3ObjectUrl")
      )

instance
  Prelude.Hashable
    ReplicationTaskAssessmentResult
  where
  hashWithSalt
    _salt
    ReplicationTaskAssessmentResult' {..} =
      _salt
        `Prelude.hashWithSalt` assessmentResults
        `Prelude.hashWithSalt` assessmentResultsFile
        `Prelude.hashWithSalt` assessmentStatus
        `Prelude.hashWithSalt` replicationTaskArn
        `Prelude.hashWithSalt` replicationTaskIdentifier
        `Prelude.hashWithSalt` replicationTaskLastAssessmentDate
        `Prelude.hashWithSalt` s3ObjectUrl

instance
  Prelude.NFData
    ReplicationTaskAssessmentResult
  where
  rnf ReplicationTaskAssessmentResult' {..} =
    Prelude.rnf assessmentResults
      `Prelude.seq` Prelude.rnf assessmentResultsFile
      `Prelude.seq` Prelude.rnf assessmentStatus
      `Prelude.seq` Prelude.rnf replicationTaskArn
      `Prelude.seq` Prelude.rnf replicationTaskIdentifier
      `Prelude.seq` Prelude.rnf replicationTaskLastAssessmentDate
      `Prelude.seq` Prelude.rnf s3ObjectUrl
