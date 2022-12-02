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
-- Module      : Amazonka.SageMaker.Types.ProcessingJobSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProcessingJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProcessingJobStatus

-- | Summary of information about a processing job.
--
-- /See:/ 'newProcessingJobSummary' smart constructor.
data ProcessingJobSummary = ProcessingJobSummary'
  { -- | A timestamp that indicates the last time the processing job was
    -- modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The time at which the processing job completed.
    processingEndTime :: Prelude.Maybe Data.POSIX,
    -- | An optional string, up to one KB in size, that contains metadata from
    -- the processing container when the processing job exits.
    exitMessage :: Prelude.Maybe Prelude.Text,
    -- | A string, up to one KB in size, that contains the reason a processing
    -- job failed, if it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the processing job.
    processingJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the processing job..
    processingJobArn :: Prelude.Text,
    -- | The time at which the processing job was created.
    creationTime :: Data.POSIX,
    -- | The status of the processing job.
    processingJobStatus :: ProcessingJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProcessingJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'processingJobSummary_lastModifiedTime' - A timestamp that indicates the last time the processing job was
-- modified.
--
-- 'processingEndTime', 'processingJobSummary_processingEndTime' - The time at which the processing job completed.
--
-- 'exitMessage', 'processingJobSummary_exitMessage' - An optional string, up to one KB in size, that contains metadata from
-- the processing container when the processing job exits.
--
-- 'failureReason', 'processingJobSummary_failureReason' - A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
--
-- 'processingJobName', 'processingJobSummary_processingJobName' - The name of the processing job.
--
-- 'processingJobArn', 'processingJobSummary_processingJobArn' - The Amazon Resource Name (ARN) of the processing job..
--
-- 'creationTime', 'processingJobSummary_creationTime' - The time at which the processing job was created.
--
-- 'processingJobStatus', 'processingJobSummary_processingJobStatus' - The status of the processing job.
newProcessingJobSummary ::
  -- | 'processingJobName'
  Prelude.Text ->
  -- | 'processingJobArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'processingJobStatus'
  ProcessingJobStatus ->
  ProcessingJobSummary
newProcessingJobSummary
  pProcessingJobName_
  pProcessingJobArn_
  pCreationTime_
  pProcessingJobStatus_ =
    ProcessingJobSummary'
      { lastModifiedTime =
          Prelude.Nothing,
        processingEndTime = Prelude.Nothing,
        exitMessage = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        processingJobName = pProcessingJobName_,
        processingJobArn = pProcessingJobArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        processingJobStatus = pProcessingJobStatus_
      }

-- | A timestamp that indicates the last time the processing job was
-- modified.
processingJobSummary_lastModifiedTime :: Lens.Lens' ProcessingJobSummary (Prelude.Maybe Prelude.UTCTime)
processingJobSummary_lastModifiedTime = Lens.lens (\ProcessingJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@ProcessingJobSummary' {} a -> s {lastModifiedTime = a} :: ProcessingJobSummary) Prelude.. Lens.mapping Data._Time

-- | The time at which the processing job completed.
processingJobSummary_processingEndTime :: Lens.Lens' ProcessingJobSummary (Prelude.Maybe Prelude.UTCTime)
processingJobSummary_processingEndTime = Lens.lens (\ProcessingJobSummary' {processingEndTime} -> processingEndTime) (\s@ProcessingJobSummary' {} a -> s {processingEndTime = a} :: ProcessingJobSummary) Prelude.. Lens.mapping Data._Time

-- | An optional string, up to one KB in size, that contains metadata from
-- the processing container when the processing job exits.
processingJobSummary_exitMessage :: Lens.Lens' ProcessingJobSummary (Prelude.Maybe Prelude.Text)
processingJobSummary_exitMessage = Lens.lens (\ProcessingJobSummary' {exitMessage} -> exitMessage) (\s@ProcessingJobSummary' {} a -> s {exitMessage = a} :: ProcessingJobSummary)

-- | A string, up to one KB in size, that contains the reason a processing
-- job failed, if it failed.
processingJobSummary_failureReason :: Lens.Lens' ProcessingJobSummary (Prelude.Maybe Prelude.Text)
processingJobSummary_failureReason = Lens.lens (\ProcessingJobSummary' {failureReason} -> failureReason) (\s@ProcessingJobSummary' {} a -> s {failureReason = a} :: ProcessingJobSummary)

-- | The name of the processing job.
processingJobSummary_processingJobName :: Lens.Lens' ProcessingJobSummary Prelude.Text
processingJobSummary_processingJobName = Lens.lens (\ProcessingJobSummary' {processingJobName} -> processingJobName) (\s@ProcessingJobSummary' {} a -> s {processingJobName = a} :: ProcessingJobSummary)

-- | The Amazon Resource Name (ARN) of the processing job..
processingJobSummary_processingJobArn :: Lens.Lens' ProcessingJobSummary Prelude.Text
processingJobSummary_processingJobArn = Lens.lens (\ProcessingJobSummary' {processingJobArn} -> processingJobArn) (\s@ProcessingJobSummary' {} a -> s {processingJobArn = a} :: ProcessingJobSummary)

-- | The time at which the processing job was created.
processingJobSummary_creationTime :: Lens.Lens' ProcessingJobSummary Prelude.UTCTime
processingJobSummary_creationTime = Lens.lens (\ProcessingJobSummary' {creationTime} -> creationTime) (\s@ProcessingJobSummary' {} a -> s {creationTime = a} :: ProcessingJobSummary) Prelude.. Data._Time

-- | The status of the processing job.
processingJobSummary_processingJobStatus :: Lens.Lens' ProcessingJobSummary ProcessingJobStatus
processingJobSummary_processingJobStatus = Lens.lens (\ProcessingJobSummary' {processingJobStatus} -> processingJobStatus) (\s@ProcessingJobSummary' {} a -> s {processingJobStatus = a} :: ProcessingJobSummary)

instance Data.FromJSON ProcessingJobSummary where
  parseJSON =
    Data.withObject
      "ProcessingJobSummary"
      ( \x ->
          ProcessingJobSummary'
            Prelude.<$> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "ProcessingEndTime")
            Prelude.<*> (x Data..:? "ExitMessage")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..: "ProcessingJobName")
            Prelude.<*> (x Data..: "ProcessingJobArn")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "ProcessingJobStatus")
      )

instance Prelude.Hashable ProcessingJobSummary where
  hashWithSalt _salt ProcessingJobSummary' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` processingEndTime
      `Prelude.hashWithSalt` exitMessage
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` processingJobName
      `Prelude.hashWithSalt` processingJobArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` processingJobStatus

instance Prelude.NFData ProcessingJobSummary where
  rnf ProcessingJobSummary' {..} =
    Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf processingEndTime
      `Prelude.seq` Prelude.rnf exitMessage
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf processingJobName
      `Prelude.seq` Prelude.rnf processingJobArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf processingJobStatus
