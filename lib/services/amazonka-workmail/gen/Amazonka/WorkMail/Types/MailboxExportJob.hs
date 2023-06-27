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
-- Module      : Amazonka.WorkMail.Types.MailboxExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.MailboxExportJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.MailboxExportJobState

-- | The details of a mailbox export job, including the user or resource ID
-- associated with the mailbox and the S3 bucket that the mailbox contents
-- are exported to.
--
-- /See:/ 'newMailboxExportJob' smart constructor.
data MailboxExportJob = MailboxExportJob'
  { -- | The mailbox export job description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The mailbox export job end timestamp.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the user or resource associated with the mailbox.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | The estimated progress of the mailbox export job, in percentage points.
    estimatedProgress :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the mailbox export job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | The path to the S3 bucket and file that the mailbox export job exports
    -- to.
    s3Path :: Prelude.Maybe Prelude.Text,
    -- | The mailbox export job start timestamp.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the mailbox export job.
    state :: Prelude.Maybe MailboxExportJobState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MailboxExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'mailboxExportJob_description' - The mailbox export job description.
--
-- 'endTime', 'mailboxExportJob_endTime' - The mailbox export job end timestamp.
--
-- 'entityId', 'mailboxExportJob_entityId' - The identifier of the user or resource associated with the mailbox.
--
-- 'estimatedProgress', 'mailboxExportJob_estimatedProgress' - The estimated progress of the mailbox export job, in percentage points.
--
-- 'jobId', 'mailboxExportJob_jobId' - The identifier of the mailbox export job.
--
-- 's3BucketName', 'mailboxExportJob_s3BucketName' - The name of the S3 bucket.
--
-- 's3Path', 'mailboxExportJob_s3Path' - The path to the S3 bucket and file that the mailbox export job exports
-- to.
--
-- 'startTime', 'mailboxExportJob_startTime' - The mailbox export job start timestamp.
--
-- 'state', 'mailboxExportJob_state' - The state of the mailbox export job.
newMailboxExportJob ::
  MailboxExportJob
newMailboxExportJob =
  MailboxExportJob'
    { description = Prelude.Nothing,
      endTime = Prelude.Nothing,
      entityId = Prelude.Nothing,
      estimatedProgress = Prelude.Nothing,
      jobId = Prelude.Nothing,
      s3BucketName = Prelude.Nothing,
      s3Path = Prelude.Nothing,
      startTime = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The mailbox export job description.
mailboxExportJob_description :: Lens.Lens' MailboxExportJob (Prelude.Maybe Prelude.Text)
mailboxExportJob_description = Lens.lens (\MailboxExportJob' {description} -> description) (\s@MailboxExportJob' {} a -> s {description = a} :: MailboxExportJob)

-- | The mailbox export job end timestamp.
mailboxExportJob_endTime :: Lens.Lens' MailboxExportJob (Prelude.Maybe Prelude.UTCTime)
mailboxExportJob_endTime = Lens.lens (\MailboxExportJob' {endTime} -> endTime) (\s@MailboxExportJob' {} a -> s {endTime = a} :: MailboxExportJob) Prelude.. Lens.mapping Data._Time

-- | The identifier of the user or resource associated with the mailbox.
mailboxExportJob_entityId :: Lens.Lens' MailboxExportJob (Prelude.Maybe Prelude.Text)
mailboxExportJob_entityId = Lens.lens (\MailboxExportJob' {entityId} -> entityId) (\s@MailboxExportJob' {} a -> s {entityId = a} :: MailboxExportJob)

-- | The estimated progress of the mailbox export job, in percentage points.
mailboxExportJob_estimatedProgress :: Lens.Lens' MailboxExportJob (Prelude.Maybe Prelude.Natural)
mailboxExportJob_estimatedProgress = Lens.lens (\MailboxExportJob' {estimatedProgress} -> estimatedProgress) (\s@MailboxExportJob' {} a -> s {estimatedProgress = a} :: MailboxExportJob)

-- | The identifier of the mailbox export job.
mailboxExportJob_jobId :: Lens.Lens' MailboxExportJob (Prelude.Maybe Prelude.Text)
mailboxExportJob_jobId = Lens.lens (\MailboxExportJob' {jobId} -> jobId) (\s@MailboxExportJob' {} a -> s {jobId = a} :: MailboxExportJob)

-- | The name of the S3 bucket.
mailboxExportJob_s3BucketName :: Lens.Lens' MailboxExportJob (Prelude.Maybe Prelude.Text)
mailboxExportJob_s3BucketName = Lens.lens (\MailboxExportJob' {s3BucketName} -> s3BucketName) (\s@MailboxExportJob' {} a -> s {s3BucketName = a} :: MailboxExportJob)

-- | The path to the S3 bucket and file that the mailbox export job exports
-- to.
mailboxExportJob_s3Path :: Lens.Lens' MailboxExportJob (Prelude.Maybe Prelude.Text)
mailboxExportJob_s3Path = Lens.lens (\MailboxExportJob' {s3Path} -> s3Path) (\s@MailboxExportJob' {} a -> s {s3Path = a} :: MailboxExportJob)

-- | The mailbox export job start timestamp.
mailboxExportJob_startTime :: Lens.Lens' MailboxExportJob (Prelude.Maybe Prelude.UTCTime)
mailboxExportJob_startTime = Lens.lens (\MailboxExportJob' {startTime} -> startTime) (\s@MailboxExportJob' {} a -> s {startTime = a} :: MailboxExportJob) Prelude.. Lens.mapping Data._Time

-- | The state of the mailbox export job.
mailboxExportJob_state :: Lens.Lens' MailboxExportJob (Prelude.Maybe MailboxExportJobState)
mailboxExportJob_state = Lens.lens (\MailboxExportJob' {state} -> state) (\s@MailboxExportJob' {} a -> s {state = a} :: MailboxExportJob)

instance Data.FromJSON MailboxExportJob where
  parseJSON =
    Data.withObject
      "MailboxExportJob"
      ( \x ->
          MailboxExportJob'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "EntityId")
            Prelude.<*> (x Data..:? "EstimatedProgress")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "S3BucketName")
            Prelude.<*> (x Data..:? "S3Path")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable MailboxExportJob where
  hashWithSalt _salt MailboxExportJob' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` estimatedProgress
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3Path
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` state

instance Prelude.NFData MailboxExportJob where
  rnf MailboxExportJob' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf estimatedProgress
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf s3Path
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf state
