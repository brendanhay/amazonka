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
-- Module      : Amazonka.Braket.Types.JobSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.JobSummary where

import Amazonka.Braket.Types.JobPrimaryStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides summary information about an Amazon Braket job.
--
-- /See:/ 'newJobSummary' smart constructor.
data JobSummary = JobSummary'
  { -- | The date and time that the Amazon Braket job ended.
    endedAt :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the Amazon Braket job was started.
    startedAt :: Prelude.Maybe Data.POSIX,
    -- | A tag object that consists of a key and an optional value, used to
    -- manage metadata for Amazon Braket resources.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date and time that the Amazon Braket job was created.
    createdAt :: Data.POSIX,
    -- | Provides summary information about the primary device used by an Amazon
    -- Braket job.
    device :: Prelude.Text,
    -- | The ARN of the Amazon Braket job.
    jobArn :: Prelude.Text,
    -- | The name of the Amazon Braket job.
    jobName :: Prelude.Text,
    -- | The status of the Amazon Braket job.
    status :: JobPrimaryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endedAt', 'jobSummary_endedAt' - The date and time that the Amazon Braket job ended.
--
-- 'startedAt', 'jobSummary_startedAt' - The date and time that the Amazon Braket job was started.
--
-- 'tags', 'jobSummary_tags' - A tag object that consists of a key and an optional value, used to
-- manage metadata for Amazon Braket resources.
--
-- 'createdAt', 'jobSummary_createdAt' - The date and time that the Amazon Braket job was created.
--
-- 'device', 'jobSummary_device' - Provides summary information about the primary device used by an Amazon
-- Braket job.
--
-- 'jobArn', 'jobSummary_jobArn' - The ARN of the Amazon Braket job.
--
-- 'jobName', 'jobSummary_jobName' - The name of the Amazon Braket job.
--
-- 'status', 'jobSummary_status' - The status of the Amazon Braket job.
newJobSummary ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'device'
  Prelude.Text ->
  -- | 'jobArn'
  Prelude.Text ->
  -- | 'jobName'
  Prelude.Text ->
  -- | 'status'
  JobPrimaryStatus ->
  JobSummary
newJobSummary
  pCreatedAt_
  pDevice_
  pJobArn_
  pJobName_
  pStatus_ =
    JobSummary'
      { endedAt = Prelude.Nothing,
        startedAt = Prelude.Nothing,
        tags = Prelude.Nothing,
        createdAt = Data._Time Lens.# pCreatedAt_,
        device = pDevice_,
        jobArn = pJobArn_,
        jobName = pJobName_,
        status = pStatus_
      }

-- | The date and time that the Amazon Braket job ended.
jobSummary_endedAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.UTCTime)
jobSummary_endedAt = Lens.lens (\JobSummary' {endedAt} -> endedAt) (\s@JobSummary' {} a -> s {endedAt = a} :: JobSummary) Prelude.. Lens.mapping Data._Time

-- | The date and time that the Amazon Braket job was started.
jobSummary_startedAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.UTCTime)
jobSummary_startedAt = Lens.lens (\JobSummary' {startedAt} -> startedAt) (\s@JobSummary' {} a -> s {startedAt = a} :: JobSummary) Prelude.. Lens.mapping Data._Time

-- | A tag object that consists of a key and an optional value, used to
-- manage metadata for Amazon Braket resources.
jobSummary_tags :: Lens.Lens' JobSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobSummary_tags = Lens.lens (\JobSummary' {tags} -> tags) (\s@JobSummary' {} a -> s {tags = a} :: JobSummary) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the Amazon Braket job was created.
jobSummary_createdAt :: Lens.Lens' JobSummary Prelude.UTCTime
jobSummary_createdAt = Lens.lens (\JobSummary' {createdAt} -> createdAt) (\s@JobSummary' {} a -> s {createdAt = a} :: JobSummary) Prelude.. Data._Time

-- | Provides summary information about the primary device used by an Amazon
-- Braket job.
jobSummary_device :: Lens.Lens' JobSummary Prelude.Text
jobSummary_device = Lens.lens (\JobSummary' {device} -> device) (\s@JobSummary' {} a -> s {device = a} :: JobSummary)

-- | The ARN of the Amazon Braket job.
jobSummary_jobArn :: Lens.Lens' JobSummary Prelude.Text
jobSummary_jobArn = Lens.lens (\JobSummary' {jobArn} -> jobArn) (\s@JobSummary' {} a -> s {jobArn = a} :: JobSummary)

-- | The name of the Amazon Braket job.
jobSummary_jobName :: Lens.Lens' JobSummary Prelude.Text
jobSummary_jobName = Lens.lens (\JobSummary' {jobName} -> jobName) (\s@JobSummary' {} a -> s {jobName = a} :: JobSummary)

-- | The status of the Amazon Braket job.
jobSummary_status :: Lens.Lens' JobSummary JobPrimaryStatus
jobSummary_status = Lens.lens (\JobSummary' {status} -> status) (\s@JobSummary' {} a -> s {status = a} :: JobSummary)

instance Data.FromJSON JobSummary where
  parseJSON =
    Data.withObject
      "JobSummary"
      ( \x ->
          JobSummary'
            Prelude.<$> (x Data..:? "endedAt")
            Prelude.<*> (x Data..:? "startedAt")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "device")
            Prelude.<*> (x Data..: "jobArn")
            Prelude.<*> (x Data..: "jobName")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable JobSummary where
  hashWithSalt _salt JobSummary' {..} =
    _salt `Prelude.hashWithSalt` endedAt
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` device
      `Prelude.hashWithSalt` jobArn
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` status

instance Prelude.NFData JobSummary where
  rnf JobSummary' {..} =
    Prelude.rnf endedAt
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf device
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf status
