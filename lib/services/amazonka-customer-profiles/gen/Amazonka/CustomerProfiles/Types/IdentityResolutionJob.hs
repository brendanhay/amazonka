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
-- Module      : Amazonka.CustomerProfiles.Types.IdentityResolutionJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.IdentityResolutionJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.ExportingLocation
import Amazonka.CustomerProfiles.Types.IdentityResolutionJobStatus
import Amazonka.CustomerProfiles.Types.JobStats
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the Identity Resolution Job.
--
-- /See:/ 'newIdentityResolutionJob' smart constructor.
data IdentityResolutionJob = IdentityResolutionJob'
  { -- | The unique name of the domain.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The S3 location where the Identity Resolution Job writes result files.
    exportingLocation :: Prelude.Maybe ExportingLocation,
    -- | The timestamp of when the job was completed.
    jobEndTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier of the Identity Resolution Job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the job was started or will be started.
    jobStartTime :: Prelude.Maybe Data.POSIX,
    -- | Statistics about an Identity Resolution Job.
    jobStats :: Prelude.Maybe JobStats,
    -- | The error messages that are generated when the Identity Resolution Job
    -- runs.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status of the Identity Resolution Job.
    --
    -- -   @PENDING@: The Identity Resolution Job is scheduled but has not
    --     started yet. If you turn off the Identity Resolution feature in your
    --     domain, jobs in the @PENDING@ state are deleted.
    --
    -- -   @PREPROCESSING@: The Identity Resolution Job is loading your data.
    --
    -- -   @FIND_MATCHING@: The Identity Resolution Job is using the machine
    --     learning model to identify profiles that belong to the same matching
    --     group.
    --
    -- -   @MERGING@: The Identity Resolution Job is merging duplicate
    --     profiles.
    --
    -- -   @COMPLETED@: The Identity Resolution Job completed successfully.
    --
    -- -   @PARTIAL_SUCCESS@: There\'s a system error and not all of the data
    --     is merged. The Identity Resolution Job writes a message indicating
    --     the source of the problem.
    --
    -- -   @FAILED@: The Identity Resolution Job did not merge any data. It
    --     writes a message indicating the source of the problem.
    status :: Prelude.Maybe IdentityResolutionJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityResolutionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'identityResolutionJob_domainName' - The unique name of the domain.
--
-- 'exportingLocation', 'identityResolutionJob_exportingLocation' - The S3 location where the Identity Resolution Job writes result files.
--
-- 'jobEndTime', 'identityResolutionJob_jobEndTime' - The timestamp of when the job was completed.
--
-- 'jobId', 'identityResolutionJob_jobId' - The unique identifier of the Identity Resolution Job.
--
-- 'jobStartTime', 'identityResolutionJob_jobStartTime' - The timestamp of when the job was started or will be started.
--
-- 'jobStats', 'identityResolutionJob_jobStats' - Statistics about an Identity Resolution Job.
--
-- 'message', 'identityResolutionJob_message' - The error messages that are generated when the Identity Resolution Job
-- runs.
--
-- 'status', 'identityResolutionJob_status' - The status of the Identity Resolution Job.
--
-- -   @PENDING@: The Identity Resolution Job is scheduled but has not
--     started yet. If you turn off the Identity Resolution feature in your
--     domain, jobs in the @PENDING@ state are deleted.
--
-- -   @PREPROCESSING@: The Identity Resolution Job is loading your data.
--
-- -   @FIND_MATCHING@: The Identity Resolution Job is using the machine
--     learning model to identify profiles that belong to the same matching
--     group.
--
-- -   @MERGING@: The Identity Resolution Job is merging duplicate
--     profiles.
--
-- -   @COMPLETED@: The Identity Resolution Job completed successfully.
--
-- -   @PARTIAL_SUCCESS@: There\'s a system error and not all of the data
--     is merged. The Identity Resolution Job writes a message indicating
--     the source of the problem.
--
-- -   @FAILED@: The Identity Resolution Job did not merge any data. It
--     writes a message indicating the source of the problem.
newIdentityResolutionJob ::
  IdentityResolutionJob
newIdentityResolutionJob =
  IdentityResolutionJob'
    { domainName =
        Prelude.Nothing,
      exportingLocation = Prelude.Nothing,
      jobEndTime = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobStartTime = Prelude.Nothing,
      jobStats = Prelude.Nothing,
      message = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The unique name of the domain.
identityResolutionJob_domainName :: Lens.Lens' IdentityResolutionJob (Prelude.Maybe Prelude.Text)
identityResolutionJob_domainName = Lens.lens (\IdentityResolutionJob' {domainName} -> domainName) (\s@IdentityResolutionJob' {} a -> s {domainName = a} :: IdentityResolutionJob)

-- | The S3 location where the Identity Resolution Job writes result files.
identityResolutionJob_exportingLocation :: Lens.Lens' IdentityResolutionJob (Prelude.Maybe ExportingLocation)
identityResolutionJob_exportingLocation = Lens.lens (\IdentityResolutionJob' {exportingLocation} -> exportingLocation) (\s@IdentityResolutionJob' {} a -> s {exportingLocation = a} :: IdentityResolutionJob)

-- | The timestamp of when the job was completed.
identityResolutionJob_jobEndTime :: Lens.Lens' IdentityResolutionJob (Prelude.Maybe Prelude.UTCTime)
identityResolutionJob_jobEndTime = Lens.lens (\IdentityResolutionJob' {jobEndTime} -> jobEndTime) (\s@IdentityResolutionJob' {} a -> s {jobEndTime = a} :: IdentityResolutionJob) Prelude.. Lens.mapping Data._Time

-- | The unique identifier of the Identity Resolution Job.
identityResolutionJob_jobId :: Lens.Lens' IdentityResolutionJob (Prelude.Maybe Prelude.Text)
identityResolutionJob_jobId = Lens.lens (\IdentityResolutionJob' {jobId} -> jobId) (\s@IdentityResolutionJob' {} a -> s {jobId = a} :: IdentityResolutionJob)

-- | The timestamp of when the job was started or will be started.
identityResolutionJob_jobStartTime :: Lens.Lens' IdentityResolutionJob (Prelude.Maybe Prelude.UTCTime)
identityResolutionJob_jobStartTime = Lens.lens (\IdentityResolutionJob' {jobStartTime} -> jobStartTime) (\s@IdentityResolutionJob' {} a -> s {jobStartTime = a} :: IdentityResolutionJob) Prelude.. Lens.mapping Data._Time

-- | Statistics about an Identity Resolution Job.
identityResolutionJob_jobStats :: Lens.Lens' IdentityResolutionJob (Prelude.Maybe JobStats)
identityResolutionJob_jobStats = Lens.lens (\IdentityResolutionJob' {jobStats} -> jobStats) (\s@IdentityResolutionJob' {} a -> s {jobStats = a} :: IdentityResolutionJob)

-- | The error messages that are generated when the Identity Resolution Job
-- runs.
identityResolutionJob_message :: Lens.Lens' IdentityResolutionJob (Prelude.Maybe Prelude.Text)
identityResolutionJob_message = Lens.lens (\IdentityResolutionJob' {message} -> message) (\s@IdentityResolutionJob' {} a -> s {message = a} :: IdentityResolutionJob)

-- | The status of the Identity Resolution Job.
--
-- -   @PENDING@: The Identity Resolution Job is scheduled but has not
--     started yet. If you turn off the Identity Resolution feature in your
--     domain, jobs in the @PENDING@ state are deleted.
--
-- -   @PREPROCESSING@: The Identity Resolution Job is loading your data.
--
-- -   @FIND_MATCHING@: The Identity Resolution Job is using the machine
--     learning model to identify profiles that belong to the same matching
--     group.
--
-- -   @MERGING@: The Identity Resolution Job is merging duplicate
--     profiles.
--
-- -   @COMPLETED@: The Identity Resolution Job completed successfully.
--
-- -   @PARTIAL_SUCCESS@: There\'s a system error and not all of the data
--     is merged. The Identity Resolution Job writes a message indicating
--     the source of the problem.
--
-- -   @FAILED@: The Identity Resolution Job did not merge any data. It
--     writes a message indicating the source of the problem.
identityResolutionJob_status :: Lens.Lens' IdentityResolutionJob (Prelude.Maybe IdentityResolutionJobStatus)
identityResolutionJob_status = Lens.lens (\IdentityResolutionJob' {status} -> status) (\s@IdentityResolutionJob' {} a -> s {status = a} :: IdentityResolutionJob)

instance Data.FromJSON IdentityResolutionJob where
  parseJSON =
    Data.withObject
      "IdentityResolutionJob"
      ( \x ->
          IdentityResolutionJob'
            Prelude.<$> (x Data..:? "DomainName")
            Prelude.<*> (x Data..:? "ExportingLocation")
            Prelude.<*> (x Data..:? "JobEndTime")
            Prelude.<*> (x Data..:? "JobId")
            Prelude.<*> (x Data..:? "JobStartTime")
            Prelude.<*> (x Data..:? "JobStats")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable IdentityResolutionJob where
  hashWithSalt _salt IdentityResolutionJob' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` exportingLocation
      `Prelude.hashWithSalt` jobEndTime
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobStartTime
      `Prelude.hashWithSalt` jobStats
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` status

instance Prelude.NFData IdentityResolutionJob where
  rnf IdentityResolutionJob' {..} =
    Prelude.rnf domainName `Prelude.seq`
      Prelude.rnf exportingLocation `Prelude.seq`
        Prelude.rnf jobEndTime `Prelude.seq`
          Prelude.rnf jobId `Prelude.seq`
            Prelude.rnf jobStartTime `Prelude.seq`
              Prelude.rnf jobStats `Prelude.seq`
                Prelude.rnf message `Prelude.seq`
                  Prelude.rnf status
