{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CustomerProfiles.GetIdentityResolutionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an Identity Resolution Job in a specific
-- domain.
--
-- Identity Resolution Jobs are set up using the Amazon Connect admin
-- console. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/use-identity-resolution.html Use Identity Resolution to consolidate similar profiles>.
module Amazonka.CustomerProfiles.GetIdentityResolutionJob
  ( -- * Creating a Request
    GetIdentityResolutionJob (..),
    newGetIdentityResolutionJob,

    -- * Request Lenses
    getIdentityResolutionJob_domainName,
    getIdentityResolutionJob_jobId,

    -- * Destructuring the Response
    GetIdentityResolutionJobResponse (..),
    newGetIdentityResolutionJobResponse,

    -- * Response Lenses
    getIdentityResolutionJobResponse_exportingLocation,
    getIdentityResolutionJobResponse_jobStats,
    getIdentityResolutionJobResponse_message,
    getIdentityResolutionJobResponse_lastUpdatedAt,
    getIdentityResolutionJobResponse_domainName,
    getIdentityResolutionJobResponse_autoMerging,
    getIdentityResolutionJobResponse_jobId,
    getIdentityResolutionJobResponse_status,
    getIdentityResolutionJobResponse_jobStartTime,
    getIdentityResolutionJobResponse_jobEndTime,
    getIdentityResolutionJobResponse_jobExpirationTime,
    getIdentityResolutionJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIdentityResolutionJob' smart constructor.
data GetIdentityResolutionJob = GetIdentityResolutionJob'
  { -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The unique identifier of the Identity Resolution Job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIdentityResolutionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getIdentityResolutionJob_domainName' - The unique name of the domain.
--
-- 'jobId', 'getIdentityResolutionJob_jobId' - The unique identifier of the Identity Resolution Job.
newGetIdentityResolutionJob ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  GetIdentityResolutionJob
newGetIdentityResolutionJob pDomainName_ pJobId_ =
  GetIdentityResolutionJob'
    { domainName =
        pDomainName_,
      jobId = pJobId_
    }

-- | The unique name of the domain.
getIdentityResolutionJob_domainName :: Lens.Lens' GetIdentityResolutionJob Prelude.Text
getIdentityResolutionJob_domainName = Lens.lens (\GetIdentityResolutionJob' {domainName} -> domainName) (\s@GetIdentityResolutionJob' {} a -> s {domainName = a} :: GetIdentityResolutionJob)

-- | The unique identifier of the Identity Resolution Job.
getIdentityResolutionJob_jobId :: Lens.Lens' GetIdentityResolutionJob Prelude.Text
getIdentityResolutionJob_jobId = Lens.lens (\GetIdentityResolutionJob' {jobId} -> jobId) (\s@GetIdentityResolutionJob' {} a -> s {jobId = a} :: GetIdentityResolutionJob)

instance Core.AWSRequest GetIdentityResolutionJob where
  type
    AWSResponse GetIdentityResolutionJob =
      GetIdentityResolutionJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIdentityResolutionJobResponse'
            Prelude.<$> (x Data..?> "ExportingLocation")
            Prelude.<*> (x Data..?> "JobStats")
            Prelude.<*> (x Data..?> "Message")
            Prelude.<*> (x Data..?> "LastUpdatedAt")
            Prelude.<*> (x Data..?> "DomainName")
            Prelude.<*> (x Data..?> "AutoMerging")
            Prelude.<*> (x Data..?> "JobId")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "JobStartTime")
            Prelude.<*> (x Data..?> "JobEndTime")
            Prelude.<*> (x Data..?> "JobExpirationTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIdentityResolutionJob where
  hashWithSalt _salt GetIdentityResolutionJob' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData GetIdentityResolutionJob where
  rnf GetIdentityResolutionJob' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders GetIdentityResolutionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetIdentityResolutionJob where
  toPath GetIdentityResolutionJob' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/identity-resolution-jobs/",
        Data.toBS jobId
      ]

instance Data.ToQuery GetIdentityResolutionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIdentityResolutionJobResponse' smart constructor.
data GetIdentityResolutionJobResponse = GetIdentityResolutionJobResponse'
  { -- | The S3 location where the Identity Resolution Job writes result files.
    exportingLocation :: Prelude.Maybe ExportingLocation,
    -- | Statistics about the Identity Resolution Job.
    jobStats :: Prelude.Maybe JobStats,
    -- | The error messages that are generated when the Identity Resolution Job
    -- runs.
    message :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the Identity Resolution Job was most recently
    -- edited.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The unique name of the domain.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | Configuration settings for how to perform the auto-merging of profiles.
    autoMerging :: Prelude.Maybe AutoMerging,
    -- | The unique identifier of the Identity Resolution Job.
    jobId :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe IdentityResolutionJobStatus,
    -- | The timestamp of when the Identity Resolution Job was started or will be
    -- started.
    jobStartTime :: Prelude.Maybe Data.POSIX,
    -- | The timestamp of when the Identity Resolution Job was completed.
    jobEndTime :: Prelude.Maybe Data.POSIX,
    -- | The timestamp of when the Identity Resolution Job will expire.
    jobExpirationTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIdentityResolutionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportingLocation', 'getIdentityResolutionJobResponse_exportingLocation' - The S3 location where the Identity Resolution Job writes result files.
--
-- 'jobStats', 'getIdentityResolutionJobResponse_jobStats' - Statistics about the Identity Resolution Job.
--
-- 'message', 'getIdentityResolutionJobResponse_message' - The error messages that are generated when the Identity Resolution Job
-- runs.
--
-- 'lastUpdatedAt', 'getIdentityResolutionJobResponse_lastUpdatedAt' - The timestamp of when the Identity Resolution Job was most recently
-- edited.
--
-- 'domainName', 'getIdentityResolutionJobResponse_domainName' - The unique name of the domain.
--
-- 'autoMerging', 'getIdentityResolutionJobResponse_autoMerging' - Configuration settings for how to perform the auto-merging of profiles.
--
-- 'jobId', 'getIdentityResolutionJobResponse_jobId' - The unique identifier of the Identity Resolution Job.
--
-- 'status', 'getIdentityResolutionJobResponse_status' - The status of the Identity Resolution Job.
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
--
-- 'jobStartTime', 'getIdentityResolutionJobResponse_jobStartTime' - The timestamp of when the Identity Resolution Job was started or will be
-- started.
--
-- 'jobEndTime', 'getIdentityResolutionJobResponse_jobEndTime' - The timestamp of when the Identity Resolution Job was completed.
--
-- 'jobExpirationTime', 'getIdentityResolutionJobResponse_jobExpirationTime' - The timestamp of when the Identity Resolution Job will expire.
--
-- 'httpStatus', 'getIdentityResolutionJobResponse_httpStatus' - The response's http status code.
newGetIdentityResolutionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIdentityResolutionJobResponse
newGetIdentityResolutionJobResponse pHttpStatus_ =
  GetIdentityResolutionJobResponse'
    { exportingLocation =
        Prelude.Nothing,
      jobStats = Prelude.Nothing,
      message = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      domainName = Prelude.Nothing,
      autoMerging = Prelude.Nothing,
      jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      jobStartTime = Prelude.Nothing,
      jobEndTime = Prelude.Nothing,
      jobExpirationTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The S3 location where the Identity Resolution Job writes result files.
getIdentityResolutionJobResponse_exportingLocation :: Lens.Lens' GetIdentityResolutionJobResponse (Prelude.Maybe ExportingLocation)
getIdentityResolutionJobResponse_exportingLocation = Lens.lens (\GetIdentityResolutionJobResponse' {exportingLocation} -> exportingLocation) (\s@GetIdentityResolutionJobResponse' {} a -> s {exportingLocation = a} :: GetIdentityResolutionJobResponse)

-- | Statistics about the Identity Resolution Job.
getIdentityResolutionJobResponse_jobStats :: Lens.Lens' GetIdentityResolutionJobResponse (Prelude.Maybe JobStats)
getIdentityResolutionJobResponse_jobStats = Lens.lens (\GetIdentityResolutionJobResponse' {jobStats} -> jobStats) (\s@GetIdentityResolutionJobResponse' {} a -> s {jobStats = a} :: GetIdentityResolutionJobResponse)

-- | The error messages that are generated when the Identity Resolution Job
-- runs.
getIdentityResolutionJobResponse_message :: Lens.Lens' GetIdentityResolutionJobResponse (Prelude.Maybe Prelude.Text)
getIdentityResolutionJobResponse_message = Lens.lens (\GetIdentityResolutionJobResponse' {message} -> message) (\s@GetIdentityResolutionJobResponse' {} a -> s {message = a} :: GetIdentityResolutionJobResponse)

-- | The timestamp of when the Identity Resolution Job was most recently
-- edited.
getIdentityResolutionJobResponse_lastUpdatedAt :: Lens.Lens' GetIdentityResolutionJobResponse (Prelude.Maybe Prelude.UTCTime)
getIdentityResolutionJobResponse_lastUpdatedAt = Lens.lens (\GetIdentityResolutionJobResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetIdentityResolutionJobResponse' {} a -> s {lastUpdatedAt = a} :: GetIdentityResolutionJobResponse) Prelude.. Lens.mapping Data._Time

-- | The unique name of the domain.
getIdentityResolutionJobResponse_domainName :: Lens.Lens' GetIdentityResolutionJobResponse (Prelude.Maybe Prelude.Text)
getIdentityResolutionJobResponse_domainName = Lens.lens (\GetIdentityResolutionJobResponse' {domainName} -> domainName) (\s@GetIdentityResolutionJobResponse' {} a -> s {domainName = a} :: GetIdentityResolutionJobResponse)

-- | Configuration settings for how to perform the auto-merging of profiles.
getIdentityResolutionJobResponse_autoMerging :: Lens.Lens' GetIdentityResolutionJobResponse (Prelude.Maybe AutoMerging)
getIdentityResolutionJobResponse_autoMerging = Lens.lens (\GetIdentityResolutionJobResponse' {autoMerging} -> autoMerging) (\s@GetIdentityResolutionJobResponse' {} a -> s {autoMerging = a} :: GetIdentityResolutionJobResponse)

-- | The unique identifier of the Identity Resolution Job.
getIdentityResolutionJobResponse_jobId :: Lens.Lens' GetIdentityResolutionJobResponse (Prelude.Maybe Prelude.Text)
getIdentityResolutionJobResponse_jobId = Lens.lens (\GetIdentityResolutionJobResponse' {jobId} -> jobId) (\s@GetIdentityResolutionJobResponse' {} a -> s {jobId = a} :: GetIdentityResolutionJobResponse)

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
getIdentityResolutionJobResponse_status :: Lens.Lens' GetIdentityResolutionJobResponse (Prelude.Maybe IdentityResolutionJobStatus)
getIdentityResolutionJobResponse_status = Lens.lens (\GetIdentityResolutionJobResponse' {status} -> status) (\s@GetIdentityResolutionJobResponse' {} a -> s {status = a} :: GetIdentityResolutionJobResponse)

-- | The timestamp of when the Identity Resolution Job was started or will be
-- started.
getIdentityResolutionJobResponse_jobStartTime :: Lens.Lens' GetIdentityResolutionJobResponse (Prelude.Maybe Prelude.UTCTime)
getIdentityResolutionJobResponse_jobStartTime = Lens.lens (\GetIdentityResolutionJobResponse' {jobStartTime} -> jobStartTime) (\s@GetIdentityResolutionJobResponse' {} a -> s {jobStartTime = a} :: GetIdentityResolutionJobResponse) Prelude.. Lens.mapping Data._Time

-- | The timestamp of when the Identity Resolution Job was completed.
getIdentityResolutionJobResponse_jobEndTime :: Lens.Lens' GetIdentityResolutionJobResponse (Prelude.Maybe Prelude.UTCTime)
getIdentityResolutionJobResponse_jobEndTime = Lens.lens (\GetIdentityResolutionJobResponse' {jobEndTime} -> jobEndTime) (\s@GetIdentityResolutionJobResponse' {} a -> s {jobEndTime = a} :: GetIdentityResolutionJobResponse) Prelude.. Lens.mapping Data._Time

-- | The timestamp of when the Identity Resolution Job will expire.
getIdentityResolutionJobResponse_jobExpirationTime :: Lens.Lens' GetIdentityResolutionJobResponse (Prelude.Maybe Prelude.UTCTime)
getIdentityResolutionJobResponse_jobExpirationTime = Lens.lens (\GetIdentityResolutionJobResponse' {jobExpirationTime} -> jobExpirationTime) (\s@GetIdentityResolutionJobResponse' {} a -> s {jobExpirationTime = a} :: GetIdentityResolutionJobResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getIdentityResolutionJobResponse_httpStatus :: Lens.Lens' GetIdentityResolutionJobResponse Prelude.Int
getIdentityResolutionJobResponse_httpStatus = Lens.lens (\GetIdentityResolutionJobResponse' {httpStatus} -> httpStatus) (\s@GetIdentityResolutionJobResponse' {} a -> s {httpStatus = a} :: GetIdentityResolutionJobResponse)

instance
  Prelude.NFData
    GetIdentityResolutionJobResponse
  where
  rnf GetIdentityResolutionJobResponse' {..} =
    Prelude.rnf exportingLocation
      `Prelude.seq` Prelude.rnf jobStats
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf autoMerging
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf jobStartTime
      `Prelude.seq` Prelude.rnf jobEndTime
      `Prelude.seq` Prelude.rnf jobExpirationTime
      `Prelude.seq` Prelude.rnf httpStatus
