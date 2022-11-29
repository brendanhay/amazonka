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
-- Module      : Amazonka.Glacier.ListJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists jobs for a vault, including jobs that are
-- in-progress and jobs that have recently finished. The List Job operation
-- returns a list of these jobs sorted by job initiation time.
--
-- Amazon Glacier retains recently completed jobs for a period before
-- deleting them; however, it eventually removes completed jobs. The output
-- of completed jobs can be retrieved. Retaining completed jobs for a
-- period of time after they have completed enables you to get a job output
-- in the event you miss the job completion notification or your first
-- attempt to download it fails. For example, suppose you start an archive
-- retrieval job to download an archive. After the job completes, you start
-- to download the archive but encounter a network error. In this scenario,
-- you can retry and download the archive while the job exists.
--
-- The List Jobs operation supports pagination. You should always check the
-- response @Marker@ field. If there are no more jobs to list, the @Marker@
-- field is set to @null@. If there are more jobs to list, the @Marker@
-- field is set to a non-null value, which you can use to continue the
-- pagination of the list. To return a list of jobs that begins at a
-- specific job, set the marker request parameter to the @Marker@ value for
-- that job that you obtained from a previous List Jobs request.
--
-- You can set a maximum limit for the number of jobs returned in the
-- response by specifying the @limit@ parameter in the request. The default
-- limit is 50. The number of jobs returned might be fewer than the limit,
-- but the number of returned jobs never exceeds the limit.
--
-- Additionally, you can filter the jobs list returned by specifying the
-- optional @statuscode@ parameter or @completed@ parameter, or both. Using
-- the @statuscode@ parameter, you can specify to return only jobs that
-- match either the @InProgress@, @Succeeded@, or @Failed@ status. Using
-- the @completed@ parameter, you can specify to return only jobs that were
-- completed (@true@) or jobs that were not completed (@false@).
--
-- For more information about using this operation, see the documentation
-- for the underlying REST API
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-jobs-get.html List Jobs>.
--
-- This operation returns paginated results.
module Amazonka.Glacier.ListJobs
  ( -- * Creating a Request
    ListJobs (..),
    newListJobs,

    -- * Request Lenses
    listJobs_marker,
    listJobs_limit,
    listJobs_completed,
    listJobs_statuscode,
    listJobs_accountId,
    listJobs_vaultName,

    -- * Destructuring the Response
    ListJobsResponse (..),
    newListJobsResponse,

    -- * Response Lenses
    listJobsResponse_marker,
    listJobsResponse_jobList,
    listJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Provides options for retrieving a job list for an Amazon S3 Glacier
-- vault.
--
-- /See:/ 'newListJobs' smart constructor.
data ListJobs = ListJobs'
  { -- | An opaque string used for pagination. This value specifies the job at
    -- which the listing of jobs should begin. Get the marker value from a
    -- previous List Jobs response. You only need to include the marker if you
    -- are continuing the pagination of results started in a previous List Jobs
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of jobs to be returned. The default limit is 50. The
    -- number of jobs returned might be fewer than the specified limit, but the
    -- number of returned jobs never exceeds the limit.
    limit :: Prelude.Maybe Prelude.Text,
    -- | The state of the jobs to return. You can specify @true@ or @false@.
    completed :: Prelude.Maybe Prelude.Text,
    -- | The type of job status to return. You can specify the following values:
    -- @InProgress@, @Succeeded@, or @Failed@.
    statuscode :: Prelude.Maybe Prelude.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listJobs_marker' - An opaque string used for pagination. This value specifies the job at
-- which the listing of jobs should begin. Get the marker value from a
-- previous List Jobs response. You only need to include the marker if you
-- are continuing the pagination of results started in a previous List Jobs
-- request.
--
-- 'limit', 'listJobs_limit' - The maximum number of jobs to be returned. The default limit is 50. The
-- number of jobs returned might be fewer than the specified limit, but the
-- number of returned jobs never exceeds the limit.
--
-- 'completed', 'listJobs_completed' - The state of the jobs to return. You can specify @true@ or @false@.
--
-- 'statuscode', 'listJobs_statuscode' - The type of job status to return. You can specify the following values:
-- @InProgress@, @Succeeded@, or @Failed@.
--
-- 'accountId', 'listJobs_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'listJobs_vaultName' - The name of the vault.
newListJobs ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  ListJobs
newListJobs pAccountId_ pVaultName_ =
  ListJobs'
    { marker = Prelude.Nothing,
      limit = Prelude.Nothing,
      completed = Prelude.Nothing,
      statuscode = Prelude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | An opaque string used for pagination. This value specifies the job at
-- which the listing of jobs should begin. Get the marker value from a
-- previous List Jobs response. You only need to include the marker if you
-- are continuing the pagination of results started in a previous List Jobs
-- request.
listJobs_marker :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_marker = Lens.lens (\ListJobs' {marker} -> marker) (\s@ListJobs' {} a -> s {marker = a} :: ListJobs)

-- | The maximum number of jobs to be returned. The default limit is 50. The
-- number of jobs returned might be fewer than the specified limit, but the
-- number of returned jobs never exceeds the limit.
listJobs_limit :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_limit = Lens.lens (\ListJobs' {limit} -> limit) (\s@ListJobs' {} a -> s {limit = a} :: ListJobs)

-- | The state of the jobs to return. You can specify @true@ or @false@.
listJobs_completed :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_completed = Lens.lens (\ListJobs' {completed} -> completed) (\s@ListJobs' {} a -> s {completed = a} :: ListJobs)

-- | The type of job status to return. You can specify the following values:
-- @InProgress@, @Succeeded@, or @Failed@.
listJobs_statuscode :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_statuscode = Lens.lens (\ListJobs' {statuscode} -> statuscode) (\s@ListJobs' {} a -> s {statuscode = a} :: ListJobs)

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
listJobs_accountId :: Lens.Lens' ListJobs Prelude.Text
listJobs_accountId = Lens.lens (\ListJobs' {accountId} -> accountId) (\s@ListJobs' {} a -> s {accountId = a} :: ListJobs)

-- | The name of the vault.
listJobs_vaultName :: Lens.Lens' ListJobs Prelude.Text
listJobs_vaultName = Lens.lens (\ListJobs' {vaultName} -> vaultName) (\s@ListJobs' {} a -> s {vaultName = a} :: ListJobs)

instance Core.AWSPager ListJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_jobList Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listJobs_marker
          Lens..~ rs
          Lens.^? listJobsResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListJobs where
  type AWSResponse ListJobs = ListJobsResponse
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Prelude.<$> (x Core..?> "Marker")
            Prelude.<*> (x Core..?> "JobList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobs where
  hashWithSalt _salt ListJobs' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` completed
      `Prelude.hashWithSalt` statuscode
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vaultName

instance Prelude.NFData ListJobs where
  rnf ListJobs' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf completed
      `Prelude.seq` Prelude.rnf statuscode
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf vaultName

instance Core.ToHeaders ListJobs where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListJobs where
  toPath ListJobs' {..} =
    Prelude.mconcat
      [ "/",
        Core.toBS accountId,
        "/vaults/",
        Core.toBS vaultName,
        "/jobs"
      ]

instance Core.ToQuery ListJobs where
  toQuery ListJobs' {..} =
    Prelude.mconcat
      [ "marker" Core.=: marker,
        "limit" Core.=: limit,
        "completed" Core.=: completed,
        "statuscode" Core.=: statuscode
      ]

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { -- | An opaque string used for pagination that specifies the job at which the
    -- listing of jobs should begin. You get the @marker@ value from a previous
    -- List Jobs response. You only need to include the marker if you are
    -- continuing the pagination of the results started in a previous List Jobs
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of job objects. Each job object contains metadata describing the
    -- job.
    jobList :: Prelude.Maybe [GlacierJobDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listJobsResponse_marker' - An opaque string used for pagination that specifies the job at which the
-- listing of jobs should begin. You get the @marker@ value from a previous
-- List Jobs response. You only need to include the marker if you are
-- continuing the pagination of the results started in a previous List Jobs
-- request.
--
-- 'jobList', 'listJobsResponse_jobList' - A list of job objects. Each job object contains metadata describing the
-- job.
--
-- 'httpStatus', 'listJobsResponse_httpStatus' - The response's http status code.
newListJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobsResponse
newListJobsResponse pHttpStatus_ =
  ListJobsResponse'
    { marker = Prelude.Nothing,
      jobList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque string used for pagination that specifies the job at which the
-- listing of jobs should begin. You get the @marker@ value from a previous
-- List Jobs response. You only need to include the marker if you are
-- continuing the pagination of the results started in a previous List Jobs
-- request.
listJobsResponse_marker :: Lens.Lens' ListJobsResponse (Prelude.Maybe Prelude.Text)
listJobsResponse_marker = Lens.lens (\ListJobsResponse' {marker} -> marker) (\s@ListJobsResponse' {} a -> s {marker = a} :: ListJobsResponse)

-- | A list of job objects. Each job object contains metadata describing the
-- job.
listJobsResponse_jobList :: Lens.Lens' ListJobsResponse (Prelude.Maybe [GlacierJobDescription])
listJobsResponse_jobList = Lens.lens (\ListJobsResponse' {jobList} -> jobList) (\s@ListJobsResponse' {} a -> s {jobList = a} :: ListJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listJobsResponse_httpStatus :: Lens.Lens' ListJobsResponse Prelude.Int
listJobsResponse_httpStatus = Lens.lens (\ListJobsResponse' {httpStatus} -> httpStatus) (\s@ListJobsResponse' {} a -> s {httpStatus = a} :: ListJobsResponse)

instance Prelude.NFData ListJobsResponse where
  rnf ListJobsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf jobList
      `Prelude.seq` Prelude.rnf httpStatus
