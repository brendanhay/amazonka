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
-- Module      : Amazonka.Backup.ListRestoreJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of jobs that Backup initiated to restore a saved
-- resource, including details about the recovery process.
--
-- This operation returns paginated results.
module Amazonka.Backup.ListRestoreJobs
  ( -- * Creating a Request
    ListRestoreJobs (..),
    newListRestoreJobs,

    -- * Request Lenses
    listRestoreJobs_byAccountId,
    listRestoreJobs_byCompleteAfter,
    listRestoreJobs_byCompleteBefore,
    listRestoreJobs_byCreatedAfter,
    listRestoreJobs_byCreatedBefore,
    listRestoreJobs_byStatus,
    listRestoreJobs_maxResults,
    listRestoreJobs_nextToken,

    -- * Destructuring the Response
    ListRestoreJobsResponse (..),
    newListRestoreJobsResponse,

    -- * Response Lenses
    listRestoreJobsResponse_nextToken,
    listRestoreJobsResponse_restoreJobs,
    listRestoreJobsResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRestoreJobs' smart constructor.
data ListRestoreJobs = ListRestoreJobs'
  { -- | The account ID to list the jobs from. Returns only restore jobs
    -- associated with the specified account ID.
    byAccountId :: Prelude.Maybe Prelude.Text,
    -- | Returns only copy jobs completed after a date expressed in Unix format
    -- and Coordinated Universal Time (UTC).
    byCompleteAfter :: Prelude.Maybe Data.POSIX,
    -- | Returns only copy jobs completed before a date expressed in Unix format
    -- and Coordinated Universal Time (UTC).
    byCompleteBefore :: Prelude.Maybe Data.POSIX,
    -- | Returns only restore jobs that were created after the specified date.
    byCreatedAfter :: Prelude.Maybe Data.POSIX,
    -- | Returns only restore jobs that were created before the specified date.
    byCreatedBefore :: Prelude.Maybe Data.POSIX,
    -- | Returns only restore jobs associated with the specified job status.
    byStatus :: Prelude.Maybe RestoreJobStatus,
    -- | The maximum number of items to be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRestoreJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byAccountId', 'listRestoreJobs_byAccountId' - The account ID to list the jobs from. Returns only restore jobs
-- associated with the specified account ID.
--
-- 'byCompleteAfter', 'listRestoreJobs_byCompleteAfter' - Returns only copy jobs completed after a date expressed in Unix format
-- and Coordinated Universal Time (UTC).
--
-- 'byCompleteBefore', 'listRestoreJobs_byCompleteBefore' - Returns only copy jobs completed before a date expressed in Unix format
-- and Coordinated Universal Time (UTC).
--
-- 'byCreatedAfter', 'listRestoreJobs_byCreatedAfter' - Returns only restore jobs that were created after the specified date.
--
-- 'byCreatedBefore', 'listRestoreJobs_byCreatedBefore' - Returns only restore jobs that were created before the specified date.
--
-- 'byStatus', 'listRestoreJobs_byStatus' - Returns only restore jobs associated with the specified job status.
--
-- 'maxResults', 'listRestoreJobs_maxResults' - The maximum number of items to be returned.
--
-- 'nextToken', 'listRestoreJobs_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
newListRestoreJobs ::
  ListRestoreJobs
newListRestoreJobs =
  ListRestoreJobs'
    { byAccountId = Prelude.Nothing,
      byCompleteAfter = Prelude.Nothing,
      byCompleteBefore = Prelude.Nothing,
      byCreatedAfter = Prelude.Nothing,
      byCreatedBefore = Prelude.Nothing,
      byStatus = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The account ID to list the jobs from. Returns only restore jobs
-- associated with the specified account ID.
listRestoreJobs_byAccountId :: Lens.Lens' ListRestoreJobs (Prelude.Maybe Prelude.Text)
listRestoreJobs_byAccountId = Lens.lens (\ListRestoreJobs' {byAccountId} -> byAccountId) (\s@ListRestoreJobs' {} a -> s {byAccountId = a} :: ListRestoreJobs)

-- | Returns only copy jobs completed after a date expressed in Unix format
-- and Coordinated Universal Time (UTC).
listRestoreJobs_byCompleteAfter :: Lens.Lens' ListRestoreJobs (Prelude.Maybe Prelude.UTCTime)
listRestoreJobs_byCompleteAfter = Lens.lens (\ListRestoreJobs' {byCompleteAfter} -> byCompleteAfter) (\s@ListRestoreJobs' {} a -> s {byCompleteAfter = a} :: ListRestoreJobs) Prelude.. Lens.mapping Data._Time

-- | Returns only copy jobs completed before a date expressed in Unix format
-- and Coordinated Universal Time (UTC).
listRestoreJobs_byCompleteBefore :: Lens.Lens' ListRestoreJobs (Prelude.Maybe Prelude.UTCTime)
listRestoreJobs_byCompleteBefore = Lens.lens (\ListRestoreJobs' {byCompleteBefore} -> byCompleteBefore) (\s@ListRestoreJobs' {} a -> s {byCompleteBefore = a} :: ListRestoreJobs) Prelude.. Lens.mapping Data._Time

-- | Returns only restore jobs that were created after the specified date.
listRestoreJobs_byCreatedAfter :: Lens.Lens' ListRestoreJobs (Prelude.Maybe Prelude.UTCTime)
listRestoreJobs_byCreatedAfter = Lens.lens (\ListRestoreJobs' {byCreatedAfter} -> byCreatedAfter) (\s@ListRestoreJobs' {} a -> s {byCreatedAfter = a} :: ListRestoreJobs) Prelude.. Lens.mapping Data._Time

-- | Returns only restore jobs that were created before the specified date.
listRestoreJobs_byCreatedBefore :: Lens.Lens' ListRestoreJobs (Prelude.Maybe Prelude.UTCTime)
listRestoreJobs_byCreatedBefore = Lens.lens (\ListRestoreJobs' {byCreatedBefore} -> byCreatedBefore) (\s@ListRestoreJobs' {} a -> s {byCreatedBefore = a} :: ListRestoreJobs) Prelude.. Lens.mapping Data._Time

-- | Returns only restore jobs associated with the specified job status.
listRestoreJobs_byStatus :: Lens.Lens' ListRestoreJobs (Prelude.Maybe RestoreJobStatus)
listRestoreJobs_byStatus = Lens.lens (\ListRestoreJobs' {byStatus} -> byStatus) (\s@ListRestoreJobs' {} a -> s {byStatus = a} :: ListRestoreJobs)

-- | The maximum number of items to be returned.
listRestoreJobs_maxResults :: Lens.Lens' ListRestoreJobs (Prelude.Maybe Prelude.Natural)
listRestoreJobs_maxResults = Lens.lens (\ListRestoreJobs' {maxResults} -> maxResults) (\s@ListRestoreJobs' {} a -> s {maxResults = a} :: ListRestoreJobs)

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listRestoreJobs_nextToken :: Lens.Lens' ListRestoreJobs (Prelude.Maybe Prelude.Text)
listRestoreJobs_nextToken = Lens.lens (\ListRestoreJobs' {nextToken} -> nextToken) (\s@ListRestoreJobs' {} a -> s {nextToken = a} :: ListRestoreJobs)

instance Core.AWSPager ListRestoreJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRestoreJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRestoreJobsResponse_restoreJobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listRestoreJobs_nextToken
              Lens..~ rs
              Lens.^? listRestoreJobsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListRestoreJobs where
  type
    AWSResponse ListRestoreJobs =
      ListRestoreJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRestoreJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RestoreJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRestoreJobs where
  hashWithSalt _salt ListRestoreJobs' {..} =
    _salt
      `Prelude.hashWithSalt` byAccountId
      `Prelude.hashWithSalt` byCompleteAfter
      `Prelude.hashWithSalt` byCompleteBefore
      `Prelude.hashWithSalt` byCreatedAfter
      `Prelude.hashWithSalt` byCreatedBefore
      `Prelude.hashWithSalt` byStatus
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListRestoreJobs where
  rnf ListRestoreJobs' {..} =
    Prelude.rnf byAccountId `Prelude.seq`
      Prelude.rnf byCompleteAfter `Prelude.seq`
        Prelude.rnf byCompleteBefore `Prelude.seq`
          Prelude.rnf byCreatedAfter `Prelude.seq`
            Prelude.rnf byCreatedBefore `Prelude.seq`
              Prelude.rnf byStatus `Prelude.seq`
                Prelude.rnf maxResults `Prelude.seq`
                  Prelude.rnf nextToken

instance Data.ToHeaders ListRestoreJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRestoreJobs where
  toPath = Prelude.const "/restore-jobs/"

instance Data.ToQuery ListRestoreJobs where
  toQuery ListRestoreJobs' {..} =
    Prelude.mconcat
      [ "accountId" Data.=: byAccountId,
        "completeAfter" Data.=: byCompleteAfter,
        "completeBefore" Data.=: byCompleteBefore,
        "createdAfter" Data.=: byCreatedAfter,
        "createdBefore" Data.=: byCreatedBefore,
        "status" Data.=: byStatus,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListRestoreJobsResponse' smart constructor.
data ListRestoreJobsResponse = ListRestoreJobsResponse'
  { -- | The next item following a partial list of returned items. For example,
    -- if a request is made to return @maxResults@ number of items, @NextToken@
    -- allows you to return more items in your list starting at the location
    -- pointed to by the next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain detailed information about jobs to
    -- restore saved resources.
    restoreJobs :: Prelude.Maybe [RestoreJobsListMember],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRestoreJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRestoreJobsResponse_nextToken' - The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
--
-- 'restoreJobs', 'listRestoreJobsResponse_restoreJobs' - An array of objects that contain detailed information about jobs to
-- restore saved resources.
--
-- 'httpStatus', 'listRestoreJobsResponse_httpStatus' - The response's http status code.
newListRestoreJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRestoreJobsResponse
newListRestoreJobsResponse pHttpStatus_ =
  ListRestoreJobsResponse'
    { nextToken =
        Prelude.Nothing,
      restoreJobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next item following a partial list of returned items. For example,
-- if a request is made to return @maxResults@ number of items, @NextToken@
-- allows you to return more items in your list starting at the location
-- pointed to by the next token.
listRestoreJobsResponse_nextToken :: Lens.Lens' ListRestoreJobsResponse (Prelude.Maybe Prelude.Text)
listRestoreJobsResponse_nextToken = Lens.lens (\ListRestoreJobsResponse' {nextToken} -> nextToken) (\s@ListRestoreJobsResponse' {} a -> s {nextToken = a} :: ListRestoreJobsResponse)

-- | An array of objects that contain detailed information about jobs to
-- restore saved resources.
listRestoreJobsResponse_restoreJobs :: Lens.Lens' ListRestoreJobsResponse (Prelude.Maybe [RestoreJobsListMember])
listRestoreJobsResponse_restoreJobs = Lens.lens (\ListRestoreJobsResponse' {restoreJobs} -> restoreJobs) (\s@ListRestoreJobsResponse' {} a -> s {restoreJobs = a} :: ListRestoreJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRestoreJobsResponse_httpStatus :: Lens.Lens' ListRestoreJobsResponse Prelude.Int
listRestoreJobsResponse_httpStatus = Lens.lens (\ListRestoreJobsResponse' {httpStatus} -> httpStatus) (\s@ListRestoreJobsResponse' {} a -> s {httpStatus = a} :: ListRestoreJobsResponse)

instance Prelude.NFData ListRestoreJobsResponse where
  rnf ListRestoreJobsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf restoreJobs `Prelude.seq`
        Prelude.rnf httpStatus
