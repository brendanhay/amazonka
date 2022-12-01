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
-- Module      : Amazonka.WorkMail.ListMailboxExportJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the mailbox export jobs started for the specified organization
-- within the last seven days.
module Amazonka.WorkMail.ListMailboxExportJobs
  ( -- * Creating a Request
    ListMailboxExportJobs (..),
    newListMailboxExportJobs,

    -- * Request Lenses
    listMailboxExportJobs_nextToken,
    listMailboxExportJobs_maxResults,
    listMailboxExportJobs_organizationId,

    -- * Destructuring the Response
    ListMailboxExportJobsResponse (..),
    newListMailboxExportJobsResponse,

    -- * Response Lenses
    listMailboxExportJobsResponse_nextToken,
    listMailboxExportJobsResponse_jobs,
    listMailboxExportJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newListMailboxExportJobs' smart constructor.
data ListMailboxExportJobs = ListMailboxExportJobs'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The organization ID.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMailboxExportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMailboxExportJobs_nextToken' - The token to use to retrieve the next page of results.
--
-- 'maxResults', 'listMailboxExportJobs_maxResults' - The maximum number of results to return in a single call.
--
-- 'organizationId', 'listMailboxExportJobs_organizationId' - The organization ID.
newListMailboxExportJobs ::
  -- | 'organizationId'
  Prelude.Text ->
  ListMailboxExportJobs
newListMailboxExportJobs pOrganizationId_ =
  ListMailboxExportJobs'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The token to use to retrieve the next page of results.
listMailboxExportJobs_nextToken :: Lens.Lens' ListMailboxExportJobs (Prelude.Maybe Prelude.Text)
listMailboxExportJobs_nextToken = Lens.lens (\ListMailboxExportJobs' {nextToken} -> nextToken) (\s@ListMailboxExportJobs' {} a -> s {nextToken = a} :: ListMailboxExportJobs)

-- | The maximum number of results to return in a single call.
listMailboxExportJobs_maxResults :: Lens.Lens' ListMailboxExportJobs (Prelude.Maybe Prelude.Natural)
listMailboxExportJobs_maxResults = Lens.lens (\ListMailboxExportJobs' {maxResults} -> maxResults) (\s@ListMailboxExportJobs' {} a -> s {maxResults = a} :: ListMailboxExportJobs)

-- | The organization ID.
listMailboxExportJobs_organizationId :: Lens.Lens' ListMailboxExportJobs Prelude.Text
listMailboxExportJobs_organizationId = Lens.lens (\ListMailboxExportJobs' {organizationId} -> organizationId) (\s@ListMailboxExportJobs' {} a -> s {organizationId = a} :: ListMailboxExportJobs)

instance Core.AWSRequest ListMailboxExportJobs where
  type
    AWSResponse ListMailboxExportJobs =
      ListMailboxExportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMailboxExportJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMailboxExportJobs where
  hashWithSalt _salt ListMailboxExportJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` organizationId

instance Prelude.NFData ListMailboxExportJobs where
  rnf ListMailboxExportJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf organizationId

instance Core.ToHeaders ListMailboxExportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.ListMailboxExportJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListMailboxExportJobs where
  toJSON ListMailboxExportJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("OrganizationId" Core..= organizationId)
          ]
      )

instance Core.ToPath ListMailboxExportJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListMailboxExportJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMailboxExportJobsResponse' smart constructor.
data ListMailboxExportJobsResponse = ListMailboxExportJobsResponse'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The mailbox export job details.
    jobs :: Prelude.Maybe [MailboxExportJob],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMailboxExportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMailboxExportJobsResponse_nextToken' - The token to use to retrieve the next page of results.
--
-- 'jobs', 'listMailboxExportJobsResponse_jobs' - The mailbox export job details.
--
-- 'httpStatus', 'listMailboxExportJobsResponse_httpStatus' - The response's http status code.
newListMailboxExportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMailboxExportJobsResponse
newListMailboxExportJobsResponse pHttpStatus_ =
  ListMailboxExportJobsResponse'
    { nextToken =
        Prelude.Nothing,
      jobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results.
listMailboxExportJobsResponse_nextToken :: Lens.Lens' ListMailboxExportJobsResponse (Prelude.Maybe Prelude.Text)
listMailboxExportJobsResponse_nextToken = Lens.lens (\ListMailboxExportJobsResponse' {nextToken} -> nextToken) (\s@ListMailboxExportJobsResponse' {} a -> s {nextToken = a} :: ListMailboxExportJobsResponse)

-- | The mailbox export job details.
listMailboxExportJobsResponse_jobs :: Lens.Lens' ListMailboxExportJobsResponse (Prelude.Maybe [MailboxExportJob])
listMailboxExportJobsResponse_jobs = Lens.lens (\ListMailboxExportJobsResponse' {jobs} -> jobs) (\s@ListMailboxExportJobsResponse' {} a -> s {jobs = a} :: ListMailboxExportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMailboxExportJobsResponse_httpStatus :: Lens.Lens' ListMailboxExportJobsResponse Prelude.Int
listMailboxExportJobsResponse_httpStatus = Lens.lens (\ListMailboxExportJobsResponse' {httpStatus} -> httpStatus) (\s@ListMailboxExportJobsResponse' {} a -> s {httpStatus = a} :: ListMailboxExportJobsResponse)

instance Prelude.NFData ListMailboxExportJobsResponse where
  rnf ListMailboxExportJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobs
      `Prelude.seq` Prelude.rnf httpStatus
