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
-- Module      : Network.AWS.WorkMail.ListMailboxExportJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the mailbox export jobs started for the specified organization
-- within the last seven days.
module Network.AWS.WorkMail.ListMailboxExportJobs
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newListMailboxExportJobs' smart constructor.
data ListMailboxExportJobs = ListMailboxExportJobs'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Core.Maybe Core.Natural,
    -- | The organization ID.
    organizationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListMailboxExportJobs
newListMailboxExportJobs pOrganizationId_ =
  ListMailboxExportJobs'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      organizationId = pOrganizationId_
    }

-- | The token to use to retrieve the next page of results.
listMailboxExportJobs_nextToken :: Lens.Lens' ListMailboxExportJobs (Core.Maybe Core.Text)
listMailboxExportJobs_nextToken = Lens.lens (\ListMailboxExportJobs' {nextToken} -> nextToken) (\s@ListMailboxExportJobs' {} a -> s {nextToken = a} :: ListMailboxExportJobs)

-- | The maximum number of results to return in a single call.
listMailboxExportJobs_maxResults :: Lens.Lens' ListMailboxExportJobs (Core.Maybe Core.Natural)
listMailboxExportJobs_maxResults = Lens.lens (\ListMailboxExportJobs' {maxResults} -> maxResults) (\s@ListMailboxExportJobs' {} a -> s {maxResults = a} :: ListMailboxExportJobs)

-- | The organization ID.
listMailboxExportJobs_organizationId :: Lens.Lens' ListMailboxExportJobs Core.Text
listMailboxExportJobs_organizationId = Lens.lens (\ListMailboxExportJobs' {organizationId} -> organizationId) (\s@ListMailboxExportJobs' {} a -> s {organizationId = a} :: ListMailboxExportJobs)

instance Core.AWSRequest ListMailboxExportJobs where
  type
    AWSResponse ListMailboxExportJobs =
      ListMailboxExportJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMailboxExportJobsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Jobs" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListMailboxExportJobs

instance Core.NFData ListMailboxExportJobs

instance Core.ToHeaders ListMailboxExportJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.ListMailboxExportJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListMailboxExportJobs where
  toJSON ListMailboxExportJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("OrganizationId" Core..= organizationId)
          ]
      )

instance Core.ToPath ListMailboxExportJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListMailboxExportJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListMailboxExportJobsResponse' smart constructor.
data ListMailboxExportJobsResponse = ListMailboxExportJobsResponse'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The mailbox export job details.
    jobs :: Core.Maybe [MailboxExportJob],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListMailboxExportJobsResponse
newListMailboxExportJobsResponse pHttpStatus_ =
  ListMailboxExportJobsResponse'
    { nextToken =
        Core.Nothing,
      jobs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results.
listMailboxExportJobsResponse_nextToken :: Lens.Lens' ListMailboxExportJobsResponse (Core.Maybe Core.Text)
listMailboxExportJobsResponse_nextToken = Lens.lens (\ListMailboxExportJobsResponse' {nextToken} -> nextToken) (\s@ListMailboxExportJobsResponse' {} a -> s {nextToken = a} :: ListMailboxExportJobsResponse)

-- | The mailbox export job details.
listMailboxExportJobsResponse_jobs :: Lens.Lens' ListMailboxExportJobsResponse (Core.Maybe [MailboxExportJob])
listMailboxExportJobsResponse_jobs = Lens.lens (\ListMailboxExportJobsResponse' {jobs} -> jobs) (\s@ListMailboxExportJobsResponse' {} a -> s {jobs = a} :: ListMailboxExportJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listMailboxExportJobsResponse_httpStatus :: Lens.Lens' ListMailboxExportJobsResponse Core.Int
listMailboxExportJobsResponse_httpStatus = Lens.lens (\ListMailboxExportJobsResponse' {httpStatus} -> httpStatus) (\s@ListMailboxExportJobsResponse' {} a -> s {httpStatus = a} :: ListMailboxExportJobsResponse)

instance Core.NFData ListMailboxExportJobsResponse
