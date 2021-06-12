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
-- Module      : Network.AWS.DeviceFarm.ListJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about jobs for a given test run.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListJobs
  ( -- * Creating a Request
    ListJobs (..),
    newListJobs,

    -- * Request Lenses
    listJobs_nextToken,
    listJobs_arn,

    -- * Destructuring the Response
    ListJobsResponse (..),
    newListJobsResponse,

    -- * Response Lenses
    listJobsResponse_nextToken,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list jobs operation.
--
-- /See:/ 'newListJobs' smart constructor.
data ListJobs = ListJobs'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The run\'s Amazon Resource Name (ARN).
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobs_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'arn', 'listJobs_arn' - The run\'s Amazon Resource Name (ARN).
newListJobs ::
  -- | 'arn'
  Core.Text ->
  ListJobs
newListJobs pArn_ =
  ListJobs' {nextToken = Core.Nothing, arn = pArn_}

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listJobs_nextToken :: Lens.Lens' ListJobs (Core.Maybe Core.Text)
listJobs_nextToken = Lens.lens (\ListJobs' {nextToken} -> nextToken) (\s@ListJobs' {} a -> s {nextToken = a} :: ListJobs)

-- | The run\'s Amazon Resource Name (ARN).
listJobs_arn :: Lens.Lens' ListJobs Core.Text
listJobs_arn = Lens.lens (\ListJobs' {arn} -> arn) (\s@ListJobs' {} a -> s {arn = a} :: ListJobs)

instance Core.AWSPager ListJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^? listJobsResponse_jobs Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listJobs_nextToken
          Lens..~ rs
          Lens.^? listJobsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListJobs where
  type AWSResponse ListJobs = ListJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "jobs" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListJobs

instance Core.NFData ListJobs

instance Core.ToHeaders ListJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DeviceFarm_20150623.ListJobs" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListJobs where
  toJSON ListJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            Core.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath ListJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListJobs where
  toQuery = Core.const Core.mempty

-- | Represents the result of a list jobs request.
--
-- /See:/ 'newListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the jobs.
    jobs :: Core.Maybe [Job],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobsResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'jobs', 'listJobsResponse_jobs' - Information about the jobs.
--
-- 'httpStatus', 'listJobsResponse_httpStatus' - The response's http status code.
newListJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListJobsResponse
newListJobsResponse pHttpStatus_ =
  ListJobsResponse'
    { nextToken = Core.Nothing,
      jobs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listJobsResponse_nextToken :: Lens.Lens' ListJobsResponse (Core.Maybe Core.Text)
listJobsResponse_nextToken = Lens.lens (\ListJobsResponse' {nextToken} -> nextToken) (\s@ListJobsResponse' {} a -> s {nextToken = a} :: ListJobsResponse)

-- | Information about the jobs.
listJobsResponse_jobs :: Lens.Lens' ListJobsResponse (Core.Maybe [Job])
listJobsResponse_jobs = Lens.lens (\ListJobsResponse' {jobs} -> jobs) (\s@ListJobsResponse' {} a -> s {jobs = a} :: ListJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listJobsResponse_httpStatus :: Lens.Lens' ListJobsResponse Core.Int
listJobsResponse_httpStatus = Lens.lens (\ListJobsResponse' {httpStatus} -> httpStatus) (\s@ListJobsResponse' {} a -> s {httpStatus = a} :: ListJobsResponse)

instance Core.NFData ListJobsResponse
