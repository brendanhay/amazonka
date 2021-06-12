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
-- Module      : Network.AWS.ImportExport.ListJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the jobs associated with the requester. AWS
-- Import\/Export lists the jobs in reverse chronological order based on
-- the date of creation. For example if Job Test1 was created 2009Dec30 and
-- Test2 was created 2010Feb05, the ListJobs operation would return Test2
-- followed by Test1.
--
-- This operation returns paginated results.
module Network.AWS.ImportExport.ListJobs
  ( -- * Creating a Request
    ListJobs (..),
    newListJobs,

    -- * Request Lenses
    listJobs_maxJobs,
    listJobs_aPIVersion,
    listJobs_marker,

    -- * Destructuring the Response
    ListJobsResponse (..),
    newListJobsResponse,

    -- * Response Lenses
    listJobsResponse_isTruncated,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ImportExport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input structure for the ListJobs operation.
--
-- /See:/ 'newListJobs' smart constructor.
data ListJobs = ListJobs'
  { maxJobs :: Core.Maybe Core.Int,
    aPIVersion :: Core.Maybe Core.Text,
    marker :: Core.Maybe Core.Text
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
-- 'maxJobs', 'listJobs_maxJobs' - Undocumented member.
--
-- 'aPIVersion', 'listJobs_aPIVersion' - Undocumented member.
--
-- 'marker', 'listJobs_marker' - Undocumented member.
newListJobs ::
  ListJobs
newListJobs =
  ListJobs'
    { maxJobs = Core.Nothing,
      aPIVersion = Core.Nothing,
      marker = Core.Nothing
    }

-- | Undocumented member.
listJobs_maxJobs :: Lens.Lens' ListJobs (Core.Maybe Core.Int)
listJobs_maxJobs = Lens.lens (\ListJobs' {maxJobs} -> maxJobs) (\s@ListJobs' {} a -> s {maxJobs = a} :: ListJobs)

-- | Undocumented member.
listJobs_aPIVersion :: Lens.Lens' ListJobs (Core.Maybe Core.Text)
listJobs_aPIVersion = Lens.lens (\ListJobs' {aPIVersion} -> aPIVersion) (\s@ListJobs' {} a -> s {aPIVersion = a} :: ListJobs)

-- | Undocumented member.
listJobs_marker :: Lens.Lens' ListJobs (Core.Maybe Core.Text)
listJobs_marker = Lens.lens (\ListJobs' {marker} -> marker) (\s@ListJobs' {} a -> s {marker = a} :: ListJobs)

instance Core.AWSPager ListJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_isTruncated Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listJobsResponse_jobs Core.. Lens._Just
              Core.. Lens._last
              Core.. job_jobId
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listJobs_marker
          Lens..~ rs
          Lens.^? listJobsResponse_jobs Core.. Lens._Just
            Core.. Lens._last
            Core.. job_jobId

instance Core.AWSRequest ListJobs where
  type AWSResponse ListJobs = ListJobsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListJobsResult"
      ( \s h x ->
          ListJobsResponse'
            Core.<$> (x Core..@? "IsTruncated")
            Core.<*> ( x Core..@? "Jobs" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListJobs

instance Core.NFData ListJobs

instance Core.ToHeaders ListJobs where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListJobs where
  toQuery ListJobs' {..} =
    Core.mconcat
      [ "Operation=ListJobs",
        "Action" Core.=: ("ListJobs" :: Core.ByteString),
        "Version" Core.=: ("2010-06-01" :: Core.ByteString),
        "MaxJobs" Core.=: maxJobs,
        "APIVersion" Core.=: aPIVersion,
        "Marker" Core.=: marker
      ]

-- | Output structure for the ListJobs operation.
--
-- /See:/ 'newListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { isTruncated :: Core.Maybe Core.Bool,
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
-- 'isTruncated', 'listJobsResponse_isTruncated' - Undocumented member.
--
-- 'jobs', 'listJobsResponse_jobs' - Undocumented member.
--
-- 'httpStatus', 'listJobsResponse_httpStatus' - The response's http status code.
newListJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListJobsResponse
newListJobsResponse pHttpStatus_ =
  ListJobsResponse'
    { isTruncated = Core.Nothing,
      jobs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listJobsResponse_isTruncated :: Lens.Lens' ListJobsResponse (Core.Maybe Core.Bool)
listJobsResponse_isTruncated = Lens.lens (\ListJobsResponse' {isTruncated} -> isTruncated) (\s@ListJobsResponse' {} a -> s {isTruncated = a} :: ListJobsResponse)

-- | Undocumented member.
listJobsResponse_jobs :: Lens.Lens' ListJobsResponse (Core.Maybe [Job])
listJobsResponse_jobs = Lens.lens (\ListJobsResponse' {jobs} -> jobs) (\s@ListJobsResponse' {} a -> s {jobs = a} :: ListJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listJobsResponse_httpStatus :: Lens.Lens' ListJobsResponse Core.Int
listJobsResponse_httpStatus = Lens.lens (\ListJobsResponse' {httpStatus} -> httpStatus) (\s@ListJobsResponse' {} a -> s {httpStatus = a} :: ListJobsResponse)

instance Core.NFData ListJobsResponse
