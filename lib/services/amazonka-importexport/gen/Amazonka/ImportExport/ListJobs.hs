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
-- Module      : Amazonka.ImportExport.ListJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ImportExport.ListJobs
  ( -- * Creating a Request
    ListJobs (..),
    newListJobs,

    -- * Request Lenses
    listJobs_aPIVersion,
    listJobs_marker,
    listJobs_maxJobs,

    -- * Destructuring the Response
    ListJobsResponse (..),
    newListJobsResponse,

    -- * Response Lenses
    listJobsResponse_isTruncated,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImportExport.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input structure for the ListJobs operation.
--
-- /See:/ 'newListJobs' smart constructor.
data ListJobs = ListJobs'
  { aPIVersion :: Prelude.Maybe Prelude.Text,
    marker :: Prelude.Maybe Prelude.Text,
    maxJobs :: Prelude.Maybe Prelude.Int
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
-- 'aPIVersion', 'listJobs_aPIVersion' - Undocumented member.
--
-- 'marker', 'listJobs_marker' - Undocumented member.
--
-- 'maxJobs', 'listJobs_maxJobs' - Undocumented member.
newListJobs ::
  ListJobs
newListJobs =
  ListJobs'
    { aPIVersion = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxJobs = Prelude.Nothing
    }

-- | Undocumented member.
listJobs_aPIVersion :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_aPIVersion = Lens.lens (\ListJobs' {aPIVersion} -> aPIVersion) (\s@ListJobs' {} a -> s {aPIVersion = a} :: ListJobs)

-- | Undocumented member.
listJobs_marker :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_marker = Lens.lens (\ListJobs' {marker} -> marker) (\s@ListJobs' {} a -> s {marker = a} :: ListJobs)

-- | Undocumented member.
listJobs_maxJobs :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Int)
listJobs_maxJobs = Lens.lens (\ListJobs' {maxJobs} -> maxJobs) (\s@ListJobs' {} a -> s {maxJobs = a} :: ListJobs)

instance Core.AWSPager ListJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_isTruncated Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listJobsResponse_jobs Prelude.. Lens._Just
              Prelude.. Lens._last
              Prelude.. job_jobId
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listJobs_marker
          Lens..~ rs
          Lens.^? listJobsResponse_jobs Prelude.. Lens._Just
            Prelude.. Lens._last
            Prelude.. job_jobId

instance Core.AWSRequest ListJobs where
  type AWSResponse ListJobs = ListJobsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListJobsResult"
      ( \s h x ->
          ListJobsResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> ( x Data..@? "Jobs" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobs where
  hashWithSalt _salt ListJobs' {..} =
    _salt `Prelude.hashWithSalt` aPIVersion
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxJobs

instance Prelude.NFData ListJobs where
  rnf ListJobs' {..} =
    Prelude.rnf aPIVersion
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxJobs

instance Data.ToHeaders ListJobs where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListJobs where
  toQuery ListJobs' {..} =
    Prelude.mconcat
      [ "Operation=ListJobs",
        "Action" Data.=: ("ListJobs" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-06-01" :: Prelude.ByteString),
        "APIVersion" Data.=: aPIVersion,
        "Marker" Data.=: marker,
        "MaxJobs" Data.=: maxJobs
      ]

-- | Output structure for the ListJobs operation.
--
-- /See:/ 'newListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { isTruncated :: Prelude.Maybe Prelude.Bool,
    jobs :: Prelude.Maybe [Job],
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
-- 'isTruncated', 'listJobsResponse_isTruncated' - Undocumented member.
--
-- 'jobs', 'listJobsResponse_jobs' - Undocumented member.
--
-- 'httpStatus', 'listJobsResponse_httpStatus' - The response's http status code.
newListJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobsResponse
newListJobsResponse pHttpStatus_ =
  ListJobsResponse'
    { isTruncated = Prelude.Nothing,
      jobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listJobsResponse_isTruncated :: Lens.Lens' ListJobsResponse (Prelude.Maybe Prelude.Bool)
listJobsResponse_isTruncated = Lens.lens (\ListJobsResponse' {isTruncated} -> isTruncated) (\s@ListJobsResponse' {} a -> s {isTruncated = a} :: ListJobsResponse)

-- | Undocumented member.
listJobsResponse_jobs :: Lens.Lens' ListJobsResponse (Prelude.Maybe [Job])
listJobsResponse_jobs = Lens.lens (\ListJobsResponse' {jobs} -> jobs) (\s@ListJobsResponse' {} a -> s {jobs = a} :: ListJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listJobsResponse_httpStatus :: Lens.Lens' ListJobsResponse Prelude.Int
listJobsResponse_httpStatus = Lens.lens (\ListJobsResponse' {httpStatus} -> httpStatus) (\s@ListJobsResponse' {} a -> s {httpStatus = a} :: ListJobsResponse)

instance Prelude.NFData ListJobsResponse where
  rnf ListJobsResponse' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf jobs
      `Prelude.seq` Prelude.rnf httpStatus
