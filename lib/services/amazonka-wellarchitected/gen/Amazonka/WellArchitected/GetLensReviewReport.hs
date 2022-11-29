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
-- Module      : Amazonka.WellArchitected.GetLensReviewReport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get lens review report.
module Amazonka.WellArchitected.GetLensReviewReport
  ( -- * Creating a Request
    GetLensReviewReport (..),
    newGetLensReviewReport,

    -- * Request Lenses
    getLensReviewReport_milestoneNumber,
    getLensReviewReport_workloadId,
    getLensReviewReport_lensAlias,

    -- * Destructuring the Response
    GetLensReviewReportResponse (..),
    newGetLensReviewReportResponse,

    -- * Response Lenses
    getLensReviewReportResponse_lensReviewReport,
    getLensReviewReportResponse_milestoneNumber,
    getLensReviewReportResponse_workloadId,
    getLensReviewReportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input to get lens review report.
--
-- /See:/ 'newGetLensReviewReport' smart constructor.
data GetLensReviewReport = GetLensReviewReport'
  { milestoneNumber :: Prelude.Maybe Prelude.Natural,
    workloadId :: Prelude.Text,
    lensAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLensReviewReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'milestoneNumber', 'getLensReviewReport_milestoneNumber' - Undocumented member.
--
-- 'workloadId', 'getLensReviewReport_workloadId' - Undocumented member.
--
-- 'lensAlias', 'getLensReviewReport_lensAlias' - Undocumented member.
newGetLensReviewReport ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'lensAlias'
  Prelude.Text ->
  GetLensReviewReport
newGetLensReviewReport pWorkloadId_ pLensAlias_ =
  GetLensReviewReport'
    { milestoneNumber =
        Prelude.Nothing,
      workloadId = pWorkloadId_,
      lensAlias = pLensAlias_
    }

-- | Undocumented member.
getLensReviewReport_milestoneNumber :: Lens.Lens' GetLensReviewReport (Prelude.Maybe Prelude.Natural)
getLensReviewReport_milestoneNumber = Lens.lens (\GetLensReviewReport' {milestoneNumber} -> milestoneNumber) (\s@GetLensReviewReport' {} a -> s {milestoneNumber = a} :: GetLensReviewReport)

-- | Undocumented member.
getLensReviewReport_workloadId :: Lens.Lens' GetLensReviewReport Prelude.Text
getLensReviewReport_workloadId = Lens.lens (\GetLensReviewReport' {workloadId} -> workloadId) (\s@GetLensReviewReport' {} a -> s {workloadId = a} :: GetLensReviewReport)

-- | Undocumented member.
getLensReviewReport_lensAlias :: Lens.Lens' GetLensReviewReport Prelude.Text
getLensReviewReport_lensAlias = Lens.lens (\GetLensReviewReport' {lensAlias} -> lensAlias) (\s@GetLensReviewReport' {} a -> s {lensAlias = a} :: GetLensReviewReport)

instance Core.AWSRequest GetLensReviewReport where
  type
    AWSResponse GetLensReviewReport =
      GetLensReviewReportResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLensReviewReportResponse'
            Prelude.<$> (x Core..?> "LensReviewReport")
            Prelude.<*> (x Core..?> "MilestoneNumber")
            Prelude.<*> (x Core..?> "WorkloadId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLensReviewReport where
  hashWithSalt _salt GetLensReviewReport' {..} =
    _salt `Prelude.hashWithSalt` milestoneNumber
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` lensAlias

instance Prelude.NFData GetLensReviewReport where
  rnf GetLensReviewReport' {..} =
    Prelude.rnf milestoneNumber
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf lensAlias

instance Core.ToHeaders GetLensReviewReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetLensReviewReport where
  toPath GetLensReviewReport' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Core.toBS workloadId,
        "/lensReviews/",
        Core.toBS lensAlias,
        "/report"
      ]

instance Core.ToQuery GetLensReviewReport where
  toQuery GetLensReviewReport' {..} =
    Prelude.mconcat
      ["MilestoneNumber" Core.=: milestoneNumber]

-- | Output of a get lens review report call.
--
-- /See:/ 'newGetLensReviewReportResponse' smart constructor.
data GetLensReviewReportResponse = GetLensReviewReportResponse'
  { lensReviewReport :: Prelude.Maybe LensReviewReport,
    milestoneNumber :: Prelude.Maybe Prelude.Natural,
    workloadId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLensReviewReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensReviewReport', 'getLensReviewReportResponse_lensReviewReport' - Undocumented member.
--
-- 'milestoneNumber', 'getLensReviewReportResponse_milestoneNumber' - Undocumented member.
--
-- 'workloadId', 'getLensReviewReportResponse_workloadId' - Undocumented member.
--
-- 'httpStatus', 'getLensReviewReportResponse_httpStatus' - The response's http status code.
newGetLensReviewReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLensReviewReportResponse
newGetLensReviewReportResponse pHttpStatus_ =
  GetLensReviewReportResponse'
    { lensReviewReport =
        Prelude.Nothing,
      milestoneNumber = Prelude.Nothing,
      workloadId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getLensReviewReportResponse_lensReviewReport :: Lens.Lens' GetLensReviewReportResponse (Prelude.Maybe LensReviewReport)
getLensReviewReportResponse_lensReviewReport = Lens.lens (\GetLensReviewReportResponse' {lensReviewReport} -> lensReviewReport) (\s@GetLensReviewReportResponse' {} a -> s {lensReviewReport = a} :: GetLensReviewReportResponse)

-- | Undocumented member.
getLensReviewReportResponse_milestoneNumber :: Lens.Lens' GetLensReviewReportResponse (Prelude.Maybe Prelude.Natural)
getLensReviewReportResponse_milestoneNumber = Lens.lens (\GetLensReviewReportResponse' {milestoneNumber} -> milestoneNumber) (\s@GetLensReviewReportResponse' {} a -> s {milestoneNumber = a} :: GetLensReviewReportResponse)

-- | Undocumented member.
getLensReviewReportResponse_workloadId :: Lens.Lens' GetLensReviewReportResponse (Prelude.Maybe Prelude.Text)
getLensReviewReportResponse_workloadId = Lens.lens (\GetLensReviewReportResponse' {workloadId} -> workloadId) (\s@GetLensReviewReportResponse' {} a -> s {workloadId = a} :: GetLensReviewReportResponse)

-- | The response's http status code.
getLensReviewReportResponse_httpStatus :: Lens.Lens' GetLensReviewReportResponse Prelude.Int
getLensReviewReportResponse_httpStatus = Lens.lens (\GetLensReviewReportResponse' {httpStatus} -> httpStatus) (\s@GetLensReviewReportResponse' {} a -> s {httpStatus = a} :: GetLensReviewReportResponse)

instance Prelude.NFData GetLensReviewReportResponse where
  rnf GetLensReviewReportResponse' {..} =
    Prelude.rnf lensReviewReport
      `Prelude.seq` Prelude.rnf milestoneNumber
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf httpStatus
