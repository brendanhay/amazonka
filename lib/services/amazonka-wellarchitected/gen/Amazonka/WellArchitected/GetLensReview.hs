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
-- Module      : Amazonka.WellArchitected.GetLensReview
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get lens review.
module Amazonka.WellArchitected.GetLensReview
  ( -- * Creating a Request
    GetLensReview (..),
    newGetLensReview,

    -- * Request Lenses
    getLensReview_milestoneNumber,
    getLensReview_workloadId,
    getLensReview_lensAlias,

    -- * Destructuring the Response
    GetLensReviewResponse (..),
    newGetLensReviewResponse,

    -- * Response Lenses
    getLensReviewResponse_lensReview,
    getLensReviewResponse_milestoneNumber,
    getLensReviewResponse_workloadId,
    getLensReviewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input to get lens review.
--
-- /See:/ 'newGetLensReview' smart constructor.
data GetLensReview = GetLensReview'
  { milestoneNumber :: Prelude.Maybe Prelude.Natural,
    workloadId :: Prelude.Text,
    lensAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLensReview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'milestoneNumber', 'getLensReview_milestoneNumber' - Undocumented member.
--
-- 'workloadId', 'getLensReview_workloadId' - Undocumented member.
--
-- 'lensAlias', 'getLensReview_lensAlias' - Undocumented member.
newGetLensReview ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'lensAlias'
  Prelude.Text ->
  GetLensReview
newGetLensReview pWorkloadId_ pLensAlias_ =
  GetLensReview'
    { milestoneNumber = Prelude.Nothing,
      workloadId = pWorkloadId_,
      lensAlias = pLensAlias_
    }

-- | Undocumented member.
getLensReview_milestoneNumber :: Lens.Lens' GetLensReview (Prelude.Maybe Prelude.Natural)
getLensReview_milestoneNumber = Lens.lens (\GetLensReview' {milestoneNumber} -> milestoneNumber) (\s@GetLensReview' {} a -> s {milestoneNumber = a} :: GetLensReview)

-- | Undocumented member.
getLensReview_workloadId :: Lens.Lens' GetLensReview Prelude.Text
getLensReview_workloadId = Lens.lens (\GetLensReview' {workloadId} -> workloadId) (\s@GetLensReview' {} a -> s {workloadId = a} :: GetLensReview)

-- | Undocumented member.
getLensReview_lensAlias :: Lens.Lens' GetLensReview Prelude.Text
getLensReview_lensAlias = Lens.lens (\GetLensReview' {lensAlias} -> lensAlias) (\s@GetLensReview' {} a -> s {lensAlias = a} :: GetLensReview)

instance Core.AWSRequest GetLensReview where
  type
    AWSResponse GetLensReview =
      GetLensReviewResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLensReviewResponse'
            Prelude.<$> (x Data..?> "LensReview")
            Prelude.<*> (x Data..?> "MilestoneNumber")
            Prelude.<*> (x Data..?> "WorkloadId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLensReview where
  hashWithSalt _salt GetLensReview' {..} =
    _salt
      `Prelude.hashWithSalt` milestoneNumber
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` lensAlias

instance Prelude.NFData GetLensReview where
  rnf GetLensReview' {..} =
    Prelude.rnf milestoneNumber
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf lensAlias

instance Data.ToHeaders GetLensReview where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetLensReview where
  toPath GetLensReview' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Data.toBS workloadId,
        "/lensReviews/",
        Data.toBS lensAlias
      ]

instance Data.ToQuery GetLensReview where
  toQuery GetLensReview' {..} =
    Prelude.mconcat
      ["MilestoneNumber" Data.=: milestoneNumber]

-- | Output of a get lens review call.
--
-- /See:/ 'newGetLensReviewResponse' smart constructor.
data GetLensReviewResponse = GetLensReviewResponse'
  { lensReview :: Prelude.Maybe LensReview,
    milestoneNumber :: Prelude.Maybe Prelude.Natural,
    workloadId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLensReviewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensReview', 'getLensReviewResponse_lensReview' - Undocumented member.
--
-- 'milestoneNumber', 'getLensReviewResponse_milestoneNumber' - Undocumented member.
--
-- 'workloadId', 'getLensReviewResponse_workloadId' - Undocumented member.
--
-- 'httpStatus', 'getLensReviewResponse_httpStatus' - The response's http status code.
newGetLensReviewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLensReviewResponse
newGetLensReviewResponse pHttpStatus_ =
  GetLensReviewResponse'
    { lensReview =
        Prelude.Nothing,
      milestoneNumber = Prelude.Nothing,
      workloadId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getLensReviewResponse_lensReview :: Lens.Lens' GetLensReviewResponse (Prelude.Maybe LensReview)
getLensReviewResponse_lensReview = Lens.lens (\GetLensReviewResponse' {lensReview} -> lensReview) (\s@GetLensReviewResponse' {} a -> s {lensReview = a} :: GetLensReviewResponse)

-- | Undocumented member.
getLensReviewResponse_milestoneNumber :: Lens.Lens' GetLensReviewResponse (Prelude.Maybe Prelude.Natural)
getLensReviewResponse_milestoneNumber = Lens.lens (\GetLensReviewResponse' {milestoneNumber} -> milestoneNumber) (\s@GetLensReviewResponse' {} a -> s {milestoneNumber = a} :: GetLensReviewResponse)

-- | Undocumented member.
getLensReviewResponse_workloadId :: Lens.Lens' GetLensReviewResponse (Prelude.Maybe Prelude.Text)
getLensReviewResponse_workloadId = Lens.lens (\GetLensReviewResponse' {workloadId} -> workloadId) (\s@GetLensReviewResponse' {} a -> s {workloadId = a} :: GetLensReviewResponse)

-- | The response's http status code.
getLensReviewResponse_httpStatus :: Lens.Lens' GetLensReviewResponse Prelude.Int
getLensReviewResponse_httpStatus = Lens.lens (\GetLensReviewResponse' {httpStatus} -> httpStatus) (\s@GetLensReviewResponse' {} a -> s {httpStatus = a} :: GetLensReviewResponse)

instance Prelude.NFData GetLensReviewResponse where
  rnf GetLensReviewResponse' {..} =
    Prelude.rnf lensReview
      `Prelude.seq` Prelude.rnf milestoneNumber
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf httpStatus
