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
-- Module      : Network.AWS.XRay.GetInsight
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the summary information of an insight. This includes impact to
-- clients and root cause services, the top anomalous services, the
-- category, the state of the insight, and the start and end time of the
-- insight.
module Network.AWS.XRay.GetInsight
  ( -- * Creating a Request
    GetInsight (..),
    newGetInsight,

    -- * Request Lenses
    getInsight_insightId,

    -- * Destructuring the Response
    GetInsightResponse (..),
    newGetInsightResponse,

    -- * Response Lenses
    getInsightResponse_insight,
    getInsightResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetInsight' smart constructor.
data GetInsight = GetInsight'
  { -- | The insight\'s unique identifier. Use the GetInsightSummaries action to
    -- retrieve an InsightId.
    insightId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightId', 'getInsight_insightId' - The insight\'s unique identifier. Use the GetInsightSummaries action to
-- retrieve an InsightId.
newGetInsight ::
  -- | 'insightId'
  Prelude.Text ->
  GetInsight
newGetInsight pInsightId_ =
  GetInsight' {insightId = pInsightId_}

-- | The insight\'s unique identifier. Use the GetInsightSummaries action to
-- retrieve an InsightId.
getInsight_insightId :: Lens.Lens' GetInsight Prelude.Text
getInsight_insightId = Lens.lens (\GetInsight' {insightId} -> insightId) (\s@GetInsight' {} a -> s {insightId = a} :: GetInsight)

instance Core.AWSRequest GetInsight where
  type AWSResponse GetInsight = GetInsightResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightResponse'
            Prelude.<$> (x Core..?> "Insight")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInsight

instance Prelude.NFData GetInsight

instance Core.ToHeaders GetInsight where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON GetInsight where
  toJSON GetInsight' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("InsightId" Core..= insightId)]
      )

instance Core.ToPath GetInsight where
  toPath = Prelude.const "/Insight"

instance Core.ToQuery GetInsight where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInsightResponse' smart constructor.
data GetInsightResponse = GetInsightResponse'
  { -- | The summary information of an insight.
    insight :: Prelude.Maybe Insight,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insight', 'getInsightResponse_insight' - The summary information of an insight.
--
-- 'httpStatus', 'getInsightResponse_httpStatus' - The response's http status code.
newGetInsightResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInsightResponse
newGetInsightResponse pHttpStatus_ =
  GetInsightResponse'
    { insight = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The summary information of an insight.
getInsightResponse_insight :: Lens.Lens' GetInsightResponse (Prelude.Maybe Insight)
getInsightResponse_insight = Lens.lens (\GetInsightResponse' {insight} -> insight) (\s@GetInsightResponse' {} a -> s {insight = a} :: GetInsightResponse)

-- | The response's http status code.
getInsightResponse_httpStatus :: Lens.Lens' GetInsightResponse Prelude.Int
getInsightResponse_httpStatus = Lens.lens (\GetInsightResponse' {httpStatus} -> httpStatus) (\s@GetInsightResponse' {} a -> s {httpStatus = a} :: GetInsightResponse)

instance Prelude.NFData GetInsightResponse
