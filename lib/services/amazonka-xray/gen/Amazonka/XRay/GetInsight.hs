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
-- Module      : Amazonka.XRay.GetInsight
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the summary information of an insight. This includes impact to
-- clients and root cause services, the top anomalous services, the
-- category, the state of the insight, and the start and end time of the
-- insight.
module Amazonka.XRay.GetInsight
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightResponse'
            Prelude.<$> (x Data..?> "Insight")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInsight where
  hashWithSalt _salt GetInsight' {..} =
    _salt `Prelude.hashWithSalt` insightId

instance Prelude.NFData GetInsight where
  rnf GetInsight' {..} = Prelude.rnf insightId

instance Data.ToHeaders GetInsight where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetInsight where
  toJSON GetInsight' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("InsightId" Data..= insightId)]
      )

instance Data.ToPath GetInsight where
  toPath = Prelude.const "/Insight"

instance Data.ToQuery GetInsight where
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

instance Prelude.NFData GetInsightResponse where
  rnf GetInsightResponse' {..} =
    Prelude.rnf insight
      `Prelude.seq` Prelude.rnf httpStatus
