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
-- Module      : Amazonka.Personalize.GetSolutionMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the metrics for the specified solution version.
module Amazonka.Personalize.GetSolutionMetrics
  ( -- * Creating a Request
    GetSolutionMetrics (..),
    newGetSolutionMetrics,

    -- * Request Lenses
    getSolutionMetrics_solutionVersionArn,

    -- * Destructuring the Response
    GetSolutionMetricsResponse (..),
    newGetSolutionMetricsResponse,

    -- * Response Lenses
    getSolutionMetricsResponse_metrics,
    getSolutionMetricsResponse_solutionVersionArn,
    getSolutionMetricsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSolutionMetrics' smart constructor.
data GetSolutionMetrics = GetSolutionMetrics'
  { -- | The Amazon Resource Name (ARN) of the solution version for which to get
    -- metrics.
    solutionVersionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolutionMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'solutionVersionArn', 'getSolutionMetrics_solutionVersionArn' - The Amazon Resource Name (ARN) of the solution version for which to get
-- metrics.
newGetSolutionMetrics ::
  -- | 'solutionVersionArn'
  Prelude.Text ->
  GetSolutionMetrics
newGetSolutionMetrics pSolutionVersionArn_ =
  GetSolutionMetrics'
    { solutionVersionArn =
        pSolutionVersionArn_
    }

-- | The Amazon Resource Name (ARN) of the solution version for which to get
-- metrics.
getSolutionMetrics_solutionVersionArn :: Lens.Lens' GetSolutionMetrics Prelude.Text
getSolutionMetrics_solutionVersionArn = Lens.lens (\GetSolutionMetrics' {solutionVersionArn} -> solutionVersionArn) (\s@GetSolutionMetrics' {} a -> s {solutionVersionArn = a} :: GetSolutionMetrics)

instance Core.AWSRequest GetSolutionMetrics where
  type
    AWSResponse GetSolutionMetrics =
      GetSolutionMetricsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSolutionMetricsResponse'
            Prelude.<$> (x Data..?> "metrics" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "solutionVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSolutionMetrics where
  hashWithSalt _salt GetSolutionMetrics' {..} =
    _salt `Prelude.hashWithSalt` solutionVersionArn

instance Prelude.NFData GetSolutionMetrics where
  rnf GetSolutionMetrics' {..} =
    Prelude.rnf solutionVersionArn

instance Data.ToHeaders GetSolutionMetrics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.GetSolutionMetrics" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSolutionMetrics where
  toJSON GetSolutionMetrics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("solutionVersionArn" Data..= solutionVersionArn)
          ]
      )

instance Data.ToPath GetSolutionMetrics where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSolutionMetrics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSolutionMetricsResponse' smart constructor.
data GetSolutionMetricsResponse = GetSolutionMetricsResponse'
  { -- | The metrics for the solution version. For more information, see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/working-with-training-metrics.html Evaluating a solution version with metrics>
    -- .
    metrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double),
    -- | The same solution version ARN as specified in the request.
    solutionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSolutionMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'getSolutionMetricsResponse_metrics' - The metrics for the solution version. For more information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/working-with-training-metrics.html Evaluating a solution version with metrics>
-- .
--
-- 'solutionVersionArn', 'getSolutionMetricsResponse_solutionVersionArn' - The same solution version ARN as specified in the request.
--
-- 'httpStatus', 'getSolutionMetricsResponse_httpStatus' - The response's http status code.
newGetSolutionMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSolutionMetricsResponse
newGetSolutionMetricsResponse pHttpStatus_ =
  GetSolutionMetricsResponse'
    { metrics =
        Prelude.Nothing,
      solutionVersionArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metrics for the solution version. For more information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/working-with-training-metrics.html Evaluating a solution version with metrics>
-- .
getSolutionMetricsResponse_metrics :: Lens.Lens' GetSolutionMetricsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
getSolutionMetricsResponse_metrics = Lens.lens (\GetSolutionMetricsResponse' {metrics} -> metrics) (\s@GetSolutionMetricsResponse' {} a -> s {metrics = a} :: GetSolutionMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The same solution version ARN as specified in the request.
getSolutionMetricsResponse_solutionVersionArn :: Lens.Lens' GetSolutionMetricsResponse (Prelude.Maybe Prelude.Text)
getSolutionMetricsResponse_solutionVersionArn = Lens.lens (\GetSolutionMetricsResponse' {solutionVersionArn} -> solutionVersionArn) (\s@GetSolutionMetricsResponse' {} a -> s {solutionVersionArn = a} :: GetSolutionMetricsResponse)

-- | The response's http status code.
getSolutionMetricsResponse_httpStatus :: Lens.Lens' GetSolutionMetricsResponse Prelude.Int
getSolutionMetricsResponse_httpStatus = Lens.lens (\GetSolutionMetricsResponse' {httpStatus} -> httpStatus) (\s@GetSolutionMetricsResponse' {} a -> s {httpStatus = a} :: GetSolutionMetricsResponse)

instance Prelude.NFData GetSolutionMetricsResponse where
  rnf GetSolutionMetricsResponse' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf solutionVersionArn
      `Prelude.seq` Prelude.rnf httpStatus
