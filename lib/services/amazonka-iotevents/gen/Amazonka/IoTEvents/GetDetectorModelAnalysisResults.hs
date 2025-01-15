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
-- Module      : Amazonka.IoTEvents.GetDetectorModelAnalysisResults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves one or more analysis results of the detector model.
--
-- After AWS IoT Events starts analyzing your detector model, you have up
-- to 24 hours to retrieve the analysis results.
module Amazonka.IoTEvents.GetDetectorModelAnalysisResults
  ( -- * Creating a Request
    GetDetectorModelAnalysisResults (..),
    newGetDetectorModelAnalysisResults,

    -- * Request Lenses
    getDetectorModelAnalysisResults_maxResults,
    getDetectorModelAnalysisResults_nextToken,
    getDetectorModelAnalysisResults_analysisId,

    -- * Destructuring the Response
    GetDetectorModelAnalysisResultsResponse (..),
    newGetDetectorModelAnalysisResultsResponse,

    -- * Response Lenses
    getDetectorModelAnalysisResultsResponse_analysisResults,
    getDetectorModelAnalysisResultsResponse_nextToken,
    getDetectorModelAnalysisResultsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDetectorModelAnalysisResults' smart constructor.
data GetDetectorModelAnalysisResults = GetDetectorModelAnalysisResults'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token that you can use to return the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the analysis result that you want to retrieve.
    analysisId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDetectorModelAnalysisResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getDetectorModelAnalysisResults_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'getDetectorModelAnalysisResults_nextToken' - The token that you can use to return the next set of results.
--
-- 'analysisId', 'getDetectorModelAnalysisResults_analysisId' - The ID of the analysis result that you want to retrieve.
newGetDetectorModelAnalysisResults ::
  -- | 'analysisId'
  Prelude.Text ->
  GetDetectorModelAnalysisResults
newGetDetectorModelAnalysisResults pAnalysisId_ =
  GetDetectorModelAnalysisResults'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      analysisId = pAnalysisId_
    }

-- | The maximum number of results to be returned per request.
getDetectorModelAnalysisResults_maxResults :: Lens.Lens' GetDetectorModelAnalysisResults (Prelude.Maybe Prelude.Int)
getDetectorModelAnalysisResults_maxResults = Lens.lens (\GetDetectorModelAnalysisResults' {maxResults} -> maxResults) (\s@GetDetectorModelAnalysisResults' {} a -> s {maxResults = a} :: GetDetectorModelAnalysisResults)

-- | The token that you can use to return the next set of results.
getDetectorModelAnalysisResults_nextToken :: Lens.Lens' GetDetectorModelAnalysisResults (Prelude.Maybe Prelude.Text)
getDetectorModelAnalysisResults_nextToken = Lens.lens (\GetDetectorModelAnalysisResults' {nextToken} -> nextToken) (\s@GetDetectorModelAnalysisResults' {} a -> s {nextToken = a} :: GetDetectorModelAnalysisResults)

-- | The ID of the analysis result that you want to retrieve.
getDetectorModelAnalysisResults_analysisId :: Lens.Lens' GetDetectorModelAnalysisResults Prelude.Text
getDetectorModelAnalysisResults_analysisId = Lens.lens (\GetDetectorModelAnalysisResults' {analysisId} -> analysisId) (\s@GetDetectorModelAnalysisResults' {} a -> s {analysisId = a} :: GetDetectorModelAnalysisResults)

instance
  Core.AWSRequest
    GetDetectorModelAnalysisResults
  where
  type
    AWSResponse GetDetectorModelAnalysisResults =
      GetDetectorModelAnalysisResultsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDetectorModelAnalysisResultsResponse'
            Prelude.<$> ( x
                            Data..?> "analysisResults"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetDetectorModelAnalysisResults
  where
  hashWithSalt
    _salt
    GetDetectorModelAnalysisResults' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` analysisId

instance
  Prelude.NFData
    GetDetectorModelAnalysisResults
  where
  rnf GetDetectorModelAnalysisResults' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf analysisId

instance
  Data.ToHeaders
    GetDetectorModelAnalysisResults
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetDetectorModelAnalysisResults where
  toPath GetDetectorModelAnalysisResults' {..} =
    Prelude.mconcat
      [ "/analysis/detector-models/",
        Data.toBS analysisId,
        "/results"
      ]

instance Data.ToQuery GetDetectorModelAnalysisResults where
  toQuery GetDetectorModelAnalysisResults' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetDetectorModelAnalysisResultsResponse' smart constructor.
data GetDetectorModelAnalysisResultsResponse = GetDetectorModelAnalysisResultsResponse'
  { -- | Contains information about one or more analysis results.
    analysisResults :: Prelude.Maybe [AnalysisResult],
    -- | The token that you can use to return the next set of results, or @null@
    -- if there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDetectorModelAnalysisResultsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisResults', 'getDetectorModelAnalysisResultsResponse_analysisResults' - Contains information about one or more analysis results.
--
-- 'nextToken', 'getDetectorModelAnalysisResultsResponse_nextToken' - The token that you can use to return the next set of results, or @null@
-- if there are no more results.
--
-- 'httpStatus', 'getDetectorModelAnalysisResultsResponse_httpStatus' - The response's http status code.
newGetDetectorModelAnalysisResultsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDetectorModelAnalysisResultsResponse
newGetDetectorModelAnalysisResultsResponse
  pHttpStatus_ =
    GetDetectorModelAnalysisResultsResponse'
      { analysisResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Contains information about one or more analysis results.
getDetectorModelAnalysisResultsResponse_analysisResults :: Lens.Lens' GetDetectorModelAnalysisResultsResponse (Prelude.Maybe [AnalysisResult])
getDetectorModelAnalysisResultsResponse_analysisResults = Lens.lens (\GetDetectorModelAnalysisResultsResponse' {analysisResults} -> analysisResults) (\s@GetDetectorModelAnalysisResultsResponse' {} a -> s {analysisResults = a} :: GetDetectorModelAnalysisResultsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that you can use to return the next set of results, or @null@
-- if there are no more results.
getDetectorModelAnalysisResultsResponse_nextToken :: Lens.Lens' GetDetectorModelAnalysisResultsResponse (Prelude.Maybe Prelude.Text)
getDetectorModelAnalysisResultsResponse_nextToken = Lens.lens (\GetDetectorModelAnalysisResultsResponse' {nextToken} -> nextToken) (\s@GetDetectorModelAnalysisResultsResponse' {} a -> s {nextToken = a} :: GetDetectorModelAnalysisResultsResponse)

-- | The response's http status code.
getDetectorModelAnalysisResultsResponse_httpStatus :: Lens.Lens' GetDetectorModelAnalysisResultsResponse Prelude.Int
getDetectorModelAnalysisResultsResponse_httpStatus = Lens.lens (\GetDetectorModelAnalysisResultsResponse' {httpStatus} -> httpStatus) (\s@GetDetectorModelAnalysisResultsResponse' {} a -> s {httpStatus = a} :: GetDetectorModelAnalysisResultsResponse)

instance
  Prelude.NFData
    GetDetectorModelAnalysisResultsResponse
  where
  rnf GetDetectorModelAnalysisResultsResponse' {..} =
    Prelude.rnf analysisResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
