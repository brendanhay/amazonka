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
-- Module      : Amazonka.Glue.BatchGetDataQualityResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of data quality results for the specified result IDs.
module Amazonka.Glue.BatchGetDataQualityResult
  ( -- * Creating a Request
    BatchGetDataQualityResult (..),
    newBatchGetDataQualityResult,

    -- * Request Lenses
    batchGetDataQualityResult_resultIds,

    -- * Destructuring the Response
    BatchGetDataQualityResultResponse (..),
    newBatchGetDataQualityResultResponse,

    -- * Response Lenses
    batchGetDataQualityResultResponse_resultsNotFound,
    batchGetDataQualityResultResponse_httpStatus,
    batchGetDataQualityResultResponse_results,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetDataQualityResult' smart constructor.
data BatchGetDataQualityResult = BatchGetDataQualityResult'
  { -- | A list of unique result IDs for the data quality results.
    resultIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDataQualityResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resultIds', 'batchGetDataQualityResult_resultIds' - A list of unique result IDs for the data quality results.
newBatchGetDataQualityResult ::
  -- | 'resultIds'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetDataQualityResult
newBatchGetDataQualityResult pResultIds_ =
  BatchGetDataQualityResult'
    { resultIds =
        Lens.coerced Lens.# pResultIds_
    }

-- | A list of unique result IDs for the data quality results.
batchGetDataQualityResult_resultIds :: Lens.Lens' BatchGetDataQualityResult (Prelude.NonEmpty Prelude.Text)
batchGetDataQualityResult_resultIds = Lens.lens (\BatchGetDataQualityResult' {resultIds} -> resultIds) (\s@BatchGetDataQualityResult' {} a -> s {resultIds = a} :: BatchGetDataQualityResult) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetDataQualityResult where
  type
    AWSResponse BatchGetDataQualityResult =
      BatchGetDataQualityResultResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetDataQualityResultResponse'
            Prelude.<$> (x Data..?> "ResultsNotFound")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Results" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchGetDataQualityResult where
  hashWithSalt _salt BatchGetDataQualityResult' {..} =
    _salt `Prelude.hashWithSalt` resultIds

instance Prelude.NFData BatchGetDataQualityResult where
  rnf BatchGetDataQualityResult' {..} =
    Prelude.rnf resultIds

instance Data.ToHeaders BatchGetDataQualityResult where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.BatchGetDataQualityResult" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetDataQualityResult where
  toJSON BatchGetDataQualityResult' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResultIds" Data..= resultIds)]
      )

instance Data.ToPath BatchGetDataQualityResult where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetDataQualityResult where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetDataQualityResultResponse' smart constructor.
data BatchGetDataQualityResultResponse = BatchGetDataQualityResultResponse'
  { -- | A list of result IDs for which results were not found.
    resultsNotFound :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of @DataQualityResult@ objects representing the data quality
    -- results.
    results :: [DataQualityResult]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDataQualityResultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resultsNotFound', 'batchGetDataQualityResultResponse_resultsNotFound' - A list of result IDs for which results were not found.
--
-- 'httpStatus', 'batchGetDataQualityResultResponse_httpStatus' - The response's http status code.
--
-- 'results', 'batchGetDataQualityResultResponse_results' - A list of @DataQualityResult@ objects representing the data quality
-- results.
newBatchGetDataQualityResultResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetDataQualityResultResponse
newBatchGetDataQualityResultResponse pHttpStatus_ =
  BatchGetDataQualityResultResponse'
    { resultsNotFound =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      results = Prelude.mempty
    }

-- | A list of result IDs for which results were not found.
batchGetDataQualityResultResponse_resultsNotFound :: Lens.Lens' BatchGetDataQualityResultResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetDataQualityResultResponse_resultsNotFound = Lens.lens (\BatchGetDataQualityResultResponse' {resultsNotFound} -> resultsNotFound) (\s@BatchGetDataQualityResultResponse' {} a -> s {resultsNotFound = a} :: BatchGetDataQualityResultResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetDataQualityResultResponse_httpStatus :: Lens.Lens' BatchGetDataQualityResultResponse Prelude.Int
batchGetDataQualityResultResponse_httpStatus = Lens.lens (\BatchGetDataQualityResultResponse' {httpStatus} -> httpStatus) (\s@BatchGetDataQualityResultResponse' {} a -> s {httpStatus = a} :: BatchGetDataQualityResultResponse)

-- | A list of @DataQualityResult@ objects representing the data quality
-- results.
batchGetDataQualityResultResponse_results :: Lens.Lens' BatchGetDataQualityResultResponse [DataQualityResult]
batchGetDataQualityResultResponse_results = Lens.lens (\BatchGetDataQualityResultResponse' {results} -> results) (\s@BatchGetDataQualityResultResponse' {} a -> s {results = a} :: BatchGetDataQualityResultResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchGetDataQualityResultResponse
  where
  rnf BatchGetDataQualityResultResponse' {..} =
    Prelude.rnf resultsNotFound
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf results
