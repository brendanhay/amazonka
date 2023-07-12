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
-- Module      : Amazonka.LookoutEquipment.ListInferenceExecutions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all inference executions that have been performed by the specified
-- inference scheduler.
module Amazonka.LookoutEquipment.ListInferenceExecutions
  ( -- * Creating a Request
    ListInferenceExecutions (..),
    newListInferenceExecutions,

    -- * Request Lenses
    listInferenceExecutions_dataEndTimeBefore,
    listInferenceExecutions_dataStartTimeAfter,
    listInferenceExecutions_maxResults,
    listInferenceExecutions_nextToken,
    listInferenceExecutions_status,
    listInferenceExecutions_inferenceSchedulerName,

    -- * Destructuring the Response
    ListInferenceExecutionsResponse (..),
    newListInferenceExecutionsResponse,

    -- * Response Lenses
    listInferenceExecutionsResponse_inferenceExecutionSummaries,
    listInferenceExecutionsResponse_nextToken,
    listInferenceExecutionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInferenceExecutions' smart constructor.
data ListInferenceExecutions = ListInferenceExecutions'
  { -- | The time reference in the inferenced dataset before which Amazon Lookout
    -- for Equipment stopped the inference execution.
    dataEndTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The time reference in the inferenced dataset after which Amazon Lookout
    -- for Equipment started the inference execution.
    dataStartTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Specifies the maximum number of inference executions to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An opaque pagination token indicating where to continue the listing of
    -- inference executions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of the inference execution.
    status :: Prelude.Maybe InferenceExecutionStatus,
    -- | The name of the inference scheduler for the inference execution listed.
    inferenceSchedulerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInferenceExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataEndTimeBefore', 'listInferenceExecutions_dataEndTimeBefore' - The time reference in the inferenced dataset before which Amazon Lookout
-- for Equipment stopped the inference execution.
--
-- 'dataStartTimeAfter', 'listInferenceExecutions_dataStartTimeAfter' - The time reference in the inferenced dataset after which Amazon Lookout
-- for Equipment started the inference execution.
--
-- 'maxResults', 'listInferenceExecutions_maxResults' - Specifies the maximum number of inference executions to list.
--
-- 'nextToken', 'listInferenceExecutions_nextToken' - An opaque pagination token indicating where to continue the listing of
-- inference executions.
--
-- 'status', 'listInferenceExecutions_status' - The status of the inference execution.
--
-- 'inferenceSchedulerName', 'listInferenceExecutions_inferenceSchedulerName' - The name of the inference scheduler for the inference execution listed.
newListInferenceExecutions ::
  -- | 'inferenceSchedulerName'
  Prelude.Text ->
  ListInferenceExecutions
newListInferenceExecutions pInferenceSchedulerName_ =
  ListInferenceExecutions'
    { dataEndTimeBefore =
        Prelude.Nothing,
      dataStartTimeAfter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      inferenceSchedulerName = pInferenceSchedulerName_
    }

-- | The time reference in the inferenced dataset before which Amazon Lookout
-- for Equipment stopped the inference execution.
listInferenceExecutions_dataEndTimeBefore :: Lens.Lens' ListInferenceExecutions (Prelude.Maybe Prelude.UTCTime)
listInferenceExecutions_dataEndTimeBefore = Lens.lens (\ListInferenceExecutions' {dataEndTimeBefore} -> dataEndTimeBefore) (\s@ListInferenceExecutions' {} a -> s {dataEndTimeBefore = a} :: ListInferenceExecutions) Prelude.. Lens.mapping Data._Time

-- | The time reference in the inferenced dataset after which Amazon Lookout
-- for Equipment started the inference execution.
listInferenceExecutions_dataStartTimeAfter :: Lens.Lens' ListInferenceExecutions (Prelude.Maybe Prelude.UTCTime)
listInferenceExecutions_dataStartTimeAfter = Lens.lens (\ListInferenceExecutions' {dataStartTimeAfter} -> dataStartTimeAfter) (\s@ListInferenceExecutions' {} a -> s {dataStartTimeAfter = a} :: ListInferenceExecutions) Prelude.. Lens.mapping Data._Time

-- | Specifies the maximum number of inference executions to list.
listInferenceExecutions_maxResults :: Lens.Lens' ListInferenceExecutions (Prelude.Maybe Prelude.Natural)
listInferenceExecutions_maxResults = Lens.lens (\ListInferenceExecutions' {maxResults} -> maxResults) (\s@ListInferenceExecutions' {} a -> s {maxResults = a} :: ListInferenceExecutions)

-- | An opaque pagination token indicating where to continue the listing of
-- inference executions.
listInferenceExecutions_nextToken :: Lens.Lens' ListInferenceExecutions (Prelude.Maybe Prelude.Text)
listInferenceExecutions_nextToken = Lens.lens (\ListInferenceExecutions' {nextToken} -> nextToken) (\s@ListInferenceExecutions' {} a -> s {nextToken = a} :: ListInferenceExecutions)

-- | The status of the inference execution.
listInferenceExecutions_status :: Lens.Lens' ListInferenceExecutions (Prelude.Maybe InferenceExecutionStatus)
listInferenceExecutions_status = Lens.lens (\ListInferenceExecutions' {status} -> status) (\s@ListInferenceExecutions' {} a -> s {status = a} :: ListInferenceExecutions)

-- | The name of the inference scheduler for the inference execution listed.
listInferenceExecutions_inferenceSchedulerName :: Lens.Lens' ListInferenceExecutions Prelude.Text
listInferenceExecutions_inferenceSchedulerName = Lens.lens (\ListInferenceExecutions' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@ListInferenceExecutions' {} a -> s {inferenceSchedulerName = a} :: ListInferenceExecutions)

instance Core.AWSRequest ListInferenceExecutions where
  type
    AWSResponse ListInferenceExecutions =
      ListInferenceExecutionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInferenceExecutionsResponse'
            Prelude.<$> ( x
                            Data..?> "InferenceExecutionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInferenceExecutions where
  hashWithSalt _salt ListInferenceExecutions' {..} =
    _salt
      `Prelude.hashWithSalt` dataEndTimeBefore
      `Prelude.hashWithSalt` dataStartTimeAfter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` inferenceSchedulerName

instance Prelude.NFData ListInferenceExecutions where
  rnf ListInferenceExecutions' {..} =
    Prelude.rnf dataEndTimeBefore
      `Prelude.seq` Prelude.rnf dataStartTimeAfter
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf inferenceSchedulerName

instance Data.ToHeaders ListInferenceExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.ListInferenceExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListInferenceExecutions where
  toJSON ListInferenceExecutions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataEndTimeBefore" Data..=)
              Prelude.<$> dataEndTimeBefore,
            ("DataStartTimeAfter" Data..=)
              Prelude.<$> dataStartTimeAfter,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Status" Data..=) Prelude.<$> status,
            Prelude.Just
              ( "InferenceSchedulerName"
                  Data..= inferenceSchedulerName
              )
          ]
      )

instance Data.ToPath ListInferenceExecutions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListInferenceExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInferenceExecutionsResponse' smart constructor.
data ListInferenceExecutionsResponse = ListInferenceExecutionsResponse'
  { -- | Provides an array of information about the individual inference
    -- executions returned from the @ListInferenceExecutions@ operation,
    -- including model used, inference scheduler, data configuration, and so
    -- on.
    inferenceExecutionSummaries :: Prelude.Maybe [InferenceExecutionSummary],
    -- | An opaque pagination token indicating where to continue the listing of
    -- inference executions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInferenceExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceExecutionSummaries', 'listInferenceExecutionsResponse_inferenceExecutionSummaries' - Provides an array of information about the individual inference
-- executions returned from the @ListInferenceExecutions@ operation,
-- including model used, inference scheduler, data configuration, and so
-- on.
--
-- 'nextToken', 'listInferenceExecutionsResponse_nextToken' - An opaque pagination token indicating where to continue the listing of
-- inference executions.
--
-- 'httpStatus', 'listInferenceExecutionsResponse_httpStatus' - The response's http status code.
newListInferenceExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInferenceExecutionsResponse
newListInferenceExecutionsResponse pHttpStatus_ =
  ListInferenceExecutionsResponse'
    { inferenceExecutionSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides an array of information about the individual inference
-- executions returned from the @ListInferenceExecutions@ operation,
-- including model used, inference scheduler, data configuration, and so
-- on.
listInferenceExecutionsResponse_inferenceExecutionSummaries :: Lens.Lens' ListInferenceExecutionsResponse (Prelude.Maybe [InferenceExecutionSummary])
listInferenceExecutionsResponse_inferenceExecutionSummaries = Lens.lens (\ListInferenceExecutionsResponse' {inferenceExecutionSummaries} -> inferenceExecutionSummaries) (\s@ListInferenceExecutionsResponse' {} a -> s {inferenceExecutionSummaries = a} :: ListInferenceExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An opaque pagination token indicating where to continue the listing of
-- inference executions.
listInferenceExecutionsResponse_nextToken :: Lens.Lens' ListInferenceExecutionsResponse (Prelude.Maybe Prelude.Text)
listInferenceExecutionsResponse_nextToken = Lens.lens (\ListInferenceExecutionsResponse' {nextToken} -> nextToken) (\s@ListInferenceExecutionsResponse' {} a -> s {nextToken = a} :: ListInferenceExecutionsResponse)

-- | The response's http status code.
listInferenceExecutionsResponse_httpStatus :: Lens.Lens' ListInferenceExecutionsResponse Prelude.Int
listInferenceExecutionsResponse_httpStatus = Lens.lens (\ListInferenceExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListInferenceExecutionsResponse' {} a -> s {httpStatus = a} :: ListInferenceExecutionsResponse)

instance
  Prelude.NFData
    ListInferenceExecutionsResponse
  where
  rnf ListInferenceExecutionsResponse' {..} =
    Prelude.rnf inferenceExecutionSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
