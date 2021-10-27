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
-- Module      : Network.AWS.LookoutEquipment.ListInferenceExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all inference executions that have been performed by the specified
-- inference scheduler.
module Network.AWS.LookoutEquipment.ListInferenceExecutions
  ( -- * Creating a Request
    ListInferenceExecutions (..),
    newListInferenceExecutions,

    -- * Request Lenses
    listInferenceExecutions_status,
    listInferenceExecutions_dataEndTimeBefore,
    listInferenceExecutions_nextToken,
    listInferenceExecutions_maxResults,
    listInferenceExecutions_dataStartTimeAfter,
    listInferenceExecutions_inferenceSchedulerName,

    -- * Destructuring the Response
    ListInferenceExecutionsResponse (..),
    newListInferenceExecutionsResponse,

    -- * Response Lenses
    listInferenceExecutionsResponse_nextToken,
    listInferenceExecutionsResponse_inferenceExecutionSummaries,
    listInferenceExecutionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LookoutEquipment.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListInferenceExecutions' smart constructor.
data ListInferenceExecutions = ListInferenceExecutions'
  { -- | The status of the inference execution.
    status :: Prelude.Maybe InferenceExecutionStatus,
    -- | The time reference in the inferenced dataset before which Amazon Lookout
    -- for Equipment stopped the inference execution.
    dataEndTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | An opaque pagination token indicating where to continue the listing of
    -- inference executions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum number of inference executions to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The time reference in the inferenced dataset after which Amazon Lookout
    -- for Equipment started the inference execution.
    dataStartTimeAfter :: Prelude.Maybe Core.POSIX,
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
-- 'status', 'listInferenceExecutions_status' - The status of the inference execution.
--
-- 'dataEndTimeBefore', 'listInferenceExecutions_dataEndTimeBefore' - The time reference in the inferenced dataset before which Amazon Lookout
-- for Equipment stopped the inference execution.
--
-- 'nextToken', 'listInferenceExecutions_nextToken' - An opaque pagination token indicating where to continue the listing of
-- inference executions.
--
-- 'maxResults', 'listInferenceExecutions_maxResults' - Specifies the maximum number of inference executions to list.
--
-- 'dataStartTimeAfter', 'listInferenceExecutions_dataStartTimeAfter' - The time reference in the inferenced dataset after which Amazon Lookout
-- for Equipment started the inference execution.
--
-- 'inferenceSchedulerName', 'listInferenceExecutions_inferenceSchedulerName' - The name of the inference scheduler for the inference execution listed.
newListInferenceExecutions ::
  -- | 'inferenceSchedulerName'
  Prelude.Text ->
  ListInferenceExecutions
newListInferenceExecutions pInferenceSchedulerName_ =
  ListInferenceExecutions'
    { status = Prelude.Nothing,
      dataEndTimeBefore = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      dataStartTimeAfter = Prelude.Nothing,
      inferenceSchedulerName = pInferenceSchedulerName_
    }

-- | The status of the inference execution.
listInferenceExecutions_status :: Lens.Lens' ListInferenceExecutions (Prelude.Maybe InferenceExecutionStatus)
listInferenceExecutions_status = Lens.lens (\ListInferenceExecutions' {status} -> status) (\s@ListInferenceExecutions' {} a -> s {status = a} :: ListInferenceExecutions)

-- | The time reference in the inferenced dataset before which Amazon Lookout
-- for Equipment stopped the inference execution.
listInferenceExecutions_dataEndTimeBefore :: Lens.Lens' ListInferenceExecutions (Prelude.Maybe Prelude.UTCTime)
listInferenceExecutions_dataEndTimeBefore = Lens.lens (\ListInferenceExecutions' {dataEndTimeBefore} -> dataEndTimeBefore) (\s@ListInferenceExecutions' {} a -> s {dataEndTimeBefore = a} :: ListInferenceExecutions) Prelude.. Lens.mapping Core._Time

-- | An opaque pagination token indicating where to continue the listing of
-- inference executions.
listInferenceExecutions_nextToken :: Lens.Lens' ListInferenceExecutions (Prelude.Maybe Prelude.Text)
listInferenceExecutions_nextToken = Lens.lens (\ListInferenceExecutions' {nextToken} -> nextToken) (\s@ListInferenceExecutions' {} a -> s {nextToken = a} :: ListInferenceExecutions)

-- | Specifies the maximum number of inference executions to list.
listInferenceExecutions_maxResults :: Lens.Lens' ListInferenceExecutions (Prelude.Maybe Prelude.Natural)
listInferenceExecutions_maxResults = Lens.lens (\ListInferenceExecutions' {maxResults} -> maxResults) (\s@ListInferenceExecutions' {} a -> s {maxResults = a} :: ListInferenceExecutions)

-- | The time reference in the inferenced dataset after which Amazon Lookout
-- for Equipment started the inference execution.
listInferenceExecutions_dataStartTimeAfter :: Lens.Lens' ListInferenceExecutions (Prelude.Maybe Prelude.UTCTime)
listInferenceExecutions_dataStartTimeAfter = Lens.lens (\ListInferenceExecutions' {dataStartTimeAfter} -> dataStartTimeAfter) (\s@ListInferenceExecutions' {} a -> s {dataStartTimeAfter = a} :: ListInferenceExecutions) Prelude.. Lens.mapping Core._Time

-- | The name of the inference scheduler for the inference execution listed.
listInferenceExecutions_inferenceSchedulerName :: Lens.Lens' ListInferenceExecutions Prelude.Text
listInferenceExecutions_inferenceSchedulerName = Lens.lens (\ListInferenceExecutions' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@ListInferenceExecutions' {} a -> s {inferenceSchedulerName = a} :: ListInferenceExecutions)

instance Core.AWSRequest ListInferenceExecutions where
  type
    AWSResponse ListInferenceExecutions =
      ListInferenceExecutionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInferenceExecutionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "InferenceExecutionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInferenceExecutions

instance Prelude.NFData ListInferenceExecutions

instance Core.ToHeaders ListInferenceExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLookoutEquipmentFrontendService.ListInferenceExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListInferenceExecutions where
  toJSON ListInferenceExecutions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("DataEndTimeBefore" Core..=)
              Prelude.<$> dataEndTimeBefore,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("DataStartTimeAfter" Core..=)
              Prelude.<$> dataStartTimeAfter,
            Prelude.Just
              ( "InferenceSchedulerName"
                  Core..= inferenceSchedulerName
              )
          ]
      )

instance Core.ToPath ListInferenceExecutions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListInferenceExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInferenceExecutionsResponse' smart constructor.
data ListInferenceExecutionsResponse = ListInferenceExecutionsResponse'
  { -- | An opaque pagination token indicating where to continue the listing of
    -- inference executions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Provides an array of information about the individual inference
    -- executions returned from the @ListInferenceExecutions@ operation,
    -- including model used, inference scheduler, data configuration, and so
    -- on.
    inferenceExecutionSummaries :: Prelude.Maybe [InferenceExecutionSummary],
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
-- 'nextToken', 'listInferenceExecutionsResponse_nextToken' - An opaque pagination token indicating where to continue the listing of
-- inference executions.
--
-- 'inferenceExecutionSummaries', 'listInferenceExecutionsResponse_inferenceExecutionSummaries' - Provides an array of information about the individual inference
-- executions returned from the @ListInferenceExecutions@ operation,
-- including model used, inference scheduler, data configuration, and so
-- on.
--
-- 'httpStatus', 'listInferenceExecutionsResponse_httpStatus' - The response's http status code.
newListInferenceExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInferenceExecutionsResponse
newListInferenceExecutionsResponse pHttpStatus_ =
  ListInferenceExecutionsResponse'
    { nextToken =
        Prelude.Nothing,
      inferenceExecutionSummaries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque pagination token indicating where to continue the listing of
-- inference executions.
listInferenceExecutionsResponse_nextToken :: Lens.Lens' ListInferenceExecutionsResponse (Prelude.Maybe Prelude.Text)
listInferenceExecutionsResponse_nextToken = Lens.lens (\ListInferenceExecutionsResponse' {nextToken} -> nextToken) (\s@ListInferenceExecutionsResponse' {} a -> s {nextToken = a} :: ListInferenceExecutionsResponse)

-- | Provides an array of information about the individual inference
-- executions returned from the @ListInferenceExecutions@ operation,
-- including model used, inference scheduler, data configuration, and so
-- on.
listInferenceExecutionsResponse_inferenceExecutionSummaries :: Lens.Lens' ListInferenceExecutionsResponse (Prelude.Maybe [InferenceExecutionSummary])
listInferenceExecutionsResponse_inferenceExecutionSummaries = Lens.lens (\ListInferenceExecutionsResponse' {inferenceExecutionSummaries} -> inferenceExecutionSummaries) (\s@ListInferenceExecutionsResponse' {} a -> s {inferenceExecutionSummaries = a} :: ListInferenceExecutionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInferenceExecutionsResponse_httpStatus :: Lens.Lens' ListInferenceExecutionsResponse Prelude.Int
listInferenceExecutionsResponse_httpStatus = Lens.lens (\ListInferenceExecutionsResponse' {httpStatus} -> httpStatus) (\s@ListInferenceExecutionsResponse' {} a -> s {httpStatus = a} :: ListInferenceExecutionsResponse)

instance
  Prelude.NFData
    ListInferenceExecutionsResponse
