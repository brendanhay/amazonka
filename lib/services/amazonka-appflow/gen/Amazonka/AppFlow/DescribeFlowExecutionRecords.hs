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
-- Module      : Amazonka.AppFlow.DescribeFlowExecutionRecords
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches the execution history of the flow.
module Amazonka.AppFlow.DescribeFlowExecutionRecords
  ( -- * Creating a Request
    DescribeFlowExecutionRecords (..),
    newDescribeFlowExecutionRecords,

    -- * Request Lenses
    describeFlowExecutionRecords_nextToken,
    describeFlowExecutionRecords_maxResults,
    describeFlowExecutionRecords_flowName,

    -- * Destructuring the Response
    DescribeFlowExecutionRecordsResponse (..),
    newDescribeFlowExecutionRecordsResponse,

    -- * Response Lenses
    describeFlowExecutionRecordsResponse_flowExecutions,
    describeFlowExecutionRecordsResponse_nextToken,
    describeFlowExecutionRecordsResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFlowExecutionRecords' smart constructor.
data DescribeFlowExecutionRecords = DescribeFlowExecutionRecords'
  { -- | The pagination token for the next page of data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum number of items that should be returned in the
    -- result set. The default for @maxResults@ is 20 (for all paginated API
    -- operations).
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The specified name of the flow. Spaces are not allowed. Use underscores
    -- (_) or hyphens (-) only.
    flowName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFlowExecutionRecords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFlowExecutionRecords_nextToken' - The pagination token for the next page of data.
--
-- 'maxResults', 'describeFlowExecutionRecords_maxResults' - Specifies the maximum number of items that should be returned in the
-- result set. The default for @maxResults@ is 20 (for all paginated API
-- operations).
--
-- 'flowName', 'describeFlowExecutionRecords_flowName' - The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
newDescribeFlowExecutionRecords ::
  -- | 'flowName'
  Prelude.Text ->
  DescribeFlowExecutionRecords
newDescribeFlowExecutionRecords pFlowName_ =
  DescribeFlowExecutionRecords'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      flowName = pFlowName_
    }

-- | The pagination token for the next page of data.
describeFlowExecutionRecords_nextToken :: Lens.Lens' DescribeFlowExecutionRecords (Prelude.Maybe Prelude.Text)
describeFlowExecutionRecords_nextToken = Lens.lens (\DescribeFlowExecutionRecords' {nextToken} -> nextToken) (\s@DescribeFlowExecutionRecords' {} a -> s {nextToken = a} :: DescribeFlowExecutionRecords)

-- | Specifies the maximum number of items that should be returned in the
-- result set. The default for @maxResults@ is 20 (for all paginated API
-- operations).
describeFlowExecutionRecords_maxResults :: Lens.Lens' DescribeFlowExecutionRecords (Prelude.Maybe Prelude.Natural)
describeFlowExecutionRecords_maxResults = Lens.lens (\DescribeFlowExecutionRecords' {maxResults} -> maxResults) (\s@DescribeFlowExecutionRecords' {} a -> s {maxResults = a} :: DescribeFlowExecutionRecords)

-- | The specified name of the flow. Spaces are not allowed. Use underscores
-- (_) or hyphens (-) only.
describeFlowExecutionRecords_flowName :: Lens.Lens' DescribeFlowExecutionRecords Prelude.Text
describeFlowExecutionRecords_flowName = Lens.lens (\DescribeFlowExecutionRecords' {flowName} -> flowName) (\s@DescribeFlowExecutionRecords' {} a -> s {flowName = a} :: DescribeFlowExecutionRecords)

instance Core.AWSRequest DescribeFlowExecutionRecords where
  type
    AWSResponse DescribeFlowExecutionRecords =
      DescribeFlowExecutionRecordsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFlowExecutionRecordsResponse'
            Prelude.<$> (x Core..?> "flowExecutions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeFlowExecutionRecords
  where
  hashWithSalt _salt DescribeFlowExecutionRecords' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` flowName

instance Prelude.NFData DescribeFlowExecutionRecords where
  rnf DescribeFlowExecutionRecords' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf flowName

instance Core.ToHeaders DescribeFlowExecutionRecords where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFlowExecutionRecords where
  toJSON DescribeFlowExecutionRecords' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("flowName" Core..= flowName)
          ]
      )

instance Core.ToPath DescribeFlowExecutionRecords where
  toPath =
    Prelude.const "/describe-flow-execution-records"

instance Core.ToQuery DescribeFlowExecutionRecords where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFlowExecutionRecordsResponse' smart constructor.
data DescribeFlowExecutionRecordsResponse = DescribeFlowExecutionRecordsResponse'
  { -- | Returns a list of all instances when this flow was run.
    flowExecutions :: Prelude.Maybe [ExecutionRecord],
    -- | The pagination token for the next page of data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFlowExecutionRecordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowExecutions', 'describeFlowExecutionRecordsResponse_flowExecutions' - Returns a list of all instances when this flow was run.
--
-- 'nextToken', 'describeFlowExecutionRecordsResponse_nextToken' - The pagination token for the next page of data.
--
-- 'httpStatus', 'describeFlowExecutionRecordsResponse_httpStatus' - The response's http status code.
newDescribeFlowExecutionRecordsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFlowExecutionRecordsResponse
newDescribeFlowExecutionRecordsResponse pHttpStatus_ =
  DescribeFlowExecutionRecordsResponse'
    { flowExecutions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of all instances when this flow was run.
describeFlowExecutionRecordsResponse_flowExecutions :: Lens.Lens' DescribeFlowExecutionRecordsResponse (Prelude.Maybe [ExecutionRecord])
describeFlowExecutionRecordsResponse_flowExecutions = Lens.lens (\DescribeFlowExecutionRecordsResponse' {flowExecutions} -> flowExecutions) (\s@DescribeFlowExecutionRecordsResponse' {} a -> s {flowExecutions = a} :: DescribeFlowExecutionRecordsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token for the next page of data.
describeFlowExecutionRecordsResponse_nextToken :: Lens.Lens' DescribeFlowExecutionRecordsResponse (Prelude.Maybe Prelude.Text)
describeFlowExecutionRecordsResponse_nextToken = Lens.lens (\DescribeFlowExecutionRecordsResponse' {nextToken} -> nextToken) (\s@DescribeFlowExecutionRecordsResponse' {} a -> s {nextToken = a} :: DescribeFlowExecutionRecordsResponse)

-- | The response's http status code.
describeFlowExecutionRecordsResponse_httpStatus :: Lens.Lens' DescribeFlowExecutionRecordsResponse Prelude.Int
describeFlowExecutionRecordsResponse_httpStatus = Lens.lens (\DescribeFlowExecutionRecordsResponse' {httpStatus} -> httpStatus) (\s@DescribeFlowExecutionRecordsResponse' {} a -> s {httpStatus = a} :: DescribeFlowExecutionRecordsResponse)

instance
  Prelude.NFData
    DescribeFlowExecutionRecordsResponse
  where
  rnf DescribeFlowExecutionRecordsResponse' {..} =
    Prelude.rnf flowExecutions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
