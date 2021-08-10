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
-- Module      : Network.AWS.SSM.DescribeAutomationExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about all active and terminated Automation executions.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAutomationExecutions
  ( -- * Creating a Request
    DescribeAutomationExecutions (..),
    newDescribeAutomationExecutions,

    -- * Request Lenses
    describeAutomationExecutions_nextToken,
    describeAutomationExecutions_maxResults,
    describeAutomationExecutions_filters,

    -- * Destructuring the Response
    DescribeAutomationExecutionsResponse (..),
    newDescribeAutomationExecutionsResponse,

    -- * Response Lenses
    describeAutomationExecutionsResponse_nextToken,
    describeAutomationExecutionsResponse_automationExecutionMetadataList,
    describeAutomationExecutionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeAutomationExecutions' smart constructor.
data DescribeAutomationExecutions = DescribeAutomationExecutions'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filters used to limit the scope of executions that are requested.
    filters :: Prelude.Maybe (Prelude.NonEmpty AutomationExecutionFilter)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutomationExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAutomationExecutions_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'describeAutomationExecutions_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'describeAutomationExecutions_filters' - Filters used to limit the scope of executions that are requested.
newDescribeAutomationExecutions ::
  DescribeAutomationExecutions
newDescribeAutomationExecutions =
  DescribeAutomationExecutions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeAutomationExecutions_nextToken :: Lens.Lens' DescribeAutomationExecutions (Prelude.Maybe Prelude.Text)
describeAutomationExecutions_nextToken = Lens.lens (\DescribeAutomationExecutions' {nextToken} -> nextToken) (\s@DescribeAutomationExecutions' {} a -> s {nextToken = a} :: DescribeAutomationExecutions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeAutomationExecutions_maxResults :: Lens.Lens' DescribeAutomationExecutions (Prelude.Maybe Prelude.Natural)
describeAutomationExecutions_maxResults = Lens.lens (\DescribeAutomationExecutions' {maxResults} -> maxResults) (\s@DescribeAutomationExecutions' {} a -> s {maxResults = a} :: DescribeAutomationExecutions)

-- | Filters used to limit the scope of executions that are requested.
describeAutomationExecutions_filters :: Lens.Lens' DescribeAutomationExecutions (Prelude.Maybe (Prelude.NonEmpty AutomationExecutionFilter))
describeAutomationExecutions_filters = Lens.lens (\DescribeAutomationExecutions' {filters} -> filters) (\s@DescribeAutomationExecutions' {} a -> s {filters = a} :: DescribeAutomationExecutions) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeAutomationExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAutomationExecutionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAutomationExecutionsResponse_automationExecutionMetadataList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeAutomationExecutions_nextToken
          Lens..~ rs
          Lens.^? describeAutomationExecutionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeAutomationExecutions where
  type
    AWSResponse DescribeAutomationExecutions =
      DescribeAutomationExecutionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAutomationExecutionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "AutomationExecutionMetadataList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAutomationExecutions

instance Prelude.NFData DescribeAutomationExecutions

instance Core.ToHeaders DescribeAutomationExecutions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeAutomationExecutions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAutomationExecutions where
  toJSON DescribeAutomationExecutions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filters" Core..=) Prelude.<$> filters
          ]
      )

instance Core.ToPath DescribeAutomationExecutions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAutomationExecutions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAutomationExecutionsResponse' smart constructor.
data DescribeAutomationExecutionsResponse = DescribeAutomationExecutionsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of details about each automation execution which has occurred
    -- which matches the filter specification, if any.
    automationExecutionMetadataList :: Prelude.Maybe [AutomationExecutionMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAutomationExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAutomationExecutionsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'automationExecutionMetadataList', 'describeAutomationExecutionsResponse_automationExecutionMetadataList' - The list of details about each automation execution which has occurred
-- which matches the filter specification, if any.
--
-- 'httpStatus', 'describeAutomationExecutionsResponse_httpStatus' - The response's http status code.
newDescribeAutomationExecutionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAutomationExecutionsResponse
newDescribeAutomationExecutionsResponse pHttpStatus_ =
  DescribeAutomationExecutionsResponse'
    { nextToken =
        Prelude.Nothing,
      automationExecutionMetadataList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeAutomationExecutionsResponse_nextToken :: Lens.Lens' DescribeAutomationExecutionsResponse (Prelude.Maybe Prelude.Text)
describeAutomationExecutionsResponse_nextToken = Lens.lens (\DescribeAutomationExecutionsResponse' {nextToken} -> nextToken) (\s@DescribeAutomationExecutionsResponse' {} a -> s {nextToken = a} :: DescribeAutomationExecutionsResponse)

-- | The list of details about each automation execution which has occurred
-- which matches the filter specification, if any.
describeAutomationExecutionsResponse_automationExecutionMetadataList :: Lens.Lens' DescribeAutomationExecutionsResponse (Prelude.Maybe [AutomationExecutionMetadata])
describeAutomationExecutionsResponse_automationExecutionMetadataList = Lens.lens (\DescribeAutomationExecutionsResponse' {automationExecutionMetadataList} -> automationExecutionMetadataList) (\s@DescribeAutomationExecutionsResponse' {} a -> s {automationExecutionMetadataList = a} :: DescribeAutomationExecutionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAutomationExecutionsResponse_httpStatus :: Lens.Lens' DescribeAutomationExecutionsResponse Prelude.Int
describeAutomationExecutionsResponse_httpStatus = Lens.lens (\DescribeAutomationExecutionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAutomationExecutionsResponse' {} a -> s {httpStatus = a} :: DescribeAutomationExecutionsResponse)

instance
  Prelude.NFData
    DescribeAutomationExecutionsResponse
