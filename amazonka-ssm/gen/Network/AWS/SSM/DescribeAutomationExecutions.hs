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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeAutomationExecutions' smart constructor.
data DescribeAutomationExecutions = DescribeAutomationExecutions'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | Filters used to limit the scope of executions that are requested.
    filters :: Core.Maybe (Core.NonEmpty AutomationExecutionFilter)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeAutomationExecutions_nextToken :: Lens.Lens' DescribeAutomationExecutions (Core.Maybe Core.Text)
describeAutomationExecutions_nextToken = Lens.lens (\DescribeAutomationExecutions' {nextToken} -> nextToken) (\s@DescribeAutomationExecutions' {} a -> s {nextToken = a} :: DescribeAutomationExecutions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeAutomationExecutions_maxResults :: Lens.Lens' DescribeAutomationExecutions (Core.Maybe Core.Natural)
describeAutomationExecutions_maxResults = Lens.lens (\DescribeAutomationExecutions' {maxResults} -> maxResults) (\s@DescribeAutomationExecutions' {} a -> s {maxResults = a} :: DescribeAutomationExecutions)

-- | Filters used to limit the scope of executions that are requested.
describeAutomationExecutions_filters :: Lens.Lens' DescribeAutomationExecutions (Core.Maybe (Core.NonEmpty AutomationExecutionFilter))
describeAutomationExecutions_filters = Lens.lens (\DescribeAutomationExecutions' {filters} -> filters) (\s@DescribeAutomationExecutions' {} a -> s {filters = a} :: DescribeAutomationExecutions) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeAutomationExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAutomationExecutionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAutomationExecutionsResponse_automationExecutionMetadataList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeAutomationExecutions_nextToken
          Lens..~ rs
          Lens.^? describeAutomationExecutionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeAutomationExecutions where
  type
    AWSResponse DescribeAutomationExecutions =
      DescribeAutomationExecutionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAutomationExecutionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "AutomationExecutionMetadataList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAutomationExecutions

instance Core.NFData DescribeAutomationExecutions

instance Core.ToHeaders DescribeAutomationExecutions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeAutomationExecutions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAutomationExecutions where
  toJSON DescribeAutomationExecutions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath DescribeAutomationExecutions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAutomationExecutions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAutomationExecutionsResponse' smart constructor.
data DescribeAutomationExecutionsResponse = DescribeAutomationExecutionsResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of details about each automation execution which has occurred
    -- which matches the filter specification, if any.
    automationExecutionMetadataList :: Core.Maybe [AutomationExecutionMetadata],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeAutomationExecutionsResponse
newDescribeAutomationExecutionsResponse pHttpStatus_ =
  DescribeAutomationExecutionsResponse'
    { nextToken =
        Core.Nothing,
      automationExecutionMetadataList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
describeAutomationExecutionsResponse_nextToken :: Lens.Lens' DescribeAutomationExecutionsResponse (Core.Maybe Core.Text)
describeAutomationExecutionsResponse_nextToken = Lens.lens (\DescribeAutomationExecutionsResponse' {nextToken} -> nextToken) (\s@DescribeAutomationExecutionsResponse' {} a -> s {nextToken = a} :: DescribeAutomationExecutionsResponse)

-- | The list of details about each automation execution which has occurred
-- which matches the filter specification, if any.
describeAutomationExecutionsResponse_automationExecutionMetadataList :: Lens.Lens' DescribeAutomationExecutionsResponse (Core.Maybe [AutomationExecutionMetadata])
describeAutomationExecutionsResponse_automationExecutionMetadataList = Lens.lens (\DescribeAutomationExecutionsResponse' {automationExecutionMetadataList} -> automationExecutionMetadataList) (\s@DescribeAutomationExecutionsResponse' {} a -> s {automationExecutionMetadataList = a} :: DescribeAutomationExecutionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAutomationExecutionsResponse_httpStatus :: Lens.Lens' DescribeAutomationExecutionsResponse Core.Int
describeAutomationExecutionsResponse_httpStatus = Lens.lens (\DescribeAutomationExecutionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAutomationExecutionsResponse' {} a -> s {httpStatus = a} :: DescribeAutomationExecutionsResponse)

instance
  Core.NFData
    DescribeAutomationExecutionsResponse
