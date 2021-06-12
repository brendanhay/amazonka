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
-- Module      : Network.AWS.SSM.DescribeAssociationExecutions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API action to view all executions for a specific association
-- ID.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAssociationExecutions
  ( -- * Creating a Request
    DescribeAssociationExecutions (..),
    newDescribeAssociationExecutions,

    -- * Request Lenses
    describeAssociationExecutions_nextToken,
    describeAssociationExecutions_maxResults,
    describeAssociationExecutions_filters,
    describeAssociationExecutions_associationId,

    -- * Destructuring the Response
    DescribeAssociationExecutionsResponse (..),
    newDescribeAssociationExecutionsResponse,

    -- * Response Lenses
    describeAssociationExecutionsResponse_nextToken,
    describeAssociationExecutionsResponse_associationExecutions,
    describeAssociationExecutionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeAssociationExecutions' smart constructor.
data DescribeAssociationExecutions = DescribeAssociationExecutions'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | Filters for the request. You can specify the following filters and
    -- values.
    --
    -- ExecutionId (EQUAL)
    --
    -- Status (EQUAL)
    --
    -- CreatedTime (EQUAL, GREATER_THAN, LESS_THAN)
    filters :: Core.Maybe (Core.NonEmpty AssociationExecutionFilter),
    -- | The association ID for which you want to view execution history details.
    associationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAssociationExecutions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAssociationExecutions_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'describeAssociationExecutions_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'describeAssociationExecutions_filters' - Filters for the request. You can specify the following filters and
-- values.
--
-- ExecutionId (EQUAL)
--
-- Status (EQUAL)
--
-- CreatedTime (EQUAL, GREATER_THAN, LESS_THAN)
--
-- 'associationId', 'describeAssociationExecutions_associationId' - The association ID for which you want to view execution history details.
newDescribeAssociationExecutions ::
  -- | 'associationId'
  Core.Text ->
  DescribeAssociationExecutions
newDescribeAssociationExecutions pAssociationId_ =
  DescribeAssociationExecutions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      associationId = pAssociationId_
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
describeAssociationExecutions_nextToken :: Lens.Lens' DescribeAssociationExecutions (Core.Maybe Core.Text)
describeAssociationExecutions_nextToken = Lens.lens (\DescribeAssociationExecutions' {nextToken} -> nextToken) (\s@DescribeAssociationExecutions' {} a -> s {nextToken = a} :: DescribeAssociationExecutions)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeAssociationExecutions_maxResults :: Lens.Lens' DescribeAssociationExecutions (Core.Maybe Core.Natural)
describeAssociationExecutions_maxResults = Lens.lens (\DescribeAssociationExecutions' {maxResults} -> maxResults) (\s@DescribeAssociationExecutions' {} a -> s {maxResults = a} :: DescribeAssociationExecutions)

-- | Filters for the request. You can specify the following filters and
-- values.
--
-- ExecutionId (EQUAL)
--
-- Status (EQUAL)
--
-- CreatedTime (EQUAL, GREATER_THAN, LESS_THAN)
describeAssociationExecutions_filters :: Lens.Lens' DescribeAssociationExecutions (Core.Maybe (Core.NonEmpty AssociationExecutionFilter))
describeAssociationExecutions_filters = Lens.lens (\DescribeAssociationExecutions' {filters} -> filters) (\s@DescribeAssociationExecutions' {} a -> s {filters = a} :: DescribeAssociationExecutions) Core.. Lens.mapping Lens._Coerce

-- | The association ID for which you want to view execution history details.
describeAssociationExecutions_associationId :: Lens.Lens' DescribeAssociationExecutions Core.Text
describeAssociationExecutions_associationId = Lens.lens (\DescribeAssociationExecutions' {associationId} -> associationId) (\s@DescribeAssociationExecutions' {} a -> s {associationId = a} :: DescribeAssociationExecutions)

instance Core.AWSPager DescribeAssociationExecutions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAssociationExecutionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAssociationExecutionsResponse_associationExecutions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeAssociationExecutions_nextToken
          Lens..~ rs
          Lens.^? describeAssociationExecutionsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeAssociationExecutions
  where
  type
    AWSResponse DescribeAssociationExecutions =
      DescribeAssociationExecutionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssociationExecutionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "AssociationExecutions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAssociationExecutions

instance Core.NFData DescribeAssociationExecutions

instance Core.ToHeaders DescribeAssociationExecutions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeAssociationExecutions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeAssociationExecutions where
  toJSON DescribeAssociationExecutions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters,
            Core.Just ("AssociationId" Core..= associationId)
          ]
      )

instance Core.ToPath DescribeAssociationExecutions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAssociationExecutions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAssociationExecutionsResponse' smart constructor.
data DescribeAssociationExecutionsResponse = DescribeAssociationExecutionsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of the executions for the specified association ID.
    associationExecutions :: Core.Maybe [AssociationExecution],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAssociationExecutionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAssociationExecutionsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'associationExecutions', 'describeAssociationExecutionsResponse_associationExecutions' - A list of the executions for the specified association ID.
--
-- 'httpStatus', 'describeAssociationExecutionsResponse_httpStatus' - The response's http status code.
newDescribeAssociationExecutionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAssociationExecutionsResponse
newDescribeAssociationExecutionsResponse pHttpStatus_ =
  DescribeAssociationExecutionsResponse'
    { nextToken =
        Core.Nothing,
      associationExecutions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
describeAssociationExecutionsResponse_nextToken :: Lens.Lens' DescribeAssociationExecutionsResponse (Core.Maybe Core.Text)
describeAssociationExecutionsResponse_nextToken = Lens.lens (\DescribeAssociationExecutionsResponse' {nextToken} -> nextToken) (\s@DescribeAssociationExecutionsResponse' {} a -> s {nextToken = a} :: DescribeAssociationExecutionsResponse)

-- | A list of the executions for the specified association ID.
describeAssociationExecutionsResponse_associationExecutions :: Lens.Lens' DescribeAssociationExecutionsResponse (Core.Maybe [AssociationExecution])
describeAssociationExecutionsResponse_associationExecutions = Lens.lens (\DescribeAssociationExecutionsResponse' {associationExecutions} -> associationExecutions) (\s@DescribeAssociationExecutionsResponse' {} a -> s {associationExecutions = a} :: DescribeAssociationExecutionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAssociationExecutionsResponse_httpStatus :: Lens.Lens' DescribeAssociationExecutionsResponse Core.Int
describeAssociationExecutionsResponse_httpStatus = Lens.lens (\DescribeAssociationExecutionsResponse' {httpStatus} -> httpStatus) (\s@DescribeAssociationExecutionsResponse' {} a -> s {httpStatus = a} :: DescribeAssociationExecutionsResponse)

instance
  Core.NFData
    DescribeAssociationExecutionsResponse
