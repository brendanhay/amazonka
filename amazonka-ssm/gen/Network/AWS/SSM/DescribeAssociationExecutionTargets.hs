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
-- Module      : Network.AWS.SSM.DescribeAssociationExecutionTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API action to view information about a specific execution of a
-- specific association.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAssociationExecutionTargets
  ( -- * Creating a Request
    DescribeAssociationExecutionTargets (..),
    newDescribeAssociationExecutionTargets,

    -- * Request Lenses
    describeAssociationExecutionTargets_nextToken,
    describeAssociationExecutionTargets_maxResults,
    describeAssociationExecutionTargets_filters,
    describeAssociationExecutionTargets_associationId,
    describeAssociationExecutionTargets_executionId,

    -- * Destructuring the Response
    DescribeAssociationExecutionTargetsResponse (..),
    newDescribeAssociationExecutionTargetsResponse,

    -- * Response Lenses
    describeAssociationExecutionTargetsResponse_nextToken,
    describeAssociationExecutionTargetsResponse_associationExecutionTargets,
    describeAssociationExecutionTargetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeAssociationExecutionTargets' smart constructor.
data DescribeAssociationExecutionTargets = DescribeAssociationExecutionTargets'
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
    -- Status (EQUAL)
    --
    -- ResourceId (EQUAL)
    --
    -- ResourceType (EQUAL)
    filters :: Core.Maybe (Core.NonEmpty AssociationExecutionTargetsFilter),
    -- | The association ID that includes the execution for which you want to
    -- view details.
    associationId :: Core.Text,
    -- | The execution ID for which you want to view details.
    executionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAssociationExecutionTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAssociationExecutionTargets_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'describeAssociationExecutionTargets_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'describeAssociationExecutionTargets_filters' - Filters for the request. You can specify the following filters and
-- values.
--
-- Status (EQUAL)
--
-- ResourceId (EQUAL)
--
-- ResourceType (EQUAL)
--
-- 'associationId', 'describeAssociationExecutionTargets_associationId' - The association ID that includes the execution for which you want to
-- view details.
--
-- 'executionId', 'describeAssociationExecutionTargets_executionId' - The execution ID for which you want to view details.
newDescribeAssociationExecutionTargets ::
  -- | 'associationId'
  Core.Text ->
  -- | 'executionId'
  Core.Text ->
  DescribeAssociationExecutionTargets
newDescribeAssociationExecutionTargets
  pAssociationId_
  pExecutionId_ =
    DescribeAssociationExecutionTargets'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        filters = Core.Nothing,
        associationId = pAssociationId_,
        executionId = pExecutionId_
      }

-- | A token to start the list. Use this token to get the next set of
-- results.
describeAssociationExecutionTargets_nextToken :: Lens.Lens' DescribeAssociationExecutionTargets (Core.Maybe Core.Text)
describeAssociationExecutionTargets_nextToken = Lens.lens (\DescribeAssociationExecutionTargets' {nextToken} -> nextToken) (\s@DescribeAssociationExecutionTargets' {} a -> s {nextToken = a} :: DescribeAssociationExecutionTargets)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
describeAssociationExecutionTargets_maxResults :: Lens.Lens' DescribeAssociationExecutionTargets (Core.Maybe Core.Natural)
describeAssociationExecutionTargets_maxResults = Lens.lens (\DescribeAssociationExecutionTargets' {maxResults} -> maxResults) (\s@DescribeAssociationExecutionTargets' {} a -> s {maxResults = a} :: DescribeAssociationExecutionTargets)

-- | Filters for the request. You can specify the following filters and
-- values.
--
-- Status (EQUAL)
--
-- ResourceId (EQUAL)
--
-- ResourceType (EQUAL)
describeAssociationExecutionTargets_filters :: Lens.Lens' DescribeAssociationExecutionTargets (Core.Maybe (Core.NonEmpty AssociationExecutionTargetsFilter))
describeAssociationExecutionTargets_filters = Lens.lens (\DescribeAssociationExecutionTargets' {filters} -> filters) (\s@DescribeAssociationExecutionTargets' {} a -> s {filters = a} :: DescribeAssociationExecutionTargets) Core.. Lens.mapping Lens._Coerce

-- | The association ID that includes the execution for which you want to
-- view details.
describeAssociationExecutionTargets_associationId :: Lens.Lens' DescribeAssociationExecutionTargets Core.Text
describeAssociationExecutionTargets_associationId = Lens.lens (\DescribeAssociationExecutionTargets' {associationId} -> associationId) (\s@DescribeAssociationExecutionTargets' {} a -> s {associationId = a} :: DescribeAssociationExecutionTargets)

-- | The execution ID for which you want to view details.
describeAssociationExecutionTargets_executionId :: Lens.Lens' DescribeAssociationExecutionTargets Core.Text
describeAssociationExecutionTargets_executionId = Lens.lens (\DescribeAssociationExecutionTargets' {executionId} -> executionId) (\s@DescribeAssociationExecutionTargets' {} a -> s {executionId = a} :: DescribeAssociationExecutionTargets)

instance
  Core.AWSPager
    DescribeAssociationExecutionTargets
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAssociationExecutionTargetsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAssociationExecutionTargetsResponse_associationExecutionTargets
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeAssociationExecutionTargets_nextToken
          Lens..~ rs
          Lens.^? describeAssociationExecutionTargetsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeAssociationExecutionTargets
  where
  type
    AWSResponse DescribeAssociationExecutionTargets =
      DescribeAssociationExecutionTargetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAssociationExecutionTargetsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "AssociationExecutionTargets"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeAssociationExecutionTargets

instance
  Core.NFData
    DescribeAssociationExecutionTargets

instance
  Core.ToHeaders
    DescribeAssociationExecutionTargets
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DescribeAssociationExecutionTargets" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeAssociationExecutionTargets
  where
  toJSON DescribeAssociationExecutionTargets' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters,
            Core.Just ("AssociationId" Core..= associationId),
            Core.Just ("ExecutionId" Core..= executionId)
          ]
      )

instance
  Core.ToPath
    DescribeAssociationExecutionTargets
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeAssociationExecutionTargets
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAssociationExecutionTargetsResponse' smart constructor.
data DescribeAssociationExecutionTargetsResponse = DescribeAssociationExecutionTargetsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the execution.
    associationExecutionTargets :: Core.Maybe [AssociationExecutionTarget],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAssociationExecutionTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAssociationExecutionTargetsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'associationExecutionTargets', 'describeAssociationExecutionTargetsResponse_associationExecutionTargets' - Information about the execution.
--
-- 'httpStatus', 'describeAssociationExecutionTargetsResponse_httpStatus' - The response's http status code.
newDescribeAssociationExecutionTargetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAssociationExecutionTargetsResponse
newDescribeAssociationExecutionTargetsResponse
  pHttpStatus_ =
    DescribeAssociationExecutionTargetsResponse'
      { nextToken =
          Core.Nothing,
        associationExecutionTargets =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
describeAssociationExecutionTargetsResponse_nextToken :: Lens.Lens' DescribeAssociationExecutionTargetsResponse (Core.Maybe Core.Text)
describeAssociationExecutionTargetsResponse_nextToken = Lens.lens (\DescribeAssociationExecutionTargetsResponse' {nextToken} -> nextToken) (\s@DescribeAssociationExecutionTargetsResponse' {} a -> s {nextToken = a} :: DescribeAssociationExecutionTargetsResponse)

-- | Information about the execution.
describeAssociationExecutionTargetsResponse_associationExecutionTargets :: Lens.Lens' DescribeAssociationExecutionTargetsResponse (Core.Maybe [AssociationExecutionTarget])
describeAssociationExecutionTargetsResponse_associationExecutionTargets = Lens.lens (\DescribeAssociationExecutionTargetsResponse' {associationExecutionTargets} -> associationExecutionTargets) (\s@DescribeAssociationExecutionTargetsResponse' {} a -> s {associationExecutionTargets = a} :: DescribeAssociationExecutionTargetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAssociationExecutionTargetsResponse_httpStatus :: Lens.Lens' DescribeAssociationExecutionTargetsResponse Core.Int
describeAssociationExecutionTargetsResponse_httpStatus = Lens.lens (\DescribeAssociationExecutionTargetsResponse' {httpStatus} -> httpStatus) (\s@DescribeAssociationExecutionTargetsResponse' {} a -> s {httpStatus = a} :: DescribeAssociationExecutionTargetsResponse)

instance
  Core.NFData
    DescribeAssociationExecutionTargetsResponse
