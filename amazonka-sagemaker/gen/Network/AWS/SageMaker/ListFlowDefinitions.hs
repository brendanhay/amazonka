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
-- Module      : Network.AWS.SageMaker.ListFlowDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the flow definitions in your account.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListFlowDefinitions
  ( -- * Creating a Request
    ListFlowDefinitions (..),
    newListFlowDefinitions,

    -- * Request Lenses
    listFlowDefinitions_sortOrder,
    listFlowDefinitions_nextToken,
    listFlowDefinitions_maxResults,
    listFlowDefinitions_creationTimeBefore,
    listFlowDefinitions_creationTimeAfter,

    -- * Destructuring the Response
    ListFlowDefinitionsResponse (..),
    newListFlowDefinitionsResponse,

    -- * Response Lenses
    listFlowDefinitionsResponse_nextToken,
    listFlowDefinitionsResponse_httpStatus,
    listFlowDefinitionsResponse_flowDefinitionSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListFlowDefinitions' smart constructor.
data ListFlowDefinitions = ListFlowDefinitions'
  { -- | An optional value that specifies whether you want the results sorted in
    -- @Ascending@ or @Descending@ order.
    sortOrder :: Core.Maybe SortOrder,
    -- | A token to resume pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The total number of items to return. If the total number of available
    -- items is more than the value specified in @MaxResults@, then a
    -- @NextToken@ will be provided in the output that you can use to resume
    -- pagination.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only flow definitions that were created before the
    -- specified timestamp.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns only flow definitions with a creation time greater
    -- than or equal to the specified timestamp.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFlowDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listFlowDefinitions_sortOrder' - An optional value that specifies whether you want the results sorted in
-- @Ascending@ or @Descending@ order.
--
-- 'nextToken', 'listFlowDefinitions_nextToken' - A token to resume pagination.
--
-- 'maxResults', 'listFlowDefinitions_maxResults' - The total number of items to return. If the total number of available
-- items is more than the value specified in @MaxResults@, then a
-- @NextToken@ will be provided in the output that you can use to resume
-- pagination.
--
-- 'creationTimeBefore', 'listFlowDefinitions_creationTimeBefore' - A filter that returns only flow definitions that were created before the
-- specified timestamp.
--
-- 'creationTimeAfter', 'listFlowDefinitions_creationTimeAfter' - A filter that returns only flow definitions with a creation time greater
-- than or equal to the specified timestamp.
newListFlowDefinitions ::
  ListFlowDefinitions
newListFlowDefinitions =
  ListFlowDefinitions'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      creationTimeAfter = Core.Nothing
    }

-- | An optional value that specifies whether you want the results sorted in
-- @Ascending@ or @Descending@ order.
listFlowDefinitions_sortOrder :: Lens.Lens' ListFlowDefinitions (Core.Maybe SortOrder)
listFlowDefinitions_sortOrder = Lens.lens (\ListFlowDefinitions' {sortOrder} -> sortOrder) (\s@ListFlowDefinitions' {} a -> s {sortOrder = a} :: ListFlowDefinitions)

-- | A token to resume pagination.
listFlowDefinitions_nextToken :: Lens.Lens' ListFlowDefinitions (Core.Maybe Core.Text)
listFlowDefinitions_nextToken = Lens.lens (\ListFlowDefinitions' {nextToken} -> nextToken) (\s@ListFlowDefinitions' {} a -> s {nextToken = a} :: ListFlowDefinitions)

-- | The total number of items to return. If the total number of available
-- items is more than the value specified in @MaxResults@, then a
-- @NextToken@ will be provided in the output that you can use to resume
-- pagination.
listFlowDefinitions_maxResults :: Lens.Lens' ListFlowDefinitions (Core.Maybe Core.Natural)
listFlowDefinitions_maxResults = Lens.lens (\ListFlowDefinitions' {maxResults} -> maxResults) (\s@ListFlowDefinitions' {} a -> s {maxResults = a} :: ListFlowDefinitions)

-- | A filter that returns only flow definitions that were created before the
-- specified timestamp.
listFlowDefinitions_creationTimeBefore :: Lens.Lens' ListFlowDefinitions (Core.Maybe Core.UTCTime)
listFlowDefinitions_creationTimeBefore = Lens.lens (\ListFlowDefinitions' {creationTimeBefore} -> creationTimeBefore) (\s@ListFlowDefinitions' {} a -> s {creationTimeBefore = a} :: ListFlowDefinitions) Core.. Lens.mapping Core._Time

-- | A filter that returns only flow definitions with a creation time greater
-- than or equal to the specified timestamp.
listFlowDefinitions_creationTimeAfter :: Lens.Lens' ListFlowDefinitions (Core.Maybe Core.UTCTime)
listFlowDefinitions_creationTimeAfter = Lens.lens (\ListFlowDefinitions' {creationTimeAfter} -> creationTimeAfter) (\s@ListFlowDefinitions' {} a -> s {creationTimeAfter = a} :: ListFlowDefinitions) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListFlowDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFlowDefinitionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listFlowDefinitionsResponse_flowDefinitionSummaries
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listFlowDefinitions_nextToken
          Lens..~ rs
          Lens.^? listFlowDefinitionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListFlowDefinitions where
  type
    AWSResponse ListFlowDefinitions =
      ListFlowDefinitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFlowDefinitionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "FlowDefinitionSummaries"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListFlowDefinitions

instance Core.NFData ListFlowDefinitions

instance Core.ToHeaders ListFlowDefinitions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListFlowDefinitions" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListFlowDefinitions where
  toJSON ListFlowDefinitions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Core.<$> creationTimeBefore,
            ("CreationTimeAfter" Core..=)
              Core.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListFlowDefinitions where
  toPath = Core.const "/"

instance Core.ToQuery ListFlowDefinitions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListFlowDefinitionsResponse' smart constructor.
data ListFlowDefinitionsResponse = ListFlowDefinitionsResponse'
  { -- | A token to resume pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | An array of objects describing the flow definitions.
    flowDefinitionSummaries :: [FlowDefinitionSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListFlowDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFlowDefinitionsResponse_nextToken' - A token to resume pagination.
--
-- 'httpStatus', 'listFlowDefinitionsResponse_httpStatus' - The response's http status code.
--
-- 'flowDefinitionSummaries', 'listFlowDefinitionsResponse_flowDefinitionSummaries' - An array of objects describing the flow definitions.
newListFlowDefinitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListFlowDefinitionsResponse
newListFlowDefinitionsResponse pHttpStatus_ =
  ListFlowDefinitionsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      flowDefinitionSummaries = Core.mempty
    }

-- | A token to resume pagination.
listFlowDefinitionsResponse_nextToken :: Lens.Lens' ListFlowDefinitionsResponse (Core.Maybe Core.Text)
listFlowDefinitionsResponse_nextToken = Lens.lens (\ListFlowDefinitionsResponse' {nextToken} -> nextToken) (\s@ListFlowDefinitionsResponse' {} a -> s {nextToken = a} :: ListFlowDefinitionsResponse)

-- | The response's http status code.
listFlowDefinitionsResponse_httpStatus :: Lens.Lens' ListFlowDefinitionsResponse Core.Int
listFlowDefinitionsResponse_httpStatus = Lens.lens (\ListFlowDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListFlowDefinitionsResponse' {} a -> s {httpStatus = a} :: ListFlowDefinitionsResponse)

-- | An array of objects describing the flow definitions.
listFlowDefinitionsResponse_flowDefinitionSummaries :: Lens.Lens' ListFlowDefinitionsResponse [FlowDefinitionSummary]
listFlowDefinitionsResponse_flowDefinitionSummaries = Lens.lens (\ListFlowDefinitionsResponse' {flowDefinitionSummaries} -> flowDefinitionSummaries) (\s@ListFlowDefinitionsResponse' {} a -> s {flowDefinitionSummaries = a} :: ListFlowDefinitionsResponse) Core.. Lens._Coerce

instance Core.NFData ListFlowDefinitionsResponse
