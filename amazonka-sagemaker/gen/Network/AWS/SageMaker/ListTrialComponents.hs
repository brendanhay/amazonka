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
-- Module      : Network.AWS.SageMaker.ListTrialComponents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the trial components in your account. You can sort the list by
-- trial component name or creation time. You can filter the list to show
-- only components that were created in a specific time range. You can also
-- filter on one of the following:
--
-- -   @ExperimentName@
--
-- -   @SourceArn@
--
-- -   @TrialName@
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTrialComponents
  ( -- * Creating a Request
    ListTrialComponents (..),
    newListTrialComponents,

    -- * Request Lenses
    listTrialComponents_createdAfter,
    listTrialComponents_sortOrder,
    listTrialComponents_nextToken,
    listTrialComponents_createdBefore,
    listTrialComponents_maxResults,
    listTrialComponents_sortBy,
    listTrialComponents_experimentName,
    listTrialComponents_sourceArn,
    listTrialComponents_trialName,

    -- * Destructuring the Response
    ListTrialComponentsResponse (..),
    newListTrialComponentsResponse,

    -- * Response Lenses
    listTrialComponentsResponse_nextToken,
    listTrialComponentsResponse_trialComponentSummaries,
    listTrialComponentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListTrialComponents' smart constructor.
data ListTrialComponents = ListTrialComponents'
  { -- | A filter that returns only components created after the specified time.
    createdAfter :: Core.Maybe Core.POSIX,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the previous call to @ListTrialComponents@ didn\'t return the full
    -- set of components, the call returns a token for getting the next set of
    -- components.
    nextToken :: Core.Maybe Core.Text,
    -- | A filter that returns only components created before the specified time.
    createdBefore :: Core.Maybe Core.POSIX,
    -- | The maximum number of components to return in the response. The default
    -- value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Core.Maybe SortTrialComponentsBy,
    -- | A filter that returns only components that are part of the specified
    -- experiment. If you specify @ExperimentName@, you can\'t filter by
    -- @SourceArn@ or @TrialName@.
    experimentName :: Core.Maybe Core.Text,
    -- | A filter that returns only components that have the specified source
    -- Amazon Resource Name (ARN). If you specify @SourceArn@, you can\'t
    -- filter by @ExperimentName@ or @TrialName@.
    sourceArn :: Core.Maybe Core.Text,
    -- | A filter that returns only components that are part of the specified
    -- trial. If you specify @TrialName@, you can\'t filter by @ExperimentName@
    -- or @SourceArn@.
    trialName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTrialComponents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'listTrialComponents_createdAfter' - A filter that returns only components created after the specified time.
--
-- 'sortOrder', 'listTrialComponents_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'nextToken', 'listTrialComponents_nextToken' - If the previous call to @ListTrialComponents@ didn\'t return the full
-- set of components, the call returns a token for getting the next set of
-- components.
--
-- 'createdBefore', 'listTrialComponents_createdBefore' - A filter that returns only components created before the specified time.
--
-- 'maxResults', 'listTrialComponents_maxResults' - The maximum number of components to return in the response. The default
-- value is 10.
--
-- 'sortBy', 'listTrialComponents_sortBy' - The property used to sort results. The default value is @CreationTime@.
--
-- 'experimentName', 'listTrialComponents_experimentName' - A filter that returns only components that are part of the specified
-- experiment. If you specify @ExperimentName@, you can\'t filter by
-- @SourceArn@ or @TrialName@.
--
-- 'sourceArn', 'listTrialComponents_sourceArn' - A filter that returns only components that have the specified source
-- Amazon Resource Name (ARN). If you specify @SourceArn@, you can\'t
-- filter by @ExperimentName@ or @TrialName@.
--
-- 'trialName', 'listTrialComponents_trialName' - A filter that returns only components that are part of the specified
-- trial. If you specify @TrialName@, you can\'t filter by @ExperimentName@
-- or @SourceArn@.
newListTrialComponents ::
  ListTrialComponents
newListTrialComponents =
  ListTrialComponents'
    { createdAfter = Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      createdBefore = Core.Nothing,
      maxResults = Core.Nothing,
      sortBy = Core.Nothing,
      experimentName = Core.Nothing,
      sourceArn = Core.Nothing,
      trialName = Core.Nothing
    }

-- | A filter that returns only components created after the specified time.
listTrialComponents_createdAfter :: Lens.Lens' ListTrialComponents (Core.Maybe Core.UTCTime)
listTrialComponents_createdAfter = Lens.lens (\ListTrialComponents' {createdAfter} -> createdAfter) (\s@ListTrialComponents' {} a -> s {createdAfter = a} :: ListTrialComponents) Core.. Lens.mapping Core._Time

-- | The sort order. The default value is @Descending@.
listTrialComponents_sortOrder :: Lens.Lens' ListTrialComponents (Core.Maybe SortOrder)
listTrialComponents_sortOrder = Lens.lens (\ListTrialComponents' {sortOrder} -> sortOrder) (\s@ListTrialComponents' {} a -> s {sortOrder = a} :: ListTrialComponents)

-- | If the previous call to @ListTrialComponents@ didn\'t return the full
-- set of components, the call returns a token for getting the next set of
-- components.
listTrialComponents_nextToken :: Lens.Lens' ListTrialComponents (Core.Maybe Core.Text)
listTrialComponents_nextToken = Lens.lens (\ListTrialComponents' {nextToken} -> nextToken) (\s@ListTrialComponents' {} a -> s {nextToken = a} :: ListTrialComponents)

-- | A filter that returns only components created before the specified time.
listTrialComponents_createdBefore :: Lens.Lens' ListTrialComponents (Core.Maybe Core.UTCTime)
listTrialComponents_createdBefore = Lens.lens (\ListTrialComponents' {createdBefore} -> createdBefore) (\s@ListTrialComponents' {} a -> s {createdBefore = a} :: ListTrialComponents) Core.. Lens.mapping Core._Time

-- | The maximum number of components to return in the response. The default
-- value is 10.
listTrialComponents_maxResults :: Lens.Lens' ListTrialComponents (Core.Maybe Core.Natural)
listTrialComponents_maxResults = Lens.lens (\ListTrialComponents' {maxResults} -> maxResults) (\s@ListTrialComponents' {} a -> s {maxResults = a} :: ListTrialComponents)

-- | The property used to sort results. The default value is @CreationTime@.
listTrialComponents_sortBy :: Lens.Lens' ListTrialComponents (Core.Maybe SortTrialComponentsBy)
listTrialComponents_sortBy = Lens.lens (\ListTrialComponents' {sortBy} -> sortBy) (\s@ListTrialComponents' {} a -> s {sortBy = a} :: ListTrialComponents)

-- | A filter that returns only components that are part of the specified
-- experiment. If you specify @ExperimentName@, you can\'t filter by
-- @SourceArn@ or @TrialName@.
listTrialComponents_experimentName :: Lens.Lens' ListTrialComponents (Core.Maybe Core.Text)
listTrialComponents_experimentName = Lens.lens (\ListTrialComponents' {experimentName} -> experimentName) (\s@ListTrialComponents' {} a -> s {experimentName = a} :: ListTrialComponents)

-- | A filter that returns only components that have the specified source
-- Amazon Resource Name (ARN). If you specify @SourceArn@, you can\'t
-- filter by @ExperimentName@ or @TrialName@.
listTrialComponents_sourceArn :: Lens.Lens' ListTrialComponents (Core.Maybe Core.Text)
listTrialComponents_sourceArn = Lens.lens (\ListTrialComponents' {sourceArn} -> sourceArn) (\s@ListTrialComponents' {} a -> s {sourceArn = a} :: ListTrialComponents)

-- | A filter that returns only components that are part of the specified
-- trial. If you specify @TrialName@, you can\'t filter by @ExperimentName@
-- or @SourceArn@.
listTrialComponents_trialName :: Lens.Lens' ListTrialComponents (Core.Maybe Core.Text)
listTrialComponents_trialName = Lens.lens (\ListTrialComponents' {trialName} -> trialName) (\s@ListTrialComponents' {} a -> s {trialName = a} :: ListTrialComponents)

instance Core.AWSPager ListTrialComponents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTrialComponentsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTrialComponentsResponse_trialComponentSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTrialComponents_nextToken
          Lens..~ rs
          Lens.^? listTrialComponentsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListTrialComponents where
  type
    AWSResponse ListTrialComponents =
      ListTrialComponentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrialComponentsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "TrialComponentSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTrialComponents

instance Core.NFData ListTrialComponents

instance Core.ToHeaders ListTrialComponents where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListTrialComponents" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTrialComponents where
  toJSON ListTrialComponents' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreatedAfter" Core..=) Core.<$> createdAfter,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("CreatedBefore" Core..=) Core.<$> createdBefore,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("ExperimentName" Core..=) Core.<$> experimentName,
            ("SourceArn" Core..=) Core.<$> sourceArn,
            ("TrialName" Core..=) Core.<$> trialName
          ]
      )

instance Core.ToPath ListTrialComponents where
  toPath = Core.const "/"

instance Core.ToQuery ListTrialComponents where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTrialComponentsResponse' smart constructor.
data ListTrialComponentsResponse = ListTrialComponentsResponse'
  { -- | A token for getting the next set of components, if there are any.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of the summaries of your trial components.
    trialComponentSummaries :: Core.Maybe [TrialComponentSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTrialComponentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTrialComponentsResponse_nextToken' - A token for getting the next set of components, if there are any.
--
-- 'trialComponentSummaries', 'listTrialComponentsResponse_trialComponentSummaries' - A list of the summaries of your trial components.
--
-- 'httpStatus', 'listTrialComponentsResponse_httpStatus' - The response's http status code.
newListTrialComponentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTrialComponentsResponse
newListTrialComponentsResponse pHttpStatus_ =
  ListTrialComponentsResponse'
    { nextToken =
        Core.Nothing,
      trialComponentSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of components, if there are any.
listTrialComponentsResponse_nextToken :: Lens.Lens' ListTrialComponentsResponse (Core.Maybe Core.Text)
listTrialComponentsResponse_nextToken = Lens.lens (\ListTrialComponentsResponse' {nextToken} -> nextToken) (\s@ListTrialComponentsResponse' {} a -> s {nextToken = a} :: ListTrialComponentsResponse)

-- | A list of the summaries of your trial components.
listTrialComponentsResponse_trialComponentSummaries :: Lens.Lens' ListTrialComponentsResponse (Core.Maybe [TrialComponentSummary])
listTrialComponentsResponse_trialComponentSummaries = Lens.lens (\ListTrialComponentsResponse' {trialComponentSummaries} -> trialComponentSummaries) (\s@ListTrialComponentsResponse' {} a -> s {trialComponentSummaries = a} :: ListTrialComponentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTrialComponentsResponse_httpStatus :: Lens.Lens' ListTrialComponentsResponse Core.Int
listTrialComponentsResponse_httpStatus = Lens.lens (\ListTrialComponentsResponse' {httpStatus} -> httpStatus) (\s@ListTrialComponentsResponse' {} a -> s {httpStatus = a} :: ListTrialComponentsResponse)

instance Core.NFData ListTrialComponentsResponse
