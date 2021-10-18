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
-- Module      : Network.AWS.SageMaker.ListActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the actions in your account and their properties.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListActions
  ( -- * Creating a Request
    ListActions (..),
    newListActions,

    -- * Request Lenses
    listActions_nextToken,
    listActions_sortOrder,
    listActions_createdAfter,
    listActions_createdBefore,
    listActions_actionType,
    listActions_maxResults,
    listActions_sourceUri,
    listActions_sortBy,

    -- * Destructuring the Response
    ListActionsResponse (..),
    newListActionsResponse,

    -- * Response Lenses
    listActionsResponse_nextToken,
    listActionsResponse_actionSummaries,
    listActionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListActions' smart constructor.
data ListActions = ListActions'
  { -- | If the previous call to @ListActions@ didn\'t return the full set of
    -- actions, the call returns a token for getting the next set of actions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only actions created on or after the specified
    -- time.
    createdAfter :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only actions created on or before the specified
    -- time.
    createdBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only actions of the specified type.
    actionType :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of actions to return in the response. The default
    -- value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only actions with the specified source URI.
    sourceUri :: Prelude.Maybe Prelude.Text,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe SortActionsBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listActions_nextToken' - If the previous call to @ListActions@ didn\'t return the full set of
-- actions, the call returns a token for getting the next set of actions.
--
-- 'sortOrder', 'listActions_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'createdAfter', 'listActions_createdAfter' - A filter that returns only actions created on or after the specified
-- time.
--
-- 'createdBefore', 'listActions_createdBefore' - A filter that returns only actions created on or before the specified
-- time.
--
-- 'actionType', 'listActions_actionType' - A filter that returns only actions of the specified type.
--
-- 'maxResults', 'listActions_maxResults' - The maximum number of actions to return in the response. The default
-- value is 10.
--
-- 'sourceUri', 'listActions_sourceUri' - A filter that returns only actions with the specified source URI.
--
-- 'sortBy', 'listActions_sortBy' - The property used to sort results. The default value is @CreationTime@.
newListActions ::
  ListActions
newListActions =
  ListActions'
    { nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      createdAfter = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      actionType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sourceUri = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | If the previous call to @ListActions@ didn\'t return the full set of
-- actions, the call returns a token for getting the next set of actions.
listActions_nextToken :: Lens.Lens' ListActions (Prelude.Maybe Prelude.Text)
listActions_nextToken = Lens.lens (\ListActions' {nextToken} -> nextToken) (\s@ListActions' {} a -> s {nextToken = a} :: ListActions)

-- | The sort order. The default value is @Descending@.
listActions_sortOrder :: Lens.Lens' ListActions (Prelude.Maybe SortOrder)
listActions_sortOrder = Lens.lens (\ListActions' {sortOrder} -> sortOrder) (\s@ListActions' {} a -> s {sortOrder = a} :: ListActions)

-- | A filter that returns only actions created on or after the specified
-- time.
listActions_createdAfter :: Lens.Lens' ListActions (Prelude.Maybe Prelude.UTCTime)
listActions_createdAfter = Lens.lens (\ListActions' {createdAfter} -> createdAfter) (\s@ListActions' {} a -> s {createdAfter = a} :: ListActions) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only actions created on or before the specified
-- time.
listActions_createdBefore :: Lens.Lens' ListActions (Prelude.Maybe Prelude.UTCTime)
listActions_createdBefore = Lens.lens (\ListActions' {createdBefore} -> createdBefore) (\s@ListActions' {} a -> s {createdBefore = a} :: ListActions) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only actions of the specified type.
listActions_actionType :: Lens.Lens' ListActions (Prelude.Maybe Prelude.Text)
listActions_actionType = Lens.lens (\ListActions' {actionType} -> actionType) (\s@ListActions' {} a -> s {actionType = a} :: ListActions)

-- | The maximum number of actions to return in the response. The default
-- value is 10.
listActions_maxResults :: Lens.Lens' ListActions (Prelude.Maybe Prelude.Natural)
listActions_maxResults = Lens.lens (\ListActions' {maxResults} -> maxResults) (\s@ListActions' {} a -> s {maxResults = a} :: ListActions)

-- | A filter that returns only actions with the specified source URI.
listActions_sourceUri :: Lens.Lens' ListActions (Prelude.Maybe Prelude.Text)
listActions_sourceUri = Lens.lens (\ListActions' {sourceUri} -> sourceUri) (\s@ListActions' {} a -> s {sourceUri = a} :: ListActions)

-- | The property used to sort results. The default value is @CreationTime@.
listActions_sortBy :: Lens.Lens' ListActions (Prelude.Maybe SortActionsBy)
listActions_sortBy = Lens.lens (\ListActions' {sortBy} -> sortBy) (\s@ListActions' {} a -> s {sortBy = a} :: ListActions)

instance Core.AWSPager ListActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listActionsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listActionsResponse_actionSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listActions_nextToken
          Lens..~ rs
          Lens.^? listActionsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListActions where
  type AWSResponse ListActions = ListActionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ActionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListActions

instance Prelude.NFData ListActions

instance Core.ToHeaders ListActions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListActions" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListActions where
  toJSON ListActions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("CreatedAfter" Core..=) Prelude.<$> createdAfter,
            ("CreatedBefore" Core..=) Prelude.<$> createdBefore,
            ("ActionType" Core..=) Prelude.<$> actionType,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SourceUri" Core..=) Prelude.<$> sourceUri,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListActions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListActions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListActionsResponse' smart constructor.
data ListActionsResponse = ListActionsResponse'
  { -- | A token for getting the next set of actions, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of actions and their properties.
    actionSummaries :: Prelude.Maybe [ActionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listActionsResponse_nextToken' - A token for getting the next set of actions, if there are any.
--
-- 'actionSummaries', 'listActionsResponse_actionSummaries' - A list of actions and their properties.
--
-- 'httpStatus', 'listActionsResponse_httpStatus' - The response's http status code.
newListActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListActionsResponse
newListActionsResponse pHttpStatus_ =
  ListActionsResponse'
    { nextToken = Prelude.Nothing,
      actionSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of actions, if there are any.
listActionsResponse_nextToken :: Lens.Lens' ListActionsResponse (Prelude.Maybe Prelude.Text)
listActionsResponse_nextToken = Lens.lens (\ListActionsResponse' {nextToken} -> nextToken) (\s@ListActionsResponse' {} a -> s {nextToken = a} :: ListActionsResponse)

-- | A list of actions and their properties.
listActionsResponse_actionSummaries :: Lens.Lens' ListActionsResponse (Prelude.Maybe [ActionSummary])
listActionsResponse_actionSummaries = Lens.lens (\ListActionsResponse' {actionSummaries} -> actionSummaries) (\s@ListActionsResponse' {} a -> s {actionSummaries = a} :: ListActionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listActionsResponse_httpStatus :: Lens.Lens' ListActionsResponse Prelude.Int
listActionsResponse_httpStatus = Lens.lens (\ListActionsResponse' {httpStatus} -> httpStatus) (\s@ListActionsResponse' {} a -> s {httpStatus = a} :: ListActionsResponse)

instance Prelude.NFData ListActionsResponse
