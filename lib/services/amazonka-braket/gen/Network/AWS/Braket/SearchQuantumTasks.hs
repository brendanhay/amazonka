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
-- Module      : Network.AWS.Braket.SearchQuantumTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for tasks that match the specified filter values.
--
-- This operation returns paginated results.
module Network.AWS.Braket.SearchQuantumTasks
  ( -- * Creating a Request
    SearchQuantumTasks (..),
    newSearchQuantumTasks,

    -- * Request Lenses
    searchQuantumTasks_nextToken,
    searchQuantumTasks_maxResults,
    searchQuantumTasks_filters,

    -- * Destructuring the Response
    SearchQuantumTasksResponse (..),
    newSearchQuantumTasksResponse,

    -- * Response Lenses
    searchQuantumTasksResponse_nextToken,
    searchQuantumTasksResponse_httpStatus,
    searchQuantumTasksResponse_quantumTasks,
  )
where

import Network.AWS.Braket.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchQuantumTasks' smart constructor.
data SearchQuantumTasks = SearchQuantumTasks'
  { -- | A token used for pagination of results returned in the response. Use the
    -- token returned from the previous request continue results where the
    -- previous request ended.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Array of @SearchQuantumTasksFilter@ objects.
    filters :: [SearchQuantumTasksFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchQuantumTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchQuantumTasks_nextToken' - A token used for pagination of results returned in the response. Use the
-- token returned from the previous request continue results where the
-- previous request ended.
--
-- 'maxResults', 'searchQuantumTasks_maxResults' - Maximum number of results to return in the response.
--
-- 'filters', 'searchQuantumTasks_filters' - Array of @SearchQuantumTasksFilter@ objects.
newSearchQuantumTasks ::
  SearchQuantumTasks
newSearchQuantumTasks =
  SearchQuantumTasks'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.mempty
    }

-- | A token used for pagination of results returned in the response. Use the
-- token returned from the previous request continue results where the
-- previous request ended.
searchQuantumTasks_nextToken :: Lens.Lens' SearchQuantumTasks (Prelude.Maybe Prelude.Text)
searchQuantumTasks_nextToken = Lens.lens (\SearchQuantumTasks' {nextToken} -> nextToken) (\s@SearchQuantumTasks' {} a -> s {nextToken = a} :: SearchQuantumTasks)

-- | Maximum number of results to return in the response.
searchQuantumTasks_maxResults :: Lens.Lens' SearchQuantumTasks (Prelude.Maybe Prelude.Natural)
searchQuantumTasks_maxResults = Lens.lens (\SearchQuantumTasks' {maxResults} -> maxResults) (\s@SearchQuantumTasks' {} a -> s {maxResults = a} :: SearchQuantumTasks)

-- | Array of @SearchQuantumTasksFilter@ objects.
searchQuantumTasks_filters :: Lens.Lens' SearchQuantumTasks [SearchQuantumTasksFilter]
searchQuantumTasks_filters = Lens.lens (\SearchQuantumTasks' {filters} -> filters) (\s@SearchQuantumTasks' {} a -> s {filters = a} :: SearchQuantumTasks) Prelude.. Lens.coerced

instance Core.AWSPager SearchQuantumTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchQuantumTasksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. searchQuantumTasksResponse_quantumTasks) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchQuantumTasks_nextToken
          Lens..~ rs
          Lens.^? searchQuantumTasksResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchQuantumTasks where
  type
    AWSResponse SearchQuantumTasks =
      SearchQuantumTasksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchQuantumTasksResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "quantumTasks" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable SearchQuantumTasks

instance Prelude.NFData SearchQuantumTasks

instance Core.ToHeaders SearchQuantumTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchQuantumTasks where
  toJSON SearchQuantumTasks' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("filters" Core..= filters)
          ]
      )

instance Core.ToPath SearchQuantumTasks where
  toPath = Prelude.const "/quantum-tasks"

instance Core.ToQuery SearchQuantumTasks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchQuantumTasksResponse' smart constructor.
data SearchQuantumTasksResponse = SearchQuantumTasksResponse'
  { -- | A token used for pagination of results, or null if there are no
    -- additional results. Use the token value in a subsequent request to
    -- continue results where the previous request ended.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of @QuantumTaskSummary@ objects for tasks that match the
    -- specified filters.
    quantumTasks :: [QuantumTaskSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchQuantumTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchQuantumTasksResponse_nextToken' - A token used for pagination of results, or null if there are no
-- additional results. Use the token value in a subsequent request to
-- continue results where the previous request ended.
--
-- 'httpStatus', 'searchQuantumTasksResponse_httpStatus' - The response's http status code.
--
-- 'quantumTasks', 'searchQuantumTasksResponse_quantumTasks' - An array of @QuantumTaskSummary@ objects for tasks that match the
-- specified filters.
newSearchQuantumTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchQuantumTasksResponse
newSearchQuantumTasksResponse pHttpStatus_ =
  SearchQuantumTasksResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      quantumTasks = Prelude.mempty
    }

-- | A token used for pagination of results, or null if there are no
-- additional results. Use the token value in a subsequent request to
-- continue results where the previous request ended.
searchQuantumTasksResponse_nextToken :: Lens.Lens' SearchQuantumTasksResponse (Prelude.Maybe Prelude.Text)
searchQuantumTasksResponse_nextToken = Lens.lens (\SearchQuantumTasksResponse' {nextToken} -> nextToken) (\s@SearchQuantumTasksResponse' {} a -> s {nextToken = a} :: SearchQuantumTasksResponse)

-- | The response's http status code.
searchQuantumTasksResponse_httpStatus :: Lens.Lens' SearchQuantumTasksResponse Prelude.Int
searchQuantumTasksResponse_httpStatus = Lens.lens (\SearchQuantumTasksResponse' {httpStatus} -> httpStatus) (\s@SearchQuantumTasksResponse' {} a -> s {httpStatus = a} :: SearchQuantumTasksResponse)

-- | An array of @QuantumTaskSummary@ objects for tasks that match the
-- specified filters.
searchQuantumTasksResponse_quantumTasks :: Lens.Lens' SearchQuantumTasksResponse [QuantumTaskSummary]
searchQuantumTasksResponse_quantumTasks = Lens.lens (\SearchQuantumTasksResponse' {quantumTasks} -> quantumTasks) (\s@SearchQuantumTasksResponse' {} a -> s {quantumTasks = a} :: SearchQuantumTasksResponse) Prelude.. Lens.coerced

instance Prelude.NFData SearchQuantumTasksResponse
