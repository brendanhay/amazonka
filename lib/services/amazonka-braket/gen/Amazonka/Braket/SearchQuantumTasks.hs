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
-- Module      : Amazonka.Braket.SearchQuantumTasks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for tasks that match the specified filter values.
--
-- This operation returns paginated results.
module Amazonka.Braket.SearchQuantumTasks
  ( -- * Creating a Request
    SearchQuantumTasks (..),
    newSearchQuantumTasks,

    -- * Request Lenses
    searchQuantumTasks_maxResults,
    searchQuantumTasks_nextToken,
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

import Amazonka.Braket.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchQuantumTasks' smart constructor.
data SearchQuantumTasks = SearchQuantumTasks'
  { -- | Maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token used for pagination of results returned in the response. Use the
    -- token returned from the previous request continue results where the
    -- previous request ended.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'searchQuantumTasks_maxResults' - Maximum number of results to return in the response.
--
-- 'nextToken', 'searchQuantumTasks_nextToken' - A token used for pagination of results returned in the response. Use the
-- token returned from the previous request continue results where the
-- previous request ended.
--
-- 'filters', 'searchQuantumTasks_filters' - Array of @SearchQuantumTasksFilter@ objects.
newSearchQuantumTasks ::
  SearchQuantumTasks
newSearchQuantumTasks =
  SearchQuantumTasks'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filters = Prelude.mempty
    }

-- | Maximum number of results to return in the response.
searchQuantumTasks_maxResults :: Lens.Lens' SearchQuantumTasks (Prelude.Maybe Prelude.Natural)
searchQuantumTasks_maxResults = Lens.lens (\SearchQuantumTasks' {maxResults} -> maxResults) (\s@SearchQuantumTasks' {} a -> s {maxResults = a} :: SearchQuantumTasks)

-- | A token used for pagination of results returned in the response. Use the
-- token returned from the previous request continue results where the
-- previous request ended.
searchQuantumTasks_nextToken :: Lens.Lens' SearchQuantumTasks (Prelude.Maybe Prelude.Text)
searchQuantumTasks_nextToken = Lens.lens (\SearchQuantumTasks' {nextToken} -> nextToken) (\s@SearchQuantumTasks' {} a -> s {nextToken = a} :: SearchQuantumTasks)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchQuantumTasksResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "quantumTasks" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable SearchQuantumTasks where
  hashWithSalt _salt SearchQuantumTasks' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchQuantumTasks where
  rnf SearchQuantumTasks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters

instance Data.ToHeaders SearchQuantumTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchQuantumTasks where
  toJSON SearchQuantumTasks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("filters" Data..= filters)
          ]
      )

instance Data.ToPath SearchQuantumTasks where
  toPath = Prelude.const "/quantum-tasks"

instance Data.ToQuery SearchQuantumTasks where
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

instance Prelude.NFData SearchQuantumTasksResponse where
  rnf SearchQuantumTasksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf quantumTasks
