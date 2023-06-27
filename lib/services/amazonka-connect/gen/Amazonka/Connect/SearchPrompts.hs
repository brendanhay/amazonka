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
-- Module      : Amazonka.Connect.SearchPrompts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches prompts in an Amazon Connect instance, with optional filtering.
--
-- This operation returns paginated results.
module Amazonka.Connect.SearchPrompts
  ( -- * Creating a Request
    SearchPrompts (..),
    newSearchPrompts,

    -- * Request Lenses
    searchPrompts_maxResults,
    searchPrompts_nextToken,
    searchPrompts_searchCriteria,
    searchPrompts_searchFilter,
    searchPrompts_instanceId,

    -- * Destructuring the Response
    SearchPromptsResponse (..),
    newSearchPromptsResponse,

    -- * Response Lenses
    searchPromptsResponse_approximateTotalCount,
    searchPromptsResponse_nextToken,
    searchPromptsResponse_prompts,
    searchPromptsResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchPrompts' smart constructor.
data SearchPrompts = SearchPrompts'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The search criteria to be used to return prompts.
    searchCriteria :: Prelude.Maybe PromptSearchCriteria,
    -- | Filters to be applied to search results.
    searchFilter :: Prelude.Maybe PromptSearchFilter,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchPrompts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchPrompts_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'searchPrompts_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'searchCriteria', 'searchPrompts_searchCriteria' - The search criteria to be used to return prompts.
--
-- 'searchFilter', 'searchPrompts_searchFilter' - Filters to be applied to search results.
--
-- 'instanceId', 'searchPrompts_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
newSearchPrompts ::
  -- | 'instanceId'
  Prelude.Text ->
  SearchPrompts
newSearchPrompts pInstanceId_ =
  SearchPrompts'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      searchCriteria = Prelude.Nothing,
      searchFilter = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The maximum number of results to return per page.
searchPrompts_maxResults :: Lens.Lens' SearchPrompts (Prelude.Maybe Prelude.Natural)
searchPrompts_maxResults = Lens.lens (\SearchPrompts' {maxResults} -> maxResults) (\s@SearchPrompts' {} a -> s {maxResults = a} :: SearchPrompts)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
searchPrompts_nextToken :: Lens.Lens' SearchPrompts (Prelude.Maybe Prelude.Text)
searchPrompts_nextToken = Lens.lens (\SearchPrompts' {nextToken} -> nextToken) (\s@SearchPrompts' {} a -> s {nextToken = a} :: SearchPrompts)

-- | The search criteria to be used to return prompts.
searchPrompts_searchCriteria :: Lens.Lens' SearchPrompts (Prelude.Maybe PromptSearchCriteria)
searchPrompts_searchCriteria = Lens.lens (\SearchPrompts' {searchCriteria} -> searchCriteria) (\s@SearchPrompts' {} a -> s {searchCriteria = a} :: SearchPrompts)

-- | Filters to be applied to search results.
searchPrompts_searchFilter :: Lens.Lens' SearchPrompts (Prelude.Maybe PromptSearchFilter)
searchPrompts_searchFilter = Lens.lens (\SearchPrompts' {searchFilter} -> searchFilter) (\s@SearchPrompts' {} a -> s {searchFilter = a} :: SearchPrompts)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
searchPrompts_instanceId :: Lens.Lens' SearchPrompts Prelude.Text
searchPrompts_instanceId = Lens.lens (\SearchPrompts' {instanceId} -> instanceId) (\s@SearchPrompts' {} a -> s {instanceId = a} :: SearchPrompts)

instance Core.AWSPager SearchPrompts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchPromptsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchPromptsResponse_prompts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchPrompts_nextToken
          Lens..~ rs
          Lens.^? searchPromptsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchPrompts where
  type
    AWSResponse SearchPrompts =
      SearchPromptsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchPromptsResponse'
            Prelude.<$> (x Data..?> "ApproximateTotalCount")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Prompts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchPrompts where
  hashWithSalt _salt SearchPrompts' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` searchCriteria
      `Prelude.hashWithSalt` searchFilter
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData SearchPrompts where
  rnf SearchPrompts' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf searchCriteria
      `Prelude.seq` Prelude.rnf searchFilter
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders SearchPrompts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchPrompts where
  toJSON SearchPrompts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SearchCriteria" Data..=)
              Prelude.<$> searchCriteria,
            ("SearchFilter" Data..=) Prelude.<$> searchFilter,
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath SearchPrompts where
  toPath = Prelude.const "/search-prompts"

instance Data.ToQuery SearchPrompts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchPromptsResponse' smart constructor.
data SearchPromptsResponse = SearchPromptsResponse'
  { -- | The total number of quick connects which matched your search query.
    approximateTotalCount :: Prelude.Maybe Prelude.Integer,
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the prompts.
    prompts :: Prelude.Maybe [Prompt],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchPromptsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approximateTotalCount', 'searchPromptsResponse_approximateTotalCount' - The total number of quick connects which matched your search query.
--
-- 'nextToken', 'searchPromptsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'prompts', 'searchPromptsResponse_prompts' - Information about the prompts.
--
-- 'httpStatus', 'searchPromptsResponse_httpStatus' - The response's http status code.
newSearchPromptsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchPromptsResponse
newSearchPromptsResponse pHttpStatus_ =
  SearchPromptsResponse'
    { approximateTotalCount =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      prompts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total number of quick connects which matched your search query.
searchPromptsResponse_approximateTotalCount :: Lens.Lens' SearchPromptsResponse (Prelude.Maybe Prelude.Integer)
searchPromptsResponse_approximateTotalCount = Lens.lens (\SearchPromptsResponse' {approximateTotalCount} -> approximateTotalCount) (\s@SearchPromptsResponse' {} a -> s {approximateTotalCount = a} :: SearchPromptsResponse)

-- | If there are additional results, this is the token for the next set of
-- results.
searchPromptsResponse_nextToken :: Lens.Lens' SearchPromptsResponse (Prelude.Maybe Prelude.Text)
searchPromptsResponse_nextToken = Lens.lens (\SearchPromptsResponse' {nextToken} -> nextToken) (\s@SearchPromptsResponse' {} a -> s {nextToken = a} :: SearchPromptsResponse)

-- | Information about the prompts.
searchPromptsResponse_prompts :: Lens.Lens' SearchPromptsResponse (Prelude.Maybe [Prompt])
searchPromptsResponse_prompts = Lens.lens (\SearchPromptsResponse' {prompts} -> prompts) (\s@SearchPromptsResponse' {} a -> s {prompts = a} :: SearchPromptsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchPromptsResponse_httpStatus :: Lens.Lens' SearchPromptsResponse Prelude.Int
searchPromptsResponse_httpStatus = Lens.lens (\SearchPromptsResponse' {httpStatus} -> httpStatus) (\s@SearchPromptsResponse' {} a -> s {httpStatus = a} :: SearchPromptsResponse)

instance Prelude.NFData SearchPromptsResponse where
  rnf SearchPromptsResponse' {..} =
    Prelude.rnf approximateTotalCount
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf prompts
      `Prelude.seq` Prelude.rnf httpStatus
