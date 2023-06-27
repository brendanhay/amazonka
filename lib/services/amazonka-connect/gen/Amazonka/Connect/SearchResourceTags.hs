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
-- Module      : Amazonka.Connect.SearchResourceTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches tags used in an Amazon Connect instance using optional search
-- criteria.
--
-- This operation returns paginated results.
module Amazonka.Connect.SearchResourceTags
  ( -- * Creating a Request
    SearchResourceTags (..),
    newSearchResourceTags,

    -- * Request Lenses
    searchResourceTags_maxResults,
    searchResourceTags_nextToken,
    searchResourceTags_resourceTypes,
    searchResourceTags_searchCriteria,
    searchResourceTags_instanceId,

    -- * Destructuring the Response
    SearchResourceTagsResponse (..),
    newSearchResourceTagsResponse,

    -- * Response Lenses
    searchResourceTagsResponse_nextToken,
    searchResourceTagsResponse_tags,
    searchResourceTagsResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchResourceTags' smart constructor.
data SearchResourceTags = SearchResourceTags'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of resource types to be used to search tags from. If not
    -- provided or if any empty list is provided, this API will search from all
    -- supported resource types.
    resourceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The search criteria to be used to return tags.
    searchCriteria :: Prelude.Maybe ResourceTagsSearchCriteria,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResourceTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchResourceTags_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'searchResourceTags_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'resourceTypes', 'searchResourceTags_resourceTypes' - The list of resource types to be used to search tags from. If not
-- provided or if any empty list is provided, this API will search from all
-- supported resource types.
--
-- 'searchCriteria', 'searchResourceTags_searchCriteria' - The search criteria to be used to return tags.
--
-- 'instanceId', 'searchResourceTags_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the Amazon Resource Name (ARN) of the instance.
newSearchResourceTags ::
  -- | 'instanceId'
  Prelude.Text ->
  SearchResourceTags
newSearchResourceTags pInstanceId_ =
  SearchResourceTags'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      searchCriteria = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The maximum number of results to return per page.
searchResourceTags_maxResults :: Lens.Lens' SearchResourceTags (Prelude.Maybe Prelude.Natural)
searchResourceTags_maxResults = Lens.lens (\SearchResourceTags' {maxResults} -> maxResults) (\s@SearchResourceTags' {} a -> s {maxResults = a} :: SearchResourceTags)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
searchResourceTags_nextToken :: Lens.Lens' SearchResourceTags (Prelude.Maybe Prelude.Text)
searchResourceTags_nextToken = Lens.lens (\SearchResourceTags' {nextToken} -> nextToken) (\s@SearchResourceTags' {} a -> s {nextToken = a} :: SearchResourceTags)

-- | The list of resource types to be used to search tags from. If not
-- provided or if any empty list is provided, this API will search from all
-- supported resource types.
searchResourceTags_resourceTypes :: Lens.Lens' SearchResourceTags (Prelude.Maybe [Prelude.Text])
searchResourceTags_resourceTypes = Lens.lens (\SearchResourceTags' {resourceTypes} -> resourceTypes) (\s@SearchResourceTags' {} a -> s {resourceTypes = a} :: SearchResourceTags) Prelude.. Lens.mapping Lens.coerced

-- | The search criteria to be used to return tags.
searchResourceTags_searchCriteria :: Lens.Lens' SearchResourceTags (Prelude.Maybe ResourceTagsSearchCriteria)
searchResourceTags_searchCriteria = Lens.lens (\SearchResourceTags' {searchCriteria} -> searchCriteria) (\s@SearchResourceTags' {} a -> s {searchCriteria = a} :: SearchResourceTags)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the Amazon Resource Name (ARN) of the instance.
searchResourceTags_instanceId :: Lens.Lens' SearchResourceTags Prelude.Text
searchResourceTags_instanceId = Lens.lens (\SearchResourceTags' {instanceId} -> instanceId) (\s@SearchResourceTags' {} a -> s {instanceId = a} :: SearchResourceTags)

instance Core.AWSPager SearchResourceTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchResourceTagsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchResourceTagsResponse_tags
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchResourceTags_nextToken
          Lens..~ rs
          Lens.^? searchResourceTagsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchResourceTags where
  type
    AWSResponse SearchResourceTags =
      SearchResourceTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchResourceTagsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchResourceTags where
  hashWithSalt _salt SearchResourceTags' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceTypes
      `Prelude.hashWithSalt` searchCriteria
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData SearchResourceTags where
  rnf SearchResourceTags' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf searchCriteria
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders SearchResourceTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchResourceTags where
  toJSON SearchResourceTags' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ResourceTypes" Data..=) Prelude.<$> resourceTypes,
            ("SearchCriteria" Data..=)
              Prelude.<$> searchCriteria,
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath SearchResourceTags where
  toPath = Prelude.const "/search-resource-tags"

instance Data.ToQuery SearchResourceTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchResourceTagsResponse' smart constructor.
data SearchResourceTagsResponse = SearchResourceTagsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of tags used in the Amazon Connect instance.
    tags :: Prelude.Maybe [TagSet],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResourceTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchResourceTagsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'tags', 'searchResourceTagsResponse_tags' - A list of tags used in the Amazon Connect instance.
--
-- 'httpStatus', 'searchResourceTagsResponse_httpStatus' - The response's http status code.
newSearchResourceTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchResourceTagsResponse
newSearchResourceTagsResponse pHttpStatus_ =
  SearchResourceTagsResponse'
    { nextToken =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
searchResourceTagsResponse_nextToken :: Lens.Lens' SearchResourceTagsResponse (Prelude.Maybe Prelude.Text)
searchResourceTagsResponse_nextToken = Lens.lens (\SearchResourceTagsResponse' {nextToken} -> nextToken) (\s@SearchResourceTagsResponse' {} a -> s {nextToken = a} :: SearchResourceTagsResponse)

-- | A list of tags used in the Amazon Connect instance.
searchResourceTagsResponse_tags :: Lens.Lens' SearchResourceTagsResponse (Prelude.Maybe [TagSet])
searchResourceTagsResponse_tags = Lens.lens (\SearchResourceTagsResponse' {tags} -> tags) (\s@SearchResourceTagsResponse' {} a -> s {tags = a} :: SearchResourceTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchResourceTagsResponse_httpStatus :: Lens.Lens' SearchResourceTagsResponse Prelude.Int
searchResourceTagsResponse_httpStatus = Lens.lens (\SearchResourceTagsResponse' {httpStatus} -> httpStatus) (\s@SearchResourceTagsResponse' {} a -> s {httpStatus = a} :: SearchResourceTagsResponse)

instance Prelude.NFData SearchResourceTagsResponse where
  rnf SearchResourceTagsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
