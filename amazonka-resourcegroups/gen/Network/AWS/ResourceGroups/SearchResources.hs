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
-- Module      : Network.AWS.ResourceGroups.SearchResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of AWS resource identifiers that matches the specified
-- query. The query uses the same format as a resource query in a
-- CreateGroup or UpdateGroupQuery operation.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:SearchResources@
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.SearchResources
  ( -- * Creating a Request
    SearchResources (..),
    newSearchResources,

    -- * Request Lenses
    searchResources_nextToken,
    searchResources_maxResults,
    searchResources_resourceQuery,

    -- * Destructuring the Response
    SearchResourcesResponse (..),
    newSearchResourcesResponse,

    -- * Response Lenses
    searchResourcesResponse_nextToken,
    searchResourcesResponse_resourceIdentifiers,
    searchResourcesResponse_queryErrors,
    searchResourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchResources' smart constructor.
data SearchResources = SearchResources'
  { -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- provided by a previous call\'s @NextToken@ response to indicate where
    -- the output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- that is specific to the operation. If additional items exist beyond the
    -- maximum you specify, the @NextToken@ response element is present and has
    -- a value (is not null). Include that value as the @NextToken@ request
    -- parameter in the next call to the operation to get the next part of the
    -- results. Note that the service might return fewer results than the
    -- maximum even when there are more results available. You should check
    -- @NextToken@ after every operation to ensure that you receive all of the
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The search query, using the same formats that are supported for resource
    -- group definition. For more information, see CreateGroup.
    resourceQuery :: ResourceQuery
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchResources_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- provided by a previous call\'s @NextToken@ response to indicate where
-- the output should continue from.
--
-- 'maxResults', 'searchResources_maxResults' - The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that the service might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
--
-- 'resourceQuery', 'searchResources_resourceQuery' - The search query, using the same formats that are supported for resource
-- group definition. For more information, see CreateGroup.
newSearchResources ::
  -- | 'resourceQuery'
  ResourceQuery ->
  SearchResources
newSearchResources pResourceQuery_ =
  SearchResources'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceQuery = pResourceQuery_
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- provided by a previous call\'s @NextToken@ response to indicate where
-- the output should continue from.
searchResources_nextToken :: Lens.Lens' SearchResources (Prelude.Maybe Prelude.Text)
searchResources_nextToken = Lens.lens (\SearchResources' {nextToken} -> nextToken) (\s@SearchResources' {} a -> s {nextToken = a} :: SearchResources)

-- | The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that the service might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
searchResources_maxResults :: Lens.Lens' SearchResources (Prelude.Maybe Prelude.Natural)
searchResources_maxResults = Lens.lens (\SearchResources' {maxResults} -> maxResults) (\s@SearchResources' {} a -> s {maxResults = a} :: SearchResources)

-- | The search query, using the same formats that are supported for resource
-- group definition. For more information, see CreateGroup.
searchResources_resourceQuery :: Lens.Lens' SearchResources ResourceQuery
searchResources_resourceQuery = Lens.lens (\SearchResources' {resourceQuery} -> resourceQuery) (\s@SearchResources' {} a -> s {resourceQuery = a} :: SearchResources)

instance Core.AWSPager SearchResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchResourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchResourcesResponse_resourceIdentifiers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchResources_nextToken
          Lens..~ rs
          Lens.^? searchResourcesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchResources where
  type
    AWSResponse SearchResources =
      SearchResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchResourcesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ResourceIdentifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "QueryErrors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchResources

instance Prelude.NFData SearchResources

instance Core.ToHeaders SearchResources where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON SearchResources where
  toJSON SearchResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("ResourceQuery" Core..= resourceQuery)
          ]
      )

instance Core.ToPath SearchResources where
  toPath = Prelude.const "/resources/search"

instance Core.ToQuery SearchResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchResourcesResponse' smart constructor.
data SearchResourcesResponse = SearchResourcesResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARNs and resource types of resources that are members of the group
    -- that you specified.
    resourceIdentifiers :: Prelude.Maybe [ResourceIdentifier],
    -- | A list of @QueryError@ objects. Each error is an object that contains
    -- @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@
    -- are @CLOUDFORMATION_STACK_INACTIVE@ and
    -- @CLOUDFORMATION_STACK_NOT_EXISTING@.
    queryErrors :: Prelude.Maybe [QueryError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchResourcesResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'resourceIdentifiers', 'searchResourcesResponse_resourceIdentifiers' - The ARNs and resource types of resources that are members of the group
-- that you specified.
--
-- 'queryErrors', 'searchResourcesResponse_queryErrors' - A list of @QueryError@ objects. Each error is an object that contains
-- @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@
-- are @CLOUDFORMATION_STACK_INACTIVE@ and
-- @CLOUDFORMATION_STACK_NOT_EXISTING@.
--
-- 'httpStatus', 'searchResourcesResponse_httpStatus' - The response's http status code.
newSearchResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchResourcesResponse
newSearchResourcesResponse pHttpStatus_ =
  SearchResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      resourceIdentifiers = Prelude.Nothing,
      queryErrors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
searchResourcesResponse_nextToken :: Lens.Lens' SearchResourcesResponse (Prelude.Maybe Prelude.Text)
searchResourcesResponse_nextToken = Lens.lens (\SearchResourcesResponse' {nextToken} -> nextToken) (\s@SearchResourcesResponse' {} a -> s {nextToken = a} :: SearchResourcesResponse)

-- | The ARNs and resource types of resources that are members of the group
-- that you specified.
searchResourcesResponse_resourceIdentifiers :: Lens.Lens' SearchResourcesResponse (Prelude.Maybe [ResourceIdentifier])
searchResourcesResponse_resourceIdentifiers = Lens.lens (\SearchResourcesResponse' {resourceIdentifiers} -> resourceIdentifiers) (\s@SearchResourcesResponse' {} a -> s {resourceIdentifiers = a} :: SearchResourcesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of @QueryError@ objects. Each error is an object that contains
-- @ErrorCode@ and @Message@ structures. Possible values for @ErrorCode@
-- are @CLOUDFORMATION_STACK_INACTIVE@ and
-- @CLOUDFORMATION_STACK_NOT_EXISTING@.
searchResourcesResponse_queryErrors :: Lens.Lens' SearchResourcesResponse (Prelude.Maybe [QueryError])
searchResourcesResponse_queryErrors = Lens.lens (\SearchResourcesResponse' {queryErrors} -> queryErrors) (\s@SearchResourcesResponse' {} a -> s {queryErrors = a} :: SearchResourcesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
searchResourcesResponse_httpStatus :: Lens.Lens' SearchResourcesResponse Prelude.Int
searchResourcesResponse_httpStatus = Lens.lens (\SearchResourcesResponse' {httpStatus} -> httpStatus) (\s@SearchResourcesResponse' {} a -> s {httpStatus = a} :: SearchResourcesResponse)

instance Prelude.NFData SearchResourcesResponse
