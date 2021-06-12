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
-- Module      : Network.AWS.Route53AutoNaming.ListNamespaces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information about the namespaces that were created by the
-- current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListNamespaces
  ( -- * Creating a Request
    ListNamespaces (..),
    newListNamespaces,

    -- * Request Lenses
    listNamespaces_nextToken,
    listNamespaces_maxResults,
    listNamespaces_filters,

    -- * Destructuring the Response
    ListNamespacesResponse (..),
    newListNamespacesResponse,

    -- * Response Lenses
    listNamespacesResponse_nextToken,
    listNamespacesResponse_namespaces,
    listNamespacesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newListNamespaces' smart constructor.
data ListNamespaces = ListNamespaces'
  { -- | For the first @ListNamespaces@ request, omit this value.
    --
    -- If the response contains @NextToken@, submit another @ListNamespaces@
    -- request to get the next group of results. Specify the value of
    -- @NextToken@ from the previous response in the next request.
    --
    -- AWS Cloud Map gets @MaxResults@ namespaces and then filters them based
    -- on the specified criteria. It\'s possible that no namespaces in the
    -- first @MaxResults@ namespaces matched the specified criteria but that
    -- subsequent groups of @MaxResults@ namespaces do contain namespaces that
    -- match the criteria.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of namespaces that you want AWS Cloud Map to return
    -- in the response to a @ListNamespaces@ request. If you don\'t specify a
    -- value for @MaxResults@, AWS Cloud Map returns up to 100 namespaces.
    maxResults :: Core.Maybe Core.Natural,
    -- | A complex type that contains specifications for the namespaces that you
    -- want to list.
    --
    -- If you specify more than one filter, a namespace must match all filters
    -- to be returned by @ListNamespaces@.
    filters :: Core.Maybe [NamespaceFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListNamespaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNamespaces_nextToken' - For the first @ListNamespaces@ request, omit this value.
--
-- If the response contains @NextToken@, submit another @ListNamespaces@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- AWS Cloud Map gets @MaxResults@ namespaces and then filters them based
-- on the specified criteria. It\'s possible that no namespaces in the
-- first @MaxResults@ namespaces matched the specified criteria but that
-- subsequent groups of @MaxResults@ namespaces do contain namespaces that
-- match the criteria.
--
-- 'maxResults', 'listNamespaces_maxResults' - The maximum number of namespaces that you want AWS Cloud Map to return
-- in the response to a @ListNamespaces@ request. If you don\'t specify a
-- value for @MaxResults@, AWS Cloud Map returns up to 100 namespaces.
--
-- 'filters', 'listNamespaces_filters' - A complex type that contains specifications for the namespaces that you
-- want to list.
--
-- If you specify more than one filter, a namespace must match all filters
-- to be returned by @ListNamespaces@.
newListNamespaces ::
  ListNamespaces
newListNamespaces =
  ListNamespaces'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | For the first @ListNamespaces@ request, omit this value.
--
-- If the response contains @NextToken@, submit another @ListNamespaces@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- AWS Cloud Map gets @MaxResults@ namespaces and then filters them based
-- on the specified criteria. It\'s possible that no namespaces in the
-- first @MaxResults@ namespaces matched the specified criteria but that
-- subsequent groups of @MaxResults@ namespaces do contain namespaces that
-- match the criteria.
listNamespaces_nextToken :: Lens.Lens' ListNamespaces (Core.Maybe Core.Text)
listNamespaces_nextToken = Lens.lens (\ListNamespaces' {nextToken} -> nextToken) (\s@ListNamespaces' {} a -> s {nextToken = a} :: ListNamespaces)

-- | The maximum number of namespaces that you want AWS Cloud Map to return
-- in the response to a @ListNamespaces@ request. If you don\'t specify a
-- value for @MaxResults@, AWS Cloud Map returns up to 100 namespaces.
listNamespaces_maxResults :: Lens.Lens' ListNamespaces (Core.Maybe Core.Natural)
listNamespaces_maxResults = Lens.lens (\ListNamespaces' {maxResults} -> maxResults) (\s@ListNamespaces' {} a -> s {maxResults = a} :: ListNamespaces)

-- | A complex type that contains specifications for the namespaces that you
-- want to list.
--
-- If you specify more than one filter, a namespace must match all filters
-- to be returned by @ListNamespaces@.
listNamespaces_filters :: Lens.Lens' ListNamespaces (Core.Maybe [NamespaceFilter])
listNamespaces_filters = Lens.lens (\ListNamespaces' {filters} -> filters) (\s@ListNamespaces' {} a -> s {filters = a} :: ListNamespaces) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListNamespaces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNamespacesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listNamespacesResponse_namespaces Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listNamespaces_nextToken
          Lens..~ rs
          Lens.^? listNamespacesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListNamespaces where
  type
    AWSResponse ListNamespaces =
      ListNamespacesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNamespacesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Namespaces" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListNamespaces

instance Core.NFData ListNamespaces

instance Core.ToHeaders ListNamespaces where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.ListNamespaces" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListNamespaces where
  toJSON ListNamespaces' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath ListNamespaces where
  toPath = Core.const "/"

instance Core.ToQuery ListNamespaces where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListNamespacesResponse' smart constructor.
data ListNamespacesResponse = ListNamespacesResponse'
  { -- | If the response contains @NextToken@, submit another @ListNamespaces@
    -- request to get the next group of results. Specify the value of
    -- @NextToken@ from the previous response in the next request.
    --
    -- AWS Cloud Map gets @MaxResults@ namespaces and then filters them based
    -- on the specified criteria. It\'s possible that no namespaces in the
    -- first @MaxResults@ namespaces matched the specified criteria but that
    -- subsequent groups of @MaxResults@ namespaces do contain namespaces that
    -- match the criteria.
    nextToken :: Core.Maybe Core.Text,
    -- | An array that contains one @NamespaceSummary@ object for each namespace
    -- that matches the specified filter criteria.
    namespaces :: Core.Maybe [NamespaceSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListNamespacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNamespacesResponse_nextToken' - If the response contains @NextToken@, submit another @ListNamespaces@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- AWS Cloud Map gets @MaxResults@ namespaces and then filters them based
-- on the specified criteria. It\'s possible that no namespaces in the
-- first @MaxResults@ namespaces matched the specified criteria but that
-- subsequent groups of @MaxResults@ namespaces do contain namespaces that
-- match the criteria.
--
-- 'namespaces', 'listNamespacesResponse_namespaces' - An array that contains one @NamespaceSummary@ object for each namespace
-- that matches the specified filter criteria.
--
-- 'httpStatus', 'listNamespacesResponse_httpStatus' - The response's http status code.
newListNamespacesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListNamespacesResponse
newListNamespacesResponse pHttpStatus_ =
  ListNamespacesResponse'
    { nextToken = Core.Nothing,
      namespaces = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response contains @NextToken@, submit another @ListNamespaces@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- AWS Cloud Map gets @MaxResults@ namespaces and then filters them based
-- on the specified criteria. It\'s possible that no namespaces in the
-- first @MaxResults@ namespaces matched the specified criteria but that
-- subsequent groups of @MaxResults@ namespaces do contain namespaces that
-- match the criteria.
listNamespacesResponse_nextToken :: Lens.Lens' ListNamespacesResponse (Core.Maybe Core.Text)
listNamespacesResponse_nextToken = Lens.lens (\ListNamespacesResponse' {nextToken} -> nextToken) (\s@ListNamespacesResponse' {} a -> s {nextToken = a} :: ListNamespacesResponse)

-- | An array that contains one @NamespaceSummary@ object for each namespace
-- that matches the specified filter criteria.
listNamespacesResponse_namespaces :: Lens.Lens' ListNamespacesResponse (Core.Maybe [NamespaceSummary])
listNamespacesResponse_namespaces = Lens.lens (\ListNamespacesResponse' {namespaces} -> namespaces) (\s@ListNamespacesResponse' {} a -> s {namespaces = a} :: ListNamespacesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listNamespacesResponse_httpStatus :: Lens.Lens' ListNamespacesResponse Core.Int
listNamespacesResponse_httpStatus = Lens.lens (\ListNamespacesResponse' {httpStatus} -> httpStatus) (\s@ListNamespacesResponse' {} a -> s {httpStatus = a} :: ListNamespacesResponse)

instance Core.NFData ListNamespacesResponse
