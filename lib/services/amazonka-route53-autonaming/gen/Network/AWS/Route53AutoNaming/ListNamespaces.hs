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
-- current account.
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListNamespaces
  ( -- * Creating a Request
    ListNamespaces (..),
    newListNamespaces,

    -- * Request Lenses
    listNamespaces_filters,
    listNamespaces_nextToken,
    listNamespaces_maxResults,

    -- * Destructuring the Response
    ListNamespacesResponse (..),
    newListNamespacesResponse,

    -- * Response Lenses
    listNamespacesResponse_namespaces,
    listNamespacesResponse_nextToken,
    listNamespacesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newListNamespaces' smart constructor.
data ListNamespaces = ListNamespaces'
  { -- | A complex type that contains specifications for the namespaces that you
    -- want to list.
    --
    -- If you specify more than one filter, a namespace must match all filters
    -- to be returned by @ListNamespaces@.
    filters :: Prelude.Maybe [NamespaceFilter],
    -- | For the first @ListNamespaces@ request, omit this value.
    --
    -- If the response contains @NextToken@, submit another @ListNamespaces@
    -- request to get the next group of results. Specify the value of
    -- @NextToken@ from the previous response in the next request.
    --
    -- Cloud Map gets @MaxResults@ namespaces and then filters them based on
    -- the specified criteria. It\'s possible that no namespaces in the first
    -- @MaxResults@ namespaces matched the specified criteria but that
    -- subsequent groups of @MaxResults@ namespaces do contain namespaces that
    -- match the criteria.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of namespaces that you want Cloud Map to return in
    -- the response to a @ListNamespaces@ request. If you don\'t specify a
    -- value for @MaxResults@, Cloud Map returns up to 100 namespaces.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNamespaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listNamespaces_filters' - A complex type that contains specifications for the namespaces that you
-- want to list.
--
-- If you specify more than one filter, a namespace must match all filters
-- to be returned by @ListNamespaces@.
--
-- 'nextToken', 'listNamespaces_nextToken' - For the first @ListNamespaces@ request, omit this value.
--
-- If the response contains @NextToken@, submit another @ListNamespaces@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- Cloud Map gets @MaxResults@ namespaces and then filters them based on
-- the specified criteria. It\'s possible that no namespaces in the first
-- @MaxResults@ namespaces matched the specified criteria but that
-- subsequent groups of @MaxResults@ namespaces do contain namespaces that
-- match the criteria.
--
-- 'maxResults', 'listNamespaces_maxResults' - The maximum number of namespaces that you want Cloud Map to return in
-- the response to a @ListNamespaces@ request. If you don\'t specify a
-- value for @MaxResults@, Cloud Map returns up to 100 namespaces.
newListNamespaces ::
  ListNamespaces
newListNamespaces =
  ListNamespaces'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A complex type that contains specifications for the namespaces that you
-- want to list.
--
-- If you specify more than one filter, a namespace must match all filters
-- to be returned by @ListNamespaces@.
listNamespaces_filters :: Lens.Lens' ListNamespaces (Prelude.Maybe [NamespaceFilter])
listNamespaces_filters = Lens.lens (\ListNamespaces' {filters} -> filters) (\s@ListNamespaces' {} a -> s {filters = a} :: ListNamespaces) Prelude.. Lens.mapping Lens.coerced

-- | For the first @ListNamespaces@ request, omit this value.
--
-- If the response contains @NextToken@, submit another @ListNamespaces@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- Cloud Map gets @MaxResults@ namespaces and then filters them based on
-- the specified criteria. It\'s possible that no namespaces in the first
-- @MaxResults@ namespaces matched the specified criteria but that
-- subsequent groups of @MaxResults@ namespaces do contain namespaces that
-- match the criteria.
listNamespaces_nextToken :: Lens.Lens' ListNamespaces (Prelude.Maybe Prelude.Text)
listNamespaces_nextToken = Lens.lens (\ListNamespaces' {nextToken} -> nextToken) (\s@ListNamespaces' {} a -> s {nextToken = a} :: ListNamespaces)

-- | The maximum number of namespaces that you want Cloud Map to return in
-- the response to a @ListNamespaces@ request. If you don\'t specify a
-- value for @MaxResults@, Cloud Map returns up to 100 namespaces.
listNamespaces_maxResults :: Lens.Lens' ListNamespaces (Prelude.Maybe Prelude.Natural)
listNamespaces_maxResults = Lens.lens (\ListNamespaces' {maxResults} -> maxResults) (\s@ListNamespaces' {} a -> s {maxResults = a} :: ListNamespaces)

instance Core.AWSPager ListNamespaces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNamespacesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listNamespacesResponse_namespaces
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listNamespaces_nextToken
          Lens..~ rs
          Lens.^? listNamespacesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListNamespaces where
  type
    AWSResponse ListNamespaces =
      ListNamespacesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNamespacesResponse'
            Prelude.<$> (x Core..?> "Namespaces" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNamespaces

instance Prelude.NFData ListNamespaces

instance Core.ToHeaders ListNamespaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.ListNamespaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListNamespaces where
  toJSON ListNamespaces' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Filters" Core..=) Prelude.<$> filters,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListNamespaces where
  toPath = Prelude.const "/"

instance Core.ToQuery ListNamespaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListNamespacesResponse' smart constructor.
data ListNamespacesResponse = ListNamespacesResponse'
  { -- | An array that contains one @NamespaceSummary@ object for each namespace
    -- that matches the specified filter criteria.
    namespaces :: Prelude.Maybe [NamespaceSummary],
    -- | If the response contains @NextToken@, submit another @ListNamespaces@
    -- request to get the next group of results. Specify the value of
    -- @NextToken@ from the previous response in the next request.
    --
    -- Cloud Map gets @MaxResults@ namespaces and then filters them based on
    -- the specified criteria. It\'s possible that no namespaces in the first
    -- @MaxResults@ namespaces matched the specified criteria but that
    -- subsequent groups of @MaxResults@ namespaces do contain namespaces that
    -- match the criteria.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNamespacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaces', 'listNamespacesResponse_namespaces' - An array that contains one @NamespaceSummary@ object for each namespace
-- that matches the specified filter criteria.
--
-- 'nextToken', 'listNamespacesResponse_nextToken' - If the response contains @NextToken@, submit another @ListNamespaces@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- Cloud Map gets @MaxResults@ namespaces and then filters them based on
-- the specified criteria. It\'s possible that no namespaces in the first
-- @MaxResults@ namespaces matched the specified criteria but that
-- subsequent groups of @MaxResults@ namespaces do contain namespaces that
-- match the criteria.
--
-- 'httpStatus', 'listNamespacesResponse_httpStatus' - The response's http status code.
newListNamespacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNamespacesResponse
newListNamespacesResponse pHttpStatus_ =
  ListNamespacesResponse'
    { namespaces =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array that contains one @NamespaceSummary@ object for each namespace
-- that matches the specified filter criteria.
listNamespacesResponse_namespaces :: Lens.Lens' ListNamespacesResponse (Prelude.Maybe [NamespaceSummary])
listNamespacesResponse_namespaces = Lens.lens (\ListNamespacesResponse' {namespaces} -> namespaces) (\s@ListNamespacesResponse' {} a -> s {namespaces = a} :: ListNamespacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response contains @NextToken@, submit another @ListNamespaces@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- Cloud Map gets @MaxResults@ namespaces and then filters them based on
-- the specified criteria. It\'s possible that no namespaces in the first
-- @MaxResults@ namespaces matched the specified criteria but that
-- subsequent groups of @MaxResults@ namespaces do contain namespaces that
-- match the criteria.
listNamespacesResponse_nextToken :: Lens.Lens' ListNamespacesResponse (Prelude.Maybe Prelude.Text)
listNamespacesResponse_nextToken = Lens.lens (\ListNamespacesResponse' {nextToken} -> nextToken) (\s@ListNamespacesResponse' {} a -> s {nextToken = a} :: ListNamespacesResponse)

-- | The response's http status code.
listNamespacesResponse_httpStatus :: Lens.Lens' ListNamespacesResponse Prelude.Int
listNamespacesResponse_httpStatus = Lens.lens (\ListNamespacesResponse' {httpStatus} -> httpStatus) (\s@ListNamespacesResponse' {} a -> s {httpStatus = a} :: ListNamespacesResponse)

instance Prelude.NFData ListNamespacesResponse
