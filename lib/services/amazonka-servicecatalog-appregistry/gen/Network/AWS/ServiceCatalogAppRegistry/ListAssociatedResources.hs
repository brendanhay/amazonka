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
-- Module      : Network.AWS.ServiceCatalogAppRegistry.ListAssociatedResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all resources that are associated with specified application.
-- Results are paginated.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalogAppRegistry.ListAssociatedResources
  ( -- * Creating a Request
    ListAssociatedResources (..),
    newListAssociatedResources,

    -- * Request Lenses
    listAssociatedResources_nextToken,
    listAssociatedResources_maxResults,
    listAssociatedResources_application,

    -- * Destructuring the Response
    ListAssociatedResourcesResponse (..),
    newListAssociatedResourcesResponse,

    -- * Response Lenses
    listAssociatedResourcesResponse_resources,
    listAssociatedResourcesResponse_nextToken,
    listAssociatedResourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalogAppRegistry.Types

-- | /See:/ 'newListAssociatedResources' smart constructor.
data ListAssociatedResources = ListAssociatedResources'
  { -- | The token to use to get the next page of results after a previous API
    -- call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The upper bound of the number of results to return (cannot exceed 25).
    -- If this parameter is omitted, it defaults to 25. This value is optional.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name or ID of the application.
    application :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssociatedResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociatedResources_nextToken' - The token to use to get the next page of results after a previous API
-- call.
--
-- 'maxResults', 'listAssociatedResources_maxResults' - The upper bound of the number of results to return (cannot exceed 25).
-- If this parameter is omitted, it defaults to 25. This value is optional.
--
-- 'application', 'listAssociatedResources_application' - The name or ID of the application.
newListAssociatedResources ::
  -- | 'application'
  Prelude.Text ->
  ListAssociatedResources
newListAssociatedResources pApplication_ =
  ListAssociatedResources'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      application = pApplication_
    }

-- | The token to use to get the next page of results after a previous API
-- call.
listAssociatedResources_nextToken :: Lens.Lens' ListAssociatedResources (Prelude.Maybe Prelude.Text)
listAssociatedResources_nextToken = Lens.lens (\ListAssociatedResources' {nextToken} -> nextToken) (\s@ListAssociatedResources' {} a -> s {nextToken = a} :: ListAssociatedResources)

-- | The upper bound of the number of results to return (cannot exceed 25).
-- If this parameter is omitted, it defaults to 25. This value is optional.
listAssociatedResources_maxResults :: Lens.Lens' ListAssociatedResources (Prelude.Maybe Prelude.Natural)
listAssociatedResources_maxResults = Lens.lens (\ListAssociatedResources' {maxResults} -> maxResults) (\s@ListAssociatedResources' {} a -> s {maxResults = a} :: ListAssociatedResources)

-- | The name or ID of the application.
listAssociatedResources_application :: Lens.Lens' ListAssociatedResources Prelude.Text
listAssociatedResources_application = Lens.lens (\ListAssociatedResources' {application} -> application) (\s@ListAssociatedResources' {} a -> s {application = a} :: ListAssociatedResources)

instance Core.AWSPager ListAssociatedResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssociatedResourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAssociatedResourcesResponse_resources
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAssociatedResources_nextToken
          Lens..~ rs
          Lens.^? listAssociatedResourcesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAssociatedResources where
  type
    AWSResponse ListAssociatedResources =
      ListAssociatedResourcesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedResourcesResponse'
            Prelude.<$> (x Core..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssociatedResources

instance Prelude.NFData ListAssociatedResources

instance Core.ToHeaders ListAssociatedResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListAssociatedResources where
  toPath ListAssociatedResources' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS application,
        "/resources"
      ]

instance Core.ToQuery ListAssociatedResources where
  toQuery ListAssociatedResources' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListAssociatedResourcesResponse' smart constructor.
data ListAssociatedResourcesResponse = ListAssociatedResourcesResponse'
  { -- | Information about the resources.
    resources :: Prelude.Maybe [ResourceInfo],
    -- | The token to use to get the next page of results after a previous API
    -- call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssociatedResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resources', 'listAssociatedResourcesResponse_resources' - Information about the resources.
--
-- 'nextToken', 'listAssociatedResourcesResponse_nextToken' - The token to use to get the next page of results after a previous API
-- call.
--
-- 'httpStatus', 'listAssociatedResourcesResponse_httpStatus' - The response's http status code.
newListAssociatedResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssociatedResourcesResponse
newListAssociatedResourcesResponse pHttpStatus_ =
  ListAssociatedResourcesResponse'
    { resources =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the resources.
listAssociatedResourcesResponse_resources :: Lens.Lens' ListAssociatedResourcesResponse (Prelude.Maybe [ResourceInfo])
listAssociatedResourcesResponse_resources = Lens.lens (\ListAssociatedResourcesResponse' {resources} -> resources) (\s@ListAssociatedResourcesResponse' {} a -> s {resources = a} :: ListAssociatedResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to get the next page of results after a previous API
-- call.
listAssociatedResourcesResponse_nextToken :: Lens.Lens' ListAssociatedResourcesResponse (Prelude.Maybe Prelude.Text)
listAssociatedResourcesResponse_nextToken = Lens.lens (\ListAssociatedResourcesResponse' {nextToken} -> nextToken) (\s@ListAssociatedResourcesResponse' {} a -> s {nextToken = a} :: ListAssociatedResourcesResponse)

-- | The response's http status code.
listAssociatedResourcesResponse_httpStatus :: Lens.Lens' ListAssociatedResourcesResponse Prelude.Int
listAssociatedResourcesResponse_httpStatus = Lens.lens (\ListAssociatedResourcesResponse' {httpStatus} -> httpStatus) (\s@ListAssociatedResourcesResponse' {} a -> s {httpStatus = a} :: ListAssociatedResourcesResponse)

instance
  Prelude.NFData
    ListAssociatedResourcesResponse
