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
-- Module      : Network.AWS.WorkMail.ListResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the organization\'s resources.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListResources
  ( -- * Creating a Request
    ListResources (..),
    newListResources,

    -- * Request Lenses
    listResources_nextToken,
    listResources_maxResults,
    listResources_organizationId,

    -- * Destructuring the Response
    ListResourcesResponse (..),
    newListResourcesResponse,

    -- * Response Lenses
    listResourcesResponse_nextToken,
    listResourcesResponse_resources,
    listResourcesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newListResources' smart constructor.
data ListResources = ListResources'
  { -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier for the organization under which the resources exist.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResources_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
--
-- 'maxResults', 'listResources_maxResults' - The maximum number of results to return in a single call.
--
-- 'organizationId', 'listResources_organizationId' - The identifier for the organization under which the resources exist.
newListResources ::
  -- | 'organizationId'
  Prelude.Text ->
  ListResources
newListResources pOrganizationId_ =
  ListResources'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listResources_nextToken :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Text)
listResources_nextToken = Lens.lens (\ListResources' {nextToken} -> nextToken) (\s@ListResources' {} a -> s {nextToken = a} :: ListResources)

-- | The maximum number of results to return in a single call.
listResources_maxResults :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Natural)
listResources_maxResults = Lens.lens (\ListResources' {maxResults} -> maxResults) (\s@ListResources' {} a -> s {maxResults = a} :: ListResources)

-- | The identifier for the organization under which the resources exist.
listResources_organizationId :: Lens.Lens' ListResources Prelude.Text
listResources_organizationId = Lens.lens (\ListResources' {organizationId} -> organizationId) (\s@ListResources' {} a -> s {organizationId = a} :: ListResources)

instance Core.AWSPager ListResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourcesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourcesResponse_resources Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResources_nextToken
          Lens..~ rs
          Lens.^? listResourcesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListResources where
  type
    AWSResponse ListResources =
      ListResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Resources" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResources

instance Prelude.NFData ListResources

instance Core.ToHeaders ListResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.ListResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResources where
  toJSON ListResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("OrganizationId" Core..= organizationId)
          ]
      )

instance Core.ToPath ListResources where
  toPath = Prelude.const "/"

instance Core.ToQuery ListResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourcesResponse' smart constructor.
data ListResourcesResponse = ListResourcesResponse'
  { -- | The token used to paginate through all the organization\'s resources.
    -- While results are still available, it has an associated value. When the
    -- last page is reached, the token is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One page of the organization\'s resource representation.
    resources :: Prelude.Maybe [Resource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourcesResponse_nextToken' - The token used to paginate through all the organization\'s resources.
-- While results are still available, it has an associated value. When the
-- last page is reached, the token is empty.
--
-- 'resources', 'listResourcesResponse_resources' - One page of the organization\'s resource representation.
--
-- 'httpStatus', 'listResourcesResponse_httpStatus' - The response's http status code.
newListResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourcesResponse
newListResourcesResponse pHttpStatus_ =
  ListResourcesResponse'
    { nextToken = Prelude.Nothing,
      resources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to paginate through all the organization\'s resources.
-- While results are still available, it has an associated value. When the
-- last page is reached, the token is empty.
listResourcesResponse_nextToken :: Lens.Lens' ListResourcesResponse (Prelude.Maybe Prelude.Text)
listResourcesResponse_nextToken = Lens.lens (\ListResourcesResponse' {nextToken} -> nextToken) (\s@ListResourcesResponse' {} a -> s {nextToken = a} :: ListResourcesResponse)

-- | One page of the organization\'s resource representation.
listResourcesResponse_resources :: Lens.Lens' ListResourcesResponse (Prelude.Maybe [Resource])
listResourcesResponse_resources = Lens.lens (\ListResourcesResponse' {resources} -> resources) (\s@ListResourcesResponse' {} a -> s {resources = a} :: ListResourcesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listResourcesResponse_httpStatus :: Lens.Lens' ListResourcesResponse Prelude.Int
listResourcesResponse_httpStatus = Lens.lens (\ListResourcesResponse' {httpStatus} -> httpStatus) (\s@ListResourcesResponse' {} a -> s {httpStatus = a} :: ListResourcesResponse)

instance Prelude.NFData ListResourcesResponse
