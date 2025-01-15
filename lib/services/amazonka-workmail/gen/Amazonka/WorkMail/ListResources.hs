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
-- Module      : Amazonka.WorkMail.ListResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the organization\'s resources.
--
-- This operation returns paginated results.
module Amazonka.WorkMail.ListResources
  ( -- * Creating a Request
    ListResources (..),
    newListResources,

    -- * Request Lenses
    listResources_maxResults,
    listResources_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newListResources' smart constructor.
data ListResources = ListResources'
  { -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'listResources_maxResults' - The maximum number of results to return in a single call.
--
-- 'nextToken', 'listResources_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
--
-- 'organizationId', 'listResources_organizationId' - The identifier for the organization under which the resources exist.
newListResources ::
  -- | 'organizationId'
  Prelude.Text ->
  ListResources
newListResources pOrganizationId_ =
  ListResources'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The maximum number of results to return in a single call.
listResources_maxResults :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Natural)
listResources_maxResults = Lens.lens (\ListResources' {maxResults} -> maxResults) (\s@ListResources' {} a -> s {maxResults = a} :: ListResources)

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listResources_nextToken :: Lens.Lens' ListResources (Prelude.Maybe Prelude.Text)
listResources_nextToken = Lens.lens (\ListResources' {nextToken} -> nextToken) (\s@ListResources' {} a -> s {nextToken = a} :: ListResources)

-- | The identifier for the organization under which the resources exist.
listResources_organizationId :: Lens.Lens' ListResources Prelude.Text
listResources_organizationId = Lens.lens (\ListResources' {organizationId} -> organizationId) (\s@ListResources' {} a -> s {organizationId = a} :: ListResources)

instance Core.AWSPager ListResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourcesResponse_resources
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listResources_nextToken
              Lens..~ rs
              Lens.^? listResourcesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListResources where
  type
    AWSResponse ListResources =
      ListResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourcesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Resources" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResources where
  hashWithSalt _salt ListResources' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` organizationId

instance Prelude.NFData ListResources where
  rnf ListResources' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf organizationId

instance Data.ToHeaders ListResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.ListResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResources where
  toJSON ListResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("OrganizationId" Data..= organizationId)
          ]
      )

instance Data.ToPath ListResources where
  toPath = Prelude.const "/"

instance Data.ToQuery ListResources where
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
listResourcesResponse_resources = Lens.lens (\ListResourcesResponse' {resources} -> resources) (\s@ListResourcesResponse' {} a -> s {resources = a} :: ListResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourcesResponse_httpStatus :: Lens.Lens' ListResourcesResponse Prelude.Int
listResourcesResponse_httpStatus = Lens.lens (\ListResourcesResponse' {httpStatus} -> httpStatus) (\s@ListResourcesResponse' {} a -> s {httpStatus = a} :: ListResourcesResponse)

instance Prelude.NFData ListResourcesResponse where
  rnf ListResourcesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf resources `Prelude.seq`
        Prelude.rnf httpStatus
