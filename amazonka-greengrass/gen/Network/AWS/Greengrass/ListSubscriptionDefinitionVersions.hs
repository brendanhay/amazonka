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
-- Module      : Network.AWS.Greengrass.ListSubscriptionDefinitionVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a subscription definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListSubscriptionDefinitionVersions
  ( -- * Creating a Request
    ListSubscriptionDefinitionVersions (..),
    newListSubscriptionDefinitionVersions,

    -- * Request Lenses
    listSubscriptionDefinitionVersions_nextToken,
    listSubscriptionDefinitionVersions_maxResults,
    listSubscriptionDefinitionVersions_subscriptionDefinitionId,

    -- * Destructuring the Response
    ListSubscriptionDefinitionVersionsResponse (..),
    newListSubscriptionDefinitionVersionsResponse,

    -- * Response Lenses
    listSubscriptionDefinitionVersionsResponse_nextToken,
    listSubscriptionDefinitionVersionsResponse_versions,
    listSubscriptionDefinitionVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSubscriptionDefinitionVersions' smart constructor.
data ListSubscriptionDefinitionVersions = ListSubscriptionDefinitionVersions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSubscriptionDefinitionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscriptionDefinitionVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listSubscriptionDefinitionVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'subscriptionDefinitionId', 'listSubscriptionDefinitionVersions_subscriptionDefinitionId' - The ID of the subscription definition.
newListSubscriptionDefinitionVersions ::
  -- | 'subscriptionDefinitionId'
  Core.Text ->
  ListSubscriptionDefinitionVersions
newListSubscriptionDefinitionVersions
  pSubscriptionDefinitionId_ =
    ListSubscriptionDefinitionVersions'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        subscriptionDefinitionId =
          pSubscriptionDefinitionId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listSubscriptionDefinitionVersions_nextToken :: Lens.Lens' ListSubscriptionDefinitionVersions (Core.Maybe Core.Text)
listSubscriptionDefinitionVersions_nextToken = Lens.lens (\ListSubscriptionDefinitionVersions' {nextToken} -> nextToken) (\s@ListSubscriptionDefinitionVersions' {} a -> s {nextToken = a} :: ListSubscriptionDefinitionVersions)

-- | The maximum number of results to be returned per request.
listSubscriptionDefinitionVersions_maxResults :: Lens.Lens' ListSubscriptionDefinitionVersions (Core.Maybe Core.Text)
listSubscriptionDefinitionVersions_maxResults = Lens.lens (\ListSubscriptionDefinitionVersions' {maxResults} -> maxResults) (\s@ListSubscriptionDefinitionVersions' {} a -> s {maxResults = a} :: ListSubscriptionDefinitionVersions)

-- | The ID of the subscription definition.
listSubscriptionDefinitionVersions_subscriptionDefinitionId :: Lens.Lens' ListSubscriptionDefinitionVersions Core.Text
listSubscriptionDefinitionVersions_subscriptionDefinitionId = Lens.lens (\ListSubscriptionDefinitionVersions' {subscriptionDefinitionId} -> subscriptionDefinitionId) (\s@ListSubscriptionDefinitionVersions' {} a -> s {subscriptionDefinitionId = a} :: ListSubscriptionDefinitionVersions)

instance
  Core.AWSPager
    ListSubscriptionDefinitionVersions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSubscriptionDefinitionVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSubscriptionDefinitionVersionsResponse_versions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSubscriptionDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listSubscriptionDefinitionVersionsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListSubscriptionDefinitionVersions
  where
  type
    AWSResponse ListSubscriptionDefinitionVersions =
      ListSubscriptionDefinitionVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSubscriptionDefinitionVersionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Versions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListSubscriptionDefinitionVersions

instance
  Core.NFData
    ListSubscriptionDefinitionVersions

instance
  Core.ToHeaders
    ListSubscriptionDefinitionVersions
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToPath
    ListSubscriptionDefinitionVersions
  where
  toPath ListSubscriptionDefinitionVersions' {..} =
    Core.mconcat
      [ "/greengrass/definition/subscriptions/",
        Core.toBS subscriptionDefinitionId,
        "/versions"
      ]

instance
  Core.ToQuery
    ListSubscriptionDefinitionVersions
  where
  toQuery ListSubscriptionDefinitionVersions' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListSubscriptionDefinitionVersionsResponse' smart constructor.
data ListSubscriptionDefinitionVersionsResponse = ListSubscriptionDefinitionVersionsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about a version.
    versions :: Core.Maybe [VersionInformation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSubscriptionDefinitionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscriptionDefinitionVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'versions', 'listSubscriptionDefinitionVersionsResponse_versions' - Information about a version.
--
-- 'httpStatus', 'listSubscriptionDefinitionVersionsResponse_httpStatus' - The response's http status code.
newListSubscriptionDefinitionVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSubscriptionDefinitionVersionsResponse
newListSubscriptionDefinitionVersionsResponse
  pHttpStatus_ =
    ListSubscriptionDefinitionVersionsResponse'
      { nextToken =
          Core.Nothing,
        versions = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listSubscriptionDefinitionVersionsResponse_nextToken :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse (Core.Maybe Core.Text)
listSubscriptionDefinitionVersionsResponse_nextToken = Lens.lens (\ListSubscriptionDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListSubscriptionDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListSubscriptionDefinitionVersionsResponse)

-- | Information about a version.
listSubscriptionDefinitionVersionsResponse_versions :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse (Core.Maybe [VersionInformation])
listSubscriptionDefinitionVersionsResponse_versions = Lens.lens (\ListSubscriptionDefinitionVersionsResponse' {versions} -> versions) (\s@ListSubscriptionDefinitionVersionsResponse' {} a -> s {versions = a} :: ListSubscriptionDefinitionVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSubscriptionDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse Core.Int
listSubscriptionDefinitionVersionsResponse_httpStatus = Lens.lens (\ListSubscriptionDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListSubscriptionDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListSubscriptionDefinitionVersionsResponse)

instance
  Core.NFData
    ListSubscriptionDefinitionVersionsResponse
