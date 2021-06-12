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
-- Module      : Network.AWS.Connect.ListUseCases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Lists the use cases.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListUseCases
  ( -- * Creating a Request
    ListUseCases (..),
    newListUseCases,

    -- * Request Lenses
    listUseCases_nextToken,
    listUseCases_maxResults,
    listUseCases_instanceId,
    listUseCases_integrationAssociationId,

    -- * Destructuring the Response
    ListUseCasesResponse (..),
    newListUseCasesResponse,

    -- * Response Lenses
    listUseCasesResponse_nextToken,
    listUseCasesResponse_useCaseSummaryList,
    listUseCasesResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides summary information about the use cases for the specified
-- Amazon Connect AppIntegration association.
--
-- /See:/ 'newListUseCases' smart constructor.
data ListUseCases = ListUseCases'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier for the integration association.
    integrationAssociationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUseCases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUseCases_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listUseCases_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listUseCases_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'integrationAssociationId', 'listUseCases_integrationAssociationId' - The identifier for the integration association.
newListUseCases ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'integrationAssociationId'
  Core.Text ->
  ListUseCases
newListUseCases
  pInstanceId_
  pIntegrationAssociationId_ =
    ListUseCases'
      { nextToken = Core.Nothing,
        maxResults = Core.Nothing,
        instanceId = pInstanceId_,
        integrationAssociationId =
          pIntegrationAssociationId_
      }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listUseCases_nextToken :: Lens.Lens' ListUseCases (Core.Maybe Core.Text)
listUseCases_nextToken = Lens.lens (\ListUseCases' {nextToken} -> nextToken) (\s@ListUseCases' {} a -> s {nextToken = a} :: ListUseCases)

-- | The maximum number of results to return per page.
listUseCases_maxResults :: Lens.Lens' ListUseCases (Core.Maybe Core.Natural)
listUseCases_maxResults = Lens.lens (\ListUseCases' {maxResults} -> maxResults) (\s@ListUseCases' {} a -> s {maxResults = a} :: ListUseCases)

-- | The identifier of the Amazon Connect instance.
listUseCases_instanceId :: Lens.Lens' ListUseCases Core.Text
listUseCases_instanceId = Lens.lens (\ListUseCases' {instanceId} -> instanceId) (\s@ListUseCases' {} a -> s {instanceId = a} :: ListUseCases)

-- | The identifier for the integration association.
listUseCases_integrationAssociationId :: Lens.Lens' ListUseCases Core.Text
listUseCases_integrationAssociationId = Lens.lens (\ListUseCases' {integrationAssociationId} -> integrationAssociationId) (\s@ListUseCases' {} a -> s {integrationAssociationId = a} :: ListUseCases)

instance Core.AWSPager ListUseCases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUseCasesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listUseCasesResponse_useCaseSummaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listUseCases_nextToken
          Lens..~ rs
          Lens.^? listUseCasesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListUseCases where
  type AWSResponse ListUseCases = ListUseCasesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUseCasesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "UseCaseSummaryList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListUseCases

instance Core.NFData ListUseCases

instance Core.ToHeaders ListUseCases where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListUseCases where
  toPath ListUseCases' {..} =
    Core.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/integration-associations/",
        Core.toBS integrationAssociationId,
        "/use-cases"
      ]

instance Core.ToQuery ListUseCases where
  toQuery ListUseCases' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListUseCasesResponse' smart constructor.
data ListUseCasesResponse = ListUseCasesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The use cases.
    useCaseSummaryList :: Core.Maybe [UseCase],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUseCasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUseCasesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'useCaseSummaryList', 'listUseCasesResponse_useCaseSummaryList' - The use cases.
--
-- 'httpStatus', 'listUseCasesResponse_httpStatus' - The response's http status code.
newListUseCasesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListUseCasesResponse
newListUseCasesResponse pHttpStatus_ =
  ListUseCasesResponse'
    { nextToken = Core.Nothing,
      useCaseSummaryList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listUseCasesResponse_nextToken :: Lens.Lens' ListUseCasesResponse (Core.Maybe Core.Text)
listUseCasesResponse_nextToken = Lens.lens (\ListUseCasesResponse' {nextToken} -> nextToken) (\s@ListUseCasesResponse' {} a -> s {nextToken = a} :: ListUseCasesResponse)

-- | The use cases.
listUseCasesResponse_useCaseSummaryList :: Lens.Lens' ListUseCasesResponse (Core.Maybe [UseCase])
listUseCasesResponse_useCaseSummaryList = Lens.lens (\ListUseCasesResponse' {useCaseSummaryList} -> useCaseSummaryList) (\s@ListUseCasesResponse' {} a -> s {useCaseSummaryList = a} :: ListUseCasesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listUseCasesResponse_httpStatus :: Lens.Lens' ListUseCasesResponse Core.Int
listUseCasesResponse_httpStatus = Lens.lens (\ListUseCasesResponse' {httpStatus} -> httpStatus) (\s@ListUseCasesResponse' {} a -> s {httpStatus = a} :: ListUseCasesResponse)

instance Core.NFData ListUseCasesResponse
