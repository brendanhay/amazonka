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
-- Module      : Network.AWS.ServiceCatalog.ListBudgetsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the budgets associated to the specified resource.
module Network.AWS.ServiceCatalog.ListBudgetsForResource
  ( -- * Creating a Request
    ListBudgetsForResource (..),
    newListBudgetsForResource,

    -- * Request Lenses
    listBudgetsForResource_pageSize,
    listBudgetsForResource_pageToken,
    listBudgetsForResource_acceptLanguage,
    listBudgetsForResource_resourceId,

    -- * Destructuring the Response
    ListBudgetsForResourceResponse (..),
    newListBudgetsForResourceResponse,

    -- * Response Lenses
    listBudgetsForResourceResponse_nextPageToken,
    listBudgetsForResourceResponse_budgets,
    listBudgetsForResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListBudgetsForResource' smart constructor.
data ListBudgetsForResource = ListBudgetsForResource'
  { -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The resource identifier.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBudgetsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listBudgetsForResource_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listBudgetsForResource_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'acceptLanguage', 'listBudgetsForResource_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'resourceId', 'listBudgetsForResource_resourceId' - The resource identifier.
newListBudgetsForResource ::
  -- | 'resourceId'
  Core.Text ->
  ListBudgetsForResource
newListBudgetsForResource pResourceId_ =
  ListBudgetsForResource'
    { pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      acceptLanguage = Core.Nothing,
      resourceId = pResourceId_
    }

-- | The maximum number of items to return with this call.
listBudgetsForResource_pageSize :: Lens.Lens' ListBudgetsForResource (Core.Maybe Core.Natural)
listBudgetsForResource_pageSize = Lens.lens (\ListBudgetsForResource' {pageSize} -> pageSize) (\s@ListBudgetsForResource' {} a -> s {pageSize = a} :: ListBudgetsForResource)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listBudgetsForResource_pageToken :: Lens.Lens' ListBudgetsForResource (Core.Maybe Core.Text)
listBudgetsForResource_pageToken = Lens.lens (\ListBudgetsForResource' {pageToken} -> pageToken) (\s@ListBudgetsForResource' {} a -> s {pageToken = a} :: ListBudgetsForResource)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listBudgetsForResource_acceptLanguage :: Lens.Lens' ListBudgetsForResource (Core.Maybe Core.Text)
listBudgetsForResource_acceptLanguage = Lens.lens (\ListBudgetsForResource' {acceptLanguage} -> acceptLanguage) (\s@ListBudgetsForResource' {} a -> s {acceptLanguage = a} :: ListBudgetsForResource)

-- | The resource identifier.
listBudgetsForResource_resourceId :: Lens.Lens' ListBudgetsForResource Core.Text
listBudgetsForResource_resourceId = Lens.lens (\ListBudgetsForResource' {resourceId} -> resourceId) (\s@ListBudgetsForResource' {} a -> s {resourceId = a} :: ListBudgetsForResource)

instance Core.AWSRequest ListBudgetsForResource where
  type
    AWSResponse ListBudgetsForResource =
      ListBudgetsForResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBudgetsForResourceResponse'
            Core.<$> (x Core..?> "NextPageToken")
            Core.<*> (x Core..?> "Budgets" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBudgetsForResource

instance Core.NFData ListBudgetsForResource

instance Core.ToHeaders ListBudgetsForResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListBudgetsForResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListBudgetsForResource where
  toJSON ListBudgetsForResource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath ListBudgetsForResource where
  toPath = Core.const "/"

instance Core.ToQuery ListBudgetsForResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListBudgetsForResourceResponse' smart constructor.
data ListBudgetsForResourceResponse = ListBudgetsForResourceResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | Information about the associated budgets.
    budgets :: Core.Maybe [BudgetDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBudgetsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listBudgetsForResourceResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'budgets', 'listBudgetsForResourceResponse_budgets' - Information about the associated budgets.
--
-- 'httpStatus', 'listBudgetsForResourceResponse_httpStatus' - The response's http status code.
newListBudgetsForResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBudgetsForResourceResponse
newListBudgetsForResourceResponse pHttpStatus_ =
  ListBudgetsForResourceResponse'
    { nextPageToken =
        Core.Nothing,
      budgets = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listBudgetsForResourceResponse_nextPageToken :: Lens.Lens' ListBudgetsForResourceResponse (Core.Maybe Core.Text)
listBudgetsForResourceResponse_nextPageToken = Lens.lens (\ListBudgetsForResourceResponse' {nextPageToken} -> nextPageToken) (\s@ListBudgetsForResourceResponse' {} a -> s {nextPageToken = a} :: ListBudgetsForResourceResponse)

-- | Information about the associated budgets.
listBudgetsForResourceResponse_budgets :: Lens.Lens' ListBudgetsForResourceResponse (Core.Maybe [BudgetDetail])
listBudgetsForResourceResponse_budgets = Lens.lens (\ListBudgetsForResourceResponse' {budgets} -> budgets) (\s@ListBudgetsForResourceResponse' {} a -> s {budgets = a} :: ListBudgetsForResourceResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBudgetsForResourceResponse_httpStatus :: Lens.Lens' ListBudgetsForResourceResponse Core.Int
listBudgetsForResourceResponse_httpStatus = Lens.lens (\ListBudgetsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListBudgetsForResourceResponse' {} a -> s {httpStatus = a} :: ListBudgetsForResourceResponse)

instance Core.NFData ListBudgetsForResourceResponse
