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
-- Module      : Amazonka.ServiceCatalog.ListBudgetsForResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the budgets associated to the specified resource.
module Amazonka.ServiceCatalog.ListBudgetsForResource
  ( -- * Creating a Request
    ListBudgetsForResource (..),
    newListBudgetsForResource,

    -- * Request Lenses
    listBudgetsForResource_pageToken,
    listBudgetsForResource_pageSize,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newListBudgetsForResource' smart constructor.
data ListBudgetsForResource = ListBudgetsForResource'
  { -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The resource identifier.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBudgetsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'listBudgetsForResource_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'pageSize', 'listBudgetsForResource_pageSize' - The maximum number of items to return with this call.
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
  Prelude.Text ->
  ListBudgetsForResource
newListBudgetsForResource pResourceId_ =
  ListBudgetsForResource'
    { pageToken =
        Prelude.Nothing,
      pageSize = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listBudgetsForResource_pageToken :: Lens.Lens' ListBudgetsForResource (Prelude.Maybe Prelude.Text)
listBudgetsForResource_pageToken = Lens.lens (\ListBudgetsForResource' {pageToken} -> pageToken) (\s@ListBudgetsForResource' {} a -> s {pageToken = a} :: ListBudgetsForResource)

-- | The maximum number of items to return with this call.
listBudgetsForResource_pageSize :: Lens.Lens' ListBudgetsForResource (Prelude.Maybe Prelude.Natural)
listBudgetsForResource_pageSize = Lens.lens (\ListBudgetsForResource' {pageSize} -> pageSize) (\s@ListBudgetsForResource' {} a -> s {pageSize = a} :: ListBudgetsForResource)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listBudgetsForResource_acceptLanguage :: Lens.Lens' ListBudgetsForResource (Prelude.Maybe Prelude.Text)
listBudgetsForResource_acceptLanguage = Lens.lens (\ListBudgetsForResource' {acceptLanguage} -> acceptLanguage) (\s@ListBudgetsForResource' {} a -> s {acceptLanguage = a} :: ListBudgetsForResource)

-- | The resource identifier.
listBudgetsForResource_resourceId :: Lens.Lens' ListBudgetsForResource Prelude.Text
listBudgetsForResource_resourceId = Lens.lens (\ListBudgetsForResource' {resourceId} -> resourceId) (\s@ListBudgetsForResource' {} a -> s {resourceId = a} :: ListBudgetsForResource)

instance Core.AWSRequest ListBudgetsForResource where
  type
    AWSResponse ListBudgetsForResource =
      ListBudgetsForResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBudgetsForResourceResponse'
            Prelude.<$> (x Core..?> "NextPageToken")
            Prelude.<*> (x Core..?> "Budgets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBudgetsForResource where
  hashWithSalt _salt ListBudgetsForResource' {..} =
    _salt `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData ListBudgetsForResource where
  rnf ListBudgetsForResource' {..} =
    Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf resourceId

instance Core.ToHeaders ListBudgetsForResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListBudgetsForResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListBudgetsForResource where
  toJSON ListBudgetsForResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PageToken" Core..=) Prelude.<$> pageToken,
            ("PageSize" Core..=) Prelude.<$> pageSize,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath ListBudgetsForResource where
  toPath = Prelude.const "/"

instance Core.ToQuery ListBudgetsForResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBudgetsForResourceResponse' smart constructor.
data ListBudgetsForResourceResponse = ListBudgetsForResourceResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the associated budgets.
    budgets :: Prelude.Maybe [BudgetDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListBudgetsForResourceResponse
newListBudgetsForResourceResponse pHttpStatus_ =
  ListBudgetsForResourceResponse'
    { nextPageToken =
        Prelude.Nothing,
      budgets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listBudgetsForResourceResponse_nextPageToken :: Lens.Lens' ListBudgetsForResourceResponse (Prelude.Maybe Prelude.Text)
listBudgetsForResourceResponse_nextPageToken = Lens.lens (\ListBudgetsForResourceResponse' {nextPageToken} -> nextPageToken) (\s@ListBudgetsForResourceResponse' {} a -> s {nextPageToken = a} :: ListBudgetsForResourceResponse)

-- | Information about the associated budgets.
listBudgetsForResourceResponse_budgets :: Lens.Lens' ListBudgetsForResourceResponse (Prelude.Maybe [BudgetDetail])
listBudgetsForResourceResponse_budgets = Lens.lens (\ListBudgetsForResourceResponse' {budgets} -> budgets) (\s@ListBudgetsForResourceResponse' {} a -> s {budgets = a} :: ListBudgetsForResourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listBudgetsForResourceResponse_httpStatus :: Lens.Lens' ListBudgetsForResourceResponse Prelude.Int
listBudgetsForResourceResponse_httpStatus = Lens.lens (\ListBudgetsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListBudgetsForResourceResponse' {} a -> s {httpStatus = a} :: ListBudgetsForResourceResponse)

instance
  Prelude.NFData
    ListBudgetsForResourceResponse
  where
  rnf ListBudgetsForResourceResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf budgets
      `Prelude.seq` Prelude.rnf httpStatus
