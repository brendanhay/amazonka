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
-- Module      : Amazonka.ServiceCatalog.ListServiceActions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all self-service actions.
--
-- This operation returns paginated results.
module Amazonka.ServiceCatalog.ListServiceActions
  ( -- * Creating a Request
    ListServiceActions (..),
    newListServiceActions,

    -- * Request Lenses
    listServiceActions_acceptLanguage,
    listServiceActions_pageSize,
    listServiceActions_pageToken,

    -- * Destructuring the Response
    ListServiceActionsResponse (..),
    newListServiceActionsResponse,

    -- * Response Lenses
    listServiceActionsResponse_nextPageToken,
    listServiceActionsResponse_serviceActionSummaries,
    listServiceActionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newListServiceActions' smart constructor.
data ListServiceActions = ListServiceActions'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'listServiceActions_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'pageSize', 'listServiceActions_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listServiceActions_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
newListServiceActions ::
  ListServiceActions
newListServiceActions =
  ListServiceActions'
    { acceptLanguage =
        Prelude.Nothing,
      pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listServiceActions_acceptLanguage :: Lens.Lens' ListServiceActions (Prelude.Maybe Prelude.Text)
listServiceActions_acceptLanguage = Lens.lens (\ListServiceActions' {acceptLanguage} -> acceptLanguage) (\s@ListServiceActions' {} a -> s {acceptLanguage = a} :: ListServiceActions)

-- | The maximum number of items to return with this call.
listServiceActions_pageSize :: Lens.Lens' ListServiceActions (Prelude.Maybe Prelude.Natural)
listServiceActions_pageSize = Lens.lens (\ListServiceActions' {pageSize} -> pageSize) (\s@ListServiceActions' {} a -> s {pageSize = a} :: ListServiceActions)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listServiceActions_pageToken :: Lens.Lens' ListServiceActions (Prelude.Maybe Prelude.Text)
listServiceActions_pageToken = Lens.lens (\ListServiceActions' {pageToken} -> pageToken) (\s@ListServiceActions' {} a -> s {pageToken = a} :: ListServiceActions)

instance Core.AWSPager ListServiceActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServiceActionsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listServiceActionsResponse_serviceActionSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listServiceActions_pageToken
          Lens..~ rs
          Lens.^? listServiceActionsResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListServiceActions where
  type
    AWSResponse ListServiceActions =
      ListServiceActionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceActionsResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> ( x Data..?> "ServiceActionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListServiceActions where
  hashWithSalt _salt ListServiceActions' {..} =
    _salt `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` pageToken

instance Prelude.NFData ListServiceActions where
  rnf ListServiceActions' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf pageToken

instance Data.ToHeaders ListServiceActions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.ListServiceActions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListServiceActions where
  toJSON ListServiceActions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("PageToken" Data..=) Prelude.<$> pageToken
          ]
      )

instance Data.ToPath ListServiceActions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListServiceActions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServiceActionsResponse' smart constructor.
data ListServiceActionsResponse = ListServiceActionsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An object containing information about the service actions associated
    -- with the provisioning artifact.
    serviceActionSummaries :: Prelude.Maybe [ServiceActionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listServiceActionsResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'serviceActionSummaries', 'listServiceActionsResponse_serviceActionSummaries' - An object containing information about the service actions associated
-- with the provisioning artifact.
--
-- 'httpStatus', 'listServiceActionsResponse_httpStatus' - The response's http status code.
newListServiceActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServiceActionsResponse
newListServiceActionsResponse pHttpStatus_ =
  ListServiceActionsResponse'
    { nextPageToken =
        Prelude.Nothing,
      serviceActionSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listServiceActionsResponse_nextPageToken :: Lens.Lens' ListServiceActionsResponse (Prelude.Maybe Prelude.Text)
listServiceActionsResponse_nextPageToken = Lens.lens (\ListServiceActionsResponse' {nextPageToken} -> nextPageToken) (\s@ListServiceActionsResponse' {} a -> s {nextPageToken = a} :: ListServiceActionsResponse)

-- | An object containing information about the service actions associated
-- with the provisioning artifact.
listServiceActionsResponse_serviceActionSummaries :: Lens.Lens' ListServiceActionsResponse (Prelude.Maybe [ServiceActionSummary])
listServiceActionsResponse_serviceActionSummaries = Lens.lens (\ListServiceActionsResponse' {serviceActionSummaries} -> serviceActionSummaries) (\s@ListServiceActionsResponse' {} a -> s {serviceActionSummaries = a} :: ListServiceActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listServiceActionsResponse_httpStatus :: Lens.Lens' ListServiceActionsResponse Prelude.Int
listServiceActionsResponse_httpStatus = Lens.lens (\ListServiceActionsResponse' {httpStatus} -> httpStatus) (\s@ListServiceActionsResponse' {} a -> s {httpStatus = a} :: ListServiceActionsResponse)

instance Prelude.NFData ListServiceActionsResponse where
  rnf ListServiceActionsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf serviceActionSummaries
      `Prelude.seq` Prelude.rnf httpStatus
