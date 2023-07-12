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
-- Module      : Amazonka.ServiceCatalog.ListProvisionedProductPlans
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the plans for the specified provisioned product or all plans to
-- which the user has access.
--
-- This operation returns paginated results.
module Amazonka.ServiceCatalog.ListProvisionedProductPlans
  ( -- * Creating a Request
    ListProvisionedProductPlans (..),
    newListProvisionedProductPlans,

    -- * Request Lenses
    listProvisionedProductPlans_acceptLanguage,
    listProvisionedProductPlans_accessLevelFilter,
    listProvisionedProductPlans_pageSize,
    listProvisionedProductPlans_pageToken,
    listProvisionedProductPlans_provisionProductId,

    -- * Destructuring the Response
    ListProvisionedProductPlansResponse (..),
    newListProvisionedProductPlansResponse,

    -- * Response Lenses
    listProvisionedProductPlansResponse_nextPageToken,
    listProvisionedProductPlansResponse_provisionedProductPlans,
    listProvisionedProductPlansResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newListProvisionedProductPlans' smart constructor.
data ListProvisionedProductPlans = ListProvisionedProductPlans'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The access level to use to obtain results. The default is @User@.
    accessLevelFilter :: Prelude.Maybe AccessLevelFilter,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    provisionProductId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProvisionedProductPlans' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'listProvisionedProductPlans_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'accessLevelFilter', 'listProvisionedProductPlans_accessLevelFilter' - The access level to use to obtain results. The default is @User@.
--
-- 'pageSize', 'listProvisionedProductPlans_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listProvisionedProductPlans_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'provisionProductId', 'listProvisionedProductPlans_provisionProductId' - The product identifier.
newListProvisionedProductPlans ::
  ListProvisionedProductPlans
newListProvisionedProductPlans =
  ListProvisionedProductPlans'
    { acceptLanguage =
        Prelude.Nothing,
      accessLevelFilter = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      provisionProductId = Prelude.Nothing
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listProvisionedProductPlans_acceptLanguage :: Lens.Lens' ListProvisionedProductPlans (Prelude.Maybe Prelude.Text)
listProvisionedProductPlans_acceptLanguage = Lens.lens (\ListProvisionedProductPlans' {acceptLanguage} -> acceptLanguage) (\s@ListProvisionedProductPlans' {} a -> s {acceptLanguage = a} :: ListProvisionedProductPlans)

-- | The access level to use to obtain results. The default is @User@.
listProvisionedProductPlans_accessLevelFilter :: Lens.Lens' ListProvisionedProductPlans (Prelude.Maybe AccessLevelFilter)
listProvisionedProductPlans_accessLevelFilter = Lens.lens (\ListProvisionedProductPlans' {accessLevelFilter} -> accessLevelFilter) (\s@ListProvisionedProductPlans' {} a -> s {accessLevelFilter = a} :: ListProvisionedProductPlans)

-- | The maximum number of items to return with this call.
listProvisionedProductPlans_pageSize :: Lens.Lens' ListProvisionedProductPlans (Prelude.Maybe Prelude.Natural)
listProvisionedProductPlans_pageSize = Lens.lens (\ListProvisionedProductPlans' {pageSize} -> pageSize) (\s@ListProvisionedProductPlans' {} a -> s {pageSize = a} :: ListProvisionedProductPlans)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listProvisionedProductPlans_pageToken :: Lens.Lens' ListProvisionedProductPlans (Prelude.Maybe Prelude.Text)
listProvisionedProductPlans_pageToken = Lens.lens (\ListProvisionedProductPlans' {pageToken} -> pageToken) (\s@ListProvisionedProductPlans' {} a -> s {pageToken = a} :: ListProvisionedProductPlans)

-- | The product identifier.
listProvisionedProductPlans_provisionProductId :: Lens.Lens' ListProvisionedProductPlans (Prelude.Maybe Prelude.Text)
listProvisionedProductPlans_provisionProductId = Lens.lens (\ListProvisionedProductPlans' {provisionProductId} -> provisionProductId) (\s@ListProvisionedProductPlans' {} a -> s {provisionProductId = a} :: ListProvisionedProductPlans)

instance Core.AWSPager ListProvisionedProductPlans where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProvisionedProductPlansResponse_nextPageToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listProvisionedProductPlansResponse_provisionedProductPlans
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listProvisionedProductPlans_pageToken
          Lens..~ rs
          Lens.^? listProvisionedProductPlansResponse_nextPageToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListProvisionedProductPlans where
  type
    AWSResponse ListProvisionedProductPlans =
      ListProvisionedProductPlansResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisionedProductPlansResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> ( x
                            Data..?> "ProvisionedProductPlans"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProvisionedProductPlans where
  hashWithSalt _salt ListProvisionedProductPlans' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` accessLevelFilter
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` provisionProductId

instance Prelude.NFData ListProvisionedProductPlans where
  rnf ListProvisionedProductPlans' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf accessLevelFilter
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf provisionProductId

instance Data.ToHeaders ListProvisionedProductPlans where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.ListProvisionedProductPlans" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListProvisionedProductPlans where
  toJSON ListProvisionedProductPlans' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("AccessLevelFilter" Data..=)
              Prelude.<$> accessLevelFilter,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("PageToken" Data..=) Prelude.<$> pageToken,
            ("ProvisionProductId" Data..=)
              Prelude.<$> provisionProductId
          ]
      )

instance Data.ToPath ListProvisionedProductPlans where
  toPath = Prelude.const "/"

instance Data.ToQuery ListProvisionedProductPlans where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProvisionedProductPlansResponse' smart constructor.
data ListProvisionedProductPlansResponse = ListProvisionedProductPlansResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the plans.
    provisionedProductPlans :: Prelude.Maybe [ProvisionedProductPlanSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProvisionedProductPlansResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listProvisionedProductPlansResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'provisionedProductPlans', 'listProvisionedProductPlansResponse_provisionedProductPlans' - Information about the plans.
--
-- 'httpStatus', 'listProvisionedProductPlansResponse_httpStatus' - The response's http status code.
newListProvisionedProductPlansResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProvisionedProductPlansResponse
newListProvisionedProductPlansResponse pHttpStatus_ =
  ListProvisionedProductPlansResponse'
    { nextPageToken =
        Prelude.Nothing,
      provisionedProductPlans =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listProvisionedProductPlansResponse_nextPageToken :: Lens.Lens' ListProvisionedProductPlansResponse (Prelude.Maybe Prelude.Text)
listProvisionedProductPlansResponse_nextPageToken = Lens.lens (\ListProvisionedProductPlansResponse' {nextPageToken} -> nextPageToken) (\s@ListProvisionedProductPlansResponse' {} a -> s {nextPageToken = a} :: ListProvisionedProductPlansResponse)

-- | Information about the plans.
listProvisionedProductPlansResponse_provisionedProductPlans :: Lens.Lens' ListProvisionedProductPlansResponse (Prelude.Maybe [ProvisionedProductPlanSummary])
listProvisionedProductPlansResponse_provisionedProductPlans = Lens.lens (\ListProvisionedProductPlansResponse' {provisionedProductPlans} -> provisionedProductPlans) (\s@ListProvisionedProductPlansResponse' {} a -> s {provisionedProductPlans = a} :: ListProvisionedProductPlansResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProvisionedProductPlansResponse_httpStatus :: Lens.Lens' ListProvisionedProductPlansResponse Prelude.Int
listProvisionedProductPlansResponse_httpStatus = Lens.lens (\ListProvisionedProductPlansResponse' {httpStatus} -> httpStatus) (\s@ListProvisionedProductPlansResponse' {} a -> s {httpStatus = a} :: ListProvisionedProductPlansResponse)

instance
  Prelude.NFData
    ListProvisionedProductPlansResponse
  where
  rnf ListProvisionedProductPlansResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf provisionedProductPlans
      `Prelude.seq` Prelude.rnf httpStatus
