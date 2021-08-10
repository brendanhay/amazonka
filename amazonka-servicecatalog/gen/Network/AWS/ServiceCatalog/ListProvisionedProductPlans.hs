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
-- Module      : Network.AWS.ServiceCatalog.ListProvisionedProductPlans
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the plans for the specified provisioned product or all plans to
-- which the user has access.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListProvisionedProductPlans
  ( -- * Creating a Request
    ListProvisionedProductPlans (..),
    newListProvisionedProductPlans,

    -- * Request Lenses
    listProvisionedProductPlans_provisionProductId,
    listProvisionedProductPlans_pageSize,
    listProvisionedProductPlans_pageToken,
    listProvisionedProductPlans_accessLevelFilter,
    listProvisionedProductPlans_acceptLanguage,

    -- * Destructuring the Response
    ListProvisionedProductPlansResponse (..),
    newListProvisionedProductPlansResponse,

    -- * Response Lenses
    listProvisionedProductPlansResponse_nextPageToken,
    listProvisionedProductPlansResponse_provisionedProductPlans,
    listProvisionedProductPlansResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListProvisionedProductPlans' smart constructor.
data ListProvisionedProductPlans = ListProvisionedProductPlans'
  { -- | The product identifier.
    provisionProductId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The access level to use to obtain results. The default is @User@.
    accessLevelFilter :: Prelude.Maybe AccessLevelFilter,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text
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
-- 'provisionProductId', 'listProvisionedProductPlans_provisionProductId' - The product identifier.
--
-- 'pageSize', 'listProvisionedProductPlans_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listProvisionedProductPlans_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'accessLevelFilter', 'listProvisionedProductPlans_accessLevelFilter' - The access level to use to obtain results. The default is @User@.
--
-- 'acceptLanguage', 'listProvisionedProductPlans_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
newListProvisionedProductPlans ::
  ListProvisionedProductPlans
newListProvisionedProductPlans =
  ListProvisionedProductPlans'
    { provisionProductId =
        Prelude.Nothing,
      pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      accessLevelFilter = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing
    }

-- | The product identifier.
listProvisionedProductPlans_provisionProductId :: Lens.Lens' ListProvisionedProductPlans (Prelude.Maybe Prelude.Text)
listProvisionedProductPlans_provisionProductId = Lens.lens (\ListProvisionedProductPlans' {provisionProductId} -> provisionProductId) (\s@ListProvisionedProductPlans' {} a -> s {provisionProductId = a} :: ListProvisionedProductPlans)

-- | The maximum number of items to return with this call.
listProvisionedProductPlans_pageSize :: Lens.Lens' ListProvisionedProductPlans (Prelude.Maybe Prelude.Natural)
listProvisionedProductPlans_pageSize = Lens.lens (\ListProvisionedProductPlans' {pageSize} -> pageSize) (\s@ListProvisionedProductPlans' {} a -> s {pageSize = a} :: ListProvisionedProductPlans)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listProvisionedProductPlans_pageToken :: Lens.Lens' ListProvisionedProductPlans (Prelude.Maybe Prelude.Text)
listProvisionedProductPlans_pageToken = Lens.lens (\ListProvisionedProductPlans' {pageToken} -> pageToken) (\s@ListProvisionedProductPlans' {} a -> s {pageToken = a} :: ListProvisionedProductPlans)

-- | The access level to use to obtain results. The default is @User@.
listProvisionedProductPlans_accessLevelFilter :: Lens.Lens' ListProvisionedProductPlans (Prelude.Maybe AccessLevelFilter)
listProvisionedProductPlans_accessLevelFilter = Lens.lens (\ListProvisionedProductPlans' {accessLevelFilter} -> accessLevelFilter) (\s@ListProvisionedProductPlans' {} a -> s {accessLevelFilter = a} :: ListProvisionedProductPlans)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listProvisionedProductPlans_acceptLanguage :: Lens.Lens' ListProvisionedProductPlans (Prelude.Maybe Prelude.Text)
listProvisionedProductPlans_acceptLanguage = Lens.lens (\ListProvisionedProductPlans' {acceptLanguage} -> acceptLanguage) (\s@ListProvisionedProductPlans' {} a -> s {acceptLanguage = a} :: ListProvisionedProductPlans)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& listProvisionedProductPlans_pageToken
          Lens..~ rs
          Lens.^? listProvisionedProductPlansResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListProvisionedProductPlans where
  type
    AWSResponse ListProvisionedProductPlans =
      ListProvisionedProductPlansResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisionedProductPlansResponse'
            Prelude.<$> (x Core..?> "NextPageToken")
            Prelude.<*> ( x Core..?> "ProvisionedProductPlans"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProvisionedProductPlans

instance Prelude.NFData ListProvisionedProductPlans

instance Core.ToHeaders ListProvisionedProductPlans where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListProvisionedProductPlans" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListProvisionedProductPlans where
  toJSON ListProvisionedProductPlans' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProvisionProductId" Core..=)
              Prelude.<$> provisionProductId,
            ("PageSize" Core..=) Prelude.<$> pageSize,
            ("PageToken" Core..=) Prelude.<$> pageToken,
            ("AccessLevelFilter" Core..=)
              Prelude.<$> accessLevelFilter,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage
          ]
      )

instance Core.ToPath ListProvisionedProductPlans where
  toPath = Prelude.const "/"

instance Core.ToQuery ListProvisionedProductPlans where
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
listProvisionedProductPlansResponse_provisionedProductPlans = Lens.lens (\ListProvisionedProductPlansResponse' {provisionedProductPlans} -> provisionedProductPlans) (\s@ListProvisionedProductPlansResponse' {} a -> s {provisionedProductPlans = a} :: ListProvisionedProductPlansResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listProvisionedProductPlansResponse_httpStatus :: Lens.Lens' ListProvisionedProductPlansResponse Prelude.Int
listProvisionedProductPlansResponse_httpStatus = Lens.lens (\ListProvisionedProductPlansResponse' {httpStatus} -> httpStatus) (\s@ListProvisionedProductPlansResponse' {} a -> s {httpStatus = a} :: ListProvisionedProductPlansResponse)

instance
  Prelude.NFData
    ListProvisionedProductPlansResponse
