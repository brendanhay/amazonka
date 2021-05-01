{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ServiceCatalog.ListServiceActionsForProvisioningArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of self-service actions associated with the
-- specified Product ID and Provisioning Artifact ID.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListServiceActionsForProvisioningArtifact
  ( -- * Creating a Request
    ListServiceActionsForProvisioningArtifact (..),
    newListServiceActionsForProvisioningArtifact,

    -- * Request Lenses
    listServiceActionsForProvisioningArtifact_pageSize,
    listServiceActionsForProvisioningArtifact_pageToken,
    listServiceActionsForProvisioningArtifact_acceptLanguage,
    listServiceActionsForProvisioningArtifact_productId,
    listServiceActionsForProvisioningArtifact_provisioningArtifactId,

    -- * Destructuring the Response
    ListServiceActionsForProvisioningArtifactResponse (..),
    newListServiceActionsForProvisioningArtifactResponse,

    -- * Response Lenses
    listServiceActionsForProvisioningArtifactResponse_nextPageToken,
    listServiceActionsForProvisioningArtifactResponse_serviceActionSummaries,
    listServiceActionsForProvisioningArtifactResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListServiceActionsForProvisioningArtifact' smart constructor.
data ListServiceActionsForProvisioningArtifact = ListServiceActionsForProvisioningArtifact'
  { -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The product identifier. For example, @prod-abcdzk7xy33qa@.
    productId :: Prelude.Text,
    -- | The identifier of the provisioning artifact. For example,
    -- @pa-4abcdjnxjj6ne@.
    provisioningArtifactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListServiceActionsForProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listServiceActionsForProvisioningArtifact_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listServiceActionsForProvisioningArtifact_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'acceptLanguage', 'listServiceActionsForProvisioningArtifact_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'listServiceActionsForProvisioningArtifact_productId' - The product identifier. For example, @prod-abcdzk7xy33qa@.
--
-- 'provisioningArtifactId', 'listServiceActionsForProvisioningArtifact_provisioningArtifactId' - The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
newListServiceActionsForProvisioningArtifact ::
  -- | 'productId'
  Prelude.Text ->
  -- | 'provisioningArtifactId'
  Prelude.Text ->
  ListServiceActionsForProvisioningArtifact
newListServiceActionsForProvisioningArtifact
  pProductId_
  pProvisioningArtifactId_ =
    ListServiceActionsForProvisioningArtifact'
      { pageSize =
          Prelude.Nothing,
        pageToken = Prelude.Nothing,
        acceptLanguage = Prelude.Nothing,
        productId = pProductId_,
        provisioningArtifactId =
          pProvisioningArtifactId_
      }

-- | The maximum number of items to return with this call.
listServiceActionsForProvisioningArtifact_pageSize :: Lens.Lens' ListServiceActionsForProvisioningArtifact (Prelude.Maybe Prelude.Natural)
listServiceActionsForProvisioningArtifact_pageSize = Lens.lens (\ListServiceActionsForProvisioningArtifact' {pageSize} -> pageSize) (\s@ListServiceActionsForProvisioningArtifact' {} a -> s {pageSize = a} :: ListServiceActionsForProvisioningArtifact)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listServiceActionsForProvisioningArtifact_pageToken :: Lens.Lens' ListServiceActionsForProvisioningArtifact (Prelude.Maybe Prelude.Text)
listServiceActionsForProvisioningArtifact_pageToken = Lens.lens (\ListServiceActionsForProvisioningArtifact' {pageToken} -> pageToken) (\s@ListServiceActionsForProvisioningArtifact' {} a -> s {pageToken = a} :: ListServiceActionsForProvisioningArtifact)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listServiceActionsForProvisioningArtifact_acceptLanguage :: Lens.Lens' ListServiceActionsForProvisioningArtifact (Prelude.Maybe Prelude.Text)
listServiceActionsForProvisioningArtifact_acceptLanguage = Lens.lens (\ListServiceActionsForProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@ListServiceActionsForProvisioningArtifact' {} a -> s {acceptLanguage = a} :: ListServiceActionsForProvisioningArtifact)

-- | The product identifier. For example, @prod-abcdzk7xy33qa@.
listServiceActionsForProvisioningArtifact_productId :: Lens.Lens' ListServiceActionsForProvisioningArtifact Prelude.Text
listServiceActionsForProvisioningArtifact_productId = Lens.lens (\ListServiceActionsForProvisioningArtifact' {productId} -> productId) (\s@ListServiceActionsForProvisioningArtifact' {} a -> s {productId = a} :: ListServiceActionsForProvisioningArtifact)

-- | The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
listServiceActionsForProvisioningArtifact_provisioningArtifactId :: Lens.Lens' ListServiceActionsForProvisioningArtifact Prelude.Text
listServiceActionsForProvisioningArtifact_provisioningArtifactId = Lens.lens (\ListServiceActionsForProvisioningArtifact' {provisioningArtifactId} -> provisioningArtifactId) (\s@ListServiceActionsForProvisioningArtifact' {} a -> s {provisioningArtifactId = a} :: ListServiceActionsForProvisioningArtifact)

instance
  Pager.AWSPager
    ListServiceActionsForProvisioningArtifact
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listServiceActionsForProvisioningArtifactResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listServiceActionsForProvisioningArtifactResponse_serviceActionSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listServiceActionsForProvisioningArtifact_pageToken
          Lens..~ rs
            Lens.^? listServiceActionsForProvisioningArtifactResponse_nextPageToken
              Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    ListServiceActionsForProvisioningArtifact
  where
  type
    Rs ListServiceActionsForProvisioningArtifact =
      ListServiceActionsForProvisioningArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceActionsForProvisioningArtifactResponse'
            Prelude.<$> (x Prelude..?> "NextPageToken")
              Prelude.<*> ( x Prelude..?> "ServiceActionSummaries"
                              Prelude..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListServiceActionsForProvisioningArtifact

instance
  Prelude.NFData
    ListServiceActionsForProvisioningArtifact

instance
  Prelude.ToHeaders
    ListServiceActionsForProvisioningArtifact
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.ListServiceActionsForProvisioningArtifact" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    ListServiceActionsForProvisioningArtifact
  where
  toJSON ListServiceActionsForProvisioningArtifact' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PageSize" Prelude..=) Prelude.<$> pageSize,
            ("PageToken" Prelude..=) Prelude.<$> pageToken,
            ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("ProductId" Prelude..= productId),
            Prelude.Just
              ( "ProvisioningArtifactId"
                  Prelude..= provisioningArtifactId
              )
          ]
      )

instance
  Prelude.ToPath
    ListServiceActionsForProvisioningArtifact
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ListServiceActionsForProvisioningArtifact
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServiceActionsForProvisioningArtifactResponse' smart constructor.
data ListServiceActionsForProvisioningArtifactResponse = ListServiceActionsForProvisioningArtifactResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An object containing information about the self-service actions
    -- associated with the provisioning artifact.
    serviceActionSummaries :: Prelude.Maybe [ServiceActionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListServiceActionsForProvisioningArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listServiceActionsForProvisioningArtifactResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'serviceActionSummaries', 'listServiceActionsForProvisioningArtifactResponse_serviceActionSummaries' - An object containing information about the self-service actions
-- associated with the provisioning artifact.
--
-- 'httpStatus', 'listServiceActionsForProvisioningArtifactResponse_httpStatus' - The response's http status code.
newListServiceActionsForProvisioningArtifactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServiceActionsForProvisioningArtifactResponse
newListServiceActionsForProvisioningArtifactResponse
  pHttpStatus_ =
    ListServiceActionsForProvisioningArtifactResponse'
      { nextPageToken =
          Prelude.Nothing,
        serviceActionSummaries =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listServiceActionsForProvisioningArtifactResponse_nextPageToken :: Lens.Lens' ListServiceActionsForProvisioningArtifactResponse (Prelude.Maybe Prelude.Text)
listServiceActionsForProvisioningArtifactResponse_nextPageToken = Lens.lens (\ListServiceActionsForProvisioningArtifactResponse' {nextPageToken} -> nextPageToken) (\s@ListServiceActionsForProvisioningArtifactResponse' {} a -> s {nextPageToken = a} :: ListServiceActionsForProvisioningArtifactResponse)

-- | An object containing information about the self-service actions
-- associated with the provisioning artifact.
listServiceActionsForProvisioningArtifactResponse_serviceActionSummaries :: Lens.Lens' ListServiceActionsForProvisioningArtifactResponse (Prelude.Maybe [ServiceActionSummary])
listServiceActionsForProvisioningArtifactResponse_serviceActionSummaries = Lens.lens (\ListServiceActionsForProvisioningArtifactResponse' {serviceActionSummaries} -> serviceActionSummaries) (\s@ListServiceActionsForProvisioningArtifactResponse' {} a -> s {serviceActionSummaries = a} :: ListServiceActionsForProvisioningArtifactResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listServiceActionsForProvisioningArtifactResponse_httpStatus :: Lens.Lens' ListServiceActionsForProvisioningArtifactResponse Prelude.Int
listServiceActionsForProvisioningArtifactResponse_httpStatus = Lens.lens (\ListServiceActionsForProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@ListServiceActionsForProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: ListServiceActionsForProvisioningArtifactResponse)

instance
  Prelude.NFData
    ListServiceActionsForProvisioningArtifactResponse
