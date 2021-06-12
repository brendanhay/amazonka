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
-- Module      : Network.AWS.ServiceCatalog.ListProvisioningArtifacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all provisioning artifacts (also known as versions) for the
-- specified product.
module Network.AWS.ServiceCatalog.ListProvisioningArtifacts
  ( -- * Creating a Request
    ListProvisioningArtifacts (..),
    newListProvisioningArtifacts,

    -- * Request Lenses
    listProvisioningArtifacts_acceptLanguage,
    listProvisioningArtifacts_productId,

    -- * Destructuring the Response
    ListProvisioningArtifactsResponse (..),
    newListProvisioningArtifactsResponse,

    -- * Response Lenses
    listProvisioningArtifactsResponse_nextPageToken,
    listProvisioningArtifactsResponse_provisioningArtifactDetails,
    listProvisioningArtifactsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListProvisioningArtifacts' smart constructor.
data ListProvisioningArtifacts = ListProvisioningArtifacts'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The product identifier.
    productId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProvisioningArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'listProvisioningArtifacts_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'listProvisioningArtifacts_productId' - The product identifier.
newListProvisioningArtifacts ::
  -- | 'productId'
  Core.Text ->
  ListProvisioningArtifacts
newListProvisioningArtifacts pProductId_ =
  ListProvisioningArtifacts'
    { acceptLanguage =
        Core.Nothing,
      productId = pProductId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listProvisioningArtifacts_acceptLanguage :: Lens.Lens' ListProvisioningArtifacts (Core.Maybe Core.Text)
listProvisioningArtifacts_acceptLanguage = Lens.lens (\ListProvisioningArtifacts' {acceptLanguage} -> acceptLanguage) (\s@ListProvisioningArtifacts' {} a -> s {acceptLanguage = a} :: ListProvisioningArtifacts)

-- | The product identifier.
listProvisioningArtifacts_productId :: Lens.Lens' ListProvisioningArtifacts Core.Text
listProvisioningArtifacts_productId = Lens.lens (\ListProvisioningArtifacts' {productId} -> productId) (\s@ListProvisioningArtifacts' {} a -> s {productId = a} :: ListProvisioningArtifacts)

instance Core.AWSRequest ListProvisioningArtifacts where
  type
    AWSResponse ListProvisioningArtifacts =
      ListProvisioningArtifactsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisioningArtifactsResponse'
            Core.<$> (x Core..?> "NextPageToken")
            Core.<*> ( x Core..?> "ProvisioningArtifactDetails"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListProvisioningArtifacts

instance Core.NFData ListProvisioningArtifacts

instance Core.ToHeaders ListProvisioningArtifacts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListProvisioningArtifacts" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListProvisioningArtifacts where
  toJSON ListProvisioningArtifacts' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("ProductId" Core..= productId)
          ]
      )

instance Core.ToPath ListProvisioningArtifacts where
  toPath = Core.const "/"

instance Core.ToQuery ListProvisioningArtifacts where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListProvisioningArtifactsResponse' smart constructor.
data ListProvisioningArtifactsResponse = ListProvisioningArtifactsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | Information about the provisioning artifacts.
    provisioningArtifactDetails :: Core.Maybe [ProvisioningArtifactDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProvisioningArtifactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listProvisioningArtifactsResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'provisioningArtifactDetails', 'listProvisioningArtifactsResponse_provisioningArtifactDetails' - Information about the provisioning artifacts.
--
-- 'httpStatus', 'listProvisioningArtifactsResponse_httpStatus' - The response's http status code.
newListProvisioningArtifactsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListProvisioningArtifactsResponse
newListProvisioningArtifactsResponse pHttpStatus_ =
  ListProvisioningArtifactsResponse'
    { nextPageToken =
        Core.Nothing,
      provisioningArtifactDetails =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listProvisioningArtifactsResponse_nextPageToken :: Lens.Lens' ListProvisioningArtifactsResponse (Core.Maybe Core.Text)
listProvisioningArtifactsResponse_nextPageToken = Lens.lens (\ListProvisioningArtifactsResponse' {nextPageToken} -> nextPageToken) (\s@ListProvisioningArtifactsResponse' {} a -> s {nextPageToken = a} :: ListProvisioningArtifactsResponse)

-- | Information about the provisioning artifacts.
listProvisioningArtifactsResponse_provisioningArtifactDetails :: Lens.Lens' ListProvisioningArtifactsResponse (Core.Maybe [ProvisioningArtifactDetail])
listProvisioningArtifactsResponse_provisioningArtifactDetails = Lens.lens (\ListProvisioningArtifactsResponse' {provisioningArtifactDetails} -> provisioningArtifactDetails) (\s@ListProvisioningArtifactsResponse' {} a -> s {provisioningArtifactDetails = a} :: ListProvisioningArtifactsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listProvisioningArtifactsResponse_httpStatus :: Lens.Lens' ListProvisioningArtifactsResponse Core.Int
listProvisioningArtifactsResponse_httpStatus = Lens.lens (\ListProvisioningArtifactsResponse' {httpStatus} -> httpStatus) (\s@ListProvisioningArtifactsResponse' {} a -> s {httpStatus = a} :: ListProvisioningArtifactsResponse)

instance
  Core.NFData
    ListProvisioningArtifactsResponse
