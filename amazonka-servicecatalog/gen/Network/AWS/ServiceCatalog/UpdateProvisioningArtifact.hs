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
-- Module      : Network.AWS.ServiceCatalog.UpdateProvisioningArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified provisioning artifact (also known as a version)
-- for the specified product.
--
-- You cannot update a provisioning artifact for a product that was shared
-- with you.
module Network.AWS.ServiceCatalog.UpdateProvisioningArtifact
  ( -- * Creating a Request
    UpdateProvisioningArtifact (..),
    newUpdateProvisioningArtifact,

    -- * Request Lenses
    updateProvisioningArtifact_guidance,
    updateProvisioningArtifact_name,
    updateProvisioningArtifact_active,
    updateProvisioningArtifact_description,
    updateProvisioningArtifact_acceptLanguage,
    updateProvisioningArtifact_productId,
    updateProvisioningArtifact_provisioningArtifactId,

    -- * Destructuring the Response
    UpdateProvisioningArtifactResponse (..),
    newUpdateProvisioningArtifactResponse,

    -- * Response Lenses
    updateProvisioningArtifactResponse_status,
    updateProvisioningArtifactResponse_info,
    updateProvisioningArtifactResponse_provisioningArtifactDetail,
    updateProvisioningArtifactResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newUpdateProvisioningArtifact' smart constructor.
data UpdateProvisioningArtifact = UpdateProvisioningArtifact'
  { -- | Information set by the administrator to provide guidance to end users
    -- about which provisioning artifacts to use.
    --
    -- The @DEFAULT@ value indicates that the product version is active.
    --
    -- The administrator can set the guidance to @DEPRECATED@ to inform users
    -- that the product version is deprecated. Users are able to make updates
    -- to a provisioned product of a deprecated version but cannot launch new
    -- provisioned products using a deprecated version.
    guidance :: Core.Maybe ProvisioningArtifactGuidance,
    -- | The updated name of the provisioning artifact.
    name :: Core.Maybe Core.Text,
    -- | Indicates whether the product version is active.
    --
    -- Inactive provisioning artifacts are invisible to end users. End users
    -- cannot launch or update a provisioned product from an inactive
    -- provisioning artifact.
    active :: Core.Maybe Core.Bool,
    -- | The updated description of the provisioning artifact.
    description :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The product identifier.
    productId :: Core.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'guidance', 'updateProvisioningArtifact_guidance' - Information set by the administrator to provide guidance to end users
-- about which provisioning artifacts to use.
--
-- The @DEFAULT@ value indicates that the product version is active.
--
-- The administrator can set the guidance to @DEPRECATED@ to inform users
-- that the product version is deprecated. Users are able to make updates
-- to a provisioned product of a deprecated version but cannot launch new
-- provisioned products using a deprecated version.
--
-- 'name', 'updateProvisioningArtifact_name' - The updated name of the provisioning artifact.
--
-- 'active', 'updateProvisioningArtifact_active' - Indicates whether the product version is active.
--
-- Inactive provisioning artifacts are invisible to end users. End users
-- cannot launch or update a provisioned product from an inactive
-- provisioning artifact.
--
-- 'description', 'updateProvisioningArtifact_description' - The updated description of the provisioning artifact.
--
-- 'acceptLanguage', 'updateProvisioningArtifact_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'updateProvisioningArtifact_productId' - The product identifier.
--
-- 'provisioningArtifactId', 'updateProvisioningArtifact_provisioningArtifactId' - The identifier of the provisioning artifact.
newUpdateProvisioningArtifact ::
  -- | 'productId'
  Core.Text ->
  -- | 'provisioningArtifactId'
  Core.Text ->
  UpdateProvisioningArtifact
newUpdateProvisioningArtifact
  pProductId_
  pProvisioningArtifactId_ =
    UpdateProvisioningArtifact'
      { guidance =
          Core.Nothing,
        name = Core.Nothing,
        active = Core.Nothing,
        description = Core.Nothing,
        acceptLanguage = Core.Nothing,
        productId = pProductId_,
        provisioningArtifactId =
          pProvisioningArtifactId_
      }

-- | Information set by the administrator to provide guidance to end users
-- about which provisioning artifacts to use.
--
-- The @DEFAULT@ value indicates that the product version is active.
--
-- The administrator can set the guidance to @DEPRECATED@ to inform users
-- that the product version is deprecated. Users are able to make updates
-- to a provisioned product of a deprecated version but cannot launch new
-- provisioned products using a deprecated version.
updateProvisioningArtifact_guidance :: Lens.Lens' UpdateProvisioningArtifact (Core.Maybe ProvisioningArtifactGuidance)
updateProvisioningArtifact_guidance = Lens.lens (\UpdateProvisioningArtifact' {guidance} -> guidance) (\s@UpdateProvisioningArtifact' {} a -> s {guidance = a} :: UpdateProvisioningArtifact)

-- | The updated name of the provisioning artifact.
updateProvisioningArtifact_name :: Lens.Lens' UpdateProvisioningArtifact (Core.Maybe Core.Text)
updateProvisioningArtifact_name = Lens.lens (\UpdateProvisioningArtifact' {name} -> name) (\s@UpdateProvisioningArtifact' {} a -> s {name = a} :: UpdateProvisioningArtifact)

-- | Indicates whether the product version is active.
--
-- Inactive provisioning artifacts are invisible to end users. End users
-- cannot launch or update a provisioned product from an inactive
-- provisioning artifact.
updateProvisioningArtifact_active :: Lens.Lens' UpdateProvisioningArtifact (Core.Maybe Core.Bool)
updateProvisioningArtifact_active = Lens.lens (\UpdateProvisioningArtifact' {active} -> active) (\s@UpdateProvisioningArtifact' {} a -> s {active = a} :: UpdateProvisioningArtifact)

-- | The updated description of the provisioning artifact.
updateProvisioningArtifact_description :: Lens.Lens' UpdateProvisioningArtifact (Core.Maybe Core.Text)
updateProvisioningArtifact_description = Lens.lens (\UpdateProvisioningArtifact' {description} -> description) (\s@UpdateProvisioningArtifact' {} a -> s {description = a} :: UpdateProvisioningArtifact)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updateProvisioningArtifact_acceptLanguage :: Lens.Lens' UpdateProvisioningArtifact (Core.Maybe Core.Text)
updateProvisioningArtifact_acceptLanguage = Lens.lens (\UpdateProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@UpdateProvisioningArtifact' {} a -> s {acceptLanguage = a} :: UpdateProvisioningArtifact)

-- | The product identifier.
updateProvisioningArtifact_productId :: Lens.Lens' UpdateProvisioningArtifact Core.Text
updateProvisioningArtifact_productId = Lens.lens (\UpdateProvisioningArtifact' {productId} -> productId) (\s@UpdateProvisioningArtifact' {} a -> s {productId = a} :: UpdateProvisioningArtifact)

-- | The identifier of the provisioning artifact.
updateProvisioningArtifact_provisioningArtifactId :: Lens.Lens' UpdateProvisioningArtifact Core.Text
updateProvisioningArtifact_provisioningArtifactId = Lens.lens (\UpdateProvisioningArtifact' {provisioningArtifactId} -> provisioningArtifactId) (\s@UpdateProvisioningArtifact' {} a -> s {provisioningArtifactId = a} :: UpdateProvisioningArtifact)

instance Core.AWSRequest UpdateProvisioningArtifact where
  type
    AWSResponse UpdateProvisioningArtifact =
      UpdateProvisioningArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProvisioningArtifactResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "Info" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ProvisioningArtifactDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateProvisioningArtifact

instance Core.NFData UpdateProvisioningArtifact

instance Core.ToHeaders UpdateProvisioningArtifact where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.UpdateProvisioningArtifact" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateProvisioningArtifact where
  toJSON UpdateProvisioningArtifact' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Guidance" Core..=) Core.<$> guidance,
            ("Name" Core..=) Core.<$> name,
            ("Active" Core..=) Core.<$> active,
            ("Description" Core..=) Core.<$> description,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("ProductId" Core..= productId),
            Core.Just
              ( "ProvisioningArtifactId"
                  Core..= provisioningArtifactId
              )
          ]
      )

instance Core.ToPath UpdateProvisioningArtifact where
  toPath = Core.const "/"

instance Core.ToQuery UpdateProvisioningArtifact where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateProvisioningArtifactResponse' smart constructor.
data UpdateProvisioningArtifactResponse = UpdateProvisioningArtifactResponse'
  { -- | The status of the current request.
    status :: Core.Maybe RequestStatus,
    -- | The URL of the CloudFormation template in Amazon S3.
    info :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Information about the provisioning artifact.
    provisioningArtifactDetail :: Core.Maybe ProvisioningArtifactDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateProvisioningArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'updateProvisioningArtifactResponse_status' - The status of the current request.
--
-- 'info', 'updateProvisioningArtifactResponse_info' - The URL of the CloudFormation template in Amazon S3.
--
-- 'provisioningArtifactDetail', 'updateProvisioningArtifactResponse_provisioningArtifactDetail' - Information about the provisioning artifact.
--
-- 'httpStatus', 'updateProvisioningArtifactResponse_httpStatus' - The response's http status code.
newUpdateProvisioningArtifactResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateProvisioningArtifactResponse
newUpdateProvisioningArtifactResponse pHttpStatus_ =
  UpdateProvisioningArtifactResponse'
    { status =
        Core.Nothing,
      info = Core.Nothing,
      provisioningArtifactDetail =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the current request.
updateProvisioningArtifactResponse_status :: Lens.Lens' UpdateProvisioningArtifactResponse (Core.Maybe RequestStatus)
updateProvisioningArtifactResponse_status = Lens.lens (\UpdateProvisioningArtifactResponse' {status} -> status) (\s@UpdateProvisioningArtifactResponse' {} a -> s {status = a} :: UpdateProvisioningArtifactResponse)

-- | The URL of the CloudFormation template in Amazon S3.
updateProvisioningArtifactResponse_info :: Lens.Lens' UpdateProvisioningArtifactResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateProvisioningArtifactResponse_info = Lens.lens (\UpdateProvisioningArtifactResponse' {info} -> info) (\s@UpdateProvisioningArtifactResponse' {} a -> s {info = a} :: UpdateProvisioningArtifactResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the provisioning artifact.
updateProvisioningArtifactResponse_provisioningArtifactDetail :: Lens.Lens' UpdateProvisioningArtifactResponse (Core.Maybe ProvisioningArtifactDetail)
updateProvisioningArtifactResponse_provisioningArtifactDetail = Lens.lens (\UpdateProvisioningArtifactResponse' {provisioningArtifactDetail} -> provisioningArtifactDetail) (\s@UpdateProvisioningArtifactResponse' {} a -> s {provisioningArtifactDetail = a} :: UpdateProvisioningArtifactResponse)

-- | The response's http status code.
updateProvisioningArtifactResponse_httpStatus :: Lens.Lens' UpdateProvisioningArtifactResponse Core.Int
updateProvisioningArtifactResponse_httpStatus = Lens.lens (\UpdateProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@UpdateProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: UpdateProvisioningArtifactResponse)

instance
  Core.NFData
    UpdateProvisioningArtifactResponse
