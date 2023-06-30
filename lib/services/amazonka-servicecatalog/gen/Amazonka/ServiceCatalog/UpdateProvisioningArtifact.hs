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
-- Module      : Amazonka.ServiceCatalog.UpdateProvisioningArtifact
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.ServiceCatalog.UpdateProvisioningArtifact
  ( -- * Creating a Request
    UpdateProvisioningArtifact (..),
    newUpdateProvisioningArtifact,

    -- * Request Lenses
    updateProvisioningArtifact_acceptLanguage,
    updateProvisioningArtifact_active,
    updateProvisioningArtifact_description,
    updateProvisioningArtifact_guidance,
    updateProvisioningArtifact_name,
    updateProvisioningArtifact_productId,
    updateProvisioningArtifact_provisioningArtifactId,

    -- * Destructuring the Response
    UpdateProvisioningArtifactResponse (..),
    newUpdateProvisioningArtifactResponse,

    -- * Response Lenses
    updateProvisioningArtifactResponse_info,
    updateProvisioningArtifactResponse_provisioningArtifactDetail,
    updateProvisioningArtifactResponse_status,
    updateProvisioningArtifactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newUpdateProvisioningArtifact' smart constructor.
data UpdateProvisioningArtifact = UpdateProvisioningArtifact'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the product version is active.
    --
    -- Inactive provisioning artifacts are invisible to end users. End users
    -- cannot launch or update a provisioned product from an inactive
    -- provisioning artifact.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The updated description of the provisioning artifact.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information set by the administrator to provide guidance to end users
    -- about which provisioning artifacts to use.
    --
    -- The @DEFAULT@ value indicates that the product version is active.
    --
    -- The administrator can set the guidance to @DEPRECATED@ to inform users
    -- that the product version is deprecated. Users are able to make updates
    -- to a provisioned product of a deprecated version but cannot launch new
    -- provisioned products using a deprecated version.
    guidance :: Prelude.Maybe ProvisioningArtifactGuidance,
    -- | The updated name of the provisioning artifact.
    name :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'updateProvisioningArtifact_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'active', 'updateProvisioningArtifact_active' - Indicates whether the product version is active.
--
-- Inactive provisioning artifacts are invisible to end users. End users
-- cannot launch or update a provisioned product from an inactive
-- provisioning artifact.
--
-- 'description', 'updateProvisioningArtifact_description' - The updated description of the provisioning artifact.
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
-- 'productId', 'updateProvisioningArtifact_productId' - The product identifier.
--
-- 'provisioningArtifactId', 'updateProvisioningArtifact_provisioningArtifactId' - The identifier of the provisioning artifact.
newUpdateProvisioningArtifact ::
  -- | 'productId'
  Prelude.Text ->
  -- | 'provisioningArtifactId'
  Prelude.Text ->
  UpdateProvisioningArtifact
newUpdateProvisioningArtifact
  pProductId_
  pProvisioningArtifactId_ =
    UpdateProvisioningArtifact'
      { acceptLanguage =
          Prelude.Nothing,
        active = Prelude.Nothing,
        description = Prelude.Nothing,
        guidance = Prelude.Nothing,
        name = Prelude.Nothing,
        productId = pProductId_,
        provisioningArtifactId =
          pProvisioningArtifactId_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updateProvisioningArtifact_acceptLanguage :: Lens.Lens' UpdateProvisioningArtifact (Prelude.Maybe Prelude.Text)
updateProvisioningArtifact_acceptLanguage = Lens.lens (\UpdateProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@UpdateProvisioningArtifact' {} a -> s {acceptLanguage = a} :: UpdateProvisioningArtifact)

-- | Indicates whether the product version is active.
--
-- Inactive provisioning artifacts are invisible to end users. End users
-- cannot launch or update a provisioned product from an inactive
-- provisioning artifact.
updateProvisioningArtifact_active :: Lens.Lens' UpdateProvisioningArtifact (Prelude.Maybe Prelude.Bool)
updateProvisioningArtifact_active = Lens.lens (\UpdateProvisioningArtifact' {active} -> active) (\s@UpdateProvisioningArtifact' {} a -> s {active = a} :: UpdateProvisioningArtifact)

-- | The updated description of the provisioning artifact.
updateProvisioningArtifact_description :: Lens.Lens' UpdateProvisioningArtifact (Prelude.Maybe Prelude.Text)
updateProvisioningArtifact_description = Lens.lens (\UpdateProvisioningArtifact' {description} -> description) (\s@UpdateProvisioningArtifact' {} a -> s {description = a} :: UpdateProvisioningArtifact)

-- | Information set by the administrator to provide guidance to end users
-- about which provisioning artifacts to use.
--
-- The @DEFAULT@ value indicates that the product version is active.
--
-- The administrator can set the guidance to @DEPRECATED@ to inform users
-- that the product version is deprecated. Users are able to make updates
-- to a provisioned product of a deprecated version but cannot launch new
-- provisioned products using a deprecated version.
updateProvisioningArtifact_guidance :: Lens.Lens' UpdateProvisioningArtifact (Prelude.Maybe ProvisioningArtifactGuidance)
updateProvisioningArtifact_guidance = Lens.lens (\UpdateProvisioningArtifact' {guidance} -> guidance) (\s@UpdateProvisioningArtifact' {} a -> s {guidance = a} :: UpdateProvisioningArtifact)

-- | The updated name of the provisioning artifact.
updateProvisioningArtifact_name :: Lens.Lens' UpdateProvisioningArtifact (Prelude.Maybe Prelude.Text)
updateProvisioningArtifact_name = Lens.lens (\UpdateProvisioningArtifact' {name} -> name) (\s@UpdateProvisioningArtifact' {} a -> s {name = a} :: UpdateProvisioningArtifact)

-- | The product identifier.
updateProvisioningArtifact_productId :: Lens.Lens' UpdateProvisioningArtifact Prelude.Text
updateProvisioningArtifact_productId = Lens.lens (\UpdateProvisioningArtifact' {productId} -> productId) (\s@UpdateProvisioningArtifact' {} a -> s {productId = a} :: UpdateProvisioningArtifact)

-- | The identifier of the provisioning artifact.
updateProvisioningArtifact_provisioningArtifactId :: Lens.Lens' UpdateProvisioningArtifact Prelude.Text
updateProvisioningArtifact_provisioningArtifactId = Lens.lens (\UpdateProvisioningArtifact' {provisioningArtifactId} -> provisioningArtifactId) (\s@UpdateProvisioningArtifact' {} a -> s {provisioningArtifactId = a} :: UpdateProvisioningArtifact)

instance Core.AWSRequest UpdateProvisioningArtifact where
  type
    AWSResponse UpdateProvisioningArtifact =
      UpdateProvisioningArtifactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProvisioningArtifactResponse'
            Prelude.<$> (x Data..?> "Info" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ProvisioningArtifactDetail")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProvisioningArtifact where
  hashWithSalt _salt UpdateProvisioningArtifact' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` guidance
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` provisioningArtifactId

instance Prelude.NFData UpdateProvisioningArtifact where
  rnf UpdateProvisioningArtifact' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf active
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf guidance
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf provisioningArtifactId

instance Data.ToHeaders UpdateProvisioningArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.UpdateProvisioningArtifact" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProvisioningArtifact where
  toJSON UpdateProvisioningArtifact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("Active" Data..=) Prelude.<$> active,
            ("Description" Data..=) Prelude.<$> description,
            ("Guidance" Data..=) Prelude.<$> guidance,
            ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("ProductId" Data..= productId),
            Prelude.Just
              ( "ProvisioningArtifactId"
                  Data..= provisioningArtifactId
              )
          ]
      )

instance Data.ToPath UpdateProvisioningArtifact where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateProvisioningArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProvisioningArtifactResponse' smart constructor.
data UpdateProvisioningArtifactResponse = UpdateProvisioningArtifactResponse'
  { -- | The URL of the CloudFormation template in Amazon S3 or GitHub in JSON
    -- format.
    info :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Information about the provisioning artifact.
    provisioningArtifactDetail :: Prelude.Maybe ProvisioningArtifactDetail,
    -- | The status of the current request.
    status :: Prelude.Maybe RequestStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProvisioningArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'info', 'updateProvisioningArtifactResponse_info' - The URL of the CloudFormation template in Amazon S3 or GitHub in JSON
-- format.
--
-- 'provisioningArtifactDetail', 'updateProvisioningArtifactResponse_provisioningArtifactDetail' - Information about the provisioning artifact.
--
-- 'status', 'updateProvisioningArtifactResponse_status' - The status of the current request.
--
-- 'httpStatus', 'updateProvisioningArtifactResponse_httpStatus' - The response's http status code.
newUpdateProvisioningArtifactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProvisioningArtifactResponse
newUpdateProvisioningArtifactResponse pHttpStatus_ =
  UpdateProvisioningArtifactResponse'
    { info =
        Prelude.Nothing,
      provisioningArtifactDetail =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL of the CloudFormation template in Amazon S3 or GitHub in JSON
-- format.
updateProvisioningArtifactResponse_info :: Lens.Lens' UpdateProvisioningArtifactResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateProvisioningArtifactResponse_info = Lens.lens (\UpdateProvisioningArtifactResponse' {info} -> info) (\s@UpdateProvisioningArtifactResponse' {} a -> s {info = a} :: UpdateProvisioningArtifactResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the provisioning artifact.
updateProvisioningArtifactResponse_provisioningArtifactDetail :: Lens.Lens' UpdateProvisioningArtifactResponse (Prelude.Maybe ProvisioningArtifactDetail)
updateProvisioningArtifactResponse_provisioningArtifactDetail = Lens.lens (\UpdateProvisioningArtifactResponse' {provisioningArtifactDetail} -> provisioningArtifactDetail) (\s@UpdateProvisioningArtifactResponse' {} a -> s {provisioningArtifactDetail = a} :: UpdateProvisioningArtifactResponse)

-- | The status of the current request.
updateProvisioningArtifactResponse_status :: Lens.Lens' UpdateProvisioningArtifactResponse (Prelude.Maybe RequestStatus)
updateProvisioningArtifactResponse_status = Lens.lens (\UpdateProvisioningArtifactResponse' {status} -> status) (\s@UpdateProvisioningArtifactResponse' {} a -> s {status = a} :: UpdateProvisioningArtifactResponse)

-- | The response's http status code.
updateProvisioningArtifactResponse_httpStatus :: Lens.Lens' UpdateProvisioningArtifactResponse Prelude.Int
updateProvisioningArtifactResponse_httpStatus = Lens.lens (\UpdateProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@UpdateProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: UpdateProvisioningArtifactResponse)

instance
  Prelude.NFData
    UpdateProvisioningArtifactResponse
  where
  rnf UpdateProvisioningArtifactResponse' {..} =
    Prelude.rnf info
      `Prelude.seq` Prelude.rnf provisioningArtifactDetail
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
