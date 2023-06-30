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
-- Module      : Amazonka.ServiceCatalog.CreateProvisioningArtifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a provisioning artifact (also known as a version) for the
-- specified product.
--
-- You cannot create a provisioning artifact for a product that was shared
-- with you.
--
-- The user or role that performs this operation must have the
-- @cloudformation:GetTemplate@ IAM policy permission. This policy
-- permission is required when using the @ImportFromPhysicalId@ template
-- source in the information data section.
module Amazonka.ServiceCatalog.CreateProvisioningArtifact
  ( -- * Creating a Request
    CreateProvisioningArtifact (..),
    newCreateProvisioningArtifact,

    -- * Request Lenses
    createProvisioningArtifact_acceptLanguage,
    createProvisioningArtifact_productId,
    createProvisioningArtifact_parameters,
    createProvisioningArtifact_idempotencyToken,

    -- * Destructuring the Response
    CreateProvisioningArtifactResponse (..),
    newCreateProvisioningArtifactResponse,

    -- * Response Lenses
    createProvisioningArtifactResponse_info,
    createProvisioningArtifactResponse_provisioningArtifactDetail,
    createProvisioningArtifactResponse_status,
    createProvisioningArtifactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newCreateProvisioningArtifact' smart constructor.
data CreateProvisioningArtifact = CreateProvisioningArtifact'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Text,
    -- | The configuration for the provisioning artifact.
    parameters :: ProvisioningArtifactProperties,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'createProvisioningArtifact_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'createProvisioningArtifact_productId' - The product identifier.
--
-- 'parameters', 'createProvisioningArtifact_parameters' - The configuration for the provisioning artifact.
--
-- 'idempotencyToken', 'createProvisioningArtifact_idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
newCreateProvisioningArtifact ::
  -- | 'productId'
  Prelude.Text ->
  -- | 'parameters'
  ProvisioningArtifactProperties ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  CreateProvisioningArtifact
newCreateProvisioningArtifact
  pProductId_
  pParameters_
  pIdempotencyToken_ =
    CreateProvisioningArtifact'
      { acceptLanguage =
          Prelude.Nothing,
        productId = pProductId_,
        parameters = pParameters_,
        idempotencyToken = pIdempotencyToken_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
createProvisioningArtifact_acceptLanguage :: Lens.Lens' CreateProvisioningArtifact (Prelude.Maybe Prelude.Text)
createProvisioningArtifact_acceptLanguage = Lens.lens (\CreateProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@CreateProvisioningArtifact' {} a -> s {acceptLanguage = a} :: CreateProvisioningArtifact)

-- | The product identifier.
createProvisioningArtifact_productId :: Lens.Lens' CreateProvisioningArtifact Prelude.Text
createProvisioningArtifact_productId = Lens.lens (\CreateProvisioningArtifact' {productId} -> productId) (\s@CreateProvisioningArtifact' {} a -> s {productId = a} :: CreateProvisioningArtifact)

-- | The configuration for the provisioning artifact.
createProvisioningArtifact_parameters :: Lens.Lens' CreateProvisioningArtifact ProvisioningArtifactProperties
createProvisioningArtifact_parameters = Lens.lens (\CreateProvisioningArtifact' {parameters} -> parameters) (\s@CreateProvisioningArtifact' {} a -> s {parameters = a} :: CreateProvisioningArtifact)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
createProvisioningArtifact_idempotencyToken :: Lens.Lens' CreateProvisioningArtifact Prelude.Text
createProvisioningArtifact_idempotencyToken = Lens.lens (\CreateProvisioningArtifact' {idempotencyToken} -> idempotencyToken) (\s@CreateProvisioningArtifact' {} a -> s {idempotencyToken = a} :: CreateProvisioningArtifact)

instance Core.AWSRequest CreateProvisioningArtifact where
  type
    AWSResponse CreateProvisioningArtifact =
      CreateProvisioningArtifactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProvisioningArtifactResponse'
            Prelude.<$> (x Data..?> "Info" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ProvisioningArtifactDetail")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProvisioningArtifact where
  hashWithSalt _salt CreateProvisioningArtifact' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` idempotencyToken

instance Prelude.NFData CreateProvisioningArtifact where
  rnf CreateProvisioningArtifact' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf idempotencyToken

instance Data.ToHeaders CreateProvisioningArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.CreateProvisioningArtifact" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProvisioningArtifact where
  toJSON CreateProvisioningArtifact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("ProductId" Data..= productId),
            Prelude.Just ("Parameters" Data..= parameters),
            Prelude.Just
              ("IdempotencyToken" Data..= idempotencyToken)
          ]
      )

instance Data.ToPath CreateProvisioningArtifact where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateProvisioningArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProvisioningArtifactResponse' smart constructor.
data CreateProvisioningArtifactResponse = CreateProvisioningArtifactResponse'
  { -- | Specify the template source with one of the following options, but not
    -- both. Keys accepted: [ @LoadTemplateFromURL@, @ImportFromPhysicalId@ ].
    --
    -- Use the URL of the CloudFormation template in Amazon S3 or GitHub in
    -- JSON format.
    --
    -- @LoadTemplateFromURL@
    --
    -- Use the URL of the CloudFormation template in Amazon S3 or GitHub in
    -- JSON format.
    --
    -- @ImportFromPhysicalId@
    --
    -- Use the physical id of the resource that contains the template;
    -- currently supports CloudFormation stack ARN.
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
-- Create a value of 'CreateProvisioningArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'info', 'createProvisioningArtifactResponse_info' - Specify the template source with one of the following options, but not
-- both. Keys accepted: [ @LoadTemplateFromURL@, @ImportFromPhysicalId@ ].
--
-- Use the URL of the CloudFormation template in Amazon S3 or GitHub in
-- JSON format.
--
-- @LoadTemplateFromURL@
--
-- Use the URL of the CloudFormation template in Amazon S3 or GitHub in
-- JSON format.
--
-- @ImportFromPhysicalId@
--
-- Use the physical id of the resource that contains the template;
-- currently supports CloudFormation stack ARN.
--
-- 'provisioningArtifactDetail', 'createProvisioningArtifactResponse_provisioningArtifactDetail' - Information about the provisioning artifact.
--
-- 'status', 'createProvisioningArtifactResponse_status' - The status of the current request.
--
-- 'httpStatus', 'createProvisioningArtifactResponse_httpStatus' - The response's http status code.
newCreateProvisioningArtifactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProvisioningArtifactResponse
newCreateProvisioningArtifactResponse pHttpStatus_ =
  CreateProvisioningArtifactResponse'
    { info =
        Prelude.Nothing,
      provisioningArtifactDetail =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specify the template source with one of the following options, but not
-- both. Keys accepted: [ @LoadTemplateFromURL@, @ImportFromPhysicalId@ ].
--
-- Use the URL of the CloudFormation template in Amazon S3 or GitHub in
-- JSON format.
--
-- @LoadTemplateFromURL@
--
-- Use the URL of the CloudFormation template in Amazon S3 or GitHub in
-- JSON format.
--
-- @ImportFromPhysicalId@
--
-- Use the physical id of the resource that contains the template;
-- currently supports CloudFormation stack ARN.
createProvisioningArtifactResponse_info :: Lens.Lens' CreateProvisioningArtifactResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createProvisioningArtifactResponse_info = Lens.lens (\CreateProvisioningArtifactResponse' {info} -> info) (\s@CreateProvisioningArtifactResponse' {} a -> s {info = a} :: CreateProvisioningArtifactResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the provisioning artifact.
createProvisioningArtifactResponse_provisioningArtifactDetail :: Lens.Lens' CreateProvisioningArtifactResponse (Prelude.Maybe ProvisioningArtifactDetail)
createProvisioningArtifactResponse_provisioningArtifactDetail = Lens.lens (\CreateProvisioningArtifactResponse' {provisioningArtifactDetail} -> provisioningArtifactDetail) (\s@CreateProvisioningArtifactResponse' {} a -> s {provisioningArtifactDetail = a} :: CreateProvisioningArtifactResponse)

-- | The status of the current request.
createProvisioningArtifactResponse_status :: Lens.Lens' CreateProvisioningArtifactResponse (Prelude.Maybe RequestStatus)
createProvisioningArtifactResponse_status = Lens.lens (\CreateProvisioningArtifactResponse' {status} -> status) (\s@CreateProvisioningArtifactResponse' {} a -> s {status = a} :: CreateProvisioningArtifactResponse)

-- | The response's http status code.
createProvisioningArtifactResponse_httpStatus :: Lens.Lens' CreateProvisioningArtifactResponse Prelude.Int
createProvisioningArtifactResponse_httpStatus = Lens.lens (\CreateProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@CreateProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: CreateProvisioningArtifactResponse)

instance
  Prelude.NFData
    CreateProvisioningArtifactResponse
  where
  rnf CreateProvisioningArtifactResponse' {..} =
    Prelude.rnf info
      `Prelude.seq` Prelude.rnf provisioningArtifactDetail
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
