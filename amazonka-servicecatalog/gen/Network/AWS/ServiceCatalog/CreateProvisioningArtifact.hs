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
-- Module      : Network.AWS.ServiceCatalog.CreateProvisioningArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.ServiceCatalog.CreateProvisioningArtifact
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
    createProvisioningArtifactResponse_status,
    createProvisioningArtifactResponse_info,
    createProvisioningArtifactResponse_provisioningArtifactDetail,
    createProvisioningArtifactResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newCreateProvisioningArtifact' smart constructor.
data CreateProvisioningArtifact = CreateProvisioningArtifact'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The product identifier.
    productId :: Core.Text,
    -- | The configuration for the provisioning artifact.
    parameters :: ProvisioningArtifactProperties,
    -- | A unique identifier that you provide to ensure idempotency. If multiple
    -- requests differ only by the idempotency token, the same response is
    -- returned for each repeated request.
    idempotencyToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'parameters'
  ProvisioningArtifactProperties ->
  -- | 'idempotencyToken'
  Core.Text ->
  CreateProvisioningArtifact
newCreateProvisioningArtifact
  pProductId_
  pParameters_
  pIdempotencyToken_ =
    CreateProvisioningArtifact'
      { acceptLanguage =
          Core.Nothing,
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
createProvisioningArtifact_acceptLanguage :: Lens.Lens' CreateProvisioningArtifact (Core.Maybe Core.Text)
createProvisioningArtifact_acceptLanguage = Lens.lens (\CreateProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@CreateProvisioningArtifact' {} a -> s {acceptLanguage = a} :: CreateProvisioningArtifact)

-- | The product identifier.
createProvisioningArtifact_productId :: Lens.Lens' CreateProvisioningArtifact Core.Text
createProvisioningArtifact_productId = Lens.lens (\CreateProvisioningArtifact' {productId} -> productId) (\s@CreateProvisioningArtifact' {} a -> s {productId = a} :: CreateProvisioningArtifact)

-- | The configuration for the provisioning artifact.
createProvisioningArtifact_parameters :: Lens.Lens' CreateProvisioningArtifact ProvisioningArtifactProperties
createProvisioningArtifact_parameters = Lens.lens (\CreateProvisioningArtifact' {parameters} -> parameters) (\s@CreateProvisioningArtifact' {} a -> s {parameters = a} :: CreateProvisioningArtifact)

-- | A unique identifier that you provide to ensure idempotency. If multiple
-- requests differ only by the idempotency token, the same response is
-- returned for each repeated request.
createProvisioningArtifact_idempotencyToken :: Lens.Lens' CreateProvisioningArtifact Core.Text
createProvisioningArtifact_idempotencyToken = Lens.lens (\CreateProvisioningArtifact' {idempotencyToken} -> idempotencyToken) (\s@CreateProvisioningArtifact' {} a -> s {idempotencyToken = a} :: CreateProvisioningArtifact)

instance Core.AWSRequest CreateProvisioningArtifact where
  type
    AWSResponse CreateProvisioningArtifact =
      CreateProvisioningArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProvisioningArtifactResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "Info" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ProvisioningArtifactDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateProvisioningArtifact

instance Core.NFData CreateProvisioningArtifact

instance Core.ToHeaders CreateProvisioningArtifact where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.CreateProvisioningArtifact" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateProvisioningArtifact where
  toJSON CreateProvisioningArtifact' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("ProductId" Core..= productId),
            Core.Just ("Parameters" Core..= parameters),
            Core.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath CreateProvisioningArtifact where
  toPath = Core.const "/"

instance Core.ToQuery CreateProvisioningArtifact where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateProvisioningArtifactResponse' smart constructor.
data CreateProvisioningArtifactResponse = CreateProvisioningArtifactResponse'
  { -- | The status of the current request.
    status :: Core.Maybe RequestStatus,
    -- | Specify the template source with one of the following options, but not
    -- both. Keys accepted: [ @LoadTemplateFromURL@, @ImportFromPhysicalId@ ].
    --
    -- The URL of the CloudFormation template in Amazon S3, in JSON format.
    --
    -- @LoadTemplateFromURL@
    --
    -- Use the URL of the CloudFormation template in Amazon S3 in JSON format.
    --
    -- @ImportFromPhysicalId@
    --
    -- Use the physical id of the resource that contains the template;
    -- currently supports CloudFormation stack ARN.
    info :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Information about the provisioning artifact.
    provisioningArtifactDetail :: Core.Maybe ProvisioningArtifactDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateProvisioningArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createProvisioningArtifactResponse_status' - The status of the current request.
--
-- 'info', 'createProvisioningArtifactResponse_info' - Specify the template source with one of the following options, but not
-- both. Keys accepted: [ @LoadTemplateFromURL@, @ImportFromPhysicalId@ ].
--
-- The URL of the CloudFormation template in Amazon S3, in JSON format.
--
-- @LoadTemplateFromURL@
--
-- Use the URL of the CloudFormation template in Amazon S3 in JSON format.
--
-- @ImportFromPhysicalId@
--
-- Use the physical id of the resource that contains the template;
-- currently supports CloudFormation stack ARN.
--
-- 'provisioningArtifactDetail', 'createProvisioningArtifactResponse_provisioningArtifactDetail' - Information about the provisioning artifact.
--
-- 'httpStatus', 'createProvisioningArtifactResponse_httpStatus' - The response's http status code.
newCreateProvisioningArtifactResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateProvisioningArtifactResponse
newCreateProvisioningArtifactResponse pHttpStatus_ =
  CreateProvisioningArtifactResponse'
    { status =
        Core.Nothing,
      info = Core.Nothing,
      provisioningArtifactDetail =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the current request.
createProvisioningArtifactResponse_status :: Lens.Lens' CreateProvisioningArtifactResponse (Core.Maybe RequestStatus)
createProvisioningArtifactResponse_status = Lens.lens (\CreateProvisioningArtifactResponse' {status} -> status) (\s@CreateProvisioningArtifactResponse' {} a -> s {status = a} :: CreateProvisioningArtifactResponse)

-- | Specify the template source with one of the following options, but not
-- both. Keys accepted: [ @LoadTemplateFromURL@, @ImportFromPhysicalId@ ].
--
-- The URL of the CloudFormation template in Amazon S3, in JSON format.
--
-- @LoadTemplateFromURL@
--
-- Use the URL of the CloudFormation template in Amazon S3 in JSON format.
--
-- @ImportFromPhysicalId@
--
-- Use the physical id of the resource that contains the template;
-- currently supports CloudFormation stack ARN.
createProvisioningArtifactResponse_info :: Lens.Lens' CreateProvisioningArtifactResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
createProvisioningArtifactResponse_info = Lens.lens (\CreateProvisioningArtifactResponse' {info} -> info) (\s@CreateProvisioningArtifactResponse' {} a -> s {info = a} :: CreateProvisioningArtifactResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the provisioning artifact.
createProvisioningArtifactResponse_provisioningArtifactDetail :: Lens.Lens' CreateProvisioningArtifactResponse (Core.Maybe ProvisioningArtifactDetail)
createProvisioningArtifactResponse_provisioningArtifactDetail = Lens.lens (\CreateProvisioningArtifactResponse' {provisioningArtifactDetail} -> provisioningArtifactDetail) (\s@CreateProvisioningArtifactResponse' {} a -> s {provisioningArtifactDetail = a} :: CreateProvisioningArtifactResponse)

-- | The response's http status code.
createProvisioningArtifactResponse_httpStatus :: Lens.Lens' CreateProvisioningArtifactResponse Core.Int
createProvisioningArtifactResponse_httpStatus = Lens.lens (\CreateProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@CreateProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: CreateProvisioningArtifactResponse)

instance
  Core.NFData
    CreateProvisioningArtifactResponse
