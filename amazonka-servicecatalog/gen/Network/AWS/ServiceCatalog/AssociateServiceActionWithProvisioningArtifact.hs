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
-- Module      : Network.AWS.ServiceCatalog.AssociateServiceActionWithProvisioningArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a self-service action with a provisioning artifact.
module Network.AWS.ServiceCatalog.AssociateServiceActionWithProvisioningArtifact
  ( -- * Creating a Request
    AssociateServiceActionWithProvisioningArtifact (..),
    newAssociateServiceActionWithProvisioningArtifact,

    -- * Request Lenses
    associateServiceActionWithProvisioningArtifact_acceptLanguage,
    associateServiceActionWithProvisioningArtifact_productId,
    associateServiceActionWithProvisioningArtifact_provisioningArtifactId,
    associateServiceActionWithProvisioningArtifact_serviceActionId,

    -- * Destructuring the Response
    AssociateServiceActionWithProvisioningArtifactResponse (..),
    newAssociateServiceActionWithProvisioningArtifactResponse,

    -- * Response Lenses
    associateServiceActionWithProvisioningArtifactResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newAssociateServiceActionWithProvisioningArtifact' smart constructor.
data AssociateServiceActionWithProvisioningArtifact = AssociateServiceActionWithProvisioningArtifact'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The product identifier. For example, @prod-abcdzk7xy33qa@.
    productId :: Core.Text,
    -- | The identifier of the provisioning artifact. For example,
    -- @pa-4abcdjnxjj6ne@.
    provisioningArtifactId :: Core.Text,
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
    serviceActionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateServiceActionWithProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'associateServiceActionWithProvisioningArtifact_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'associateServiceActionWithProvisioningArtifact_productId' - The product identifier. For example, @prod-abcdzk7xy33qa@.
--
-- 'provisioningArtifactId', 'associateServiceActionWithProvisioningArtifact_provisioningArtifactId' - The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
--
-- 'serviceActionId', 'associateServiceActionWithProvisioningArtifact_serviceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@.
newAssociateServiceActionWithProvisioningArtifact ::
  -- | 'productId'
  Core.Text ->
  -- | 'provisioningArtifactId'
  Core.Text ->
  -- | 'serviceActionId'
  Core.Text ->
  AssociateServiceActionWithProvisioningArtifact
newAssociateServiceActionWithProvisioningArtifact
  pProductId_
  pProvisioningArtifactId_
  pServiceActionId_ =
    AssociateServiceActionWithProvisioningArtifact'
      { acceptLanguage =
          Core.Nothing,
        productId = pProductId_,
        provisioningArtifactId =
          pProvisioningArtifactId_,
        serviceActionId =
          pServiceActionId_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
associateServiceActionWithProvisioningArtifact_acceptLanguage :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact (Core.Maybe Core.Text)
associateServiceActionWithProvisioningArtifact_acceptLanguage = Lens.lens (\AssociateServiceActionWithProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@AssociateServiceActionWithProvisioningArtifact' {} a -> s {acceptLanguage = a} :: AssociateServiceActionWithProvisioningArtifact)

-- | The product identifier. For example, @prod-abcdzk7xy33qa@.
associateServiceActionWithProvisioningArtifact_productId :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact Core.Text
associateServiceActionWithProvisioningArtifact_productId = Lens.lens (\AssociateServiceActionWithProvisioningArtifact' {productId} -> productId) (\s@AssociateServiceActionWithProvisioningArtifact' {} a -> s {productId = a} :: AssociateServiceActionWithProvisioningArtifact)

-- | The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
associateServiceActionWithProvisioningArtifact_provisioningArtifactId :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact Core.Text
associateServiceActionWithProvisioningArtifact_provisioningArtifactId = Lens.lens (\AssociateServiceActionWithProvisioningArtifact' {provisioningArtifactId} -> provisioningArtifactId) (\s@AssociateServiceActionWithProvisioningArtifact' {} a -> s {provisioningArtifactId = a} :: AssociateServiceActionWithProvisioningArtifact)

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
associateServiceActionWithProvisioningArtifact_serviceActionId :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact Core.Text
associateServiceActionWithProvisioningArtifact_serviceActionId = Lens.lens (\AssociateServiceActionWithProvisioningArtifact' {serviceActionId} -> serviceActionId) (\s@AssociateServiceActionWithProvisioningArtifact' {} a -> s {serviceActionId = a} :: AssociateServiceActionWithProvisioningArtifact)

instance
  Core.AWSRequest
    AssociateServiceActionWithProvisioningArtifact
  where
  type
    AWSResponse
      AssociateServiceActionWithProvisioningArtifact =
      AssociateServiceActionWithProvisioningArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateServiceActionWithProvisioningArtifactResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AssociateServiceActionWithProvisioningArtifact

instance
  Core.NFData
    AssociateServiceActionWithProvisioningArtifact

instance
  Core.ToHeaders
    AssociateServiceActionWithProvisioningArtifact
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.AssociateServiceActionWithProvisioningArtifact" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    AssociateServiceActionWithProvisioningArtifact
  where
  toJSON
    AssociateServiceActionWithProvisioningArtifact' {..} =
      Core.object
        ( Core.catMaybes
            [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
              Core.Just ("ProductId" Core..= productId),
              Core.Just
                ( "ProvisioningArtifactId"
                    Core..= provisioningArtifactId
                ),
              Core.Just
                ("ServiceActionId" Core..= serviceActionId)
            ]
        )

instance
  Core.ToPath
    AssociateServiceActionWithProvisioningArtifact
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    AssociateServiceActionWithProvisioningArtifact
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateServiceActionWithProvisioningArtifactResponse' smart constructor.
data AssociateServiceActionWithProvisioningArtifactResponse = AssociateServiceActionWithProvisioningArtifactResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateServiceActionWithProvisioningArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateServiceActionWithProvisioningArtifactResponse_httpStatus' - The response's http status code.
newAssociateServiceActionWithProvisioningArtifactResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateServiceActionWithProvisioningArtifactResponse
newAssociateServiceActionWithProvisioningArtifactResponse
  pHttpStatus_ =
    AssociateServiceActionWithProvisioningArtifactResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateServiceActionWithProvisioningArtifactResponse_httpStatus :: Lens.Lens' AssociateServiceActionWithProvisioningArtifactResponse Core.Int
associateServiceActionWithProvisioningArtifactResponse_httpStatus = Lens.lens (\AssociateServiceActionWithProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@AssociateServiceActionWithProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: AssociateServiceActionWithProvisioningArtifactResponse)

instance
  Core.NFData
    AssociateServiceActionWithProvisioningArtifactResponse
