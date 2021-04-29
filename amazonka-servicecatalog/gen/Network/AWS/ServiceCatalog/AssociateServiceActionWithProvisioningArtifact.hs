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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The product identifier. For example, @prod-abcdzk7xy33qa@.
    productId :: Prelude.Text,
    -- | The identifier of the provisioning artifact. For example,
    -- @pa-4abcdjnxjj6ne@.
    provisioningArtifactId :: Prelude.Text,
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
    serviceActionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'provisioningArtifactId'
  Prelude.Text ->
  -- | 'serviceActionId'
  Prelude.Text ->
  AssociateServiceActionWithProvisioningArtifact
newAssociateServiceActionWithProvisioningArtifact
  pProductId_
  pProvisioningArtifactId_
  pServiceActionId_ =
    AssociateServiceActionWithProvisioningArtifact'
      { acceptLanguage =
          Prelude.Nothing,
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
associateServiceActionWithProvisioningArtifact_acceptLanguage :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact (Prelude.Maybe Prelude.Text)
associateServiceActionWithProvisioningArtifact_acceptLanguage = Lens.lens (\AssociateServiceActionWithProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@AssociateServiceActionWithProvisioningArtifact' {} a -> s {acceptLanguage = a} :: AssociateServiceActionWithProvisioningArtifact)

-- | The product identifier. For example, @prod-abcdzk7xy33qa@.
associateServiceActionWithProvisioningArtifact_productId :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact Prelude.Text
associateServiceActionWithProvisioningArtifact_productId = Lens.lens (\AssociateServiceActionWithProvisioningArtifact' {productId} -> productId) (\s@AssociateServiceActionWithProvisioningArtifact' {} a -> s {productId = a} :: AssociateServiceActionWithProvisioningArtifact)

-- | The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
associateServiceActionWithProvisioningArtifact_provisioningArtifactId :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact Prelude.Text
associateServiceActionWithProvisioningArtifact_provisioningArtifactId = Lens.lens (\AssociateServiceActionWithProvisioningArtifact' {provisioningArtifactId} -> provisioningArtifactId) (\s@AssociateServiceActionWithProvisioningArtifact' {} a -> s {provisioningArtifactId = a} :: AssociateServiceActionWithProvisioningArtifact)

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
associateServiceActionWithProvisioningArtifact_serviceActionId :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact Prelude.Text
associateServiceActionWithProvisioningArtifact_serviceActionId = Lens.lens (\AssociateServiceActionWithProvisioningArtifact' {serviceActionId} -> serviceActionId) (\s@AssociateServiceActionWithProvisioningArtifact' {} a -> s {serviceActionId = a} :: AssociateServiceActionWithProvisioningArtifact)

instance
  Prelude.AWSRequest
    AssociateServiceActionWithProvisioningArtifact
  where
  type
    Rs
      AssociateServiceActionWithProvisioningArtifact =
      AssociateServiceActionWithProvisioningArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateServiceActionWithProvisioningArtifactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateServiceActionWithProvisioningArtifact

instance
  Prelude.NFData
    AssociateServiceActionWithProvisioningArtifact

instance
  Prelude.ToHeaders
    AssociateServiceActionWithProvisioningArtifact
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.AssociateServiceActionWithProvisioningArtifact" ::
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
    AssociateServiceActionWithProvisioningArtifact
  where
  toJSON
    AssociateServiceActionWithProvisioningArtifact' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ ("AcceptLanguage" Prelude..=)
                Prelude.<$> acceptLanguage,
              Prelude.Just ("ProductId" Prelude..= productId),
              Prelude.Just
                ( "ProvisioningArtifactId"
                    Prelude..= provisioningArtifactId
                ),
              Prelude.Just
                ("ServiceActionId" Prelude..= serviceActionId)
            ]
        )

instance
  Prelude.ToPath
    AssociateServiceActionWithProvisioningArtifact
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    AssociateServiceActionWithProvisioningArtifact
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateServiceActionWithProvisioningArtifactResponse' smart constructor.
data AssociateServiceActionWithProvisioningArtifactResponse = AssociateServiceActionWithProvisioningArtifactResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  AssociateServiceActionWithProvisioningArtifactResponse
newAssociateServiceActionWithProvisioningArtifactResponse
  pHttpStatus_ =
    AssociateServiceActionWithProvisioningArtifactResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateServiceActionWithProvisioningArtifactResponse_httpStatus :: Lens.Lens' AssociateServiceActionWithProvisioningArtifactResponse Prelude.Int
associateServiceActionWithProvisioningArtifactResponse_httpStatus = Lens.lens (\AssociateServiceActionWithProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@AssociateServiceActionWithProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: AssociateServiceActionWithProvisioningArtifactResponse)

instance
  Prelude.NFData
    AssociateServiceActionWithProvisioningArtifactResponse
