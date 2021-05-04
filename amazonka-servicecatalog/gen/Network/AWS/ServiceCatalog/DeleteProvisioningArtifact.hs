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
-- Module      : Network.AWS.ServiceCatalog.DeleteProvisioningArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified provisioning artifact (also known as a version)
-- for the specified product.
--
-- You cannot delete a provisioning artifact associated with a product that
-- was shared with you. You cannot delete the last provisioning artifact
-- for a product, because a product must have at least one provisioning
-- artifact.
module Network.AWS.ServiceCatalog.DeleteProvisioningArtifact
  ( -- * Creating a Request
    DeleteProvisioningArtifact (..),
    newDeleteProvisioningArtifact,

    -- * Request Lenses
    deleteProvisioningArtifact_acceptLanguage,
    deleteProvisioningArtifact_productId,
    deleteProvisioningArtifact_provisioningArtifactId,

    -- * Destructuring the Response
    DeleteProvisioningArtifactResponse (..),
    newDeleteProvisioningArtifactResponse,

    -- * Response Lenses
    deleteProvisioningArtifactResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDeleteProvisioningArtifact' smart constructor.
data DeleteProvisioningArtifact = DeleteProvisioningArtifact'
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
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'deleteProvisioningArtifact_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'deleteProvisioningArtifact_productId' - The product identifier.
--
-- 'provisioningArtifactId', 'deleteProvisioningArtifact_provisioningArtifactId' - The identifier of the provisioning artifact.
newDeleteProvisioningArtifact ::
  -- | 'productId'
  Prelude.Text ->
  -- | 'provisioningArtifactId'
  Prelude.Text ->
  DeleteProvisioningArtifact
newDeleteProvisioningArtifact
  pProductId_
  pProvisioningArtifactId_ =
    DeleteProvisioningArtifact'
      { acceptLanguage =
          Prelude.Nothing,
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
deleteProvisioningArtifact_acceptLanguage :: Lens.Lens' DeleteProvisioningArtifact (Prelude.Maybe Prelude.Text)
deleteProvisioningArtifact_acceptLanguage = Lens.lens (\DeleteProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@DeleteProvisioningArtifact' {} a -> s {acceptLanguage = a} :: DeleteProvisioningArtifact)

-- | The product identifier.
deleteProvisioningArtifact_productId :: Lens.Lens' DeleteProvisioningArtifact Prelude.Text
deleteProvisioningArtifact_productId = Lens.lens (\DeleteProvisioningArtifact' {productId} -> productId) (\s@DeleteProvisioningArtifact' {} a -> s {productId = a} :: DeleteProvisioningArtifact)

-- | The identifier of the provisioning artifact.
deleteProvisioningArtifact_provisioningArtifactId :: Lens.Lens' DeleteProvisioningArtifact Prelude.Text
deleteProvisioningArtifact_provisioningArtifactId = Lens.lens (\DeleteProvisioningArtifact' {provisioningArtifactId} -> provisioningArtifactId) (\s@DeleteProvisioningArtifact' {} a -> s {provisioningArtifactId = a} :: DeleteProvisioningArtifact)

instance
  Prelude.AWSRequest
    DeleteProvisioningArtifact
  where
  type
    Rs DeleteProvisioningArtifact =
      DeleteProvisioningArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProvisioningArtifactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProvisioningArtifact

instance Prelude.NFData DeleteProvisioningArtifact

instance Prelude.ToHeaders DeleteProvisioningArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.DeleteProvisioningArtifact" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteProvisioningArtifact where
  toJSON DeleteProvisioningArtifact' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("ProductId" Prelude..= productId),
            Prelude.Just
              ( "ProvisioningArtifactId"
                  Prelude..= provisioningArtifactId
              )
          ]
      )

instance Prelude.ToPath DeleteProvisioningArtifact where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteProvisioningArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProvisioningArtifactResponse' smart constructor.
data DeleteProvisioningArtifactResponse = DeleteProvisioningArtifactResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteProvisioningArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProvisioningArtifactResponse_httpStatus' - The response's http status code.
newDeleteProvisioningArtifactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProvisioningArtifactResponse
newDeleteProvisioningArtifactResponse pHttpStatus_ =
  DeleteProvisioningArtifactResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteProvisioningArtifactResponse_httpStatus :: Lens.Lens' DeleteProvisioningArtifactResponse Prelude.Int
deleteProvisioningArtifactResponse_httpStatus = Lens.lens (\DeleteProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@DeleteProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: DeleteProvisioningArtifactResponse)

instance
  Prelude.NFData
    DeleteProvisioningArtifactResponse
