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
-- Module      : Network.AWS.ServiceCatalog.DisassociateServiceActionFromProvisioningArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified self-service action association from the
-- specified provisioning artifact.
module Network.AWS.ServiceCatalog.DisassociateServiceActionFromProvisioningArtifact
  ( -- * Creating a Request
    DisassociateServiceActionFromProvisioningArtifact (..),
    newDisassociateServiceActionFromProvisioningArtifact,

    -- * Request Lenses
    disassociateServiceActionFromProvisioningArtifact_acceptLanguage,
    disassociateServiceActionFromProvisioningArtifact_productId,
    disassociateServiceActionFromProvisioningArtifact_provisioningArtifactId,
    disassociateServiceActionFromProvisioningArtifact_serviceActionId,

    -- * Destructuring the Response
    DisassociateServiceActionFromProvisioningArtifactResponse (..),
    newDisassociateServiceActionFromProvisioningArtifactResponse,

    -- * Response Lenses
    disassociateServiceActionFromProvisioningArtifactResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDisassociateServiceActionFromProvisioningArtifact' smart constructor.
data DisassociateServiceActionFromProvisioningArtifact = DisassociateServiceActionFromProvisioningArtifact'
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
-- Create a value of 'DisassociateServiceActionFromProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'disassociateServiceActionFromProvisioningArtifact_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'disassociateServiceActionFromProvisioningArtifact_productId' - The product identifier. For example, @prod-abcdzk7xy33qa@.
--
-- 'provisioningArtifactId', 'disassociateServiceActionFromProvisioningArtifact_provisioningArtifactId' - The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
--
-- 'serviceActionId', 'disassociateServiceActionFromProvisioningArtifact_serviceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@.
newDisassociateServiceActionFromProvisioningArtifact ::
  -- | 'productId'
  Prelude.Text ->
  -- | 'provisioningArtifactId'
  Prelude.Text ->
  -- | 'serviceActionId'
  Prelude.Text ->
  DisassociateServiceActionFromProvisioningArtifact
newDisassociateServiceActionFromProvisioningArtifact
  pProductId_
  pProvisioningArtifactId_
  pServiceActionId_ =
    DisassociateServiceActionFromProvisioningArtifact'
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
disassociateServiceActionFromProvisioningArtifact_acceptLanguage :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact (Prelude.Maybe Prelude.Text)
disassociateServiceActionFromProvisioningArtifact_acceptLanguage = Lens.lens (\DisassociateServiceActionFromProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@DisassociateServiceActionFromProvisioningArtifact' {} a -> s {acceptLanguage = a} :: DisassociateServiceActionFromProvisioningArtifact)

-- | The product identifier. For example, @prod-abcdzk7xy33qa@.
disassociateServiceActionFromProvisioningArtifact_productId :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact Prelude.Text
disassociateServiceActionFromProvisioningArtifact_productId = Lens.lens (\DisassociateServiceActionFromProvisioningArtifact' {productId} -> productId) (\s@DisassociateServiceActionFromProvisioningArtifact' {} a -> s {productId = a} :: DisassociateServiceActionFromProvisioningArtifact)

-- | The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
disassociateServiceActionFromProvisioningArtifact_provisioningArtifactId :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact Prelude.Text
disassociateServiceActionFromProvisioningArtifact_provisioningArtifactId = Lens.lens (\DisassociateServiceActionFromProvisioningArtifact' {provisioningArtifactId} -> provisioningArtifactId) (\s@DisassociateServiceActionFromProvisioningArtifact' {} a -> s {provisioningArtifactId = a} :: DisassociateServiceActionFromProvisioningArtifact)

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
disassociateServiceActionFromProvisioningArtifact_serviceActionId :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact Prelude.Text
disassociateServiceActionFromProvisioningArtifact_serviceActionId = Lens.lens (\DisassociateServiceActionFromProvisioningArtifact' {serviceActionId} -> serviceActionId) (\s@DisassociateServiceActionFromProvisioningArtifact' {} a -> s {serviceActionId = a} :: DisassociateServiceActionFromProvisioningArtifact)

instance
  Prelude.AWSRequest
    DisassociateServiceActionFromProvisioningArtifact
  where
  type
    Rs
      DisassociateServiceActionFromProvisioningArtifact =
      DisassociateServiceActionFromProvisioningArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateServiceActionFromProvisioningArtifactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateServiceActionFromProvisioningArtifact

instance
  Prelude.NFData
    DisassociateServiceActionFromProvisioningArtifact

instance
  Prelude.ToHeaders
    DisassociateServiceActionFromProvisioningArtifact
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.DisassociateServiceActionFromProvisioningArtifact" ::
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
    DisassociateServiceActionFromProvisioningArtifact
  where
  toJSON
    DisassociateServiceActionFromProvisioningArtifact' {..} =
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
    DisassociateServiceActionFromProvisioningArtifact
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisassociateServiceActionFromProvisioningArtifact
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateServiceActionFromProvisioningArtifactResponse' smart constructor.
data DisassociateServiceActionFromProvisioningArtifactResponse = DisassociateServiceActionFromProvisioningArtifactResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateServiceActionFromProvisioningArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateServiceActionFromProvisioningArtifactResponse_httpStatus' - The response's http status code.
newDisassociateServiceActionFromProvisioningArtifactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateServiceActionFromProvisioningArtifactResponse
newDisassociateServiceActionFromProvisioningArtifactResponse
  pHttpStatus_ =
    DisassociateServiceActionFromProvisioningArtifactResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateServiceActionFromProvisioningArtifactResponse_httpStatus :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifactResponse Prelude.Int
disassociateServiceActionFromProvisioningArtifactResponse_httpStatus = Lens.lens (\DisassociateServiceActionFromProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@DisassociateServiceActionFromProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: DisassociateServiceActionFromProvisioningArtifactResponse)

instance
  Prelude.NFData
    DisassociateServiceActionFromProvisioningArtifactResponse
