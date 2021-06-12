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
-- Module      : Network.AWS.ServiceCatalog.BatchDisassociateServiceActionFromProvisioningArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a batch of self-service actions from the specified
-- provisioning artifact.
module Network.AWS.ServiceCatalog.BatchDisassociateServiceActionFromProvisioningArtifact
  ( -- * Creating a Request
    BatchDisassociateServiceActionFromProvisioningArtifact (..),
    newBatchDisassociateServiceActionFromProvisioningArtifact,

    -- * Request Lenses
    batchDisassociateServiceActionFromProvisioningArtifact_acceptLanguage,
    batchDisassociateServiceActionFromProvisioningArtifact_serviceActionAssociations,

    -- * Destructuring the Response
    BatchDisassociateServiceActionFromProvisioningArtifactResponse (..),
    newBatchDisassociateServiceActionFromProvisioningArtifactResponse,

    -- * Response Lenses
    batchDisassociateServiceActionFromProvisioningArtifactResponse_failedServiceActionAssociations,
    batchDisassociateServiceActionFromProvisioningArtifactResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newBatchDisassociateServiceActionFromProvisioningArtifact' smart constructor.
data BatchDisassociateServiceActionFromProvisioningArtifact = BatchDisassociateServiceActionFromProvisioningArtifact'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | One or more associations, each consisting of the Action ID, the Product
    -- ID, and the Provisioning Artifact ID.
    serviceActionAssociations :: Core.NonEmpty ServiceActionAssociation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDisassociateServiceActionFromProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'batchDisassociateServiceActionFromProvisioningArtifact_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'serviceActionAssociations', 'batchDisassociateServiceActionFromProvisioningArtifact_serviceActionAssociations' - One or more associations, each consisting of the Action ID, the Product
-- ID, and the Provisioning Artifact ID.
newBatchDisassociateServiceActionFromProvisioningArtifact ::
  -- | 'serviceActionAssociations'
  Core.NonEmpty ServiceActionAssociation ->
  BatchDisassociateServiceActionFromProvisioningArtifact
newBatchDisassociateServiceActionFromProvisioningArtifact
  pServiceActionAssociations_ =
    BatchDisassociateServiceActionFromProvisioningArtifact'
      { acceptLanguage =
          Core.Nothing,
        serviceActionAssociations =
          Lens._Coerce
            Lens.# pServiceActionAssociations_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
batchDisassociateServiceActionFromProvisioningArtifact_acceptLanguage :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifact (Core.Maybe Core.Text)
batchDisassociateServiceActionFromProvisioningArtifact_acceptLanguage = Lens.lens (\BatchDisassociateServiceActionFromProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@BatchDisassociateServiceActionFromProvisioningArtifact' {} a -> s {acceptLanguage = a} :: BatchDisassociateServiceActionFromProvisioningArtifact)

-- | One or more associations, each consisting of the Action ID, the Product
-- ID, and the Provisioning Artifact ID.
batchDisassociateServiceActionFromProvisioningArtifact_serviceActionAssociations :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifact (Core.NonEmpty ServiceActionAssociation)
batchDisassociateServiceActionFromProvisioningArtifact_serviceActionAssociations = Lens.lens (\BatchDisassociateServiceActionFromProvisioningArtifact' {serviceActionAssociations} -> serviceActionAssociations) (\s@BatchDisassociateServiceActionFromProvisioningArtifact' {} a -> s {serviceActionAssociations = a} :: BatchDisassociateServiceActionFromProvisioningArtifact) Core.. Lens._Coerce

instance
  Core.AWSRequest
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  type
    AWSResponse
      BatchDisassociateServiceActionFromProvisioningArtifact =
      BatchDisassociateServiceActionFromProvisioningArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDisassociateServiceActionFromProvisioningArtifactResponse'
            Core.<$> ( x Core..?> "FailedServiceActionAssociations"
                         Core..!@ Core.mempty
                     )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    BatchDisassociateServiceActionFromProvisioningArtifact

instance
  Core.NFData
    BatchDisassociateServiceActionFromProvisioningArtifact

instance
  Core.ToHeaders
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.BatchDisassociateServiceActionFromProvisioningArtifact" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  toJSON
    BatchDisassociateServiceActionFromProvisioningArtifact' {..} =
      Core.object
        ( Core.catMaybes
            [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
              Core.Just
                ( "ServiceActionAssociations"
                    Core..= serviceActionAssociations
                )
            ]
        )

instance
  Core.ToPath
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchDisassociateServiceActionFromProvisioningArtifactResponse' smart constructor.
data BatchDisassociateServiceActionFromProvisioningArtifactResponse = BatchDisassociateServiceActionFromProvisioningArtifactResponse'
  { -- | An object that contains a list of errors, along with information to help
    -- you identify the self-service action.
    failedServiceActionAssociations :: Core.Maybe [FailedServiceActionAssociation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDisassociateServiceActionFromProvisioningArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedServiceActionAssociations', 'batchDisassociateServiceActionFromProvisioningArtifactResponse_failedServiceActionAssociations' - An object that contains a list of errors, along with information to help
-- you identify the self-service action.
--
-- 'httpStatus', 'batchDisassociateServiceActionFromProvisioningArtifactResponse_httpStatus' - The response's http status code.
newBatchDisassociateServiceActionFromProvisioningArtifactResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchDisassociateServiceActionFromProvisioningArtifactResponse
newBatchDisassociateServiceActionFromProvisioningArtifactResponse
  pHttpStatus_ =
    BatchDisassociateServiceActionFromProvisioningArtifactResponse'
      { failedServiceActionAssociations =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | An object that contains a list of errors, along with information to help
-- you identify the self-service action.
batchDisassociateServiceActionFromProvisioningArtifactResponse_failedServiceActionAssociations :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifactResponse (Core.Maybe [FailedServiceActionAssociation])
batchDisassociateServiceActionFromProvisioningArtifactResponse_failedServiceActionAssociations = Lens.lens (\BatchDisassociateServiceActionFromProvisioningArtifactResponse' {failedServiceActionAssociations} -> failedServiceActionAssociations) (\s@BatchDisassociateServiceActionFromProvisioningArtifactResponse' {} a -> s {failedServiceActionAssociations = a} :: BatchDisassociateServiceActionFromProvisioningArtifactResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchDisassociateServiceActionFromProvisioningArtifactResponse_httpStatus :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifactResponse Core.Int
batchDisassociateServiceActionFromProvisioningArtifactResponse_httpStatus = Lens.lens (\BatchDisassociateServiceActionFromProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@BatchDisassociateServiceActionFromProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: BatchDisassociateServiceActionFromProvisioningArtifactResponse)

instance
  Core.NFData
    BatchDisassociateServiceActionFromProvisioningArtifactResponse
