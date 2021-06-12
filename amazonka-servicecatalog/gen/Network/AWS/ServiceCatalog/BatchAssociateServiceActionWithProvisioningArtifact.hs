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
-- Module      : Network.AWS.ServiceCatalog.BatchAssociateServiceActionWithProvisioningArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates multiple self-service actions with provisioning artifacts.
module Network.AWS.ServiceCatalog.BatchAssociateServiceActionWithProvisioningArtifact
  ( -- * Creating a Request
    BatchAssociateServiceActionWithProvisioningArtifact (..),
    newBatchAssociateServiceActionWithProvisioningArtifact,

    -- * Request Lenses
    batchAssociateServiceActionWithProvisioningArtifact_acceptLanguage,
    batchAssociateServiceActionWithProvisioningArtifact_serviceActionAssociations,

    -- * Destructuring the Response
    BatchAssociateServiceActionWithProvisioningArtifactResponse (..),
    newBatchAssociateServiceActionWithProvisioningArtifactResponse,

    -- * Response Lenses
    batchAssociateServiceActionWithProvisioningArtifactResponse_failedServiceActionAssociations,
    batchAssociateServiceActionWithProvisioningArtifactResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newBatchAssociateServiceActionWithProvisioningArtifact' smart constructor.
data BatchAssociateServiceActionWithProvisioningArtifact = BatchAssociateServiceActionWithProvisioningArtifact'
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
-- Create a value of 'BatchAssociateServiceActionWithProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'batchAssociateServiceActionWithProvisioningArtifact_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'serviceActionAssociations', 'batchAssociateServiceActionWithProvisioningArtifact_serviceActionAssociations' - One or more associations, each consisting of the Action ID, the Product
-- ID, and the Provisioning Artifact ID.
newBatchAssociateServiceActionWithProvisioningArtifact ::
  -- | 'serviceActionAssociations'
  Core.NonEmpty ServiceActionAssociation ->
  BatchAssociateServiceActionWithProvisioningArtifact
newBatchAssociateServiceActionWithProvisioningArtifact
  pServiceActionAssociations_ =
    BatchAssociateServiceActionWithProvisioningArtifact'
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
batchAssociateServiceActionWithProvisioningArtifact_acceptLanguage :: Lens.Lens' BatchAssociateServiceActionWithProvisioningArtifact (Core.Maybe Core.Text)
batchAssociateServiceActionWithProvisioningArtifact_acceptLanguage = Lens.lens (\BatchAssociateServiceActionWithProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@BatchAssociateServiceActionWithProvisioningArtifact' {} a -> s {acceptLanguage = a} :: BatchAssociateServiceActionWithProvisioningArtifact)

-- | One or more associations, each consisting of the Action ID, the Product
-- ID, and the Provisioning Artifact ID.
batchAssociateServiceActionWithProvisioningArtifact_serviceActionAssociations :: Lens.Lens' BatchAssociateServiceActionWithProvisioningArtifact (Core.NonEmpty ServiceActionAssociation)
batchAssociateServiceActionWithProvisioningArtifact_serviceActionAssociations = Lens.lens (\BatchAssociateServiceActionWithProvisioningArtifact' {serviceActionAssociations} -> serviceActionAssociations) (\s@BatchAssociateServiceActionWithProvisioningArtifact' {} a -> s {serviceActionAssociations = a} :: BatchAssociateServiceActionWithProvisioningArtifact) Core.. Lens._Coerce

instance
  Core.AWSRequest
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  type
    AWSResponse
      BatchAssociateServiceActionWithProvisioningArtifact =
      BatchAssociateServiceActionWithProvisioningArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAssociateServiceActionWithProvisioningArtifactResponse'
            Core.<$> ( x Core..?> "FailedServiceActionAssociations"
                         Core..!@ Core.mempty
                     )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    BatchAssociateServiceActionWithProvisioningArtifact

instance
  Core.NFData
    BatchAssociateServiceActionWithProvisioningArtifact

instance
  Core.ToHeaders
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.BatchAssociateServiceActionWithProvisioningArtifact" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  toJSON
    BatchAssociateServiceActionWithProvisioningArtifact' {..} =
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
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchAssociateServiceActionWithProvisioningArtifactResponse' smart constructor.
data BatchAssociateServiceActionWithProvisioningArtifactResponse = BatchAssociateServiceActionWithProvisioningArtifactResponse'
  { -- | An object that contains a list of errors, along with information to help
    -- you identify the self-service action.
    failedServiceActionAssociations :: Core.Maybe [FailedServiceActionAssociation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchAssociateServiceActionWithProvisioningArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedServiceActionAssociations', 'batchAssociateServiceActionWithProvisioningArtifactResponse_failedServiceActionAssociations' - An object that contains a list of errors, along with information to help
-- you identify the self-service action.
--
-- 'httpStatus', 'batchAssociateServiceActionWithProvisioningArtifactResponse_httpStatus' - The response's http status code.
newBatchAssociateServiceActionWithProvisioningArtifactResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchAssociateServiceActionWithProvisioningArtifactResponse
newBatchAssociateServiceActionWithProvisioningArtifactResponse
  pHttpStatus_ =
    BatchAssociateServiceActionWithProvisioningArtifactResponse'
      { failedServiceActionAssociations =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | An object that contains a list of errors, along with information to help
-- you identify the self-service action.
batchAssociateServiceActionWithProvisioningArtifactResponse_failedServiceActionAssociations :: Lens.Lens' BatchAssociateServiceActionWithProvisioningArtifactResponse (Core.Maybe [FailedServiceActionAssociation])
batchAssociateServiceActionWithProvisioningArtifactResponse_failedServiceActionAssociations = Lens.lens (\BatchAssociateServiceActionWithProvisioningArtifactResponse' {failedServiceActionAssociations} -> failedServiceActionAssociations) (\s@BatchAssociateServiceActionWithProvisioningArtifactResponse' {} a -> s {failedServiceActionAssociations = a} :: BatchAssociateServiceActionWithProvisioningArtifactResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchAssociateServiceActionWithProvisioningArtifactResponse_httpStatus :: Lens.Lens' BatchAssociateServiceActionWithProvisioningArtifactResponse Core.Int
batchAssociateServiceActionWithProvisioningArtifactResponse_httpStatus = Lens.lens (\BatchAssociateServiceActionWithProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@BatchAssociateServiceActionWithProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: BatchAssociateServiceActionWithProvisioningArtifactResponse)

instance
  Core.NFData
    BatchAssociateServiceActionWithProvisioningArtifactResponse
