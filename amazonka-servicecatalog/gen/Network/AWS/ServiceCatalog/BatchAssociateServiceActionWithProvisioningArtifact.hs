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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | One or more associations, each consisting of the Action ID, the Product
    -- ID, and the Provisioning Artifact ID.
    serviceActionAssociations :: Prelude.NonEmpty ServiceActionAssociation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.NonEmpty ServiceActionAssociation ->
  BatchAssociateServiceActionWithProvisioningArtifact
newBatchAssociateServiceActionWithProvisioningArtifact
  pServiceActionAssociations_ =
    BatchAssociateServiceActionWithProvisioningArtifact'
      { acceptLanguage =
          Prelude.Nothing,
        serviceActionAssociations =
          Prelude._Coerce
            Lens.# pServiceActionAssociations_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
batchAssociateServiceActionWithProvisioningArtifact_acceptLanguage :: Lens.Lens' BatchAssociateServiceActionWithProvisioningArtifact (Prelude.Maybe Prelude.Text)
batchAssociateServiceActionWithProvisioningArtifact_acceptLanguage = Lens.lens (\BatchAssociateServiceActionWithProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@BatchAssociateServiceActionWithProvisioningArtifact' {} a -> s {acceptLanguage = a} :: BatchAssociateServiceActionWithProvisioningArtifact)

-- | One or more associations, each consisting of the Action ID, the Product
-- ID, and the Provisioning Artifact ID.
batchAssociateServiceActionWithProvisioningArtifact_serviceActionAssociations :: Lens.Lens' BatchAssociateServiceActionWithProvisioningArtifact (Prelude.NonEmpty ServiceActionAssociation)
batchAssociateServiceActionWithProvisioningArtifact_serviceActionAssociations = Lens.lens (\BatchAssociateServiceActionWithProvisioningArtifact' {serviceActionAssociations} -> serviceActionAssociations) (\s@BatchAssociateServiceActionWithProvisioningArtifact' {} a -> s {serviceActionAssociations = a} :: BatchAssociateServiceActionWithProvisioningArtifact) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  type
    Rs
      BatchAssociateServiceActionWithProvisioningArtifact =
      BatchAssociateServiceActionWithProvisioningArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAssociateServiceActionWithProvisioningArtifactResponse'
            Prelude.<$> ( x Prelude..?> "FailedServiceActionAssociations"
                            Prelude..!@ Prelude.mempty
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchAssociateServiceActionWithProvisioningArtifact

instance
  Prelude.NFData
    BatchAssociateServiceActionWithProvisioningArtifact

instance
  Prelude.ToHeaders
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.BatchAssociateServiceActionWithProvisioningArtifact" ::
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
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  toJSON
    BatchAssociateServiceActionWithProvisioningArtifact' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ ("AcceptLanguage" Prelude..=)
                Prelude.<$> acceptLanguage,
              Prelude.Just
                ( "ServiceActionAssociations"
                    Prelude..= serviceActionAssociations
                )
            ]
        )

instance
  Prelude.ToPath
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    BatchAssociateServiceActionWithProvisioningArtifact
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchAssociateServiceActionWithProvisioningArtifactResponse' smart constructor.
data BatchAssociateServiceActionWithProvisioningArtifactResponse = BatchAssociateServiceActionWithProvisioningArtifactResponse'
  { -- | An object that contains a list of errors, along with information to help
    -- you identify the self-service action.
    failedServiceActionAssociations :: Prelude.Maybe [FailedServiceActionAssociation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  BatchAssociateServiceActionWithProvisioningArtifactResponse
newBatchAssociateServiceActionWithProvisioningArtifactResponse
  pHttpStatus_ =
    BatchAssociateServiceActionWithProvisioningArtifactResponse'
      { failedServiceActionAssociations =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | An object that contains a list of errors, along with information to help
-- you identify the self-service action.
batchAssociateServiceActionWithProvisioningArtifactResponse_failedServiceActionAssociations :: Lens.Lens' BatchAssociateServiceActionWithProvisioningArtifactResponse (Prelude.Maybe [FailedServiceActionAssociation])
batchAssociateServiceActionWithProvisioningArtifactResponse_failedServiceActionAssociations = Lens.lens (\BatchAssociateServiceActionWithProvisioningArtifactResponse' {failedServiceActionAssociations} -> failedServiceActionAssociations) (\s@BatchAssociateServiceActionWithProvisioningArtifactResponse' {} a -> s {failedServiceActionAssociations = a} :: BatchAssociateServiceActionWithProvisioningArtifactResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
batchAssociateServiceActionWithProvisioningArtifactResponse_httpStatus :: Lens.Lens' BatchAssociateServiceActionWithProvisioningArtifactResponse Prelude.Int
batchAssociateServiceActionWithProvisioningArtifactResponse_httpStatus = Lens.lens (\BatchAssociateServiceActionWithProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@BatchAssociateServiceActionWithProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: BatchAssociateServiceActionWithProvisioningArtifactResponse)

instance
  Prelude.NFData
    BatchAssociateServiceActionWithProvisioningArtifactResponse
