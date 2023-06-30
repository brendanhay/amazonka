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
-- Module      : Amazonka.ServiceCatalog.BatchDisassociateServiceActionFromProvisioningArtifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a batch of self-service actions from the specified
-- provisioning artifact.
module Amazonka.ServiceCatalog.BatchDisassociateServiceActionFromProvisioningArtifact
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newBatchDisassociateServiceActionFromProvisioningArtifact' smart constructor.
data BatchDisassociateServiceActionFromProvisioningArtifact = BatchDisassociateServiceActionFromProvisioningArtifact'
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty ServiceActionAssociation ->
  BatchDisassociateServiceActionFromProvisioningArtifact
newBatchDisassociateServiceActionFromProvisioningArtifact
  pServiceActionAssociations_ =
    BatchDisassociateServiceActionFromProvisioningArtifact'
      { acceptLanguage =
          Prelude.Nothing,
        serviceActionAssociations =
          Lens.coerced
            Lens.# pServiceActionAssociations_
      }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
batchDisassociateServiceActionFromProvisioningArtifact_acceptLanguage :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifact (Prelude.Maybe Prelude.Text)
batchDisassociateServiceActionFromProvisioningArtifact_acceptLanguage = Lens.lens (\BatchDisassociateServiceActionFromProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@BatchDisassociateServiceActionFromProvisioningArtifact' {} a -> s {acceptLanguage = a} :: BatchDisassociateServiceActionFromProvisioningArtifact)

-- | One or more associations, each consisting of the Action ID, the Product
-- ID, and the Provisioning Artifact ID.
batchDisassociateServiceActionFromProvisioningArtifact_serviceActionAssociations :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifact (Prelude.NonEmpty ServiceActionAssociation)
batchDisassociateServiceActionFromProvisioningArtifact_serviceActionAssociations = Lens.lens (\BatchDisassociateServiceActionFromProvisioningArtifact' {serviceActionAssociations} -> serviceActionAssociations) (\s@BatchDisassociateServiceActionFromProvisioningArtifact' {} a -> s {serviceActionAssociations = a} :: BatchDisassociateServiceActionFromProvisioningArtifact) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  type
    AWSResponse
      BatchDisassociateServiceActionFromProvisioningArtifact =
      BatchDisassociateServiceActionFromProvisioningArtifactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDisassociateServiceActionFromProvisioningArtifactResponse'
            Prelude.<$> ( x
                            Data..?> "FailedServiceActionAssociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  hashWithSalt
    _salt
    BatchDisassociateServiceActionFromProvisioningArtifact' {..} =
      _salt
        `Prelude.hashWithSalt` acceptLanguage
        `Prelude.hashWithSalt` serviceActionAssociations

instance
  Prelude.NFData
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  rnf
    BatchDisassociateServiceActionFromProvisioningArtifact' {..} =
      Prelude.rnf acceptLanguage
        `Prelude.seq` Prelude.rnf serviceActionAssociations

instance
  Data.ToHeaders
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.BatchDisassociateServiceActionFromProvisioningArtifact" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  toJSON
    BatchDisassociateServiceActionFromProvisioningArtifact' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AcceptLanguage" Data..=)
                Prelude.<$> acceptLanguage,
              Prelude.Just
                ( "ServiceActionAssociations"
                    Data..= serviceActionAssociations
                )
            ]
        )

instance
  Data.ToPath
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    BatchDisassociateServiceActionFromProvisioningArtifact
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDisassociateServiceActionFromProvisioningArtifactResponse' smart constructor.
data BatchDisassociateServiceActionFromProvisioningArtifactResponse = BatchDisassociateServiceActionFromProvisioningArtifactResponse'
  { -- | An object that contains a list of errors, along with information to help
    -- you identify the self-service action.
    failedServiceActionAssociations :: Prelude.Maybe [FailedServiceActionAssociation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchDisassociateServiceActionFromProvisioningArtifactResponse
newBatchDisassociateServiceActionFromProvisioningArtifactResponse
  pHttpStatus_ =
    BatchDisassociateServiceActionFromProvisioningArtifactResponse'
      { failedServiceActionAssociations =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | An object that contains a list of errors, along with information to help
-- you identify the self-service action.
batchDisassociateServiceActionFromProvisioningArtifactResponse_failedServiceActionAssociations :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifactResponse (Prelude.Maybe [FailedServiceActionAssociation])
batchDisassociateServiceActionFromProvisioningArtifactResponse_failedServiceActionAssociations = Lens.lens (\BatchDisassociateServiceActionFromProvisioningArtifactResponse' {failedServiceActionAssociations} -> failedServiceActionAssociations) (\s@BatchDisassociateServiceActionFromProvisioningArtifactResponse' {} a -> s {failedServiceActionAssociations = a} :: BatchDisassociateServiceActionFromProvisioningArtifactResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDisassociateServiceActionFromProvisioningArtifactResponse_httpStatus :: Lens.Lens' BatchDisassociateServiceActionFromProvisioningArtifactResponse Prelude.Int
batchDisassociateServiceActionFromProvisioningArtifactResponse_httpStatus = Lens.lens (\BatchDisassociateServiceActionFromProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@BatchDisassociateServiceActionFromProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: BatchDisassociateServiceActionFromProvisioningArtifactResponse)

instance
  Prelude.NFData
    BatchDisassociateServiceActionFromProvisioningArtifactResponse
  where
  rnf
    BatchDisassociateServiceActionFromProvisioningArtifactResponse' {..} =
      Prelude.rnf failedServiceActionAssociations
        `Prelude.seq` Prelude.rnf httpStatus
