{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServiceCatalog.Types.FailedServiceActionAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.FailedServiceActionAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ServiceActionAssociationErrorCode

-- | An object containing information about the error, along with identifying
-- information about the self-service action and its associations.
--
-- /See:/ 'newFailedServiceActionAssociation' smart constructor.
data FailedServiceActionAssociation = FailedServiceActionAssociation'
  { -- | The error code. Valid values are listed below.
    errorCode :: Prelude.Maybe ServiceActionAssociationErrorCode,
    -- | A text description of the error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The product identifier. For example, @prod-abcdzk7xy33qa@.
    productId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioning artifact. For example,
    -- @pa-4abcdjnxjj6ne@.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
    serviceActionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedServiceActionAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'failedServiceActionAssociation_errorCode' - The error code. Valid values are listed below.
--
-- 'errorMessage', 'failedServiceActionAssociation_errorMessage' - A text description of the error.
--
-- 'productId', 'failedServiceActionAssociation_productId' - The product identifier. For example, @prod-abcdzk7xy33qa@.
--
-- 'provisioningArtifactId', 'failedServiceActionAssociation_provisioningArtifactId' - The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
--
-- 'serviceActionId', 'failedServiceActionAssociation_serviceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@.
newFailedServiceActionAssociation ::
  FailedServiceActionAssociation
newFailedServiceActionAssociation =
  FailedServiceActionAssociation'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      productId = Prelude.Nothing,
      provisioningArtifactId = Prelude.Nothing,
      serviceActionId = Prelude.Nothing
    }

-- | The error code. Valid values are listed below.
failedServiceActionAssociation_errorCode :: Lens.Lens' FailedServiceActionAssociation (Prelude.Maybe ServiceActionAssociationErrorCode)
failedServiceActionAssociation_errorCode = Lens.lens (\FailedServiceActionAssociation' {errorCode} -> errorCode) (\s@FailedServiceActionAssociation' {} a -> s {errorCode = a} :: FailedServiceActionAssociation)

-- | A text description of the error.
failedServiceActionAssociation_errorMessage :: Lens.Lens' FailedServiceActionAssociation (Prelude.Maybe Prelude.Text)
failedServiceActionAssociation_errorMessage = Lens.lens (\FailedServiceActionAssociation' {errorMessage} -> errorMessage) (\s@FailedServiceActionAssociation' {} a -> s {errorMessage = a} :: FailedServiceActionAssociation)

-- | The product identifier. For example, @prod-abcdzk7xy33qa@.
failedServiceActionAssociation_productId :: Lens.Lens' FailedServiceActionAssociation (Prelude.Maybe Prelude.Text)
failedServiceActionAssociation_productId = Lens.lens (\FailedServiceActionAssociation' {productId} -> productId) (\s@FailedServiceActionAssociation' {} a -> s {productId = a} :: FailedServiceActionAssociation)

-- | The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
failedServiceActionAssociation_provisioningArtifactId :: Lens.Lens' FailedServiceActionAssociation (Prelude.Maybe Prelude.Text)
failedServiceActionAssociation_provisioningArtifactId = Lens.lens (\FailedServiceActionAssociation' {provisioningArtifactId} -> provisioningArtifactId) (\s@FailedServiceActionAssociation' {} a -> s {provisioningArtifactId = a} :: FailedServiceActionAssociation)

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
failedServiceActionAssociation_serviceActionId :: Lens.Lens' FailedServiceActionAssociation (Prelude.Maybe Prelude.Text)
failedServiceActionAssociation_serviceActionId = Lens.lens (\FailedServiceActionAssociation' {serviceActionId} -> serviceActionId) (\s@FailedServiceActionAssociation' {} a -> s {serviceActionId = a} :: FailedServiceActionAssociation)

instance Data.FromJSON FailedServiceActionAssociation where
  parseJSON =
    Data.withObject
      "FailedServiceActionAssociation"
      ( \x ->
          FailedServiceActionAssociation'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "ProductId")
            Prelude.<*> (x Data..:? "ProvisioningArtifactId")
            Prelude.<*> (x Data..:? "ServiceActionId")
      )

instance
  Prelude.Hashable
    FailedServiceActionAssociation
  where
  hashWithSalt
    _salt
    FailedServiceActionAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` productId
        `Prelude.hashWithSalt` provisioningArtifactId
        `Prelude.hashWithSalt` serviceActionId

instance
  Prelude.NFData
    FailedServiceActionAssociation
  where
  rnf FailedServiceActionAssociation' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf provisioningArtifactId
      `Prelude.seq` Prelude.rnf serviceActionId
