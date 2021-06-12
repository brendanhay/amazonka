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
-- Module      : Network.AWS.ServiceCatalog.Types.FailedServiceActionAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.FailedServiceActionAssociation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorCode

-- | An object containing information about the error, along with identifying
-- information about the self-service action and its associations.
--
-- /See:/ 'newFailedServiceActionAssociation' smart constructor.
data FailedServiceActionAssociation = FailedServiceActionAssociation'
  { -- | The identifier of the provisioning artifact. For example,
    -- @pa-4abcdjnxjj6ne@.
    provisioningArtifactId :: Core.Maybe Core.Text,
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
    serviceActionId :: Core.Maybe Core.Text,
    -- | The product identifier. For example, @prod-abcdzk7xy33qa@.
    productId :: Core.Maybe Core.Text,
    -- | A text description of the error.
    errorMessage :: Core.Maybe Core.Text,
    -- | The error code. Valid values are listed below.
    errorCode :: Core.Maybe ServiceActionAssociationErrorCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FailedServiceActionAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisioningArtifactId', 'failedServiceActionAssociation_provisioningArtifactId' - The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
--
-- 'serviceActionId', 'failedServiceActionAssociation_serviceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@.
--
-- 'productId', 'failedServiceActionAssociation_productId' - The product identifier. For example, @prod-abcdzk7xy33qa@.
--
-- 'errorMessage', 'failedServiceActionAssociation_errorMessage' - A text description of the error.
--
-- 'errorCode', 'failedServiceActionAssociation_errorCode' - The error code. Valid values are listed below.
newFailedServiceActionAssociation ::
  FailedServiceActionAssociation
newFailedServiceActionAssociation =
  FailedServiceActionAssociation'
    { provisioningArtifactId =
        Core.Nothing,
      serviceActionId = Core.Nothing,
      productId = Core.Nothing,
      errorMessage = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | The identifier of the provisioning artifact. For example,
-- @pa-4abcdjnxjj6ne@.
failedServiceActionAssociation_provisioningArtifactId :: Lens.Lens' FailedServiceActionAssociation (Core.Maybe Core.Text)
failedServiceActionAssociation_provisioningArtifactId = Lens.lens (\FailedServiceActionAssociation' {provisioningArtifactId} -> provisioningArtifactId) (\s@FailedServiceActionAssociation' {} a -> s {provisioningArtifactId = a} :: FailedServiceActionAssociation)

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
failedServiceActionAssociation_serviceActionId :: Lens.Lens' FailedServiceActionAssociation (Core.Maybe Core.Text)
failedServiceActionAssociation_serviceActionId = Lens.lens (\FailedServiceActionAssociation' {serviceActionId} -> serviceActionId) (\s@FailedServiceActionAssociation' {} a -> s {serviceActionId = a} :: FailedServiceActionAssociation)

-- | The product identifier. For example, @prod-abcdzk7xy33qa@.
failedServiceActionAssociation_productId :: Lens.Lens' FailedServiceActionAssociation (Core.Maybe Core.Text)
failedServiceActionAssociation_productId = Lens.lens (\FailedServiceActionAssociation' {productId} -> productId) (\s@FailedServiceActionAssociation' {} a -> s {productId = a} :: FailedServiceActionAssociation)

-- | A text description of the error.
failedServiceActionAssociation_errorMessage :: Lens.Lens' FailedServiceActionAssociation (Core.Maybe Core.Text)
failedServiceActionAssociation_errorMessage = Lens.lens (\FailedServiceActionAssociation' {errorMessage} -> errorMessage) (\s@FailedServiceActionAssociation' {} a -> s {errorMessage = a} :: FailedServiceActionAssociation)

-- | The error code. Valid values are listed below.
failedServiceActionAssociation_errorCode :: Lens.Lens' FailedServiceActionAssociation (Core.Maybe ServiceActionAssociationErrorCode)
failedServiceActionAssociation_errorCode = Lens.lens (\FailedServiceActionAssociation' {errorCode} -> errorCode) (\s@FailedServiceActionAssociation' {} a -> s {errorCode = a} :: FailedServiceActionAssociation)

instance Core.FromJSON FailedServiceActionAssociation where
  parseJSON =
    Core.withObject
      "FailedServiceActionAssociation"
      ( \x ->
          FailedServiceActionAssociation'
            Core.<$> (x Core..:? "ProvisioningArtifactId")
            Core.<*> (x Core..:? "ServiceActionId")
            Core.<*> (x Core..:? "ProductId")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "ErrorCode")
      )

instance Core.Hashable FailedServiceActionAssociation

instance Core.NFData FailedServiceActionAssociation
