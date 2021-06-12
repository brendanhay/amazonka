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
-- Module      : Network.AWS.SageMaker.Types.ServiceCatalogProvisioningDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ServiceCatalogProvisioningDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ProvisioningParameter

-- | Details that you specify to provision a service catalog product. For
-- information about service catalog, see
-- .<https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is AWS Service Catalog>.
--
-- /See:/ 'newServiceCatalogProvisioningDetails' smart constructor.
data ServiceCatalogProvisioningDetails = ServiceCatalogProvisioningDetails'
  { -- | A list of key value pairs that you specify when you provision a product.
    provisioningParameters :: Core.Maybe [ProvisioningParameter],
    -- | The path identifier of the product. This value is optional if the
    -- product has a default path, and required if the product has more than
    -- one path.
    pathId :: Core.Maybe Core.Text,
    -- | The ID of the product to provision.
    productId :: Core.Text,
    -- | The ID of the provisioning artifact.
    provisioningArtifactId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServiceCatalogProvisioningDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisioningParameters', 'serviceCatalogProvisioningDetails_provisioningParameters' - A list of key value pairs that you specify when you provision a product.
--
-- 'pathId', 'serviceCatalogProvisioningDetails_pathId' - The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path.
--
-- 'productId', 'serviceCatalogProvisioningDetails_productId' - The ID of the product to provision.
--
-- 'provisioningArtifactId', 'serviceCatalogProvisioningDetails_provisioningArtifactId' - The ID of the provisioning artifact.
newServiceCatalogProvisioningDetails ::
  -- | 'productId'
  Core.Text ->
  -- | 'provisioningArtifactId'
  Core.Text ->
  ServiceCatalogProvisioningDetails
newServiceCatalogProvisioningDetails
  pProductId_
  pProvisioningArtifactId_ =
    ServiceCatalogProvisioningDetails'
      { provisioningParameters =
          Core.Nothing,
        pathId = Core.Nothing,
        productId = pProductId_,
        provisioningArtifactId =
          pProvisioningArtifactId_
      }

-- | A list of key value pairs that you specify when you provision a product.
serviceCatalogProvisioningDetails_provisioningParameters :: Lens.Lens' ServiceCatalogProvisioningDetails (Core.Maybe [ProvisioningParameter])
serviceCatalogProvisioningDetails_provisioningParameters = Lens.lens (\ServiceCatalogProvisioningDetails' {provisioningParameters} -> provisioningParameters) (\s@ServiceCatalogProvisioningDetails' {} a -> s {provisioningParameters = a} :: ServiceCatalogProvisioningDetails) Core.. Lens.mapping Lens._Coerce

-- | The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path.
serviceCatalogProvisioningDetails_pathId :: Lens.Lens' ServiceCatalogProvisioningDetails (Core.Maybe Core.Text)
serviceCatalogProvisioningDetails_pathId = Lens.lens (\ServiceCatalogProvisioningDetails' {pathId} -> pathId) (\s@ServiceCatalogProvisioningDetails' {} a -> s {pathId = a} :: ServiceCatalogProvisioningDetails)

-- | The ID of the product to provision.
serviceCatalogProvisioningDetails_productId :: Lens.Lens' ServiceCatalogProvisioningDetails Core.Text
serviceCatalogProvisioningDetails_productId = Lens.lens (\ServiceCatalogProvisioningDetails' {productId} -> productId) (\s@ServiceCatalogProvisioningDetails' {} a -> s {productId = a} :: ServiceCatalogProvisioningDetails)

-- | The ID of the provisioning artifact.
serviceCatalogProvisioningDetails_provisioningArtifactId :: Lens.Lens' ServiceCatalogProvisioningDetails Core.Text
serviceCatalogProvisioningDetails_provisioningArtifactId = Lens.lens (\ServiceCatalogProvisioningDetails' {provisioningArtifactId} -> provisioningArtifactId) (\s@ServiceCatalogProvisioningDetails' {} a -> s {provisioningArtifactId = a} :: ServiceCatalogProvisioningDetails)

instance
  Core.FromJSON
    ServiceCatalogProvisioningDetails
  where
  parseJSON =
    Core.withObject
      "ServiceCatalogProvisioningDetails"
      ( \x ->
          ServiceCatalogProvisioningDetails'
            Core.<$> ( x Core..:? "ProvisioningParameters"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "PathId")
            Core.<*> (x Core..: "ProductId")
            Core.<*> (x Core..: "ProvisioningArtifactId")
      )

instance
  Core.Hashable
    ServiceCatalogProvisioningDetails

instance
  Core.NFData
    ServiceCatalogProvisioningDetails

instance
  Core.ToJSON
    ServiceCatalogProvisioningDetails
  where
  toJSON ServiceCatalogProvisioningDetails' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ProvisioningParameters" Core..=)
              Core.<$> provisioningParameters,
            ("PathId" Core..=) Core.<$> pathId,
            Core.Just ("ProductId" Core..= productId),
            Core.Just
              ( "ProvisioningArtifactId"
                  Core..= provisioningArtifactId
              )
          ]
      )
