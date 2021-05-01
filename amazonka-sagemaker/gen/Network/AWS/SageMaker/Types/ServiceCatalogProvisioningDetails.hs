{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ProvisioningParameter

-- | Details that you specify to provision a service catalog product. For
-- information about service catalog, see
-- .<https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is AWS Service Catalog>.
--
-- /See:/ 'newServiceCatalogProvisioningDetails' smart constructor.
data ServiceCatalogProvisioningDetails = ServiceCatalogProvisioningDetails'
  { -- | A list of key value pairs that you specify when you provision a product.
    provisioningParameters :: Prelude.Maybe [ProvisioningParameter],
    -- | The path identifier of the product. This value is optional if the
    -- product has a default path, and required if the product has more than
    -- one path.
    pathId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the product to provision.
    productId :: Prelude.Text,
    -- | The ID of the provisioning artifact.
    provisioningArtifactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'provisioningArtifactId'
  Prelude.Text ->
  ServiceCatalogProvisioningDetails
newServiceCatalogProvisioningDetails
  pProductId_
  pProvisioningArtifactId_ =
    ServiceCatalogProvisioningDetails'
      { provisioningParameters =
          Prelude.Nothing,
        pathId = Prelude.Nothing,
        productId = pProductId_,
        provisioningArtifactId =
          pProvisioningArtifactId_
      }

-- | A list of key value pairs that you specify when you provision a product.
serviceCatalogProvisioningDetails_provisioningParameters :: Lens.Lens' ServiceCatalogProvisioningDetails (Prelude.Maybe [ProvisioningParameter])
serviceCatalogProvisioningDetails_provisioningParameters = Lens.lens (\ServiceCatalogProvisioningDetails' {provisioningParameters} -> provisioningParameters) (\s@ServiceCatalogProvisioningDetails' {} a -> s {provisioningParameters = a} :: ServiceCatalogProvisioningDetails) Prelude.. Lens.mapping Prelude._Coerce

-- | The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path.
serviceCatalogProvisioningDetails_pathId :: Lens.Lens' ServiceCatalogProvisioningDetails (Prelude.Maybe Prelude.Text)
serviceCatalogProvisioningDetails_pathId = Lens.lens (\ServiceCatalogProvisioningDetails' {pathId} -> pathId) (\s@ServiceCatalogProvisioningDetails' {} a -> s {pathId = a} :: ServiceCatalogProvisioningDetails)

-- | The ID of the product to provision.
serviceCatalogProvisioningDetails_productId :: Lens.Lens' ServiceCatalogProvisioningDetails Prelude.Text
serviceCatalogProvisioningDetails_productId = Lens.lens (\ServiceCatalogProvisioningDetails' {productId} -> productId) (\s@ServiceCatalogProvisioningDetails' {} a -> s {productId = a} :: ServiceCatalogProvisioningDetails)

-- | The ID of the provisioning artifact.
serviceCatalogProvisioningDetails_provisioningArtifactId :: Lens.Lens' ServiceCatalogProvisioningDetails Prelude.Text
serviceCatalogProvisioningDetails_provisioningArtifactId = Lens.lens (\ServiceCatalogProvisioningDetails' {provisioningArtifactId} -> provisioningArtifactId) (\s@ServiceCatalogProvisioningDetails' {} a -> s {provisioningArtifactId = a} :: ServiceCatalogProvisioningDetails)

instance
  Prelude.FromJSON
    ServiceCatalogProvisioningDetails
  where
  parseJSON =
    Prelude.withObject
      "ServiceCatalogProvisioningDetails"
      ( \x ->
          ServiceCatalogProvisioningDetails'
            Prelude.<$> ( x Prelude..:? "ProvisioningParameters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "PathId")
            Prelude.<*> (x Prelude..: "ProductId")
            Prelude.<*> (x Prelude..: "ProvisioningArtifactId")
      )

instance
  Prelude.Hashable
    ServiceCatalogProvisioningDetails

instance
  Prelude.NFData
    ServiceCatalogProvisioningDetails

instance
  Prelude.ToJSON
    ServiceCatalogProvisioningDetails
  where
  toJSON ServiceCatalogProvisioningDetails' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ProvisioningParameters" Prelude..=)
              Prelude.<$> provisioningParameters,
            ("PathId" Prelude..=) Prelude.<$> pathId,
            Prelude.Just ("ProductId" Prelude..= productId),
            Prelude.Just
              ( "ProvisioningArtifactId"
                  Prelude..= provisioningArtifactId
              )
          ]
      )
