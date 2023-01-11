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
-- Module      : Amazonka.SageMaker.Types.ServiceCatalogProvisioningDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ServiceCatalogProvisioningDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProvisioningParameter

-- | Details that you specify to provision a service catalog product. For
-- information about service catalog, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is Amazon Web Services Service Catalog>.
--
-- /See:/ 'newServiceCatalogProvisioningDetails' smart constructor.
data ServiceCatalogProvisioningDetails = ServiceCatalogProvisioningDetails'
  { -- | The path identifier of the product. This value is optional if the
    -- product has a default path, and required if the product has more than
    -- one path.
    pathId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | A list of key value pairs that you specify when you provision a product.
    provisioningParameters :: Prelude.Maybe [ProvisioningParameter],
    -- | The ID of the product to provision.
    productId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceCatalogProvisioningDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pathId', 'serviceCatalogProvisioningDetails_pathId' - The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path.
--
-- 'provisioningArtifactId', 'serviceCatalogProvisioningDetails_provisioningArtifactId' - The ID of the provisioning artifact.
--
-- 'provisioningParameters', 'serviceCatalogProvisioningDetails_provisioningParameters' - A list of key value pairs that you specify when you provision a product.
--
-- 'productId', 'serviceCatalogProvisioningDetails_productId' - The ID of the product to provision.
newServiceCatalogProvisioningDetails ::
  -- | 'productId'
  Prelude.Text ->
  ServiceCatalogProvisioningDetails
newServiceCatalogProvisioningDetails pProductId_ =
  ServiceCatalogProvisioningDetails'
    { pathId =
        Prelude.Nothing,
      provisioningArtifactId = Prelude.Nothing,
      provisioningParameters = Prelude.Nothing,
      productId = pProductId_
    }

-- | The path identifier of the product. This value is optional if the
-- product has a default path, and required if the product has more than
-- one path.
serviceCatalogProvisioningDetails_pathId :: Lens.Lens' ServiceCatalogProvisioningDetails (Prelude.Maybe Prelude.Text)
serviceCatalogProvisioningDetails_pathId = Lens.lens (\ServiceCatalogProvisioningDetails' {pathId} -> pathId) (\s@ServiceCatalogProvisioningDetails' {} a -> s {pathId = a} :: ServiceCatalogProvisioningDetails)

-- | The ID of the provisioning artifact.
serviceCatalogProvisioningDetails_provisioningArtifactId :: Lens.Lens' ServiceCatalogProvisioningDetails (Prelude.Maybe Prelude.Text)
serviceCatalogProvisioningDetails_provisioningArtifactId = Lens.lens (\ServiceCatalogProvisioningDetails' {provisioningArtifactId} -> provisioningArtifactId) (\s@ServiceCatalogProvisioningDetails' {} a -> s {provisioningArtifactId = a} :: ServiceCatalogProvisioningDetails)

-- | A list of key value pairs that you specify when you provision a product.
serviceCatalogProvisioningDetails_provisioningParameters :: Lens.Lens' ServiceCatalogProvisioningDetails (Prelude.Maybe [ProvisioningParameter])
serviceCatalogProvisioningDetails_provisioningParameters = Lens.lens (\ServiceCatalogProvisioningDetails' {provisioningParameters} -> provisioningParameters) (\s@ServiceCatalogProvisioningDetails' {} a -> s {provisioningParameters = a} :: ServiceCatalogProvisioningDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the product to provision.
serviceCatalogProvisioningDetails_productId :: Lens.Lens' ServiceCatalogProvisioningDetails Prelude.Text
serviceCatalogProvisioningDetails_productId = Lens.lens (\ServiceCatalogProvisioningDetails' {productId} -> productId) (\s@ServiceCatalogProvisioningDetails' {} a -> s {productId = a} :: ServiceCatalogProvisioningDetails)

instance
  Data.FromJSON
    ServiceCatalogProvisioningDetails
  where
  parseJSON =
    Data.withObject
      "ServiceCatalogProvisioningDetails"
      ( \x ->
          ServiceCatalogProvisioningDetails'
            Prelude.<$> (x Data..:? "PathId")
            Prelude.<*> (x Data..:? "ProvisioningArtifactId")
            Prelude.<*> ( x Data..:? "ProvisioningParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "ProductId")
      )

instance
  Prelude.Hashable
    ServiceCatalogProvisioningDetails
  where
  hashWithSalt
    _salt
    ServiceCatalogProvisioningDetails' {..} =
      _salt `Prelude.hashWithSalt` pathId
        `Prelude.hashWithSalt` provisioningArtifactId
        `Prelude.hashWithSalt` provisioningParameters
        `Prelude.hashWithSalt` productId

instance
  Prelude.NFData
    ServiceCatalogProvisioningDetails
  where
  rnf ServiceCatalogProvisioningDetails' {..} =
    Prelude.rnf pathId
      `Prelude.seq` Prelude.rnf provisioningArtifactId
      `Prelude.seq` Prelude.rnf provisioningParameters
      `Prelude.seq` Prelude.rnf productId

instance
  Data.ToJSON
    ServiceCatalogProvisioningDetails
  where
  toJSON ServiceCatalogProvisioningDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PathId" Data..=) Prelude.<$> pathId,
            ("ProvisioningArtifactId" Data..=)
              Prelude.<$> provisioningArtifactId,
            ("ProvisioningParameters" Data..=)
              Prelude.<$> provisioningParameters,
            Prelude.Just ("ProductId" Data..= productId)
          ]
      )
