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
-- Module      : Amazonka.SageMaker.Types.ServiceCatalogProvisioningUpdateDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ServiceCatalogProvisioningUpdateDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProvisioningParameter

-- | Details that you specify to provision a service catalog product. For
-- information about service catalog, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is Amazon Web Services Service Catalog>.
--
-- /See:/ 'newServiceCatalogProvisioningUpdateDetails' smart constructor.
data ServiceCatalogProvisioningUpdateDetails = ServiceCatalogProvisioningUpdateDetails'
  { -- | The ID of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | A list of key value pairs that you specify when you provision a product.
    provisioningParameters :: Prelude.Maybe [ProvisioningParameter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceCatalogProvisioningUpdateDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisioningArtifactId', 'serviceCatalogProvisioningUpdateDetails_provisioningArtifactId' - The ID of the provisioning artifact.
--
-- 'provisioningParameters', 'serviceCatalogProvisioningUpdateDetails_provisioningParameters' - A list of key value pairs that you specify when you provision a product.
newServiceCatalogProvisioningUpdateDetails ::
  ServiceCatalogProvisioningUpdateDetails
newServiceCatalogProvisioningUpdateDetails =
  ServiceCatalogProvisioningUpdateDetails'
    { provisioningArtifactId =
        Prelude.Nothing,
      provisioningParameters =
        Prelude.Nothing
    }

-- | The ID of the provisioning artifact.
serviceCatalogProvisioningUpdateDetails_provisioningArtifactId :: Lens.Lens' ServiceCatalogProvisioningUpdateDetails (Prelude.Maybe Prelude.Text)
serviceCatalogProvisioningUpdateDetails_provisioningArtifactId = Lens.lens (\ServiceCatalogProvisioningUpdateDetails' {provisioningArtifactId} -> provisioningArtifactId) (\s@ServiceCatalogProvisioningUpdateDetails' {} a -> s {provisioningArtifactId = a} :: ServiceCatalogProvisioningUpdateDetails)

-- | A list of key value pairs that you specify when you provision a product.
serviceCatalogProvisioningUpdateDetails_provisioningParameters :: Lens.Lens' ServiceCatalogProvisioningUpdateDetails (Prelude.Maybe [ProvisioningParameter])
serviceCatalogProvisioningUpdateDetails_provisioningParameters = Lens.lens (\ServiceCatalogProvisioningUpdateDetails' {provisioningParameters} -> provisioningParameters) (\s@ServiceCatalogProvisioningUpdateDetails' {} a -> s {provisioningParameters = a} :: ServiceCatalogProvisioningUpdateDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    ServiceCatalogProvisioningUpdateDetails
  where
  hashWithSalt
    _salt
    ServiceCatalogProvisioningUpdateDetails' {..} =
      _salt
        `Prelude.hashWithSalt` provisioningArtifactId
        `Prelude.hashWithSalt` provisioningParameters

instance
  Prelude.NFData
    ServiceCatalogProvisioningUpdateDetails
  where
  rnf ServiceCatalogProvisioningUpdateDetails' {..} =
    Prelude.rnf provisioningArtifactId
      `Prelude.seq` Prelude.rnf provisioningParameters

instance
  Data.ToJSON
    ServiceCatalogProvisioningUpdateDetails
  where
  toJSON ServiceCatalogProvisioningUpdateDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ProvisioningArtifactId" Data..=)
              Prelude.<$> provisioningArtifactId,
            ("ProvisioningParameters" Data..=)
              Prelude.<$> provisioningParameters
          ]
      )
