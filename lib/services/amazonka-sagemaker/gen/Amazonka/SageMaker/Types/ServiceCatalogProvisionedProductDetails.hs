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
-- Module      : Amazonka.SageMaker.Types.ServiceCatalogProvisionedProductDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ServiceCatalogProvisionedProductDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of a provisioned service catalog product. For information about
-- service catalog, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is Amazon Web Services Service Catalog>.
--
-- /See:/ 'newServiceCatalogProvisionedProductDetails' smart constructor.
data ServiceCatalogProvisionedProductDetails = ServiceCatalogProvisionedProductDetails'
  { -- | The ID of the provisioned product.
    provisionedProductId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the product.
    --
    -- -   @AVAILABLE@ - Stable state, ready to perform any operation. The most
    --     recent operation succeeded and completed.
    --
    -- -   @UNDER_CHANGE@ - Transitive state. Operations performed might not
    --     have valid results. Wait for an AVAILABLE status before performing
    --     operations.
    --
    -- -   @TAINTED@ - Stable state, ready to perform any operation. The stack
    --     has completed the requested operation but is not exactly what was
    --     requested. For example, a request to update to a new version failed
    --     and the stack rolled back to the current version.
    --
    -- -   @ERROR@ - An unexpected error occurred. The provisioned product
    --     exists but the stack is not running. For example, CloudFormation
    --     received a parameter value that was not valid and could not launch
    --     the stack.
    --
    -- -   @PLAN_IN_PROGRESS@ - Transitive state. The plan operations were
    --     performed to provision a new product, but resources have not yet
    --     been created. After reviewing the list of resources to be created,
    --     execute the plan. Wait for an AVAILABLE status before performing
    --     operations.
    provisionedProductStatusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceCatalogProvisionedProductDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedProductId', 'serviceCatalogProvisionedProductDetails_provisionedProductId' - The ID of the provisioned product.
--
-- 'provisionedProductStatusMessage', 'serviceCatalogProvisionedProductDetails_provisionedProductStatusMessage' - The current status of the product.
--
-- -   @AVAILABLE@ - Stable state, ready to perform any operation. The most
--     recent operation succeeded and completed.
--
-- -   @UNDER_CHANGE@ - Transitive state. Operations performed might not
--     have valid results. Wait for an AVAILABLE status before performing
--     operations.
--
-- -   @TAINTED@ - Stable state, ready to perform any operation. The stack
--     has completed the requested operation but is not exactly what was
--     requested. For example, a request to update to a new version failed
--     and the stack rolled back to the current version.
--
-- -   @ERROR@ - An unexpected error occurred. The provisioned product
--     exists but the stack is not running. For example, CloudFormation
--     received a parameter value that was not valid and could not launch
--     the stack.
--
-- -   @PLAN_IN_PROGRESS@ - Transitive state. The plan operations were
--     performed to provision a new product, but resources have not yet
--     been created. After reviewing the list of resources to be created,
--     execute the plan. Wait for an AVAILABLE status before performing
--     operations.
newServiceCatalogProvisionedProductDetails ::
  ServiceCatalogProvisionedProductDetails
newServiceCatalogProvisionedProductDetails =
  ServiceCatalogProvisionedProductDetails'
    { provisionedProductId =
        Prelude.Nothing,
      provisionedProductStatusMessage =
        Prelude.Nothing
    }

-- | The ID of the provisioned product.
serviceCatalogProvisionedProductDetails_provisionedProductId :: Lens.Lens' ServiceCatalogProvisionedProductDetails (Prelude.Maybe Prelude.Text)
serviceCatalogProvisionedProductDetails_provisionedProductId = Lens.lens (\ServiceCatalogProvisionedProductDetails' {provisionedProductId} -> provisionedProductId) (\s@ServiceCatalogProvisionedProductDetails' {} a -> s {provisionedProductId = a} :: ServiceCatalogProvisionedProductDetails)

-- | The current status of the product.
--
-- -   @AVAILABLE@ - Stable state, ready to perform any operation. The most
--     recent operation succeeded and completed.
--
-- -   @UNDER_CHANGE@ - Transitive state. Operations performed might not
--     have valid results. Wait for an AVAILABLE status before performing
--     operations.
--
-- -   @TAINTED@ - Stable state, ready to perform any operation. The stack
--     has completed the requested operation but is not exactly what was
--     requested. For example, a request to update to a new version failed
--     and the stack rolled back to the current version.
--
-- -   @ERROR@ - An unexpected error occurred. The provisioned product
--     exists but the stack is not running. For example, CloudFormation
--     received a parameter value that was not valid and could not launch
--     the stack.
--
-- -   @PLAN_IN_PROGRESS@ - Transitive state. The plan operations were
--     performed to provision a new product, but resources have not yet
--     been created. After reviewing the list of resources to be created,
--     execute the plan. Wait for an AVAILABLE status before performing
--     operations.
serviceCatalogProvisionedProductDetails_provisionedProductStatusMessage :: Lens.Lens' ServiceCatalogProvisionedProductDetails (Prelude.Maybe Prelude.Text)
serviceCatalogProvisionedProductDetails_provisionedProductStatusMessage = Lens.lens (\ServiceCatalogProvisionedProductDetails' {provisionedProductStatusMessage} -> provisionedProductStatusMessage) (\s@ServiceCatalogProvisionedProductDetails' {} a -> s {provisionedProductStatusMessage = a} :: ServiceCatalogProvisionedProductDetails)

instance
  Data.FromJSON
    ServiceCatalogProvisionedProductDetails
  where
  parseJSON =
    Data.withObject
      "ServiceCatalogProvisionedProductDetails"
      ( \x ->
          ServiceCatalogProvisionedProductDetails'
            Prelude.<$> (x Data..:? "ProvisionedProductId")
            Prelude.<*> (x Data..:? "ProvisionedProductStatusMessage")
      )

instance
  Prelude.Hashable
    ServiceCatalogProvisionedProductDetails
  where
  hashWithSalt
    _salt
    ServiceCatalogProvisionedProductDetails' {..} =
      _salt
        `Prelude.hashWithSalt` provisionedProductId
        `Prelude.hashWithSalt` provisionedProductStatusMessage

instance
  Prelude.NFData
    ServiceCatalogProvisionedProductDetails
  where
  rnf ServiceCatalogProvisionedProductDetails' {..} =
    Prelude.rnf provisionedProductId
      `Prelude.seq` Prelude.rnf provisionedProductStatusMessage
