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
-- Module      : Network.AWS.SageMaker.Types.ServiceCatalogProvisionedProductDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ServiceCatalogProvisionedProductDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details of a provisioned service catalog product. For information about
-- service catalog, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is AWS Service Catalog>.
--
-- /See:/ 'newServiceCatalogProvisionedProductDetails' smart constructor.
data ServiceCatalogProvisionedProductDetails = ServiceCatalogProvisionedProductDetails'
  { -- | The current status of the product.
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
    provisionedProductStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the provisioned product.
    provisionedProductId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServiceCatalogProvisionedProductDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'provisionedProductId', 'serviceCatalogProvisionedProductDetails_provisionedProductId' - The ID of the provisioned product.
newServiceCatalogProvisionedProductDetails ::
  ServiceCatalogProvisionedProductDetails
newServiceCatalogProvisionedProductDetails =
  ServiceCatalogProvisionedProductDetails'
    { provisionedProductStatusMessage =
        Prelude.Nothing,
      provisionedProductId =
        Prelude.Nothing
    }

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

-- | The ID of the provisioned product.
serviceCatalogProvisionedProductDetails_provisionedProductId :: Lens.Lens' ServiceCatalogProvisionedProductDetails (Prelude.Maybe Prelude.Text)
serviceCatalogProvisionedProductDetails_provisionedProductId = Lens.lens (\ServiceCatalogProvisionedProductDetails' {provisionedProductId} -> provisionedProductId) (\s@ServiceCatalogProvisionedProductDetails' {} a -> s {provisionedProductId = a} :: ServiceCatalogProvisionedProductDetails)

instance
  Prelude.FromJSON
    ServiceCatalogProvisionedProductDetails
  where
  parseJSON =
    Prelude.withObject
      "ServiceCatalogProvisionedProductDetails"
      ( \x ->
          ServiceCatalogProvisionedProductDetails'
            Prelude.<$> (x Prelude..:? "ProvisionedProductStatusMessage")
            Prelude.<*> (x Prelude..:? "ProvisionedProductId")
      )

instance
  Prelude.Hashable
    ServiceCatalogProvisionedProductDetails

instance
  Prelude.NFData
    ServiceCatalogProvisionedProductDetails
