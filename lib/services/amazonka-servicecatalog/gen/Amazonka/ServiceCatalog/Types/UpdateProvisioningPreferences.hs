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
-- Module      : Amazonka.ServiceCatalog.Types.UpdateProvisioningPreferences
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.UpdateProvisioningPreferences where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.StackSetOperationType

-- | The user-defined preferences that will be applied when updating a
-- provisioned product. Not all preferences are applicable to all
-- provisioned product types.
--
-- /See:/ 'newUpdateProvisioningPreferences' smart constructor.
data UpdateProvisioningPreferences = UpdateProvisioningPreferences'
  { -- | One or more AWS Regions where the provisioned product will be available.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    -- The specified regions should be within the list of regions from the
    -- @STACKSET@ constraint. To get the list of regions in the @STACKSET@
    -- constraint, use the @DescribeProvisioningParameters@ operation.
    --
    -- If no values are specified, the default value is all regions from the
    -- @STACKSET@ constraint.
    stackSetRegions :: Prelude.Maybe [Prelude.Text],
    -- | The maximum percentage of accounts in which to perform this operation at
    -- one time.
    --
    -- When calculating the number of accounts based on the specified
    -- percentage, AWS Service Catalog rounds down to the next whole number.
    -- This is true except in cases where rounding down would result is zero.
    -- In this case, AWS Service Catalog sets the number as @1@ instead.
    --
    -- Note that this setting lets you specify the maximum for operations. For
    -- large deployments, under certain circumstances the actual number of
    -- accounts acted upon concurrently may be lower due to service throttling.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    -- Conditional: You must specify either @StackSetMaxConcurrentCount@ or
    -- @StackSetMaxConcurrentPercentage@, but not both.
    stackSetMaxConcurrencyPercentage :: Prelude.Maybe Prelude.Natural,
    -- | The number of accounts, per region, for which this operation can fail
    -- before AWS Service Catalog stops the operation in that region. If the
    -- operation is stopped in a region, AWS Service Catalog doesn\'t attempt
    -- the operation in any subsequent regions.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    -- Conditional: You must specify either @StackSetFailureToleranceCount@ or
    -- @StackSetFailureTolerancePercentage@, but not both.
    --
    -- The default value is @0@ if no value is specified.
    stackSetFailureToleranceCount :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of accounts, per region, for which this stack operation
    -- can fail before AWS Service Catalog stops the operation in that region.
    -- If the operation is stopped in a region, AWS Service Catalog doesn\'t
    -- attempt the operation in any subsequent regions.
    --
    -- When calculating the number of accounts based on the specified
    -- percentage, AWS Service Catalog rounds down to the next whole number.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    -- Conditional: You must specify either @StackSetFailureToleranceCount@ or
    -- @StackSetFailureTolerancePercentage@, but not both.
    stackSetFailureTolerancePercentage :: Prelude.Maybe Prelude.Natural,
    -- | One or more AWS accounts that will have access to the provisioned
    -- product.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    -- The AWS accounts specified should be within the list of accounts in the
    -- @STACKSET@ constraint. To get the list of accounts in the @STACKSET@
    -- constraint, use the @DescribeProvisioningParameters@ operation.
    --
    -- If no values are specified, the default value is all accounts from the
    -- @STACKSET@ constraint.
    stackSetAccounts :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of accounts in which to perform this operation at one
    -- time. This is dependent on the value of @StackSetFailureToleranceCount@.
    -- @StackSetMaxConcurrentCount@ is at most one more than the
    -- @StackSetFailureToleranceCount@.
    --
    -- Note that this setting lets you specify the maximum for operations. For
    -- large deployments, under certain circumstances the actual number of
    -- accounts acted upon concurrently may be lower due to service throttling.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    -- Conditional: You must specify either @StackSetMaxConcurrentCount@ or
    -- @StackSetMaxConcurrentPercentage@, but not both.
    stackSetMaxConcurrencyCount :: Prelude.Maybe Prelude.Natural,
    -- | Determines what action AWS Service Catalog performs to a stack set or a
    -- stack instance represented by the provisioned product. The default value
    -- is @UPDATE@ if nothing is specified.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    -- [CREATE]
    --     Creates a new stack instance in the stack set represented by the
    --     provisioned product. In this case, only new stack instances are
    --     created based on accounts and regions; if new ProductId or
    --     ProvisioningArtifactID are passed, they will be ignored.
    --
    -- [UPDATE]
    --     Updates the stack set represented by the provisioned product and
    --     also its stack instances.
    --
    -- [DELETE]
    --     Deletes a stack instance in the stack set represented by the
    --     provisioned product.
    stackSetOperationType :: Prelude.Maybe StackSetOperationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProvisioningPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackSetRegions', 'updateProvisioningPreferences_stackSetRegions' - One or more AWS Regions where the provisioned product will be available.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- The specified regions should be within the list of regions from the
-- @STACKSET@ constraint. To get the list of regions in the @STACKSET@
-- constraint, use the @DescribeProvisioningParameters@ operation.
--
-- If no values are specified, the default value is all regions from the
-- @STACKSET@ constraint.
--
-- 'stackSetMaxConcurrencyPercentage', 'updateProvisioningPreferences_stackSetMaxConcurrencyPercentage' - The maximum percentage of accounts in which to perform this operation at
-- one time.
--
-- When calculating the number of accounts based on the specified
-- percentage, AWS Service Catalog rounds down to the next whole number.
-- This is true except in cases where rounding down would result is zero.
-- In this case, AWS Service Catalog sets the number as @1@ instead.
--
-- Note that this setting lets you specify the maximum for operations. For
-- large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or
-- @StackSetMaxConcurrentPercentage@, but not both.
--
-- 'stackSetFailureToleranceCount', 'updateProvisioningPreferences_stackSetFailureToleranceCount' - The number of accounts, per region, for which this operation can fail
-- before AWS Service Catalog stops the operation in that region. If the
-- operation is stopped in a region, AWS Service Catalog doesn\'t attempt
-- the operation in any subsequent regions.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or
-- @StackSetFailureTolerancePercentage@, but not both.
--
-- The default value is @0@ if no value is specified.
--
-- 'stackSetFailureTolerancePercentage', 'updateProvisioningPreferences_stackSetFailureTolerancePercentage' - The percentage of accounts, per region, for which this stack operation
-- can fail before AWS Service Catalog stops the operation in that region.
-- If the operation is stopped in a region, AWS Service Catalog doesn\'t
-- attempt the operation in any subsequent regions.
--
-- When calculating the number of accounts based on the specified
-- percentage, AWS Service Catalog rounds down to the next whole number.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or
-- @StackSetFailureTolerancePercentage@, but not both.
--
-- 'stackSetAccounts', 'updateProvisioningPreferences_stackSetAccounts' - One or more AWS accounts that will have access to the provisioned
-- product.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- The AWS accounts specified should be within the list of accounts in the
-- @STACKSET@ constraint. To get the list of accounts in the @STACKSET@
-- constraint, use the @DescribeProvisioningParameters@ operation.
--
-- If no values are specified, the default value is all accounts from the
-- @STACKSET@ constraint.
--
-- 'stackSetMaxConcurrencyCount', 'updateProvisioningPreferences_stackSetMaxConcurrencyCount' - The maximum number of accounts in which to perform this operation at one
-- time. This is dependent on the value of @StackSetFailureToleranceCount@.
-- @StackSetMaxConcurrentCount@ is at most one more than the
-- @StackSetFailureToleranceCount@.
--
-- Note that this setting lets you specify the maximum for operations. For
-- large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or
-- @StackSetMaxConcurrentPercentage@, but not both.
--
-- 'stackSetOperationType', 'updateProvisioningPreferences_stackSetOperationType' - Determines what action AWS Service Catalog performs to a stack set or a
-- stack instance represented by the provisioned product. The default value
-- is @UPDATE@ if nothing is specified.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- [CREATE]
--     Creates a new stack instance in the stack set represented by the
--     provisioned product. In this case, only new stack instances are
--     created based on accounts and regions; if new ProductId or
--     ProvisioningArtifactID are passed, they will be ignored.
--
-- [UPDATE]
--     Updates the stack set represented by the provisioned product and
--     also its stack instances.
--
-- [DELETE]
--     Deletes a stack instance in the stack set represented by the
--     provisioned product.
newUpdateProvisioningPreferences ::
  UpdateProvisioningPreferences
newUpdateProvisioningPreferences =
  UpdateProvisioningPreferences'
    { stackSetRegions =
        Prelude.Nothing,
      stackSetMaxConcurrencyPercentage =
        Prelude.Nothing,
      stackSetFailureToleranceCount =
        Prelude.Nothing,
      stackSetFailureTolerancePercentage =
        Prelude.Nothing,
      stackSetAccounts = Prelude.Nothing,
      stackSetMaxConcurrencyCount =
        Prelude.Nothing,
      stackSetOperationType = Prelude.Nothing
    }

-- | One or more AWS Regions where the provisioned product will be available.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- The specified regions should be within the list of regions from the
-- @STACKSET@ constraint. To get the list of regions in the @STACKSET@
-- constraint, use the @DescribeProvisioningParameters@ operation.
--
-- If no values are specified, the default value is all regions from the
-- @STACKSET@ constraint.
updateProvisioningPreferences_stackSetRegions :: Lens.Lens' UpdateProvisioningPreferences (Prelude.Maybe [Prelude.Text])
updateProvisioningPreferences_stackSetRegions = Lens.lens (\UpdateProvisioningPreferences' {stackSetRegions} -> stackSetRegions) (\s@UpdateProvisioningPreferences' {} a -> s {stackSetRegions = a} :: UpdateProvisioningPreferences) Prelude.. Lens.mapping Lens.coerced

-- | The maximum percentage of accounts in which to perform this operation at
-- one time.
--
-- When calculating the number of accounts based on the specified
-- percentage, AWS Service Catalog rounds down to the next whole number.
-- This is true except in cases where rounding down would result is zero.
-- In this case, AWS Service Catalog sets the number as @1@ instead.
--
-- Note that this setting lets you specify the maximum for operations. For
-- large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or
-- @StackSetMaxConcurrentPercentage@, but not both.
updateProvisioningPreferences_stackSetMaxConcurrencyPercentage :: Lens.Lens' UpdateProvisioningPreferences (Prelude.Maybe Prelude.Natural)
updateProvisioningPreferences_stackSetMaxConcurrencyPercentage = Lens.lens (\UpdateProvisioningPreferences' {stackSetMaxConcurrencyPercentage} -> stackSetMaxConcurrencyPercentage) (\s@UpdateProvisioningPreferences' {} a -> s {stackSetMaxConcurrencyPercentage = a} :: UpdateProvisioningPreferences)

-- | The number of accounts, per region, for which this operation can fail
-- before AWS Service Catalog stops the operation in that region. If the
-- operation is stopped in a region, AWS Service Catalog doesn\'t attempt
-- the operation in any subsequent regions.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or
-- @StackSetFailureTolerancePercentage@, but not both.
--
-- The default value is @0@ if no value is specified.
updateProvisioningPreferences_stackSetFailureToleranceCount :: Lens.Lens' UpdateProvisioningPreferences (Prelude.Maybe Prelude.Natural)
updateProvisioningPreferences_stackSetFailureToleranceCount = Lens.lens (\UpdateProvisioningPreferences' {stackSetFailureToleranceCount} -> stackSetFailureToleranceCount) (\s@UpdateProvisioningPreferences' {} a -> s {stackSetFailureToleranceCount = a} :: UpdateProvisioningPreferences)

-- | The percentage of accounts, per region, for which this stack operation
-- can fail before AWS Service Catalog stops the operation in that region.
-- If the operation is stopped in a region, AWS Service Catalog doesn\'t
-- attempt the operation in any subsequent regions.
--
-- When calculating the number of accounts based on the specified
-- percentage, AWS Service Catalog rounds down to the next whole number.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or
-- @StackSetFailureTolerancePercentage@, but not both.
updateProvisioningPreferences_stackSetFailureTolerancePercentage :: Lens.Lens' UpdateProvisioningPreferences (Prelude.Maybe Prelude.Natural)
updateProvisioningPreferences_stackSetFailureTolerancePercentage = Lens.lens (\UpdateProvisioningPreferences' {stackSetFailureTolerancePercentage} -> stackSetFailureTolerancePercentage) (\s@UpdateProvisioningPreferences' {} a -> s {stackSetFailureTolerancePercentage = a} :: UpdateProvisioningPreferences)

-- | One or more AWS accounts that will have access to the provisioned
-- product.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- The AWS accounts specified should be within the list of accounts in the
-- @STACKSET@ constraint. To get the list of accounts in the @STACKSET@
-- constraint, use the @DescribeProvisioningParameters@ operation.
--
-- If no values are specified, the default value is all accounts from the
-- @STACKSET@ constraint.
updateProvisioningPreferences_stackSetAccounts :: Lens.Lens' UpdateProvisioningPreferences (Prelude.Maybe [Prelude.Text])
updateProvisioningPreferences_stackSetAccounts = Lens.lens (\UpdateProvisioningPreferences' {stackSetAccounts} -> stackSetAccounts) (\s@UpdateProvisioningPreferences' {} a -> s {stackSetAccounts = a} :: UpdateProvisioningPreferences) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of accounts in which to perform this operation at one
-- time. This is dependent on the value of @StackSetFailureToleranceCount@.
-- @StackSetMaxConcurrentCount@ is at most one more than the
-- @StackSetFailureToleranceCount@.
--
-- Note that this setting lets you specify the maximum for operations. For
-- large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or
-- @StackSetMaxConcurrentPercentage@, but not both.
updateProvisioningPreferences_stackSetMaxConcurrencyCount :: Lens.Lens' UpdateProvisioningPreferences (Prelude.Maybe Prelude.Natural)
updateProvisioningPreferences_stackSetMaxConcurrencyCount = Lens.lens (\UpdateProvisioningPreferences' {stackSetMaxConcurrencyCount} -> stackSetMaxConcurrencyCount) (\s@UpdateProvisioningPreferences' {} a -> s {stackSetMaxConcurrencyCount = a} :: UpdateProvisioningPreferences)

-- | Determines what action AWS Service Catalog performs to a stack set or a
-- stack instance represented by the provisioned product. The default value
-- is @UPDATE@ if nothing is specified.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- [CREATE]
--     Creates a new stack instance in the stack set represented by the
--     provisioned product. In this case, only new stack instances are
--     created based on accounts and regions; if new ProductId or
--     ProvisioningArtifactID are passed, they will be ignored.
--
-- [UPDATE]
--     Updates the stack set represented by the provisioned product and
--     also its stack instances.
--
-- [DELETE]
--     Deletes a stack instance in the stack set represented by the
--     provisioned product.
updateProvisioningPreferences_stackSetOperationType :: Lens.Lens' UpdateProvisioningPreferences (Prelude.Maybe StackSetOperationType)
updateProvisioningPreferences_stackSetOperationType = Lens.lens (\UpdateProvisioningPreferences' {stackSetOperationType} -> stackSetOperationType) (\s@UpdateProvisioningPreferences' {} a -> s {stackSetOperationType = a} :: UpdateProvisioningPreferences)

instance
  Prelude.Hashable
    UpdateProvisioningPreferences
  where
  hashWithSalt _salt UpdateProvisioningPreferences' {..} =
    _salt `Prelude.hashWithSalt` stackSetRegions
      `Prelude.hashWithSalt` stackSetMaxConcurrencyPercentage
      `Prelude.hashWithSalt` stackSetFailureToleranceCount
      `Prelude.hashWithSalt` stackSetFailureTolerancePercentage
      `Prelude.hashWithSalt` stackSetAccounts
      `Prelude.hashWithSalt` stackSetMaxConcurrencyCount
      `Prelude.hashWithSalt` stackSetOperationType

instance Prelude.NFData UpdateProvisioningPreferences where
  rnf UpdateProvisioningPreferences' {..} =
    Prelude.rnf stackSetRegions
      `Prelude.seq` Prelude.rnf stackSetMaxConcurrencyPercentage
      `Prelude.seq` Prelude.rnf stackSetFailureToleranceCount
      `Prelude.seq` Prelude.rnf stackSetFailureTolerancePercentage
      `Prelude.seq` Prelude.rnf stackSetAccounts
      `Prelude.seq` Prelude.rnf stackSetMaxConcurrencyCount
      `Prelude.seq` Prelude.rnf stackSetOperationType

instance Core.ToJSON UpdateProvisioningPreferences where
  toJSON UpdateProvisioningPreferences' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StackSetRegions" Core..=)
              Prelude.<$> stackSetRegions,
            ("StackSetMaxConcurrencyPercentage" Core..=)
              Prelude.<$> stackSetMaxConcurrencyPercentage,
            ("StackSetFailureToleranceCount" Core..=)
              Prelude.<$> stackSetFailureToleranceCount,
            ("StackSetFailureTolerancePercentage" Core..=)
              Prelude.<$> stackSetFailureTolerancePercentage,
            ("StackSetAccounts" Core..=)
              Prelude.<$> stackSetAccounts,
            ("StackSetMaxConcurrencyCount" Core..=)
              Prelude.<$> stackSetMaxConcurrencyCount,
            ("StackSetOperationType" Core..=)
              Prelude.<$> stackSetOperationType
          ]
      )
