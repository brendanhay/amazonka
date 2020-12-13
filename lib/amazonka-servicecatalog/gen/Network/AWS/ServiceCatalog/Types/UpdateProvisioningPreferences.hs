{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.UpdateProvisioningPreferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.UpdateProvisioningPreferences
  ( UpdateProvisioningPreferences (..),

    -- * Smart constructor
    mkUpdateProvisioningPreferences,

    -- * Lenses
    uppStackSetRegions,
    uppStackSetMaxConcurrencyPercentage,
    uppStackSetFailureToleranceCount,
    uppStackSetFailureTolerancePercentage,
    uppStackSetAccounts,
    uppStackSetMaxConcurrencyCount,
    uppStackSetOperationType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.StackSetOperationType

-- | The user-defined preferences that will be applied when updating a provisioned product. Not all preferences are applicable to all provisioned product types.
--
-- /See:/ 'mkUpdateProvisioningPreferences' smart constructor.
data UpdateProvisioningPreferences = UpdateProvisioningPreferences'
  { -- | One or more AWS Regions where the provisioned product will be available.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    -- The specified regions should be within the list of regions from the @STACKSET@ constraint. To get the list of regions in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
    -- If no values are specified, the default value is all regions from the @STACKSET@ constraint.
    stackSetRegions :: Lude.Maybe [Lude.Text],
    -- | The maximum percentage of accounts in which to perform this operation at one time.
    --
    -- When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, AWS Service Catalog sets the number as @1@ instead.
    -- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    -- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
    stackSetMaxConcurrencyPercentage :: Lude.Maybe Lude.Natural,
    -- | The number of accounts, per region, for which this operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    -- Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
    -- The default value is @0@ if no value is specified.
    stackSetFailureToleranceCount :: Lude.Maybe Lude.Natural,
    -- | The percentage of accounts, per region, for which this stack operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions.
    --
    -- When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number.
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    -- Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
    stackSetFailureTolerancePercentage :: Lude.Maybe Lude.Natural,
    -- | One or more AWS accounts that will have access to the provisioned product.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    -- The AWS accounts specified should be within the list of accounts in the @STACKSET@ constraint. To get the list of accounts in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
    -- If no values are specified, the default value is all accounts from the @STACKSET@ constraint.
    stackSetAccounts :: Lude.Maybe [Lude.Text],
    -- | The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @StackSetFailureToleranceCount@ . @StackSetMaxConcurrentCount@ is at most one more than the @StackSetFailureToleranceCount@ .
    --
    -- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    -- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
    stackSetMaxConcurrencyCount :: Lude.Maybe Lude.Natural,
    -- | Determines what action AWS Service Catalog performs to a stack set or a stack instance represented by the provisioned product. The default value is @UPDATE@ if nothing is specified.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    --     * CREATE
    --
    --     * Creates a new stack instance in the stack set represented by the provisioned product. In this case, only new stack instances are created based on accounts and regions; if new ProductId or ProvisioningArtifactID are passed, they will be ignored.
    --
    --
    --     * UPDATE
    --
    --     * Updates the stack set represented by the provisioned product and also its stack instances.
    --
    --
    --     * DELETE
    --
    --     * Deletes a stack instance in the stack set represented by the provisioned product.
    stackSetOperationType :: Lude.Maybe StackSetOperationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProvisioningPreferences' with the minimum fields required to make a request.
--
-- * 'stackSetRegions' - One or more AWS Regions where the provisioned product will be available.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- The specified regions should be within the list of regions from the @STACKSET@ constraint. To get the list of regions in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
-- If no values are specified, the default value is all regions from the @STACKSET@ constraint.
-- * 'stackSetMaxConcurrencyPercentage' - The maximum percentage of accounts in which to perform this operation at one time.
--
-- When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, AWS Service Catalog sets the number as @1@ instead.
-- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
-- * 'stackSetFailureToleranceCount' - The number of accounts, per region, for which this operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
-- The default value is @0@ if no value is specified.
-- * 'stackSetFailureTolerancePercentage' - The percentage of accounts, per region, for which this stack operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions.
--
-- When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
-- * 'stackSetAccounts' - One or more AWS accounts that will have access to the provisioned product.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- The AWS accounts specified should be within the list of accounts in the @STACKSET@ constraint. To get the list of accounts in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
-- If no values are specified, the default value is all accounts from the @STACKSET@ constraint.
-- * 'stackSetMaxConcurrencyCount' - The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @StackSetFailureToleranceCount@ . @StackSetMaxConcurrentCount@ is at most one more than the @StackSetFailureToleranceCount@ .
--
-- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
-- * 'stackSetOperationType' - Determines what action AWS Service Catalog performs to a stack set or a stack instance represented by the provisioned product. The default value is @UPDATE@ if nothing is specified.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
--     * CREATE
--
--     * Creates a new stack instance in the stack set represented by the provisioned product. In this case, only new stack instances are created based on accounts and regions; if new ProductId or ProvisioningArtifactID are passed, they will be ignored.
--
--
--     * UPDATE
--
--     * Updates the stack set represented by the provisioned product and also its stack instances.
--
--
--     * DELETE
--
--     * Deletes a stack instance in the stack set represented by the provisioned product.
mkUpdateProvisioningPreferences ::
  UpdateProvisioningPreferences
mkUpdateProvisioningPreferences =
  UpdateProvisioningPreferences'
    { stackSetRegions = Lude.Nothing,
      stackSetMaxConcurrencyPercentage = Lude.Nothing,
      stackSetFailureToleranceCount = Lude.Nothing,
      stackSetFailureTolerancePercentage = Lude.Nothing,
      stackSetAccounts = Lude.Nothing,
      stackSetMaxConcurrencyCount = Lude.Nothing,
      stackSetOperationType = Lude.Nothing
    }

-- | One or more AWS Regions where the provisioned product will be available.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- The specified regions should be within the list of regions from the @STACKSET@ constraint. To get the list of regions in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
-- If no values are specified, the default value is all regions from the @STACKSET@ constraint.
--
-- /Note:/ Consider using 'stackSetRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppStackSetRegions :: Lens.Lens' UpdateProvisioningPreferences (Lude.Maybe [Lude.Text])
uppStackSetRegions = Lens.lens (stackSetRegions :: UpdateProvisioningPreferences -> Lude.Maybe [Lude.Text]) (\s a -> s {stackSetRegions = a} :: UpdateProvisioningPreferences)
{-# DEPRECATED uppStackSetRegions "Use generic-lens or generic-optics with 'stackSetRegions' instead." #-}

-- | The maximum percentage of accounts in which to perform this operation at one time.
--
-- When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, AWS Service Catalog sets the number as @1@ instead.
-- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
--
-- /Note:/ Consider using 'stackSetMaxConcurrencyPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppStackSetMaxConcurrencyPercentage :: Lens.Lens' UpdateProvisioningPreferences (Lude.Maybe Lude.Natural)
uppStackSetMaxConcurrencyPercentage = Lens.lens (stackSetMaxConcurrencyPercentage :: UpdateProvisioningPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {stackSetMaxConcurrencyPercentage = a} :: UpdateProvisioningPreferences)
{-# DEPRECATED uppStackSetMaxConcurrencyPercentage "Use generic-lens or generic-optics with 'stackSetMaxConcurrencyPercentage' instead." #-}

-- | The number of accounts, per region, for which this operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
-- The default value is @0@ if no value is specified.
--
-- /Note:/ Consider using 'stackSetFailureToleranceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppStackSetFailureToleranceCount :: Lens.Lens' UpdateProvisioningPreferences (Lude.Maybe Lude.Natural)
uppStackSetFailureToleranceCount = Lens.lens (stackSetFailureToleranceCount :: UpdateProvisioningPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {stackSetFailureToleranceCount = a} :: UpdateProvisioningPreferences)
{-# DEPRECATED uppStackSetFailureToleranceCount "Use generic-lens or generic-optics with 'stackSetFailureToleranceCount' instead." #-}

-- | The percentage of accounts, per region, for which this stack operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions.
--
-- When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
--
-- /Note:/ Consider using 'stackSetFailureTolerancePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppStackSetFailureTolerancePercentage :: Lens.Lens' UpdateProvisioningPreferences (Lude.Maybe Lude.Natural)
uppStackSetFailureTolerancePercentage = Lens.lens (stackSetFailureTolerancePercentage :: UpdateProvisioningPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {stackSetFailureTolerancePercentage = a} :: UpdateProvisioningPreferences)
{-# DEPRECATED uppStackSetFailureTolerancePercentage "Use generic-lens or generic-optics with 'stackSetFailureTolerancePercentage' instead." #-}

-- | One or more AWS accounts that will have access to the provisioned product.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- The AWS accounts specified should be within the list of accounts in the @STACKSET@ constraint. To get the list of accounts in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
-- If no values are specified, the default value is all accounts from the @STACKSET@ constraint.
--
-- /Note:/ Consider using 'stackSetAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppStackSetAccounts :: Lens.Lens' UpdateProvisioningPreferences (Lude.Maybe [Lude.Text])
uppStackSetAccounts = Lens.lens (stackSetAccounts :: UpdateProvisioningPreferences -> Lude.Maybe [Lude.Text]) (\s a -> s {stackSetAccounts = a} :: UpdateProvisioningPreferences)
{-# DEPRECATED uppStackSetAccounts "Use generic-lens or generic-optics with 'stackSetAccounts' instead." #-}

-- | The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @StackSetFailureToleranceCount@ . @StackSetMaxConcurrentCount@ is at most one more than the @StackSetFailureToleranceCount@ .
--
-- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
--
-- /Note:/ Consider using 'stackSetMaxConcurrencyCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppStackSetMaxConcurrencyCount :: Lens.Lens' UpdateProvisioningPreferences (Lude.Maybe Lude.Natural)
uppStackSetMaxConcurrencyCount = Lens.lens (stackSetMaxConcurrencyCount :: UpdateProvisioningPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {stackSetMaxConcurrencyCount = a} :: UpdateProvisioningPreferences)
{-# DEPRECATED uppStackSetMaxConcurrencyCount "Use generic-lens or generic-optics with 'stackSetMaxConcurrencyCount' instead." #-}

-- | Determines what action AWS Service Catalog performs to a stack set or a stack instance represented by the provisioned product. The default value is @UPDATE@ if nothing is specified.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
--     * CREATE
--
--     * Creates a new stack instance in the stack set represented by the provisioned product. In this case, only new stack instances are created based on accounts and regions; if new ProductId or ProvisioningArtifactID are passed, they will be ignored.
--
--
--     * UPDATE
--
--     * Updates the stack set represented by the provisioned product and also its stack instances.
--
--
--     * DELETE
--
--     * Deletes a stack instance in the stack set represented by the provisioned product.
--
--
--
-- /Note:/ Consider using 'stackSetOperationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppStackSetOperationType :: Lens.Lens' UpdateProvisioningPreferences (Lude.Maybe StackSetOperationType)
uppStackSetOperationType = Lens.lens (stackSetOperationType :: UpdateProvisioningPreferences -> Lude.Maybe StackSetOperationType) (\s a -> s {stackSetOperationType = a} :: UpdateProvisioningPreferences)
{-# DEPRECATED uppStackSetOperationType "Use generic-lens or generic-optics with 'stackSetOperationType' instead." #-}

instance Lude.ToJSON UpdateProvisioningPreferences where
  toJSON UpdateProvisioningPreferences' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StackSetRegions" Lude..=) Lude.<$> stackSetRegions,
            ("StackSetMaxConcurrencyPercentage" Lude..=)
              Lude.<$> stackSetMaxConcurrencyPercentage,
            ("StackSetFailureToleranceCount" Lude..=)
              Lude.<$> stackSetFailureToleranceCount,
            ("StackSetFailureTolerancePercentage" Lude..=)
              Lude.<$> stackSetFailureTolerancePercentage,
            ("StackSetAccounts" Lude..=) Lude.<$> stackSetAccounts,
            ("StackSetMaxConcurrencyCount" Lude..=)
              Lude.<$> stackSetMaxConcurrencyCount,
            ("StackSetOperationType" Lude..=) Lude.<$> stackSetOperationType
          ]
      )
