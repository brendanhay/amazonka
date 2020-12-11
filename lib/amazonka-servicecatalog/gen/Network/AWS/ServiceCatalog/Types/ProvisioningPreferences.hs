-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningPreferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningPreferences
  ( ProvisioningPreferences (..),

    -- * Smart constructor
    mkProvisioningPreferences,

    -- * Lenses
    ppStackSetRegions,
    ppStackSetMaxConcurrencyPercentage,
    ppStackSetFailureToleranceCount,
    ppStackSetFailureTolerancePercentage,
    ppStackSetAccounts,
    ppStackSetMaxConcurrencyCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The user-defined preferences that will be applied when updating a provisioned product. Not all preferences are applicable to all provisioned product types.
--
-- /See:/ 'mkProvisioningPreferences' smart constructor.
data ProvisioningPreferences = ProvisioningPreferences'
  { stackSetRegions ::
      Lude.Maybe [Lude.Text],
    stackSetMaxConcurrencyPercentage ::
      Lude.Maybe Lude.Natural,
    stackSetFailureToleranceCount ::
      Lude.Maybe Lude.Natural,
    stackSetFailureTolerancePercentage ::
      Lude.Maybe Lude.Natural,
    stackSetAccounts :: Lude.Maybe [Lude.Text],
    stackSetMaxConcurrencyCount ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningPreferences' with the minimum fields required to make a request.
--
-- * 'stackSetAccounts' - One or more AWS accounts that will have access to the provisioned product.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- The AWS accounts specified should be within the list of accounts in the @STACKSET@ constraint. To get the list of accounts in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
-- If no values are specified, the default value is all accounts from the @STACKSET@ constraint.
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
-- * 'stackSetMaxConcurrencyCount' - The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @StackSetFailureToleranceCount@ . @StackSetMaxConcurrentCount@ is at most one more than the @StackSetFailureToleranceCount@ .
--
-- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
-- * 'stackSetMaxConcurrencyPercentage' - The maximum percentage of accounts in which to perform this operation at one time.
--
-- When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, AWS Service Catalog sets the number as @1@ instead.
-- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
-- * 'stackSetRegions' - One or more AWS Regions where the provisioned product will be available.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- The specified regions should be within the list of regions from the @STACKSET@ constraint. To get the list of regions in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
-- If no values are specified, the default value is all regions from the @STACKSET@ constraint.
mkProvisioningPreferences ::
  ProvisioningPreferences
mkProvisioningPreferences =
  ProvisioningPreferences'
    { stackSetRegions = Lude.Nothing,
      stackSetMaxConcurrencyPercentage = Lude.Nothing,
      stackSetFailureToleranceCount = Lude.Nothing,
      stackSetFailureTolerancePercentage = Lude.Nothing,
      stackSetAccounts = Lude.Nothing,
      stackSetMaxConcurrencyCount = Lude.Nothing
    }

-- | One or more AWS Regions where the provisioned product will be available.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- The specified regions should be within the list of regions from the @STACKSET@ constraint. To get the list of regions in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
-- If no values are specified, the default value is all regions from the @STACKSET@ constraint.
--
-- /Note:/ Consider using 'stackSetRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppStackSetRegions :: Lens.Lens' ProvisioningPreferences (Lude.Maybe [Lude.Text])
ppStackSetRegions = Lens.lens (stackSetRegions :: ProvisioningPreferences -> Lude.Maybe [Lude.Text]) (\s a -> s {stackSetRegions = a} :: ProvisioningPreferences)
{-# DEPRECATED ppStackSetRegions "Use generic-lens or generic-optics with 'stackSetRegions' instead." #-}

-- | The maximum percentage of accounts in which to perform this operation at one time.
--
-- When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, AWS Service Catalog sets the number as @1@ instead.
-- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
--
-- /Note:/ Consider using 'stackSetMaxConcurrencyPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppStackSetMaxConcurrencyPercentage :: Lens.Lens' ProvisioningPreferences (Lude.Maybe Lude.Natural)
ppStackSetMaxConcurrencyPercentage = Lens.lens (stackSetMaxConcurrencyPercentage :: ProvisioningPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {stackSetMaxConcurrencyPercentage = a} :: ProvisioningPreferences)
{-# DEPRECATED ppStackSetMaxConcurrencyPercentage "Use generic-lens or generic-optics with 'stackSetMaxConcurrencyPercentage' instead." #-}

-- | The number of accounts, per region, for which this operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
-- The default value is @0@ if no value is specified.
--
-- /Note:/ Consider using 'stackSetFailureToleranceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppStackSetFailureToleranceCount :: Lens.Lens' ProvisioningPreferences (Lude.Maybe Lude.Natural)
ppStackSetFailureToleranceCount = Lens.lens (stackSetFailureToleranceCount :: ProvisioningPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {stackSetFailureToleranceCount = a} :: ProvisioningPreferences)
{-# DEPRECATED ppStackSetFailureToleranceCount "Use generic-lens or generic-optics with 'stackSetFailureToleranceCount' instead." #-}

-- | The percentage of accounts, per region, for which this stack operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions.
--
-- When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
--
-- /Note:/ Consider using 'stackSetFailureTolerancePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppStackSetFailureTolerancePercentage :: Lens.Lens' ProvisioningPreferences (Lude.Maybe Lude.Natural)
ppStackSetFailureTolerancePercentage = Lens.lens (stackSetFailureTolerancePercentage :: ProvisioningPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {stackSetFailureTolerancePercentage = a} :: ProvisioningPreferences)
{-# DEPRECATED ppStackSetFailureTolerancePercentage "Use generic-lens or generic-optics with 'stackSetFailureTolerancePercentage' instead." #-}

-- | One or more AWS accounts that will have access to the provisioned product.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- The AWS accounts specified should be within the list of accounts in the @STACKSET@ constraint. To get the list of accounts in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
-- If no values are specified, the default value is all accounts from the @STACKSET@ constraint.
--
-- /Note:/ Consider using 'stackSetAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppStackSetAccounts :: Lens.Lens' ProvisioningPreferences (Lude.Maybe [Lude.Text])
ppStackSetAccounts = Lens.lens (stackSetAccounts :: ProvisioningPreferences -> Lude.Maybe [Lude.Text]) (\s a -> s {stackSetAccounts = a} :: ProvisioningPreferences)
{-# DEPRECATED ppStackSetAccounts "Use generic-lens or generic-optics with 'stackSetAccounts' instead." #-}

-- | The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @StackSetFailureToleranceCount@ . @StackSetMaxConcurrentCount@ is at most one more than the @StackSetFailureToleranceCount@ .
--
-- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
--
-- /Note:/ Consider using 'stackSetMaxConcurrencyCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppStackSetMaxConcurrencyCount :: Lens.Lens' ProvisioningPreferences (Lude.Maybe Lude.Natural)
ppStackSetMaxConcurrencyCount = Lens.lens (stackSetMaxConcurrencyCount :: ProvisioningPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {stackSetMaxConcurrencyCount = a} :: ProvisioningPreferences)
{-# DEPRECATED ppStackSetMaxConcurrencyCount "Use generic-lens or generic-optics with 'stackSetMaxConcurrencyCount' instead." #-}

instance Lude.ToJSON ProvisioningPreferences where
  toJSON ProvisioningPreferences' {..} =
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
              Lude.<$> stackSetMaxConcurrencyCount
          ]
      )
