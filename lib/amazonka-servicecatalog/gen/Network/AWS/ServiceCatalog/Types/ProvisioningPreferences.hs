{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningPreferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisioningPreferences
  ( ProvisioningPreferences (..)
  -- * Smart constructor
  , mkProvisioningPreferences
  -- * Lenses
  , ppStackSetAccounts
  , ppStackSetFailureToleranceCount
  , ppStackSetFailureTolerancePercentage
  , ppStackSetMaxConcurrencyCount
  , ppStackSetMaxConcurrencyPercentage
  , ppStackSetRegions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.AccountId as Types
import qualified Network.AWS.ServiceCatalog.Types.Region as Types

-- | The user-defined preferences that will be applied when updating a provisioned product. Not all preferences are applicable to all provisioned product types.
--
-- /See:/ 'mkProvisioningPreferences' smart constructor.
data ProvisioningPreferences = ProvisioningPreferences'
  { stackSetAccounts :: Core.Maybe [Types.AccountId]
    -- ^ One or more AWS accounts that will have access to the provisioned product.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- The AWS accounts specified should be within the list of accounts in the @STACKSET@ constraint. To get the list of accounts in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
-- If no values are specified, the default value is all accounts from the @STACKSET@ constraint.
  , stackSetFailureToleranceCount :: Core.Maybe Core.Natural
    -- ^ The number of accounts, per region, for which this operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
-- The default value is @0@ if no value is specified.
  , stackSetFailureTolerancePercentage :: Core.Maybe Core.Natural
    -- ^ The percentage of accounts, per region, for which this stack operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions.
--
-- When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
  , stackSetMaxConcurrencyCount :: Core.Maybe Core.Natural
    -- ^ The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @StackSetFailureToleranceCount@ . @StackSetMaxConcurrentCount@ is at most one more than the @StackSetFailureToleranceCount@ .
--
-- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
  , stackSetMaxConcurrencyPercentage :: Core.Maybe Core.Natural
    -- ^ The maximum percentage of accounts in which to perform this operation at one time.
--
-- When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, AWS Service Catalog sets the number as @1@ instead.
-- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
  , stackSetRegions :: Core.Maybe [Types.Region]
    -- ^ One or more AWS Regions where the provisioned product will be available.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- The specified regions should be within the list of regions from the @STACKSET@ constraint. To get the list of regions in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
-- If no values are specified, the default value is all regions from the @STACKSET@ constraint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisioningPreferences' value with any optional fields omitted.
mkProvisioningPreferences
    :: ProvisioningPreferences
mkProvisioningPreferences
  = ProvisioningPreferences'{stackSetAccounts = Core.Nothing,
                             stackSetFailureToleranceCount = Core.Nothing,
                             stackSetFailureTolerancePercentage = Core.Nothing,
                             stackSetMaxConcurrencyCount = Core.Nothing,
                             stackSetMaxConcurrencyPercentage = Core.Nothing,
                             stackSetRegions = Core.Nothing}

-- | One or more AWS accounts that will have access to the provisioned product.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- The AWS accounts specified should be within the list of accounts in the @STACKSET@ constraint. To get the list of accounts in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
-- If no values are specified, the default value is all accounts from the @STACKSET@ constraint.
--
-- /Note:/ Consider using 'stackSetAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppStackSetAccounts :: Lens.Lens' ProvisioningPreferences (Core.Maybe [Types.AccountId])
ppStackSetAccounts = Lens.field @"stackSetAccounts"
{-# INLINEABLE ppStackSetAccounts #-}
{-# DEPRECATED stackSetAccounts "Use generic-lens or generic-optics with 'stackSetAccounts' instead"  #-}

-- | The number of accounts, per region, for which this operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
-- The default value is @0@ if no value is specified.
--
-- /Note:/ Consider using 'stackSetFailureToleranceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppStackSetFailureToleranceCount :: Lens.Lens' ProvisioningPreferences (Core.Maybe Core.Natural)
ppStackSetFailureToleranceCount = Lens.field @"stackSetFailureToleranceCount"
{-# INLINEABLE ppStackSetFailureToleranceCount #-}
{-# DEPRECATED stackSetFailureToleranceCount "Use generic-lens or generic-optics with 'stackSetFailureToleranceCount' instead"  #-}

-- | The percentage of accounts, per region, for which this stack operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions.
--
-- When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
--
-- /Note:/ Consider using 'stackSetFailureTolerancePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppStackSetFailureTolerancePercentage :: Lens.Lens' ProvisioningPreferences (Core.Maybe Core.Natural)
ppStackSetFailureTolerancePercentage = Lens.field @"stackSetFailureTolerancePercentage"
{-# INLINEABLE ppStackSetFailureTolerancePercentage #-}
{-# DEPRECATED stackSetFailureTolerancePercentage "Use generic-lens or generic-optics with 'stackSetFailureTolerancePercentage' instead"  #-}

-- | The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @StackSetFailureToleranceCount@ . @StackSetMaxConcurrentCount@ is at most one more than the @StackSetFailureToleranceCount@ .
--
-- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
--
-- /Note:/ Consider using 'stackSetMaxConcurrencyCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppStackSetMaxConcurrencyCount :: Lens.Lens' ProvisioningPreferences (Core.Maybe Core.Natural)
ppStackSetMaxConcurrencyCount = Lens.field @"stackSetMaxConcurrencyCount"
{-# INLINEABLE ppStackSetMaxConcurrencyCount #-}
{-# DEPRECATED stackSetMaxConcurrencyCount "Use generic-lens or generic-optics with 'stackSetMaxConcurrencyCount' instead"  #-}

-- | The maximum percentage of accounts in which to perform this operation at one time.
--
-- When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, AWS Service Catalog sets the number as @1@ instead.
-- Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
--
-- /Note:/ Consider using 'stackSetMaxConcurrencyPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppStackSetMaxConcurrencyPercentage :: Lens.Lens' ProvisioningPreferences (Core.Maybe Core.Natural)
ppStackSetMaxConcurrencyPercentage = Lens.field @"stackSetMaxConcurrencyPercentage"
{-# INLINEABLE ppStackSetMaxConcurrencyPercentage #-}
{-# DEPRECATED stackSetMaxConcurrencyPercentage "Use generic-lens or generic-optics with 'stackSetMaxConcurrencyPercentage' instead"  #-}

-- | One or more AWS Regions where the provisioned product will be available.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
-- The specified regions should be within the list of regions from the @STACKSET@ constraint. To get the list of regions in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation.
-- If no values are specified, the default value is all regions from the @STACKSET@ constraint.
--
-- /Note:/ Consider using 'stackSetRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppStackSetRegions :: Lens.Lens' ProvisioningPreferences (Core.Maybe [Types.Region])
ppStackSetRegions = Lens.field @"stackSetRegions"
{-# INLINEABLE ppStackSetRegions #-}
{-# DEPRECATED stackSetRegions "Use generic-lens or generic-optics with 'stackSetRegions' instead"  #-}

instance Core.FromJSON ProvisioningPreferences where
        toJSON ProvisioningPreferences{..}
          = Core.object
              (Core.catMaybes
                 [("StackSetAccounts" Core..=) Core.<$> stackSetAccounts,
                  ("StackSetFailureToleranceCount" Core..=) Core.<$>
                    stackSetFailureToleranceCount,
                  ("StackSetFailureTolerancePercentage" Core..=) Core.<$>
                    stackSetFailureTolerancePercentage,
                  ("StackSetMaxConcurrencyCount" Core..=) Core.<$>
                    stackSetMaxConcurrencyCount,
                  ("StackSetMaxConcurrencyPercentage" Core..=) Core.<$>
                    stackSetMaxConcurrencyPercentage,
                  ("StackSetRegions" Core..=) Core.<$> stackSetRegions])
