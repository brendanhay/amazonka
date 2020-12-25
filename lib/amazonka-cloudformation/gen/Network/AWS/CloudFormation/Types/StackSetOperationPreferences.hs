{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationPreferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationPreferences
  ( StackSetOperationPreferences (..),

    -- * Smart constructor
    mkStackSetOperationPreferences,

    -- * Lenses
    ssopFailureToleranceCount,
    ssopFailureTolerancePercentage,
    ssopMaxConcurrentCount,
    ssopMaxConcurrentPercentage,
    ssopRegionOrder,
  )
where

import qualified Network.AWS.CloudFormation.Types.Region as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The user-specified preferences for how AWS CloudFormation performs a stack set operation.
--
-- For more information on maximum concurrent accounts and failure tolerance, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> .
--
-- /See:/ 'mkStackSetOperationPreferences' smart constructor.
data StackSetOperationPreferences = StackSetOperationPreferences'
  { -- | The number of accounts, per Region, for which this operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions.
    --
    -- Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ (but not both).
    failureToleranceCount :: Core.Maybe Core.Natural,
    -- | The percentage of accounts, per Region, for which this stack operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions.
    --
    -- When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds /down/ to the next whole number.
    -- Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ , but not both.
    failureTolerancePercentage :: Core.Maybe Core.Natural,
    -- | The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @FailureToleranceCount@ . @MaxConcurrentCount@ is at most one more than the @FailureToleranceCount@ .
    --
    -- Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
    -- Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
    maxConcurrentCount :: Core.Maybe Core.Natural,
    -- | The maximum percentage of accounts in which to perform this operation at one time.
    --
    -- When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, CloudFormation sets the number as one instead.
    -- Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
    -- Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
    maxConcurrentPercentage :: Core.Maybe Core.Natural,
    -- | The order of the Regions in where you want to perform the stack operation.
    regionOrder :: Core.Maybe [Types.Region]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StackSetOperationPreferences' value with any optional fields omitted.
mkStackSetOperationPreferences ::
  StackSetOperationPreferences
mkStackSetOperationPreferences =
  StackSetOperationPreferences'
    { failureToleranceCount =
        Core.Nothing,
      failureTolerancePercentage = Core.Nothing,
      maxConcurrentCount = Core.Nothing,
      maxConcurrentPercentage = Core.Nothing,
      regionOrder = Core.Nothing
    }

-- | The number of accounts, per Region, for which this operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions.
--
-- Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ (but not both).
--
-- /Note:/ Consider using 'failureToleranceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssopFailureToleranceCount :: Lens.Lens' StackSetOperationPreferences (Core.Maybe Core.Natural)
ssopFailureToleranceCount = Lens.field @"failureToleranceCount"
{-# DEPRECATED ssopFailureToleranceCount "Use generic-lens or generic-optics with 'failureToleranceCount' instead." #-}

-- | The percentage of accounts, per Region, for which this stack operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions.
--
-- When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds /down/ to the next whole number.
-- Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ , but not both.
--
-- /Note:/ Consider using 'failureTolerancePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssopFailureTolerancePercentage :: Lens.Lens' StackSetOperationPreferences (Core.Maybe Core.Natural)
ssopFailureTolerancePercentage = Lens.field @"failureTolerancePercentage"
{-# DEPRECATED ssopFailureTolerancePercentage "Use generic-lens or generic-optics with 'failureTolerancePercentage' instead." #-}

-- | The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @FailureToleranceCount@ . @MaxConcurrentCount@ is at most one more than the @FailureToleranceCount@ .
--
-- Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
--
-- /Note:/ Consider using 'maxConcurrentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssopMaxConcurrentCount :: Lens.Lens' StackSetOperationPreferences (Core.Maybe Core.Natural)
ssopMaxConcurrentCount = Lens.field @"maxConcurrentCount"
{-# DEPRECATED ssopMaxConcurrentCount "Use generic-lens or generic-optics with 'maxConcurrentCount' instead." #-}

-- | The maximum percentage of accounts in which to perform this operation at one time.
--
-- When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, CloudFormation sets the number as one instead.
-- Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
--
-- /Note:/ Consider using 'maxConcurrentPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssopMaxConcurrentPercentage :: Lens.Lens' StackSetOperationPreferences (Core.Maybe Core.Natural)
ssopMaxConcurrentPercentage = Lens.field @"maxConcurrentPercentage"
{-# DEPRECATED ssopMaxConcurrentPercentage "Use generic-lens or generic-optics with 'maxConcurrentPercentage' instead." #-}

-- | The order of the Regions in where you want to perform the stack operation.
--
-- /Note:/ Consider using 'regionOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssopRegionOrder :: Lens.Lens' StackSetOperationPreferences (Core.Maybe [Types.Region])
ssopRegionOrder = Lens.field @"regionOrder"
{-# DEPRECATED ssopRegionOrder "Use generic-lens or generic-optics with 'regionOrder' instead." #-}

instance Core.FromXML StackSetOperationPreferences where
  parseXML x =
    StackSetOperationPreferences'
      Core.<$> (x Core..@? "FailureToleranceCount")
      Core.<*> (x Core..@? "FailureTolerancePercentage")
      Core.<*> (x Core..@? "MaxConcurrentCount")
      Core.<*> (x Core..@? "MaxConcurrentPercentage")
      Core.<*> (x Core..@? "RegionOrder" Core..<@> Core.parseXMLList "member")
