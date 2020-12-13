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
    ssopRegionOrder,
    ssopMaxConcurrentCount,
    ssopMaxConcurrentPercentage,
    ssopFailureToleranceCount,
    ssopFailureTolerancePercentage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The user-specified preferences for how AWS CloudFormation performs a stack set operation.
--
-- For more information on maximum concurrent accounts and failure tolerance, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> .
--
-- /See:/ 'mkStackSetOperationPreferences' smart constructor.
data StackSetOperationPreferences = StackSetOperationPreferences'
  { -- | The order of the Regions in where you want to perform the stack operation.
    regionOrder :: Lude.Maybe [Lude.Text],
    -- | The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @FailureToleranceCount@ . @MaxConcurrentCount@ is at most one more than the @FailureToleranceCount@ .
    --
    -- Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
    -- Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
    maxConcurrentCount :: Lude.Maybe Lude.Natural,
    -- | The maximum percentage of accounts in which to perform this operation at one time.
    --
    -- When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, CloudFormation sets the number as one instead.
    -- Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
    -- Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
    maxConcurrentPercentage :: Lude.Maybe Lude.Natural,
    -- | The number of accounts, per Region, for which this operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions.
    --
    -- Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ (but not both).
    failureToleranceCount :: Lude.Maybe Lude.Natural,
    -- | The percentage of accounts, per Region, for which this stack operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions.
    --
    -- When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds /down/ to the next whole number.
    -- Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ , but not both.
    failureTolerancePercentage :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackSetOperationPreferences' with the minimum fields required to make a request.
--
-- * 'regionOrder' - The order of the Regions in where you want to perform the stack operation.
-- * 'maxConcurrentCount' - The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @FailureToleranceCount@ . @MaxConcurrentCount@ is at most one more than the @FailureToleranceCount@ .
--
-- Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
-- * 'maxConcurrentPercentage' - The maximum percentage of accounts in which to perform this operation at one time.
--
-- When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, CloudFormation sets the number as one instead.
-- Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
-- * 'failureToleranceCount' - The number of accounts, per Region, for which this operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions.
--
-- Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ (but not both).
-- * 'failureTolerancePercentage' - The percentage of accounts, per Region, for which this stack operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions.
--
-- When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds /down/ to the next whole number.
-- Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ , but not both.
mkStackSetOperationPreferences ::
  StackSetOperationPreferences
mkStackSetOperationPreferences =
  StackSetOperationPreferences'
    { regionOrder = Lude.Nothing,
      maxConcurrentCount = Lude.Nothing,
      maxConcurrentPercentage = Lude.Nothing,
      failureToleranceCount = Lude.Nothing,
      failureTolerancePercentage = Lude.Nothing
    }

-- | The order of the Regions in where you want to perform the stack operation.
--
-- /Note:/ Consider using 'regionOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssopRegionOrder :: Lens.Lens' StackSetOperationPreferences (Lude.Maybe [Lude.Text])
ssopRegionOrder = Lens.lens (regionOrder :: StackSetOperationPreferences -> Lude.Maybe [Lude.Text]) (\s a -> s {regionOrder = a} :: StackSetOperationPreferences)
{-# DEPRECATED ssopRegionOrder "Use generic-lens or generic-optics with 'regionOrder' instead." #-}

-- | The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @FailureToleranceCount@ . @MaxConcurrentCount@ is at most one more than the @FailureToleranceCount@ .
--
-- Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
--
-- /Note:/ Consider using 'maxConcurrentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssopMaxConcurrentCount :: Lens.Lens' StackSetOperationPreferences (Lude.Maybe Lude.Natural)
ssopMaxConcurrentCount = Lens.lens (maxConcurrentCount :: StackSetOperationPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {maxConcurrentCount = a} :: StackSetOperationPreferences)
{-# DEPRECATED ssopMaxConcurrentCount "Use generic-lens or generic-optics with 'maxConcurrentCount' instead." #-}

-- | The maximum percentage of accounts in which to perform this operation at one time.
--
-- When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, CloudFormation sets the number as one instead.
-- Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling.
-- Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
--
-- /Note:/ Consider using 'maxConcurrentPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssopMaxConcurrentPercentage :: Lens.Lens' StackSetOperationPreferences (Lude.Maybe Lude.Natural)
ssopMaxConcurrentPercentage = Lens.lens (maxConcurrentPercentage :: StackSetOperationPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {maxConcurrentPercentage = a} :: StackSetOperationPreferences)
{-# DEPRECATED ssopMaxConcurrentPercentage "Use generic-lens or generic-optics with 'maxConcurrentPercentage' instead." #-}

-- | The number of accounts, per Region, for which this operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions.
--
-- Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ (but not both).
--
-- /Note:/ Consider using 'failureToleranceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssopFailureToleranceCount :: Lens.Lens' StackSetOperationPreferences (Lude.Maybe Lude.Natural)
ssopFailureToleranceCount = Lens.lens (failureToleranceCount :: StackSetOperationPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {failureToleranceCount = a} :: StackSetOperationPreferences)
{-# DEPRECATED ssopFailureToleranceCount "Use generic-lens or generic-optics with 'failureToleranceCount' instead." #-}

-- | The percentage of accounts, per Region, for which this stack operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions.
--
-- When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds /down/ to the next whole number.
-- Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ , but not both.
--
-- /Note:/ Consider using 'failureTolerancePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssopFailureTolerancePercentage :: Lens.Lens' StackSetOperationPreferences (Lude.Maybe Lude.Natural)
ssopFailureTolerancePercentage = Lens.lens (failureTolerancePercentage :: StackSetOperationPreferences -> Lude.Maybe Lude.Natural) (\s a -> s {failureTolerancePercentage = a} :: StackSetOperationPreferences)
{-# DEPRECATED ssopFailureTolerancePercentage "Use generic-lens or generic-optics with 'failureTolerancePercentage' instead." #-}

instance Lude.FromXML StackSetOperationPreferences where
  parseXML x =
    StackSetOperationPreferences'
      Lude.<$> ( x Lude..@? "RegionOrder" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "MaxConcurrentCount")
      Lude.<*> (x Lude..@? "MaxConcurrentPercentage")
      Lude.<*> (x Lude..@? "FailureToleranceCount")
      Lude.<*> (x Lude..@? "FailureTolerancePercentage")

instance Lude.ToQuery StackSetOperationPreferences where
  toQuery StackSetOperationPreferences' {..} =
    Lude.mconcat
      [ "RegionOrder"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> regionOrder),
        "MaxConcurrentCount" Lude.=: maxConcurrentCount,
        "MaxConcurrentPercentage" Lude.=: maxConcurrentPercentage,
        "FailureToleranceCount" Lude.=: failureToleranceCount,
        "FailureTolerancePercentage" Lude.=: failureTolerancePercentage
      ]
