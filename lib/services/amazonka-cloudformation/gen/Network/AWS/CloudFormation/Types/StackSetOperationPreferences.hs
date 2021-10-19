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
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationPreferences
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationPreferences where

import Network.AWS.CloudFormation.Types.RegionConcurrencyType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The user-specified preferences for how CloudFormation performs a stack
-- set operation.
--
-- For more information on maximum concurrent accounts and failure
-- tolerance, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>.
--
-- /See:/ 'newStackSetOperationPreferences' smart constructor.
data StackSetOperationPreferences = StackSetOperationPreferences'
  { -- | The order of the Regions in where you want to perform the stack
    -- operation.
    regionOrder :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of accounts in which to perform this operation at one
    -- time. This is dependent on the value of @FailureToleranceCount@.
    -- @MaxConcurrentCount@ is at most one more than the
    -- @FailureToleranceCount@.
    --
    -- Note that this setting lets you specify the /maximum/ for operations.
    -- For large deployments, under certain circumstances the actual number of
    -- accounts acted upon concurrently may be lower due to service throttling.
    --
    -- Conditional: You must specify either @MaxConcurrentCount@ or
    -- @MaxConcurrentPercentage@, but not both.
    --
    -- By default, @1@ is specified.
    maxConcurrentCount :: Prelude.Maybe Prelude.Natural,
    -- | The maximum percentage of accounts in which to perform this operation at
    -- one time.
    --
    -- When calculating the number of accounts based on the specified
    -- percentage, CloudFormation rounds down to the next whole number. This is
    -- true except in cases where rounding down would result is zero. In this
    -- case, CloudFormation sets the number as one instead.
    --
    -- Note that this setting lets you specify the /maximum/ for operations.
    -- For large deployments, under certain circumstances the actual number of
    -- accounts acted upon concurrently may be lower due to service throttling.
    --
    -- Conditional: You must specify either @MaxConcurrentCount@ or
    -- @MaxConcurrentPercentage@, but not both.
    --
    -- By default, @1@ is specified.
    maxConcurrentPercentage :: Prelude.Maybe Prelude.Natural,
    -- | The number of accounts, per Region, for which this operation can fail
    -- before CloudFormation stops the operation in that Region. If the
    -- operation is stopped in a Region, CloudFormation doesn\'t attempt the
    -- operation in any subsequent Regions.
    --
    -- Conditional: You must specify either @FailureToleranceCount@ or
    -- @FailureTolerancePercentage@ (but not both).
    --
    -- By default, @0@ is specified.
    failureToleranceCount :: Prelude.Maybe Prelude.Natural,
    -- | The concurrency type of deploying StackSets operations in Regions, could
    -- be in parallel or one Region at a time.
    regionConcurrencyType :: Prelude.Maybe RegionConcurrencyType,
    -- | The percentage of accounts, per Region, for which this stack operation
    -- can fail before CloudFormation stops the operation in that Region. If
    -- the operation is stopped in a Region, CloudFormation doesn\'t attempt
    -- the operation in any subsequent Regions.
    --
    -- When calculating the number of accounts based on the specified
    -- percentage, CloudFormation rounds /down/ to the next whole number.
    --
    -- Conditional: You must specify either @FailureToleranceCount@ or
    -- @FailureTolerancePercentage@, but not both.
    --
    -- By default, @0@ is specified.
    failureTolerancePercentage :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackSetOperationPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionOrder', 'stackSetOperationPreferences_regionOrder' - The order of the Regions in where you want to perform the stack
-- operation.
--
-- 'maxConcurrentCount', 'stackSetOperationPreferences_maxConcurrentCount' - The maximum number of accounts in which to perform this operation at one
-- time. This is dependent on the value of @FailureToleranceCount@.
-- @MaxConcurrentCount@ is at most one more than the
-- @FailureToleranceCount@.
--
-- Note that this setting lets you specify the /maximum/ for operations.
-- For large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Conditional: You must specify either @MaxConcurrentCount@ or
-- @MaxConcurrentPercentage@, but not both.
--
-- By default, @1@ is specified.
--
-- 'maxConcurrentPercentage', 'stackSetOperationPreferences_maxConcurrentPercentage' - The maximum percentage of accounts in which to perform this operation at
-- one time.
--
-- When calculating the number of accounts based on the specified
-- percentage, CloudFormation rounds down to the next whole number. This is
-- true except in cases where rounding down would result is zero. In this
-- case, CloudFormation sets the number as one instead.
--
-- Note that this setting lets you specify the /maximum/ for operations.
-- For large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Conditional: You must specify either @MaxConcurrentCount@ or
-- @MaxConcurrentPercentage@, but not both.
--
-- By default, @1@ is specified.
--
-- 'failureToleranceCount', 'stackSetOperationPreferences_failureToleranceCount' - The number of accounts, per Region, for which this operation can fail
-- before CloudFormation stops the operation in that Region. If the
-- operation is stopped in a Region, CloudFormation doesn\'t attempt the
-- operation in any subsequent Regions.
--
-- Conditional: You must specify either @FailureToleranceCount@ or
-- @FailureTolerancePercentage@ (but not both).
--
-- By default, @0@ is specified.
--
-- 'regionConcurrencyType', 'stackSetOperationPreferences_regionConcurrencyType' - The concurrency type of deploying StackSets operations in Regions, could
-- be in parallel or one Region at a time.
--
-- 'failureTolerancePercentage', 'stackSetOperationPreferences_failureTolerancePercentage' - The percentage of accounts, per Region, for which this stack operation
-- can fail before CloudFormation stops the operation in that Region. If
-- the operation is stopped in a Region, CloudFormation doesn\'t attempt
-- the operation in any subsequent Regions.
--
-- When calculating the number of accounts based on the specified
-- percentage, CloudFormation rounds /down/ to the next whole number.
--
-- Conditional: You must specify either @FailureToleranceCount@ or
-- @FailureTolerancePercentage@, but not both.
--
-- By default, @0@ is specified.
newStackSetOperationPreferences ::
  StackSetOperationPreferences
newStackSetOperationPreferences =
  StackSetOperationPreferences'
    { regionOrder =
        Prelude.Nothing,
      maxConcurrentCount = Prelude.Nothing,
      maxConcurrentPercentage = Prelude.Nothing,
      failureToleranceCount = Prelude.Nothing,
      regionConcurrencyType = Prelude.Nothing,
      failureTolerancePercentage = Prelude.Nothing
    }

-- | The order of the Regions in where you want to perform the stack
-- operation.
stackSetOperationPreferences_regionOrder :: Lens.Lens' StackSetOperationPreferences (Prelude.Maybe [Prelude.Text])
stackSetOperationPreferences_regionOrder = Lens.lens (\StackSetOperationPreferences' {regionOrder} -> regionOrder) (\s@StackSetOperationPreferences' {} a -> s {regionOrder = a} :: StackSetOperationPreferences) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of accounts in which to perform this operation at one
-- time. This is dependent on the value of @FailureToleranceCount@.
-- @MaxConcurrentCount@ is at most one more than the
-- @FailureToleranceCount@.
--
-- Note that this setting lets you specify the /maximum/ for operations.
-- For large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Conditional: You must specify either @MaxConcurrentCount@ or
-- @MaxConcurrentPercentage@, but not both.
--
-- By default, @1@ is specified.
stackSetOperationPreferences_maxConcurrentCount :: Lens.Lens' StackSetOperationPreferences (Prelude.Maybe Prelude.Natural)
stackSetOperationPreferences_maxConcurrentCount = Lens.lens (\StackSetOperationPreferences' {maxConcurrentCount} -> maxConcurrentCount) (\s@StackSetOperationPreferences' {} a -> s {maxConcurrentCount = a} :: StackSetOperationPreferences)

-- | The maximum percentage of accounts in which to perform this operation at
-- one time.
--
-- When calculating the number of accounts based on the specified
-- percentage, CloudFormation rounds down to the next whole number. This is
-- true except in cases where rounding down would result is zero. In this
-- case, CloudFormation sets the number as one instead.
--
-- Note that this setting lets you specify the /maximum/ for operations.
-- For large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Conditional: You must specify either @MaxConcurrentCount@ or
-- @MaxConcurrentPercentage@, but not both.
--
-- By default, @1@ is specified.
stackSetOperationPreferences_maxConcurrentPercentage :: Lens.Lens' StackSetOperationPreferences (Prelude.Maybe Prelude.Natural)
stackSetOperationPreferences_maxConcurrentPercentage = Lens.lens (\StackSetOperationPreferences' {maxConcurrentPercentage} -> maxConcurrentPercentage) (\s@StackSetOperationPreferences' {} a -> s {maxConcurrentPercentage = a} :: StackSetOperationPreferences)

-- | The number of accounts, per Region, for which this operation can fail
-- before CloudFormation stops the operation in that Region. If the
-- operation is stopped in a Region, CloudFormation doesn\'t attempt the
-- operation in any subsequent Regions.
--
-- Conditional: You must specify either @FailureToleranceCount@ or
-- @FailureTolerancePercentage@ (but not both).
--
-- By default, @0@ is specified.
stackSetOperationPreferences_failureToleranceCount :: Lens.Lens' StackSetOperationPreferences (Prelude.Maybe Prelude.Natural)
stackSetOperationPreferences_failureToleranceCount = Lens.lens (\StackSetOperationPreferences' {failureToleranceCount} -> failureToleranceCount) (\s@StackSetOperationPreferences' {} a -> s {failureToleranceCount = a} :: StackSetOperationPreferences)

-- | The concurrency type of deploying StackSets operations in Regions, could
-- be in parallel or one Region at a time.
stackSetOperationPreferences_regionConcurrencyType :: Lens.Lens' StackSetOperationPreferences (Prelude.Maybe RegionConcurrencyType)
stackSetOperationPreferences_regionConcurrencyType = Lens.lens (\StackSetOperationPreferences' {regionConcurrencyType} -> regionConcurrencyType) (\s@StackSetOperationPreferences' {} a -> s {regionConcurrencyType = a} :: StackSetOperationPreferences)

-- | The percentage of accounts, per Region, for which this stack operation
-- can fail before CloudFormation stops the operation in that Region. If
-- the operation is stopped in a Region, CloudFormation doesn\'t attempt
-- the operation in any subsequent Regions.
--
-- When calculating the number of accounts based on the specified
-- percentage, CloudFormation rounds /down/ to the next whole number.
--
-- Conditional: You must specify either @FailureToleranceCount@ or
-- @FailureTolerancePercentage@, but not both.
--
-- By default, @0@ is specified.
stackSetOperationPreferences_failureTolerancePercentage :: Lens.Lens' StackSetOperationPreferences (Prelude.Maybe Prelude.Natural)
stackSetOperationPreferences_failureTolerancePercentage = Lens.lens (\StackSetOperationPreferences' {failureTolerancePercentage} -> failureTolerancePercentage) (\s@StackSetOperationPreferences' {} a -> s {failureTolerancePercentage = a} :: StackSetOperationPreferences)

instance Core.FromXML StackSetOperationPreferences where
  parseXML x =
    StackSetOperationPreferences'
      Prelude.<$> ( x Core..@? "RegionOrder" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "MaxConcurrentCount")
      Prelude.<*> (x Core..@? "MaxConcurrentPercentage")
      Prelude.<*> (x Core..@? "FailureToleranceCount")
      Prelude.<*> (x Core..@? "RegionConcurrencyType")
      Prelude.<*> (x Core..@? "FailureTolerancePercentage")

instance
  Prelude.Hashable
    StackSetOperationPreferences

instance Prelude.NFData StackSetOperationPreferences

instance Core.ToQuery StackSetOperationPreferences where
  toQuery StackSetOperationPreferences' {..} =
    Prelude.mconcat
      [ "RegionOrder"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> regionOrder),
        "MaxConcurrentCount" Core.=: maxConcurrentCount,
        "MaxConcurrentPercentage"
          Core.=: maxConcurrentPercentage,
        "FailureToleranceCount"
          Core.=: failureToleranceCount,
        "RegionConcurrencyType"
          Core.=: regionConcurrencyType,
        "FailureTolerancePercentage"
          Core.=: failureTolerancePercentage
      ]
