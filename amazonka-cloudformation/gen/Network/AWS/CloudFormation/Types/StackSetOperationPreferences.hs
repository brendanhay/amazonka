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
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationPreferences
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationPreferences where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The user-specified preferences for how AWS CloudFormation performs a
-- stack set operation.
--
-- For more information on maximum concurrent accounts and failure
-- tolerance, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>.
--
-- /See:/ 'newStackSetOperationPreferences' smart constructor.
data StackSetOperationPreferences = StackSetOperationPreferences'
  { -- | The maximum percentage of accounts in which to perform this operation at
    -- one time.
    --
    -- When calculating the number of accounts based on the specified
    -- percentage, AWS CloudFormation rounds down to the next whole number.
    -- This is true except in cases where rounding down would result is zero.
    -- In this case, CloudFormation sets the number as one instead.
    --
    -- Note that this setting lets you specify the /maximum/ for operations.
    -- For large deployments, under certain circumstances the actual number of
    -- accounts acted upon concurrently may be lower due to service throttling.
    --
    -- Conditional: You must specify either @MaxConcurrentCount@ or
    -- @MaxConcurrentPercentage@, but not both.
    maxConcurrentPercentage :: Prelude.Maybe Prelude.Natural,
    -- | The order of the Regions in where you want to perform the stack
    -- operation.
    regionOrder :: Prelude.Maybe [Prelude.Text],
    -- | The number of accounts, per Region, for which this operation can fail
    -- before AWS CloudFormation stops the operation in that Region. If the
    -- operation is stopped in a Region, AWS CloudFormation doesn\'t attempt
    -- the operation in any subsequent Regions.
    --
    -- Conditional: You must specify either @FailureToleranceCount@ or
    -- @FailureTolerancePercentage@ (but not both).
    failureToleranceCount :: Prelude.Maybe Prelude.Natural,
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
    maxConcurrentCount :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of accounts, per Region, for which this stack operation
    -- can fail before AWS CloudFormation stops the operation in that Region.
    -- If the operation is stopped in a Region, AWS CloudFormation doesn\'t
    -- attempt the operation in any subsequent Regions.
    --
    -- When calculating the number of accounts based on the specified
    -- percentage, AWS CloudFormation rounds /down/ to the next whole number.
    --
    -- Conditional: You must specify either @FailureToleranceCount@ or
    -- @FailureTolerancePercentage@, but not both.
    failureTolerancePercentage :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StackSetOperationPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxConcurrentPercentage', 'stackSetOperationPreferences_maxConcurrentPercentage' - The maximum percentage of accounts in which to perform this operation at
-- one time.
--
-- When calculating the number of accounts based on the specified
-- percentage, AWS CloudFormation rounds down to the next whole number.
-- This is true except in cases where rounding down would result is zero.
-- In this case, CloudFormation sets the number as one instead.
--
-- Note that this setting lets you specify the /maximum/ for operations.
-- For large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Conditional: You must specify either @MaxConcurrentCount@ or
-- @MaxConcurrentPercentage@, but not both.
--
-- 'regionOrder', 'stackSetOperationPreferences_regionOrder' - The order of the Regions in where you want to perform the stack
-- operation.
--
-- 'failureToleranceCount', 'stackSetOperationPreferences_failureToleranceCount' - The number of accounts, per Region, for which this operation can fail
-- before AWS CloudFormation stops the operation in that Region. If the
-- operation is stopped in a Region, AWS CloudFormation doesn\'t attempt
-- the operation in any subsequent Regions.
--
-- Conditional: You must specify either @FailureToleranceCount@ or
-- @FailureTolerancePercentage@ (but not both).
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
-- 'failureTolerancePercentage', 'stackSetOperationPreferences_failureTolerancePercentage' - The percentage of accounts, per Region, for which this stack operation
-- can fail before AWS CloudFormation stops the operation in that Region.
-- If the operation is stopped in a Region, AWS CloudFormation doesn\'t
-- attempt the operation in any subsequent Regions.
--
-- When calculating the number of accounts based on the specified
-- percentage, AWS CloudFormation rounds /down/ to the next whole number.
--
-- Conditional: You must specify either @FailureToleranceCount@ or
-- @FailureTolerancePercentage@, but not both.
newStackSetOperationPreferences ::
  StackSetOperationPreferences
newStackSetOperationPreferences =
  StackSetOperationPreferences'
    { maxConcurrentPercentage =
        Prelude.Nothing,
      regionOrder = Prelude.Nothing,
      failureToleranceCount = Prelude.Nothing,
      maxConcurrentCount = Prelude.Nothing,
      failureTolerancePercentage = Prelude.Nothing
    }

-- | The maximum percentage of accounts in which to perform this operation at
-- one time.
--
-- When calculating the number of accounts based on the specified
-- percentage, AWS CloudFormation rounds down to the next whole number.
-- This is true except in cases where rounding down would result is zero.
-- In this case, CloudFormation sets the number as one instead.
--
-- Note that this setting lets you specify the /maximum/ for operations.
-- For large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Conditional: You must specify either @MaxConcurrentCount@ or
-- @MaxConcurrentPercentage@, but not both.
stackSetOperationPreferences_maxConcurrentPercentage :: Lens.Lens' StackSetOperationPreferences (Prelude.Maybe Prelude.Natural)
stackSetOperationPreferences_maxConcurrentPercentage = Lens.lens (\StackSetOperationPreferences' {maxConcurrentPercentage} -> maxConcurrentPercentage) (\s@StackSetOperationPreferences' {} a -> s {maxConcurrentPercentage = a} :: StackSetOperationPreferences)

-- | The order of the Regions in where you want to perform the stack
-- operation.
stackSetOperationPreferences_regionOrder :: Lens.Lens' StackSetOperationPreferences (Prelude.Maybe [Prelude.Text])
stackSetOperationPreferences_regionOrder = Lens.lens (\StackSetOperationPreferences' {regionOrder} -> regionOrder) (\s@StackSetOperationPreferences' {} a -> s {regionOrder = a} :: StackSetOperationPreferences) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of accounts, per Region, for which this operation can fail
-- before AWS CloudFormation stops the operation in that Region. If the
-- operation is stopped in a Region, AWS CloudFormation doesn\'t attempt
-- the operation in any subsequent Regions.
--
-- Conditional: You must specify either @FailureToleranceCount@ or
-- @FailureTolerancePercentage@ (but not both).
stackSetOperationPreferences_failureToleranceCount :: Lens.Lens' StackSetOperationPreferences (Prelude.Maybe Prelude.Natural)
stackSetOperationPreferences_failureToleranceCount = Lens.lens (\StackSetOperationPreferences' {failureToleranceCount} -> failureToleranceCount) (\s@StackSetOperationPreferences' {} a -> s {failureToleranceCount = a} :: StackSetOperationPreferences)

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
stackSetOperationPreferences_maxConcurrentCount :: Lens.Lens' StackSetOperationPreferences (Prelude.Maybe Prelude.Natural)
stackSetOperationPreferences_maxConcurrentCount = Lens.lens (\StackSetOperationPreferences' {maxConcurrentCount} -> maxConcurrentCount) (\s@StackSetOperationPreferences' {} a -> s {maxConcurrentCount = a} :: StackSetOperationPreferences)

-- | The percentage of accounts, per Region, for which this stack operation
-- can fail before AWS CloudFormation stops the operation in that Region.
-- If the operation is stopped in a Region, AWS CloudFormation doesn\'t
-- attempt the operation in any subsequent Regions.
--
-- When calculating the number of accounts based on the specified
-- percentage, AWS CloudFormation rounds /down/ to the next whole number.
--
-- Conditional: You must specify either @FailureToleranceCount@ or
-- @FailureTolerancePercentage@, but not both.
stackSetOperationPreferences_failureTolerancePercentage :: Lens.Lens' StackSetOperationPreferences (Prelude.Maybe Prelude.Natural)
stackSetOperationPreferences_failureTolerancePercentage = Lens.lens (\StackSetOperationPreferences' {failureTolerancePercentage} -> failureTolerancePercentage) (\s@StackSetOperationPreferences' {} a -> s {failureTolerancePercentage = a} :: StackSetOperationPreferences)

instance Prelude.FromXML StackSetOperationPreferences where
  parseXML x =
    StackSetOperationPreferences'
      Prelude.<$> (x Prelude..@? "MaxConcurrentPercentage")
      Prelude.<*> ( x Prelude..@? "RegionOrder"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "FailureToleranceCount")
      Prelude.<*> (x Prelude..@? "MaxConcurrentCount")
      Prelude.<*> (x Prelude..@? "FailureTolerancePercentage")

instance
  Prelude.Hashable
    StackSetOperationPreferences

instance Prelude.NFData StackSetOperationPreferences

instance Prelude.ToQuery StackSetOperationPreferences where
  toQuery StackSetOperationPreferences' {..} =
    Prelude.mconcat
      [ "MaxConcurrentPercentage"
          Prelude.=: maxConcurrentPercentage,
        "RegionOrder"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> regionOrder
            ),
        "FailureToleranceCount"
          Prelude.=: failureToleranceCount,
        "MaxConcurrentCount" Prelude.=: maxConcurrentCount,
        "FailureTolerancePercentage"
          Prelude.=: failureTolerancePercentage
      ]
