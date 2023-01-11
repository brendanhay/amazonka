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
-- Module      : Amazonka.CloudFormation.Types.StackSetOperationPreferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSetOperationPreferences where

import Amazonka.CloudFormation.Types.RegionConcurrencyType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The user-specified preferences for how CloudFormation performs a stack
-- set operation.
--
-- For more information about maximum concurrent accounts and failure
-- tolerance, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options>.
--
-- /See:/ 'newStackSetOperationPreferences' smart constructor.
data StackSetOperationPreferences = StackSetOperationPreferences'
  { -- | The number of accounts, per Region, for which this operation can fail
    -- before CloudFormation stops the operation in that Region. If the
    -- operation is stopped in a Region, CloudFormation doesn\'t attempt the
    -- operation in any subsequent Regions.
    --
    -- Conditional: You must specify either @FailureToleranceCount@ or
    -- @FailureTolerancePercentage@ (but not both).
    --
    -- By default, @0@ is specified.
    failureToleranceCount :: Prelude.Maybe Prelude.Natural,
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
    failureTolerancePercentage :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of accounts in which to perform this operation at one
    -- time. This is dependent on the value of
    -- @FailureToleranceCount@.@MaxConcurrentCount@ is at most one more than
    -- the @FailureToleranceCount@.
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
    -- | The concurrency type of deploying StackSets operations in Regions, could
    -- be in parallel or one Region at a time.
    regionConcurrencyType :: Prelude.Maybe RegionConcurrencyType,
    -- | The order of the Regions in where you want to perform the stack
    -- operation.
    regionOrder :: Prelude.Maybe [Prelude.Text]
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
--
-- 'maxConcurrentCount', 'stackSetOperationPreferences_maxConcurrentCount' - The maximum number of accounts in which to perform this operation at one
-- time. This is dependent on the value of
-- @FailureToleranceCount@.@MaxConcurrentCount@ is at most one more than
-- the @FailureToleranceCount@.
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
-- 'regionConcurrencyType', 'stackSetOperationPreferences_regionConcurrencyType' - The concurrency type of deploying StackSets operations in Regions, could
-- be in parallel or one Region at a time.
--
-- 'regionOrder', 'stackSetOperationPreferences_regionOrder' - The order of the Regions in where you want to perform the stack
-- operation.
newStackSetOperationPreferences ::
  StackSetOperationPreferences
newStackSetOperationPreferences =
  StackSetOperationPreferences'
    { failureToleranceCount =
        Prelude.Nothing,
      failureTolerancePercentage = Prelude.Nothing,
      maxConcurrentCount = Prelude.Nothing,
      maxConcurrentPercentage = Prelude.Nothing,
      regionConcurrencyType = Prelude.Nothing,
      regionOrder = Prelude.Nothing
    }

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

-- | The maximum number of accounts in which to perform this operation at one
-- time. This is dependent on the value of
-- @FailureToleranceCount@.@MaxConcurrentCount@ is at most one more than
-- the @FailureToleranceCount@.
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

-- | The concurrency type of deploying StackSets operations in Regions, could
-- be in parallel or one Region at a time.
stackSetOperationPreferences_regionConcurrencyType :: Lens.Lens' StackSetOperationPreferences (Prelude.Maybe RegionConcurrencyType)
stackSetOperationPreferences_regionConcurrencyType = Lens.lens (\StackSetOperationPreferences' {regionConcurrencyType} -> regionConcurrencyType) (\s@StackSetOperationPreferences' {} a -> s {regionConcurrencyType = a} :: StackSetOperationPreferences)

-- | The order of the Regions in where you want to perform the stack
-- operation.
stackSetOperationPreferences_regionOrder :: Lens.Lens' StackSetOperationPreferences (Prelude.Maybe [Prelude.Text])
stackSetOperationPreferences_regionOrder = Lens.lens (\StackSetOperationPreferences' {regionOrder} -> regionOrder) (\s@StackSetOperationPreferences' {} a -> s {regionOrder = a} :: StackSetOperationPreferences) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML StackSetOperationPreferences where
  parseXML x =
    StackSetOperationPreferences'
      Prelude.<$> (x Data..@? "FailureToleranceCount")
      Prelude.<*> (x Data..@? "FailureTolerancePercentage")
      Prelude.<*> (x Data..@? "MaxConcurrentCount")
      Prelude.<*> (x Data..@? "MaxConcurrentPercentage")
      Prelude.<*> (x Data..@? "RegionConcurrencyType")
      Prelude.<*> ( x Data..@? "RegionOrder" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance
  Prelude.Hashable
    StackSetOperationPreferences
  where
  hashWithSalt _salt StackSetOperationPreferences' {..} =
    _salt `Prelude.hashWithSalt` failureToleranceCount
      `Prelude.hashWithSalt` failureTolerancePercentage
      `Prelude.hashWithSalt` maxConcurrentCount
      `Prelude.hashWithSalt` maxConcurrentPercentage
      `Prelude.hashWithSalt` regionConcurrencyType
      `Prelude.hashWithSalt` regionOrder

instance Prelude.NFData StackSetOperationPreferences where
  rnf StackSetOperationPreferences' {..} =
    Prelude.rnf failureToleranceCount
      `Prelude.seq` Prelude.rnf failureTolerancePercentage
      `Prelude.seq` Prelude.rnf maxConcurrentCount
      `Prelude.seq` Prelude.rnf maxConcurrentPercentage
      `Prelude.seq` Prelude.rnf regionConcurrencyType
      `Prelude.seq` Prelude.rnf regionOrder

instance Data.ToQuery StackSetOperationPreferences where
  toQuery StackSetOperationPreferences' {..} =
    Prelude.mconcat
      [ "FailureToleranceCount"
          Data.=: failureToleranceCount,
        "FailureTolerancePercentage"
          Data.=: failureTolerancePercentage,
        "MaxConcurrentCount" Data.=: maxConcurrentCount,
        "MaxConcurrentPercentage"
          Data.=: maxConcurrentPercentage,
        "RegionConcurrencyType"
          Data.=: regionConcurrencyType,
        "RegionOrder"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> regionOrder)
      ]
