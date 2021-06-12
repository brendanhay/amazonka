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
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes criteria to restrict the results when listing platform
-- versions.
--
-- The filter is evaluated as follows: @Type Operator Values[1]@
--
-- /See:/ 'newPlatformFilter' smart constructor.
data PlatformFilter = PlatformFilter'
  { -- | The list of values applied to the filtering platform version attribute.
    -- Only one value is supported for all current operators.
    --
    -- The following list shows valid filter values for some filter attributes.
    --
    -- -   @PlatformStatus@: @Creating@ | @Failed@ | @Ready@ | @Deleting@ |
    --     @Deleted@
    --
    -- -   @PlatformLifecycleState@: @recommended@
    --
    -- -   @SupportedTier@: @WebServer\/Standard@ | @Worker\/SQS\/HTTP@
    --
    -- -   @SupportedAddon@: @Log\/S3@ | @Monitoring\/Healthd@ |
    --     @WorkerDaemon\/SQSD@
    values :: Core.Maybe [Core.Text],
    -- | The operator to apply to the @Type@ with each of the @Values@.
    --
    -- Valid values: @=@ | @!=@ | @\<@ | @\<=@ | @>@ | @>=@ | @contains@ |
    -- @begins_with@ | @ends_with@
    operator :: Core.Maybe Core.Text,
    -- | The platform version attribute to which the filter values are applied.
    --
    -- Valid values: @PlatformName@ | @PlatformVersion@ | @PlatformStatus@ |
    -- @PlatformBranchName@ | @PlatformLifecycleState@ | @PlatformOwner@ |
    -- @SupportedTier@ | @SupportedAddon@ | @ProgrammingLanguageName@ |
    -- @OperatingSystemName@
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PlatformFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'platformFilter_values' - The list of values applied to the filtering platform version attribute.
-- Only one value is supported for all current operators.
--
-- The following list shows valid filter values for some filter attributes.
--
-- -   @PlatformStatus@: @Creating@ | @Failed@ | @Ready@ | @Deleting@ |
--     @Deleted@
--
-- -   @PlatformLifecycleState@: @recommended@
--
-- -   @SupportedTier@: @WebServer\/Standard@ | @Worker\/SQS\/HTTP@
--
-- -   @SupportedAddon@: @Log\/S3@ | @Monitoring\/Healthd@ |
--     @WorkerDaemon\/SQSD@
--
-- 'operator', 'platformFilter_operator' - The operator to apply to the @Type@ with each of the @Values@.
--
-- Valid values: @=@ | @!=@ | @\<@ | @\<=@ | @>@ | @>=@ | @contains@ |
-- @begins_with@ | @ends_with@
--
-- 'type'', 'platformFilter_type' - The platform version attribute to which the filter values are applied.
--
-- Valid values: @PlatformName@ | @PlatformVersion@ | @PlatformStatus@ |
-- @PlatformBranchName@ | @PlatformLifecycleState@ | @PlatformOwner@ |
-- @SupportedTier@ | @SupportedAddon@ | @ProgrammingLanguageName@ |
-- @OperatingSystemName@
newPlatformFilter ::
  PlatformFilter
newPlatformFilter =
  PlatformFilter'
    { values = Core.Nothing,
      operator = Core.Nothing,
      type' = Core.Nothing
    }

-- | The list of values applied to the filtering platform version attribute.
-- Only one value is supported for all current operators.
--
-- The following list shows valid filter values for some filter attributes.
--
-- -   @PlatformStatus@: @Creating@ | @Failed@ | @Ready@ | @Deleting@ |
--     @Deleted@
--
-- -   @PlatformLifecycleState@: @recommended@
--
-- -   @SupportedTier@: @WebServer\/Standard@ | @Worker\/SQS\/HTTP@
--
-- -   @SupportedAddon@: @Log\/S3@ | @Monitoring\/Healthd@ |
--     @WorkerDaemon\/SQSD@
platformFilter_values :: Lens.Lens' PlatformFilter (Core.Maybe [Core.Text])
platformFilter_values = Lens.lens (\PlatformFilter' {values} -> values) (\s@PlatformFilter' {} a -> s {values = a} :: PlatformFilter) Core.. Lens.mapping Lens._Coerce

-- | The operator to apply to the @Type@ with each of the @Values@.
--
-- Valid values: @=@ | @!=@ | @\<@ | @\<=@ | @>@ | @>=@ | @contains@ |
-- @begins_with@ | @ends_with@
platformFilter_operator :: Lens.Lens' PlatformFilter (Core.Maybe Core.Text)
platformFilter_operator = Lens.lens (\PlatformFilter' {operator} -> operator) (\s@PlatformFilter' {} a -> s {operator = a} :: PlatformFilter)

-- | The platform version attribute to which the filter values are applied.
--
-- Valid values: @PlatformName@ | @PlatformVersion@ | @PlatformStatus@ |
-- @PlatformBranchName@ | @PlatformLifecycleState@ | @PlatformOwner@ |
-- @SupportedTier@ | @SupportedAddon@ | @ProgrammingLanguageName@ |
-- @OperatingSystemName@
platformFilter_type :: Lens.Lens' PlatformFilter (Core.Maybe Core.Text)
platformFilter_type = Lens.lens (\PlatformFilter' {type'} -> type') (\s@PlatformFilter' {} a -> s {type' = a} :: PlatformFilter)

instance Core.Hashable PlatformFilter

instance Core.NFData PlatformFilter

instance Core.ToQuery PlatformFilter where
  toQuery PlatformFilter' {..} =
    Core.mconcat
      [ "Values"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> values),
        "Operator" Core.=: operator,
        "Type" Core.=: type'
      ]
