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
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    values :: Prelude.Maybe [Prelude.Text],
    -- | The operator to apply to the @Type@ with each of the @Values@.
    --
    -- Valid values: @=@ | @!=@ | @\<@ | @\<=@ | @>@ | @>=@ | @contains@ |
    -- @begins_with@ | @ends_with@
    operator :: Prelude.Maybe Prelude.Text,
    -- | The platform version attribute to which the filter values are applied.
    --
    -- Valid values: @PlatformName@ | @PlatformVersion@ | @PlatformStatus@ |
    -- @PlatformBranchName@ | @PlatformLifecycleState@ | @PlatformOwner@ |
    -- @SupportedTier@ | @SupportedAddon@ | @ProgrammingLanguageName@ |
    -- @OperatingSystemName@
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { values = Prelude.Nothing,
      operator = Prelude.Nothing,
      type' = Prelude.Nothing
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
platformFilter_values :: Lens.Lens' PlatformFilter (Prelude.Maybe [Prelude.Text])
platformFilter_values = Lens.lens (\PlatformFilter' {values} -> values) (\s@PlatformFilter' {} a -> s {values = a} :: PlatformFilter) Prelude.. Lens.mapping Prelude._Coerce

-- | The operator to apply to the @Type@ with each of the @Values@.
--
-- Valid values: @=@ | @!=@ | @\<@ | @\<=@ | @>@ | @>=@ | @contains@ |
-- @begins_with@ | @ends_with@
platformFilter_operator :: Lens.Lens' PlatformFilter (Prelude.Maybe Prelude.Text)
platformFilter_operator = Lens.lens (\PlatformFilter' {operator} -> operator) (\s@PlatformFilter' {} a -> s {operator = a} :: PlatformFilter)

-- | The platform version attribute to which the filter values are applied.
--
-- Valid values: @PlatformName@ | @PlatformVersion@ | @PlatformStatus@ |
-- @PlatformBranchName@ | @PlatformLifecycleState@ | @PlatformOwner@ |
-- @SupportedTier@ | @SupportedAddon@ | @ProgrammingLanguageName@ |
-- @OperatingSystemName@
platformFilter_type :: Lens.Lens' PlatformFilter (Prelude.Maybe Prelude.Text)
platformFilter_type = Lens.lens (\PlatformFilter' {type'} -> type') (\s@PlatformFilter' {} a -> s {type' = a} :: PlatformFilter)

instance Prelude.Hashable PlatformFilter

instance Prelude.NFData PlatformFilter

instance Prelude.ToQuery PlatformFilter where
  toQuery PlatformFilter' {..} =
    Prelude.mconcat
      [ "Values"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> values),
        "Operator" Prelude.=: operator,
        "Type" Prelude.=: type'
      ]
