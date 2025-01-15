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
-- Module      : Amazonka.DeviceFarm.Types.DeviceFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.DeviceFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.DeviceFilterAttribute
import Amazonka.DeviceFarm.Types.RuleOperator
import qualified Amazonka.Prelude as Prelude

-- | Represents a device filter used to select a set of devices to be
-- included in a test run. This data structure is passed in as the
-- @deviceSelectionConfiguration@ parameter to @ScheduleRun@. For an
-- example of the JSON request syntax, see ScheduleRun.
--
-- It is also passed in as the @filters@ parameter to @ListDevices@. For an
-- example of the JSON request syntax, see ListDevices.
--
-- /See:/ 'newDeviceFilter' smart constructor.
data DeviceFilter = DeviceFilter'
  { -- | The aspect of a device such as platform or model used as the selection
    -- criteria in a device filter.
    --
    -- The supported operators for each attribute are provided in the following
    -- list.
    --
    -- [ARN]
    --     The Amazon Resource Name (ARN) of the device (for example,
    --     @arn:aws:devicefarm:us-west-2::device:12345Example@).
    --
    --     Supported operators: @EQUALS@, @IN@, @NOT_IN@
    --
    -- [PLATFORM]
    --     The device platform. Valid values are ANDROID or IOS.
    --
    --     Supported operators: @EQUALS@
    --
    -- [OS_VERSION]
    --     The operating system version (for example, 10.3.2).
    --
    --     Supported operators: @EQUALS@, @GREATER_THAN@,
    --     @GREATER_THAN_OR_EQUALS@, @IN@, @LESS_THAN@, @LESS_THAN_OR_EQUALS@,
    --     @NOT_IN@
    --
    -- [MODEL]
    --     The device model (for example, iPad 5th Gen).
    --
    --     Supported operators: @CONTAINS@, @EQUALS@, @IN@, @NOT_IN@
    --
    -- [AVAILABILITY]
    --     The current availability of the device. Valid values are AVAILABLE,
    --     HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
    --
    --     Supported operators: @EQUALS@
    --
    -- [FORM_FACTOR]
    --     The device form factor. Valid values are PHONE or TABLET.
    --
    --     Supported operators: @EQUALS@
    --
    -- [MANUFACTURER]
    --     The device manufacturer (for example, Apple).
    --
    --     Supported operators: @EQUALS@, @IN@, @NOT_IN@
    --
    -- [REMOTE_ACCESS_ENABLED]
    --     Whether the device is enabled for remote access. Valid values are
    --     TRUE or FALSE.
    --
    --     Supported operators: @EQUALS@
    --
    -- [REMOTE_DEBUG_ENABLED]
    --     Whether the device is enabled for remote debugging. Valid values are
    --     TRUE or FALSE.
    --
    --     Supported operators: @EQUALS@
    --
    --     Because remote debugging is
    --     <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>,
    --     this filter is ignored.
    --
    -- [INSTANCE_ARN]
    --     The Amazon Resource Name (ARN) of the device instance.
    --
    --     Supported operators: @EQUALS@, @IN@, @NOT_IN@
    --
    -- [INSTANCE_LABELS]
    --     The label of the device instance.
    --
    --     Supported operators: @CONTAINS@
    --
    -- [FLEET_TYPE]
    --     The fleet type. Valid values are PUBLIC or PRIVATE.
    --
    --     Supported operators: @EQUALS@
    attribute :: DeviceFilterAttribute,
    -- | Specifies how Device Farm compares the filter\'s attribute to the value.
    -- See the attribute descriptions.
    operator :: RuleOperator,
    -- | An array of one or more filter values used in a device filter.
    --
    -- __Operator Values__
    --
    -- -   The IN and NOT_IN operators can take a values array that has more
    --     than one element.
    --
    -- -   The other operators require an array with a single element.
    --
    -- __Attribute Values__
    --
    -- -   The PLATFORM attribute can be set to ANDROID or IOS.
    --
    -- -   The AVAILABILITY attribute can be set to AVAILABLE,
    --     HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
    --
    -- -   The FORM_FACTOR attribute can be set to PHONE or TABLET.
    --
    -- -   The FLEET_TYPE attribute can be set to PUBLIC or PRIVATE.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'deviceFilter_attribute' - The aspect of a device such as platform or model used as the selection
-- criteria in a device filter.
--
-- The supported operators for each attribute are provided in the following
-- list.
--
-- [ARN]
--     The Amazon Resource Name (ARN) of the device (for example,
--     @arn:aws:devicefarm:us-west-2::device:12345Example@).
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [PLATFORM]
--     The device platform. Valid values are ANDROID or IOS.
--
--     Supported operators: @EQUALS@
--
-- [OS_VERSION]
--     The operating system version (for example, 10.3.2).
--
--     Supported operators: @EQUALS@, @GREATER_THAN@,
--     @GREATER_THAN_OR_EQUALS@, @IN@, @LESS_THAN@, @LESS_THAN_OR_EQUALS@,
--     @NOT_IN@
--
-- [MODEL]
--     The device model (for example, iPad 5th Gen).
--
--     Supported operators: @CONTAINS@, @EQUALS@, @IN@, @NOT_IN@
--
-- [AVAILABILITY]
--     The current availability of the device. Valid values are AVAILABLE,
--     HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
--
--     Supported operators: @EQUALS@
--
-- [FORM_FACTOR]
--     The device form factor. Valid values are PHONE or TABLET.
--
--     Supported operators: @EQUALS@
--
-- [MANUFACTURER]
--     The device manufacturer (for example, Apple).
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [REMOTE_ACCESS_ENABLED]
--     Whether the device is enabled for remote access. Valid values are
--     TRUE or FALSE.
--
--     Supported operators: @EQUALS@
--
-- [REMOTE_DEBUG_ENABLED]
--     Whether the device is enabled for remote debugging. Valid values are
--     TRUE or FALSE.
--
--     Supported operators: @EQUALS@
--
--     Because remote debugging is
--     <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>,
--     this filter is ignored.
--
-- [INSTANCE_ARN]
--     The Amazon Resource Name (ARN) of the device instance.
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [INSTANCE_LABELS]
--     The label of the device instance.
--
--     Supported operators: @CONTAINS@
--
-- [FLEET_TYPE]
--     The fleet type. Valid values are PUBLIC or PRIVATE.
--
--     Supported operators: @EQUALS@
--
-- 'operator', 'deviceFilter_operator' - Specifies how Device Farm compares the filter\'s attribute to the value.
-- See the attribute descriptions.
--
-- 'values', 'deviceFilter_values' - An array of one or more filter values used in a device filter.
--
-- __Operator Values__
--
-- -   The IN and NOT_IN operators can take a values array that has more
--     than one element.
--
-- -   The other operators require an array with a single element.
--
-- __Attribute Values__
--
-- -   The PLATFORM attribute can be set to ANDROID or IOS.
--
-- -   The AVAILABILITY attribute can be set to AVAILABLE,
--     HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
--
-- -   The FORM_FACTOR attribute can be set to PHONE or TABLET.
--
-- -   The FLEET_TYPE attribute can be set to PUBLIC or PRIVATE.
newDeviceFilter ::
  -- | 'attribute'
  DeviceFilterAttribute ->
  -- | 'operator'
  RuleOperator ->
  DeviceFilter
newDeviceFilter pAttribute_ pOperator_ =
  DeviceFilter'
    { attribute = pAttribute_,
      operator = pOperator_,
      values = Prelude.mempty
    }

-- | The aspect of a device such as platform or model used as the selection
-- criteria in a device filter.
--
-- The supported operators for each attribute are provided in the following
-- list.
--
-- [ARN]
--     The Amazon Resource Name (ARN) of the device (for example,
--     @arn:aws:devicefarm:us-west-2::device:12345Example@).
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [PLATFORM]
--     The device platform. Valid values are ANDROID or IOS.
--
--     Supported operators: @EQUALS@
--
-- [OS_VERSION]
--     The operating system version (for example, 10.3.2).
--
--     Supported operators: @EQUALS@, @GREATER_THAN@,
--     @GREATER_THAN_OR_EQUALS@, @IN@, @LESS_THAN@, @LESS_THAN_OR_EQUALS@,
--     @NOT_IN@
--
-- [MODEL]
--     The device model (for example, iPad 5th Gen).
--
--     Supported operators: @CONTAINS@, @EQUALS@, @IN@, @NOT_IN@
--
-- [AVAILABILITY]
--     The current availability of the device. Valid values are AVAILABLE,
--     HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
--
--     Supported operators: @EQUALS@
--
-- [FORM_FACTOR]
--     The device form factor. Valid values are PHONE or TABLET.
--
--     Supported operators: @EQUALS@
--
-- [MANUFACTURER]
--     The device manufacturer (for example, Apple).
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [REMOTE_ACCESS_ENABLED]
--     Whether the device is enabled for remote access. Valid values are
--     TRUE or FALSE.
--
--     Supported operators: @EQUALS@
--
-- [REMOTE_DEBUG_ENABLED]
--     Whether the device is enabled for remote debugging. Valid values are
--     TRUE or FALSE.
--
--     Supported operators: @EQUALS@
--
--     Because remote debugging is
--     <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>,
--     this filter is ignored.
--
-- [INSTANCE_ARN]
--     The Amazon Resource Name (ARN) of the device instance.
--
--     Supported operators: @EQUALS@, @IN@, @NOT_IN@
--
-- [INSTANCE_LABELS]
--     The label of the device instance.
--
--     Supported operators: @CONTAINS@
--
-- [FLEET_TYPE]
--     The fleet type. Valid values are PUBLIC or PRIVATE.
--
--     Supported operators: @EQUALS@
deviceFilter_attribute :: Lens.Lens' DeviceFilter DeviceFilterAttribute
deviceFilter_attribute = Lens.lens (\DeviceFilter' {attribute} -> attribute) (\s@DeviceFilter' {} a -> s {attribute = a} :: DeviceFilter)

-- | Specifies how Device Farm compares the filter\'s attribute to the value.
-- See the attribute descriptions.
deviceFilter_operator :: Lens.Lens' DeviceFilter RuleOperator
deviceFilter_operator = Lens.lens (\DeviceFilter' {operator} -> operator) (\s@DeviceFilter' {} a -> s {operator = a} :: DeviceFilter)

-- | An array of one or more filter values used in a device filter.
--
-- __Operator Values__
--
-- -   The IN and NOT_IN operators can take a values array that has more
--     than one element.
--
-- -   The other operators require an array with a single element.
--
-- __Attribute Values__
--
-- -   The PLATFORM attribute can be set to ANDROID or IOS.
--
-- -   The AVAILABILITY attribute can be set to AVAILABLE,
--     HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
--
-- -   The FORM_FACTOR attribute can be set to PHONE or TABLET.
--
-- -   The FLEET_TYPE attribute can be set to PUBLIC or PRIVATE.
deviceFilter_values :: Lens.Lens' DeviceFilter [Prelude.Text]
deviceFilter_values = Lens.lens (\DeviceFilter' {values} -> values) (\s@DeviceFilter' {} a -> s {values = a} :: DeviceFilter) Prelude.. Lens.coerced

instance Data.FromJSON DeviceFilter where
  parseJSON =
    Data.withObject
      "DeviceFilter"
      ( \x ->
          DeviceFilter'
            Prelude.<$> (x Data..: "attribute")
            Prelude.<*> (x Data..: "operator")
            Prelude.<*> (x Data..:? "values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DeviceFilter where
  hashWithSalt _salt DeviceFilter' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` values

instance Prelude.NFData DeviceFilter where
  rnf DeviceFilter' {..} =
    Prelude.rnf attribute `Prelude.seq`
      Prelude.rnf operator `Prelude.seq`
        Prelude.rnf values

instance Data.ToJSON DeviceFilter where
  toJSON DeviceFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("attribute" Data..= attribute),
            Prelude.Just ("operator" Data..= operator),
            Prelude.Just ("values" Data..= values)
          ]
      )
