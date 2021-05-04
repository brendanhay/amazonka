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
-- Module      : Network.AWS.DeviceFarm.Types.DeviceFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceFilter where

import Network.AWS.DeviceFarm.Types.DeviceFilterAttribute
import Network.AWS.DeviceFarm.Types.RuleOperator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  { -- | Specifies how Device Farm compares the filter\'s attribute to the value.
    -- See the attribute descriptions.
    operator :: Prelude.Maybe RuleOperator,
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
    values :: Prelude.Maybe [Prelude.Text],
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
    attribute :: Prelude.Maybe DeviceFilterAttribute
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeviceFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
newDeviceFilter ::
  DeviceFilter
newDeviceFilter =
  DeviceFilter'
    { operator = Prelude.Nothing,
      values = Prelude.Nothing,
      attribute = Prelude.Nothing
    }

-- | Specifies how Device Farm compares the filter\'s attribute to the value.
-- See the attribute descriptions.
deviceFilter_operator :: Lens.Lens' DeviceFilter (Prelude.Maybe RuleOperator)
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
deviceFilter_values :: Lens.Lens' DeviceFilter (Prelude.Maybe [Prelude.Text])
deviceFilter_values = Lens.lens (\DeviceFilter' {values} -> values) (\s@DeviceFilter' {} a -> s {values = a} :: DeviceFilter) Prelude.. Lens.mapping Prelude._Coerce

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
deviceFilter_attribute :: Lens.Lens' DeviceFilter (Prelude.Maybe DeviceFilterAttribute)
deviceFilter_attribute = Lens.lens (\DeviceFilter' {attribute} -> attribute) (\s@DeviceFilter' {} a -> s {attribute = a} :: DeviceFilter)

instance Prelude.FromJSON DeviceFilter where
  parseJSON =
    Prelude.withObject
      "DeviceFilter"
      ( \x ->
          DeviceFilter'
            Prelude.<$> (x Prelude..:? "operator")
            Prelude.<*> (x Prelude..:? "values" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "attribute")
      )

instance Prelude.Hashable DeviceFilter

instance Prelude.NFData DeviceFilter

instance Prelude.ToJSON DeviceFilter where
  toJSON DeviceFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("operator" Prelude..=) Prelude.<$> operator,
            ("values" Prelude..=) Prelude.<$> values,
            ("attribute" Prelude..=) Prelude.<$> attribute
          ]
      )
