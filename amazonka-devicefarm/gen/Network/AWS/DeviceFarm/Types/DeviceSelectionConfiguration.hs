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
-- Module      : Network.AWS.DeviceFarm.Types.DeviceSelectionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceSelectionConfiguration where

import Network.AWS.DeviceFarm.Types.DeviceFilter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the device filters used in a test run and the maximum number
-- of devices to be included in the run. It is passed in as the
-- @deviceSelectionConfiguration@ request parameter in ScheduleRun.
--
-- /See:/ 'newDeviceSelectionConfiguration' smart constructor.
data DeviceSelectionConfiguration = DeviceSelectionConfiguration'
  { -- | Used to dynamically select a set of devices for a test run. A filter is
    -- made up of an attribute, an operator, and one or more values.
    --
    -- -   __Attribute__
    --
    --     The aspect of a device such as platform or model used as the
    --     selection criteria in a device filter.
    --
    --     Allowed values include:
    --
    --     -   ARN: The Amazon Resource Name (ARN) of the device (for example,
    --         @arn:aws:devicefarm:us-west-2::device:12345Example@).
    --
    --     -   PLATFORM: The device platform. Valid values are ANDROID or IOS.
    --
    --     -   OS_VERSION: The operating system version (for example, 10.3.2).
    --
    --     -   MODEL: The device model (for example, iPad 5th Gen).
    --
    --     -   AVAILABILITY: The current availability of the device. Valid
    --         values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or
    --         TEMPORARY_NOT_AVAILABLE.
    --
    --     -   FORM_FACTOR: The device form factor. Valid values are PHONE or
    --         TABLET.
    --
    --     -   MANUFACTURER: The device manufacturer (for example, Apple).
    --
    --     -   REMOTE_ACCESS_ENABLED: Whether the device is enabled for remote
    --         access. Valid values are TRUE or FALSE.
    --
    --     -   REMOTE_DEBUG_ENABLED: Whether the device is enabled for remote
    --         debugging. Valid values are TRUE or FALSE. Because remote
    --         debugging is
    --         <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>,
    --         this filter is ignored.
    --
    --     -   INSTANCE_ARN: The Amazon Resource Name (ARN) of the device
    --         instance.
    --
    --     -   INSTANCE_LABELS: The label of the device instance.
    --
    --     -   FLEET_TYPE: The fleet type. Valid values are PUBLIC or PRIVATE.
    --
    -- -   __Operator__
    --
    --     The filter operator.
    --
    --     -   The EQUALS operator is available for every attribute except
    --         INSTANCE_LABELS.
    --
    --     -   The CONTAINS operator is available for the INSTANCE_LABELS and
    --         MODEL attributes.
    --
    --     -   The IN and NOT_IN operators are available for the ARN,
    --         OS_VERSION, MODEL, MANUFACTURER, and INSTANCE_ARN attributes.
    --
    --     -   The LESS_THAN, GREATER_THAN, LESS_THAN_OR_EQUALS, and
    --         GREATER_THAN_OR_EQUALS operators are also available for the
    --         OS_VERSION attribute.
    --
    -- -   __Values__
    --
    --     An array of one or more filter values.
    --
    --     __Operator Values__
    --
    --     -   The IN and NOT_IN operators can take a values array that has
    --         more than one element.
    --
    --     -   The other operators require an array with a single element.
    --
    --     __Attribute Values__
    --
    --     -   The PLATFORM attribute can be set to ANDROID or IOS.
    --
    --     -   The AVAILABILITY attribute can be set to AVAILABLE,
    --         HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
    --
    --     -   The FORM_FACTOR attribute can be set to PHONE or TABLET.
    --
    --     -   The FLEET_TYPE attribute can be set to PUBLIC or PRIVATE.
    filters :: [DeviceFilter],
    -- | The maximum number of devices to be included in a test run.
    maxDevices :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeviceSelectionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'deviceSelectionConfiguration_filters' - Used to dynamically select a set of devices for a test run. A filter is
-- made up of an attribute, an operator, and one or more values.
--
-- -   __Attribute__
--
--     The aspect of a device such as platform or model used as the
--     selection criteria in a device filter.
--
--     Allowed values include:
--
--     -   ARN: The Amazon Resource Name (ARN) of the device (for example,
--         @arn:aws:devicefarm:us-west-2::device:12345Example@).
--
--     -   PLATFORM: The device platform. Valid values are ANDROID or IOS.
--
--     -   OS_VERSION: The operating system version (for example, 10.3.2).
--
--     -   MODEL: The device model (for example, iPad 5th Gen).
--
--     -   AVAILABILITY: The current availability of the device. Valid
--         values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or
--         TEMPORARY_NOT_AVAILABLE.
--
--     -   FORM_FACTOR: The device form factor. Valid values are PHONE or
--         TABLET.
--
--     -   MANUFACTURER: The device manufacturer (for example, Apple).
--
--     -   REMOTE_ACCESS_ENABLED: Whether the device is enabled for remote
--         access. Valid values are TRUE or FALSE.
--
--     -   REMOTE_DEBUG_ENABLED: Whether the device is enabled for remote
--         debugging. Valid values are TRUE or FALSE. Because remote
--         debugging is
--         <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>,
--         this filter is ignored.
--
--     -   INSTANCE_ARN: The Amazon Resource Name (ARN) of the device
--         instance.
--
--     -   INSTANCE_LABELS: The label of the device instance.
--
--     -   FLEET_TYPE: The fleet type. Valid values are PUBLIC or PRIVATE.
--
-- -   __Operator__
--
--     The filter operator.
--
--     -   The EQUALS operator is available for every attribute except
--         INSTANCE_LABELS.
--
--     -   The CONTAINS operator is available for the INSTANCE_LABELS and
--         MODEL attributes.
--
--     -   The IN and NOT_IN operators are available for the ARN,
--         OS_VERSION, MODEL, MANUFACTURER, and INSTANCE_ARN attributes.
--
--     -   The LESS_THAN, GREATER_THAN, LESS_THAN_OR_EQUALS, and
--         GREATER_THAN_OR_EQUALS operators are also available for the
--         OS_VERSION attribute.
--
-- -   __Values__
--
--     An array of one or more filter values.
--
--     __Operator Values__
--
--     -   The IN and NOT_IN operators can take a values array that has
--         more than one element.
--
--     -   The other operators require an array with a single element.
--
--     __Attribute Values__
--
--     -   The PLATFORM attribute can be set to ANDROID or IOS.
--
--     -   The AVAILABILITY attribute can be set to AVAILABLE,
--         HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
--
--     -   The FORM_FACTOR attribute can be set to PHONE or TABLET.
--
--     -   The FLEET_TYPE attribute can be set to PUBLIC or PRIVATE.
--
-- 'maxDevices', 'deviceSelectionConfiguration_maxDevices' - The maximum number of devices to be included in a test run.
newDeviceSelectionConfiguration ::
  -- | 'maxDevices'
  Prelude.Int ->
  DeviceSelectionConfiguration
newDeviceSelectionConfiguration pMaxDevices_ =
  DeviceSelectionConfiguration'
    { filters =
        Prelude.mempty,
      maxDevices = pMaxDevices_
    }

-- | Used to dynamically select a set of devices for a test run. A filter is
-- made up of an attribute, an operator, and one or more values.
--
-- -   __Attribute__
--
--     The aspect of a device such as platform or model used as the
--     selection criteria in a device filter.
--
--     Allowed values include:
--
--     -   ARN: The Amazon Resource Name (ARN) of the device (for example,
--         @arn:aws:devicefarm:us-west-2::device:12345Example@).
--
--     -   PLATFORM: The device platform. Valid values are ANDROID or IOS.
--
--     -   OS_VERSION: The operating system version (for example, 10.3.2).
--
--     -   MODEL: The device model (for example, iPad 5th Gen).
--
--     -   AVAILABILITY: The current availability of the device. Valid
--         values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or
--         TEMPORARY_NOT_AVAILABLE.
--
--     -   FORM_FACTOR: The device form factor. Valid values are PHONE or
--         TABLET.
--
--     -   MANUFACTURER: The device manufacturer (for example, Apple).
--
--     -   REMOTE_ACCESS_ENABLED: Whether the device is enabled for remote
--         access. Valid values are TRUE or FALSE.
--
--     -   REMOTE_DEBUG_ENABLED: Whether the device is enabled for remote
--         debugging. Valid values are TRUE or FALSE. Because remote
--         debugging is
--         <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>,
--         this filter is ignored.
--
--     -   INSTANCE_ARN: The Amazon Resource Name (ARN) of the device
--         instance.
--
--     -   INSTANCE_LABELS: The label of the device instance.
--
--     -   FLEET_TYPE: The fleet type. Valid values are PUBLIC or PRIVATE.
--
-- -   __Operator__
--
--     The filter operator.
--
--     -   The EQUALS operator is available for every attribute except
--         INSTANCE_LABELS.
--
--     -   The CONTAINS operator is available for the INSTANCE_LABELS and
--         MODEL attributes.
--
--     -   The IN and NOT_IN operators are available for the ARN,
--         OS_VERSION, MODEL, MANUFACTURER, and INSTANCE_ARN attributes.
--
--     -   The LESS_THAN, GREATER_THAN, LESS_THAN_OR_EQUALS, and
--         GREATER_THAN_OR_EQUALS operators are also available for the
--         OS_VERSION attribute.
--
-- -   __Values__
--
--     An array of one or more filter values.
--
--     __Operator Values__
--
--     -   The IN and NOT_IN operators can take a values array that has
--         more than one element.
--
--     -   The other operators require an array with a single element.
--
--     __Attribute Values__
--
--     -   The PLATFORM attribute can be set to ANDROID or IOS.
--
--     -   The AVAILABILITY attribute can be set to AVAILABLE,
--         HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
--
--     -   The FORM_FACTOR attribute can be set to PHONE or TABLET.
--
--     -   The FLEET_TYPE attribute can be set to PUBLIC or PRIVATE.
deviceSelectionConfiguration_filters :: Lens.Lens' DeviceSelectionConfiguration [DeviceFilter]
deviceSelectionConfiguration_filters = Lens.lens (\DeviceSelectionConfiguration' {filters} -> filters) (\s@DeviceSelectionConfiguration' {} a -> s {filters = a} :: DeviceSelectionConfiguration) Prelude.. Prelude._Coerce

-- | The maximum number of devices to be included in a test run.
deviceSelectionConfiguration_maxDevices :: Lens.Lens' DeviceSelectionConfiguration Prelude.Int
deviceSelectionConfiguration_maxDevices = Lens.lens (\DeviceSelectionConfiguration' {maxDevices} -> maxDevices) (\s@DeviceSelectionConfiguration' {} a -> s {maxDevices = a} :: DeviceSelectionConfiguration)

instance
  Prelude.Hashable
    DeviceSelectionConfiguration

instance Prelude.NFData DeviceSelectionConfiguration

instance Prelude.ToJSON DeviceSelectionConfiguration where
  toJSON DeviceSelectionConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("filters" Prelude..= filters),
            Prelude.Just ("maxDevices" Prelude..= maxDevices)
          ]
      )
