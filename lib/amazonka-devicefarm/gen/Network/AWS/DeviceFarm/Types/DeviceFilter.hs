{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceFilter where

import Network.AWS.DeviceFarm.Types.DeviceFilterAttribute
import Network.AWS.DeviceFarm.Types.RuleOperator
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a device filter used to select a set of devices to be included in a test run. This data structure is passed in as the @deviceSelectionConfiguration@ parameter to @ScheduleRun@ . For an example of the JSON request syntax, see 'ScheduleRun' .
--
--
-- It is also passed in as the @filters@ parameter to @ListDevices@ . For an example of the JSON request syntax, see 'ListDevices' .
--
--
-- /See:/ 'deviceFilter' smart constructor.
data DeviceFilter = DeviceFilter'
  { _dfAttribute ::
      !(Maybe DeviceFilterAttribute),
    _dfOperator :: !(Maybe RuleOperator),
    _dfValues :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfAttribute' - The aspect of a device such as platform or model used as the selection criteria in a device filter. The supported operators for each attribute are provided in the following list.     * ARN    * The Amazon Resource Name (ARN) of the device (for example, @arn:aws:devicefarm:us-west-2::device:12345Example@ ). Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * PLATFORM    * The device platform. Valid values are ANDROID or IOS. Supported operators: @EQUALS@      * OS_VERSION    * The operating system version (for example, 10.3.2). Supported operators: @EQUALS@ , @GREATER_THAN@ , @GREATER_THAN_OR_EQUALS@ , @IN@ , @LESS_THAN@ , @LESS_THAN_OR_EQUALS@ , @NOT_IN@      * MODEL    * The device model (for example, iPad 5th Gen). Supported operators: @CONTAINS@ , @EQUALS@ , @IN@ , @NOT_IN@      * AVAILABILITY    * The current availability of the device. Valid values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE. Supported operators: @EQUALS@      * FORM_FACTOR    * The device form factor. Valid values are PHONE or TABLET. Supported operators: @EQUALS@      * MANUFACTURER    * The device manufacturer (for example, Apple). Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * REMOTE_ACCESS_ENABLED    * Whether the device is enabled for remote access. Valid values are TRUE or FALSE. Supported operators: @EQUALS@      * REMOTE_DEBUG_ENABLED    * Whether the device is enabled for remote debugging. Valid values are TRUE or FALSE. Supported operators: @EQUALS@  Because remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> , this filter is ignored.     * INSTANCE_ARN    * The Amazon Resource Name (ARN) of the device instance. Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * INSTANCE_LABELS    * The label of the device instance. Supported operators: @CONTAINS@      * FLEET_TYPE    * The fleet type. Valid values are PUBLIC or PRIVATE. Supported operators: @EQUALS@
--
-- * 'dfOperator' - Specifies how Device Farm compares the filter's attribute to the value. See the attribute descriptions.
--
-- * 'dfValues' - An array of one or more filter values used in a device filter. __Operator Values__      * The IN and NOT_IN operators can take a values array that has more than one element.     * The other operators require an array with a single element. __Attribute Values__      * The PLATFORM attribute can be set to ANDROID or IOS.     * The AVAILABILITY attribute can be set to AVAILABLE, HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.     * The FORM_FACTOR attribute can be set to PHONE or TABLET.     * The FLEET_TYPE attribute can be set to PUBLIC or PRIVATE.
deviceFilter ::
  DeviceFilter
deviceFilter =
  DeviceFilter'
    { _dfAttribute = Nothing,
      _dfOperator = Nothing,
      _dfValues = Nothing
    }

-- | The aspect of a device such as platform or model used as the selection criteria in a device filter. The supported operators for each attribute are provided in the following list.     * ARN    * The Amazon Resource Name (ARN) of the device (for example, @arn:aws:devicefarm:us-west-2::device:12345Example@ ). Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * PLATFORM    * The device platform. Valid values are ANDROID or IOS. Supported operators: @EQUALS@      * OS_VERSION    * The operating system version (for example, 10.3.2). Supported operators: @EQUALS@ , @GREATER_THAN@ , @GREATER_THAN_OR_EQUALS@ , @IN@ , @LESS_THAN@ , @LESS_THAN_OR_EQUALS@ , @NOT_IN@      * MODEL    * The device model (for example, iPad 5th Gen). Supported operators: @CONTAINS@ , @EQUALS@ , @IN@ , @NOT_IN@      * AVAILABILITY    * The current availability of the device. Valid values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE. Supported operators: @EQUALS@      * FORM_FACTOR    * The device form factor. Valid values are PHONE or TABLET. Supported operators: @EQUALS@      * MANUFACTURER    * The device manufacturer (for example, Apple). Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * REMOTE_ACCESS_ENABLED    * Whether the device is enabled for remote access. Valid values are TRUE or FALSE. Supported operators: @EQUALS@      * REMOTE_DEBUG_ENABLED    * Whether the device is enabled for remote debugging. Valid values are TRUE or FALSE. Supported operators: @EQUALS@  Because remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> , this filter is ignored.     * INSTANCE_ARN    * The Amazon Resource Name (ARN) of the device instance. Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * INSTANCE_LABELS    * The label of the device instance. Supported operators: @CONTAINS@      * FLEET_TYPE    * The fleet type. Valid values are PUBLIC or PRIVATE. Supported operators: @EQUALS@
dfAttribute :: Lens' DeviceFilter (Maybe DeviceFilterAttribute)
dfAttribute = lens _dfAttribute (\s a -> s {_dfAttribute = a})

-- | Specifies how Device Farm compares the filter's attribute to the value. See the attribute descriptions.
dfOperator :: Lens' DeviceFilter (Maybe RuleOperator)
dfOperator = lens _dfOperator (\s a -> s {_dfOperator = a})

-- | An array of one or more filter values used in a device filter. __Operator Values__      * The IN and NOT_IN operators can take a values array that has more than one element.     * The other operators require an array with a single element. __Attribute Values__      * The PLATFORM attribute can be set to ANDROID or IOS.     * The AVAILABILITY attribute can be set to AVAILABLE, HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.     * The FORM_FACTOR attribute can be set to PHONE or TABLET.     * The FLEET_TYPE attribute can be set to PUBLIC or PRIVATE.
dfValues :: Lens' DeviceFilter [Text]
dfValues = lens _dfValues (\s a -> s {_dfValues = a}) . _Default . _Coerce

instance FromJSON DeviceFilter where
  parseJSON =
    withObject
      "DeviceFilter"
      ( \x ->
          DeviceFilter'
            <$> (x .:? "attribute")
            <*> (x .:? "operator")
            <*> (x .:? "values" .!= mempty)
      )

instance Hashable DeviceFilter

instance NFData DeviceFilter

instance ToJSON DeviceFilter where
  toJSON DeviceFilter' {..} =
    object
      ( catMaybes
          [ ("attribute" .=) <$> _dfAttribute,
            ("operator" .=) <$> _dfOperator,
            ("values" .=) <$> _dfValues
          ]
      )
