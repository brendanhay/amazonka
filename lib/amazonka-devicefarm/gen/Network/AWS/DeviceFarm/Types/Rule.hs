{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Rule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Rule where

import Network.AWS.DeviceFarm.Types.DeviceAttribute
import Network.AWS.DeviceFarm.Types.RuleOperator
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a condition for a device pool.
--
--
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
  { _rAttribute :: !(Maybe DeviceAttribute),
    _rOperator :: !(Maybe RuleOperator),
    _rValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rAttribute' - The rule's stringified attribute. For example, specify the value as @"\"abc\""@ . The supported operators for each attribute are provided in the following list.     * APPIUM_VERSION    * The Appium version for the test. Supported operators: @CONTAINS@      * ARN    * The Amazon Resource Name (ARN) of the device (for example, @arn:aws:devicefarm:us-west-2::device:12345Example@ . Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * AVAILABILITY    * The current availability of the device. Valid values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE. Supported operators: @EQUALS@      * FLEET_TYPE    * The fleet type. Valid values are PUBLIC or PRIVATE. Supported operators: @EQUALS@      * FORM_FACTOR    * The device form factor. Valid values are PHONE or TABLET. Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * INSTANCE_ARN    * The Amazon Resource Name (ARN) of the device instance. Supported operators: @IN@ , @NOT_IN@      * INSTANCE_LABELS    * The label of the device instance. Supported operators: @CONTAINS@      * MANUFACTURER    * The device manufacturer (for example, Apple). Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * MODEL    * The device model, such as Apple iPad Air 2 or Google Pixel. Supported operators: @CONTAINS@ , @EQUALS@ , @IN@ , @NOT_IN@      * OS_VERSION    * The operating system version (for example, 10.3.2). Supported operators: @EQUALS@ , @GREATER_THAN@ , @GREATER_THAN_OR_EQUALS@ , @IN@ , @LESS_THAN@ , @LESS_THAN_OR_EQUALS@ , @NOT_IN@      * PLATFORM    * The device platform. Valid values are ANDROID or IOS. Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * REMOTE_ACCESS_ENABLED    * Whether the device is enabled for remote access. Valid values are TRUE or FALSE. Supported operators: @EQUALS@      * REMOTE_DEBUG_ENABLED    * Whether the device is enabled for remote debugging. Valid values are TRUE or FALSE. Supported operators: @EQUALS@  Because remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> , this filter is ignored.
--
-- * 'rOperator' - Specifies how Device Farm compares the rule's attribute to the value. For the operators that are supported by each attribute, see the attribute descriptions.
--
-- * 'rValue' - The rule's value.
rule ::
  Rule
rule =
  Rule'
    { _rAttribute = Nothing,
      _rOperator = Nothing,
      _rValue = Nothing
    }

-- | The rule's stringified attribute. For example, specify the value as @"\"abc\""@ . The supported operators for each attribute are provided in the following list.     * APPIUM_VERSION    * The Appium version for the test. Supported operators: @CONTAINS@      * ARN    * The Amazon Resource Name (ARN) of the device (for example, @arn:aws:devicefarm:us-west-2::device:12345Example@ . Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * AVAILABILITY    * The current availability of the device. Valid values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE. Supported operators: @EQUALS@      * FLEET_TYPE    * The fleet type. Valid values are PUBLIC or PRIVATE. Supported operators: @EQUALS@      * FORM_FACTOR    * The device form factor. Valid values are PHONE or TABLET. Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * INSTANCE_ARN    * The Amazon Resource Name (ARN) of the device instance. Supported operators: @IN@ , @NOT_IN@      * INSTANCE_LABELS    * The label of the device instance. Supported operators: @CONTAINS@      * MANUFACTURER    * The device manufacturer (for example, Apple). Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * MODEL    * The device model, such as Apple iPad Air 2 or Google Pixel. Supported operators: @CONTAINS@ , @EQUALS@ , @IN@ , @NOT_IN@      * OS_VERSION    * The operating system version (for example, 10.3.2). Supported operators: @EQUALS@ , @GREATER_THAN@ , @GREATER_THAN_OR_EQUALS@ , @IN@ , @LESS_THAN@ , @LESS_THAN_OR_EQUALS@ , @NOT_IN@      * PLATFORM    * The device platform. Valid values are ANDROID or IOS. Supported operators: @EQUALS@ , @IN@ , @NOT_IN@      * REMOTE_ACCESS_ENABLED    * Whether the device is enabled for remote access. Valid values are TRUE or FALSE. Supported operators: @EQUALS@      * REMOTE_DEBUG_ENABLED    * Whether the device is enabled for remote debugging. Valid values are TRUE or FALSE. Supported operators: @EQUALS@  Because remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> , this filter is ignored.
rAttribute :: Lens' Rule (Maybe DeviceAttribute)
rAttribute = lens _rAttribute (\s a -> s {_rAttribute = a})

-- | Specifies how Device Farm compares the rule's attribute to the value. For the operators that are supported by each attribute, see the attribute descriptions.
rOperator :: Lens' Rule (Maybe RuleOperator)
rOperator = lens _rOperator (\s a -> s {_rOperator = a})

-- | The rule's value.
rValue :: Lens' Rule (Maybe Text)
rValue = lens _rValue (\s a -> s {_rValue = a})

instance FromJSON Rule where
  parseJSON =
    withObject
      "Rule"
      ( \x ->
          Rule'
            <$> (x .:? "attribute") <*> (x .:? "operator") <*> (x .:? "value")
      )

instance Hashable Rule

instance NFData Rule

instance ToJSON Rule where
  toJSON Rule' {..} =
    object
      ( catMaybes
          [ ("attribute" .=) <$> _rAttribute,
            ("operator" .=) <$> _rOperator,
            ("value" .=) <$> _rValue
          ]
      )
