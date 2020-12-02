{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionDescription where

import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionValueType
import Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the possible values for a configuration option.
--
--
--
-- /See:/ 'configurationOptionDescription' smart constructor.
data ConfigurationOptionDescription = ConfigurationOptionDescription'
  { _codMaxValue ::
      !(Maybe Int),
    _codRegex ::
      !( Maybe
           OptionRestrictionRegex
       ),
    _codMaxLength :: !(Maybe Int),
    _codUserDefined ::
      !(Maybe Bool),
    _codNamespace ::
      !(Maybe Text),
    _codValueOptions ::
      !(Maybe [Text]),
    _codName :: !(Maybe Text),
    _codChangeSeverity ::
      !(Maybe Text),
    _codDefaultValue ::
      !(Maybe Text),
    _codValueType ::
      !( Maybe
           ConfigurationOptionValueType
       ),
    _codMinValue :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigurationOptionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'codMaxValue' - If specified, the configuration option must be a numeric value less than this value.
--
-- * 'codRegex' - If specified, the configuration option must be a string value that satisfies this regular expression.
--
-- * 'codMaxLength' - If specified, the configuration option must be a string value no longer than this value.
--
-- * 'codUserDefined' - An indication of whether the user defined this configuration option:     * @true@ : This configuration option was defined by the user. It is a valid choice for specifying if this as an @Option to Remove@ when updating configuration settings.      * @false@ : This configuration was not defined by the user. Constraint: You can remove only @UserDefined@ options from a configuration.  Valid Values: @true@ | @false@
--
-- * 'codNamespace' - A unique namespace identifying the option's associated AWS resource.
--
-- * 'codValueOptions' - If specified, values for the configuration option are selected from this list.
--
-- * 'codName' - The name of the configuration option.
--
-- * 'codChangeSeverity' - An indication of which action is required if the value for this configuration option changes:     * @NoInterruption@ : There is no interruption to the environment or application availability.     * @RestartEnvironment@ : The environment is entirely restarted, all AWS resources are deleted and recreated, and the environment is unavailable during the process.     * @RestartApplicationServer@ : The environment is available the entire time. However, a short application outage occurs when the application servers on the running Amazon EC2 instances are restarted.
--
-- * 'codDefaultValue' - The default value for this configuration option.
--
-- * 'codValueType' - An indication of which type of values this option has and whether it is allowable to select one or more than one of the possible values:     * @Scalar@ : Values for this option are a single selection from the possible values, or an unformatted string, or numeric value governed by the @MIN/MAX/Regex@ constraints.     * @List@ : Values for this option are multiple selections from the possible values.     * @Boolean@ : Values for this option are either @true@ or @false@ .     * @Json@ : Values for this option are a JSON representation of a @ConfigDocument@ .
--
-- * 'codMinValue' - If specified, the configuration option must be a numeric value greater than this value.
configurationOptionDescription ::
  ConfigurationOptionDescription
configurationOptionDescription =
  ConfigurationOptionDescription'
    { _codMaxValue = Nothing,
      _codRegex = Nothing,
      _codMaxLength = Nothing,
      _codUserDefined = Nothing,
      _codNamespace = Nothing,
      _codValueOptions = Nothing,
      _codName = Nothing,
      _codChangeSeverity = Nothing,
      _codDefaultValue = Nothing,
      _codValueType = Nothing,
      _codMinValue = Nothing
    }

-- | If specified, the configuration option must be a numeric value less than this value.
codMaxValue :: Lens' ConfigurationOptionDescription (Maybe Int)
codMaxValue = lens _codMaxValue (\s a -> s {_codMaxValue = a})

-- | If specified, the configuration option must be a string value that satisfies this regular expression.
codRegex :: Lens' ConfigurationOptionDescription (Maybe OptionRestrictionRegex)
codRegex = lens _codRegex (\s a -> s {_codRegex = a})

-- | If specified, the configuration option must be a string value no longer than this value.
codMaxLength :: Lens' ConfigurationOptionDescription (Maybe Int)
codMaxLength = lens _codMaxLength (\s a -> s {_codMaxLength = a})

-- | An indication of whether the user defined this configuration option:     * @true@ : This configuration option was defined by the user. It is a valid choice for specifying if this as an @Option to Remove@ when updating configuration settings.      * @false@ : This configuration was not defined by the user. Constraint: You can remove only @UserDefined@ options from a configuration.  Valid Values: @true@ | @false@
codUserDefined :: Lens' ConfigurationOptionDescription (Maybe Bool)
codUserDefined = lens _codUserDefined (\s a -> s {_codUserDefined = a})

-- | A unique namespace identifying the option's associated AWS resource.
codNamespace :: Lens' ConfigurationOptionDescription (Maybe Text)
codNamespace = lens _codNamespace (\s a -> s {_codNamespace = a})

-- | If specified, values for the configuration option are selected from this list.
codValueOptions :: Lens' ConfigurationOptionDescription [Text]
codValueOptions = lens _codValueOptions (\s a -> s {_codValueOptions = a}) . _Default . _Coerce

-- | The name of the configuration option.
codName :: Lens' ConfigurationOptionDescription (Maybe Text)
codName = lens _codName (\s a -> s {_codName = a})

-- | An indication of which action is required if the value for this configuration option changes:     * @NoInterruption@ : There is no interruption to the environment or application availability.     * @RestartEnvironment@ : The environment is entirely restarted, all AWS resources are deleted and recreated, and the environment is unavailable during the process.     * @RestartApplicationServer@ : The environment is available the entire time. However, a short application outage occurs when the application servers on the running Amazon EC2 instances are restarted.
codChangeSeverity :: Lens' ConfigurationOptionDescription (Maybe Text)
codChangeSeverity = lens _codChangeSeverity (\s a -> s {_codChangeSeverity = a})

-- | The default value for this configuration option.
codDefaultValue :: Lens' ConfigurationOptionDescription (Maybe Text)
codDefaultValue = lens _codDefaultValue (\s a -> s {_codDefaultValue = a})

-- | An indication of which type of values this option has and whether it is allowable to select one or more than one of the possible values:     * @Scalar@ : Values for this option are a single selection from the possible values, or an unformatted string, or numeric value governed by the @MIN/MAX/Regex@ constraints.     * @List@ : Values for this option are multiple selections from the possible values.     * @Boolean@ : Values for this option are either @true@ or @false@ .     * @Json@ : Values for this option are a JSON representation of a @ConfigDocument@ .
codValueType :: Lens' ConfigurationOptionDescription (Maybe ConfigurationOptionValueType)
codValueType = lens _codValueType (\s a -> s {_codValueType = a})

-- | If specified, the configuration option must be a numeric value greater than this value.
codMinValue :: Lens' ConfigurationOptionDescription (Maybe Int)
codMinValue = lens _codMinValue (\s a -> s {_codMinValue = a})

instance FromXML ConfigurationOptionDescription where
  parseXML x =
    ConfigurationOptionDescription'
      <$> (x .@? "MaxValue")
      <*> (x .@? "Regex")
      <*> (x .@? "MaxLength")
      <*> (x .@? "UserDefined")
      <*> (x .@? "Namespace")
      <*> (x .@? "ValueOptions" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Name")
      <*> (x .@? "ChangeSeverity")
      <*> (x .@? "DefaultValue")
      <*> (x .@? "ValueType")
      <*> (x .@? "MinValue")

instance Hashable ConfigurationOptionDescription

instance NFData ConfigurationOptionDescription
