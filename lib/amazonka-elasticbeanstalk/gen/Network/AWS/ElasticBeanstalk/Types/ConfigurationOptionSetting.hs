{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A specification identifying an individual configuration option along with its current value. For a list of possible namespaces and option values, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values> in the /AWS Elastic Beanstalk Developer Guide/ .
--
--
--
-- /See:/ 'configurationOptionSetting' smart constructor.
data ConfigurationOptionSetting = ConfigurationOptionSetting'
  { _cosOptionName ::
      !(Maybe Text),
    _cosResourceName :: !(Maybe Text),
    _cosNamespace :: !(Maybe Text),
    _cosValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigurationOptionSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cosOptionName' - The name of the configuration option.
--
-- * 'cosResourceName' - A unique resource name for the option setting. Use it for a time–based scaling configuration option.
--
-- * 'cosNamespace' - A unique namespace that identifies the option's associated AWS resource.
--
-- * 'cosValue' - The current value for the configuration option.
configurationOptionSetting ::
  ConfigurationOptionSetting
configurationOptionSetting =
  ConfigurationOptionSetting'
    { _cosOptionName = Nothing,
      _cosResourceName = Nothing,
      _cosNamespace = Nothing,
      _cosValue = Nothing
    }

-- | The name of the configuration option.
cosOptionName :: Lens' ConfigurationOptionSetting (Maybe Text)
cosOptionName = lens _cosOptionName (\s a -> s {_cosOptionName = a})

-- | A unique resource name for the option setting. Use it for a time–based scaling configuration option.
cosResourceName :: Lens' ConfigurationOptionSetting (Maybe Text)
cosResourceName = lens _cosResourceName (\s a -> s {_cosResourceName = a})

-- | A unique namespace that identifies the option's associated AWS resource.
cosNamespace :: Lens' ConfigurationOptionSetting (Maybe Text)
cosNamespace = lens _cosNamespace (\s a -> s {_cosNamespace = a})

-- | The current value for the configuration option.
cosValue :: Lens' ConfigurationOptionSetting (Maybe Text)
cosValue = lens _cosValue (\s a -> s {_cosValue = a})

instance FromXML ConfigurationOptionSetting where
  parseXML x =
    ConfigurationOptionSetting'
      <$> (x .@? "OptionName")
      <*> (x .@? "ResourceName")
      <*> (x .@? "Namespace")
      <*> (x .@? "Value")

instance Hashable ConfigurationOptionSetting

instance NFData ConfigurationOptionSetting

instance ToQuery ConfigurationOptionSetting where
  toQuery ConfigurationOptionSetting' {..} =
    mconcat
      [ "OptionName" =: _cosOptionName,
        "ResourceName" =: _cosResourceName,
        "Namespace" =: _cosNamespace,
        "Value" =: _cosValue
      ]
