{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ConfigurationTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ConfigurationTag where

import Network.AWS.Discovery.Types.ConfigurationItemType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Tags for a configuration item. Tags are metadata that help you categorize IT assets.
--
--
--
-- /See:/ 'configurationTag' smart constructor.
data ConfigurationTag = ConfigurationTag'
  { _ctTimeOfCreation ::
      !(Maybe POSIX),
    _ctConfigurationId :: !(Maybe Text),
    _ctConfigurationType :: !(Maybe ConfigurationItemType),
    _ctValue :: !(Maybe Text),
    _ctKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigurationTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctTimeOfCreation' - The time the configuration tag was created in Coordinated Universal Time (UTC).
--
-- * 'ctConfigurationId' - The configuration ID for the item to tag. You can specify a list of keys and values.
--
-- * 'ctConfigurationType' - A type of IT asset to tag.
--
-- * 'ctValue' - A value on which to filter. For example /key = serverType/ and /value = web server/ .
--
-- * 'ctKey' - A type of tag on which to filter. For example, /serverType/ .
configurationTag ::
  ConfigurationTag
configurationTag =
  ConfigurationTag'
    { _ctTimeOfCreation = Nothing,
      _ctConfigurationId = Nothing,
      _ctConfigurationType = Nothing,
      _ctValue = Nothing,
      _ctKey = Nothing
    }

-- | The time the configuration tag was created in Coordinated Universal Time (UTC).
ctTimeOfCreation :: Lens' ConfigurationTag (Maybe UTCTime)
ctTimeOfCreation = lens _ctTimeOfCreation (\s a -> s {_ctTimeOfCreation = a}) . mapping _Time

-- | The configuration ID for the item to tag. You can specify a list of keys and values.
ctConfigurationId :: Lens' ConfigurationTag (Maybe Text)
ctConfigurationId = lens _ctConfigurationId (\s a -> s {_ctConfigurationId = a})

-- | A type of IT asset to tag.
ctConfigurationType :: Lens' ConfigurationTag (Maybe ConfigurationItemType)
ctConfigurationType = lens _ctConfigurationType (\s a -> s {_ctConfigurationType = a})

-- | A value on which to filter. For example /key = serverType/ and /value = web server/ .
ctValue :: Lens' ConfigurationTag (Maybe Text)
ctValue = lens _ctValue (\s a -> s {_ctValue = a})

-- | A type of tag on which to filter. For example, /serverType/ .
ctKey :: Lens' ConfigurationTag (Maybe Text)
ctKey = lens _ctKey (\s a -> s {_ctKey = a})

instance FromJSON ConfigurationTag where
  parseJSON =
    withObject
      "ConfigurationTag"
      ( \x ->
          ConfigurationTag'
            <$> (x .:? "timeOfCreation")
            <*> (x .:? "configurationId")
            <*> (x .:? "configurationType")
            <*> (x .:? "value")
            <*> (x .:? "key")
      )

instance Hashable ConfigurationTag

instance NFData ConfigurationTag
