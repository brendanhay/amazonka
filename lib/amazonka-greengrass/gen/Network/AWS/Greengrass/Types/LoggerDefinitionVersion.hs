{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.LoggerDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LoggerDefinitionVersion where

import Network.AWS.Greengrass.Types.GreengrassLogger
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a logger definition version.
--
-- /See:/ 'loggerDefinitionVersion' smart constructor.
newtype LoggerDefinitionVersion = LoggerDefinitionVersion'
  { _ldvLoggers ::
      Maybe [GreengrassLogger]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoggerDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldvLoggers' - A list of loggers.
loggerDefinitionVersion ::
  LoggerDefinitionVersion
loggerDefinitionVersion =
  LoggerDefinitionVersion' {_ldvLoggers = Nothing}

-- | A list of loggers.
ldvLoggers :: Lens' LoggerDefinitionVersion [GreengrassLogger]
ldvLoggers = lens _ldvLoggers (\s a -> s {_ldvLoggers = a}) . _Default . _Coerce

instance FromJSON LoggerDefinitionVersion where
  parseJSON =
    withObject
      "LoggerDefinitionVersion"
      (\x -> LoggerDefinitionVersion' <$> (x .:? "Loggers" .!= mempty))

instance Hashable LoggerDefinitionVersion

instance NFData LoggerDefinitionVersion

instance ToJSON LoggerDefinitionVersion where
  toJSON LoggerDefinitionVersion' {..} =
    object (catMaybes [("Loggers" .=) <$> _ldvLoggers])
