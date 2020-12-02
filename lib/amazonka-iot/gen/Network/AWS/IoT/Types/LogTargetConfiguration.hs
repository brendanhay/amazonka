{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LogTargetConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LogTargetConfiguration where

import Network.AWS.IoT.Types.LogLevel
import Network.AWS.IoT.Types.LogTarget
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The target configuration.
--
--
--
-- /See:/ 'logTargetConfiguration' smart constructor.
data LogTargetConfiguration = LogTargetConfiguration'
  { _ltcLogLevel ::
      !(Maybe LogLevel),
    _ltcLogTarget :: !(Maybe LogTarget)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LogTargetConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltcLogLevel' - The logging level.
--
-- * 'ltcLogTarget' - A log target
logTargetConfiguration ::
  LogTargetConfiguration
logTargetConfiguration =
  LogTargetConfiguration'
    { _ltcLogLevel = Nothing,
      _ltcLogTarget = Nothing
    }

-- | The logging level.
ltcLogLevel :: Lens' LogTargetConfiguration (Maybe LogLevel)
ltcLogLevel = lens _ltcLogLevel (\s a -> s {_ltcLogLevel = a})

-- | A log target
ltcLogTarget :: Lens' LogTargetConfiguration (Maybe LogTarget)
ltcLogTarget = lens _ltcLogTarget (\s a -> s {_ltcLogTarget = a})

instance FromJSON LogTargetConfiguration where
  parseJSON =
    withObject
      "LogTargetConfiguration"
      ( \x ->
          LogTargetConfiguration'
            <$> (x .:? "logLevel") <*> (x .:? "logTarget")
      )

instance Hashable LogTargetConfiguration

instance NFData LogTargetConfiguration
