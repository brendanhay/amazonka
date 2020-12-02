{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.RuntimeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.RuntimeConfiguration where

import Network.AWS.Greengrass.Types.TelemetryConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Runtime configuration for a thing.
--
-- /See:/ 'runtimeConfiguration' smart constructor.
newtype RuntimeConfiguration = RuntimeConfiguration'
  { _rcTelemetryConfiguration ::
      Maybe TelemetryConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RuntimeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcTelemetryConfiguration' - Configuration for telemetry service.
runtimeConfiguration ::
  RuntimeConfiguration
runtimeConfiguration =
  RuntimeConfiguration' {_rcTelemetryConfiguration = Nothing}

-- | Configuration for telemetry service.
rcTelemetryConfiguration :: Lens' RuntimeConfiguration (Maybe TelemetryConfiguration)
rcTelemetryConfiguration = lens _rcTelemetryConfiguration (\s a -> s {_rcTelemetryConfiguration = a})

instance FromJSON RuntimeConfiguration where
  parseJSON =
    withObject
      "RuntimeConfiguration"
      (\x -> RuntimeConfiguration' <$> (x .:? "TelemetryConfiguration"))

instance Hashable RuntimeConfiguration

instance NFData RuntimeConfiguration
