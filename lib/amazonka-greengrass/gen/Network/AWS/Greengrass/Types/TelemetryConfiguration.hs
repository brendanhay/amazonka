{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.TelemetryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.TelemetryConfiguration where

import Network.AWS.Greengrass.Types.ConfigurationSyncStatus
import Network.AWS.Greengrass.Types.Telemetry
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration settings for running telemetry.
--
-- /See:/ 'telemetryConfiguration' smart constructor.
data TelemetryConfiguration = TelemetryConfiguration'
  { _tcConfigurationSyncStatus ::
      !(Maybe ConfigurationSyncStatus),
    _tcTelemetry :: !Telemetry
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TelemetryConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcConfigurationSyncStatus' - Synchronization status of the device reported configuration with the desired configuration.
--
-- * 'tcTelemetry' - Configure telemetry to be on or off.
telemetryConfiguration ::
  -- | 'tcTelemetry'
  Telemetry ->
  TelemetryConfiguration
telemetryConfiguration pTelemetry_ =
  TelemetryConfiguration'
    { _tcConfigurationSyncStatus = Nothing,
      _tcTelemetry = pTelemetry_
    }

-- | Synchronization status of the device reported configuration with the desired configuration.
tcConfigurationSyncStatus :: Lens' TelemetryConfiguration (Maybe ConfigurationSyncStatus)
tcConfigurationSyncStatus = lens _tcConfigurationSyncStatus (\s a -> s {_tcConfigurationSyncStatus = a})

-- | Configure telemetry to be on or off.
tcTelemetry :: Lens' TelemetryConfiguration Telemetry
tcTelemetry = lens _tcTelemetry (\s a -> s {_tcTelemetry = a})

instance FromJSON TelemetryConfiguration where
  parseJSON =
    withObject
      "TelemetryConfiguration"
      ( \x ->
          TelemetryConfiguration'
            <$> (x .:? "ConfigurationSyncStatus") <*> (x .: "Telemetry")
      )

instance Hashable TelemetryConfiguration

instance NFData TelemetryConfiguration
