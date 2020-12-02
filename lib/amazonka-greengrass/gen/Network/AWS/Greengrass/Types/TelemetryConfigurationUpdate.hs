{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.TelemetryConfigurationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.TelemetryConfigurationUpdate where

import Network.AWS.Greengrass.Types.Telemetry
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration settings for running telemetry.
--
-- /See:/ 'telemetryConfigurationUpdate' smart constructor.
newtype TelemetryConfigurationUpdate = TelemetryConfigurationUpdate'
  { _tcuTelemetry ::
      Telemetry
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TelemetryConfigurationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcuTelemetry' - Configure telemetry to be on or off.
telemetryConfigurationUpdate ::
  -- | 'tcuTelemetry'
  Telemetry ->
  TelemetryConfigurationUpdate
telemetryConfigurationUpdate pTelemetry_ =
  TelemetryConfigurationUpdate' {_tcuTelemetry = pTelemetry_}

-- | Configure telemetry to be on or off.
tcuTelemetry :: Lens' TelemetryConfigurationUpdate Telemetry
tcuTelemetry = lens _tcuTelemetry (\s a -> s {_tcuTelemetry = a})

instance Hashable TelemetryConfigurationUpdate

instance NFData TelemetryConfigurationUpdate

instance ToJSON TelemetryConfigurationUpdate where
  toJSON TelemetryConfigurationUpdate' {..} =
    object (catMaybes [Just ("Telemetry" .= _tcuTelemetry)])
