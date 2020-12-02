{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateThingRuntimeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the runtime configuration of a thing.
module Network.AWS.Greengrass.UpdateThingRuntimeConfiguration
  ( -- * Creating a Request
    updateThingRuntimeConfiguration,
    UpdateThingRuntimeConfiguration,

    -- * Request Lenses
    utrcTelemetryConfiguration,
    utrcThingName,

    -- * Destructuring the Response
    updateThingRuntimeConfigurationResponse,
    UpdateThingRuntimeConfigurationResponse,

    -- * Response Lenses
    utrcrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateThingRuntimeConfiguration' smart constructor.
data UpdateThingRuntimeConfiguration = UpdateThingRuntimeConfiguration'
  { _utrcTelemetryConfiguration ::
      !( Maybe
           TelemetryConfigurationUpdate
       ),
    _utrcThingName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateThingRuntimeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrcTelemetryConfiguration' - Configuration for telemetry service.
--
-- * 'utrcThingName' - The thing name.
updateThingRuntimeConfiguration ::
  -- | 'utrcThingName'
  Text ->
  UpdateThingRuntimeConfiguration
updateThingRuntimeConfiguration pThingName_ =
  UpdateThingRuntimeConfiguration'
    { _utrcTelemetryConfiguration =
        Nothing,
      _utrcThingName = pThingName_
    }

-- | Configuration for telemetry service.
utrcTelemetryConfiguration :: Lens' UpdateThingRuntimeConfiguration (Maybe TelemetryConfigurationUpdate)
utrcTelemetryConfiguration = lens _utrcTelemetryConfiguration (\s a -> s {_utrcTelemetryConfiguration = a})

-- | The thing name.
utrcThingName :: Lens' UpdateThingRuntimeConfiguration Text
utrcThingName = lens _utrcThingName (\s a -> s {_utrcThingName = a})

instance AWSRequest UpdateThingRuntimeConfiguration where
  type
    Rs UpdateThingRuntimeConfiguration =
      UpdateThingRuntimeConfigurationResponse
  request = putJSON greengrass
  response =
    receiveEmpty
      ( \s h x ->
          UpdateThingRuntimeConfigurationResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateThingRuntimeConfiguration

instance NFData UpdateThingRuntimeConfiguration

instance ToHeaders UpdateThingRuntimeConfiguration where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateThingRuntimeConfiguration where
  toJSON UpdateThingRuntimeConfiguration' {..} =
    object
      ( catMaybes
          [("TelemetryConfiguration" .=) <$> _utrcTelemetryConfiguration]
      )

instance ToPath UpdateThingRuntimeConfiguration where
  toPath UpdateThingRuntimeConfiguration' {..} =
    mconcat
      ["/greengrass/things/", toBS _utrcThingName, "/runtimeconfig"]

instance ToQuery UpdateThingRuntimeConfiguration where
  toQuery = const mempty

-- | /See:/ 'updateThingRuntimeConfigurationResponse' smart constructor.
newtype UpdateThingRuntimeConfigurationResponse = UpdateThingRuntimeConfigurationResponse'
  { _utrcrsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'UpdateThingRuntimeConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrcrsResponseStatus' - -- | The response status code.
updateThingRuntimeConfigurationResponse ::
  -- | 'utrcrsResponseStatus'
  Int ->
  UpdateThingRuntimeConfigurationResponse
updateThingRuntimeConfigurationResponse pResponseStatus_ =
  UpdateThingRuntimeConfigurationResponse'
    { _utrcrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
utrcrsResponseStatus :: Lens' UpdateThingRuntimeConfigurationResponse Int
utrcrsResponseStatus = lens _utrcrsResponseStatus (\s a -> s {_utrcrsResponseStatus = a})

instance NFData UpdateThingRuntimeConfigurationResponse
