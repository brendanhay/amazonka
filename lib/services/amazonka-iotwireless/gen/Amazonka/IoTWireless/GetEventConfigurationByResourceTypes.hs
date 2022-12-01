{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTWireless.GetEventConfigurationByResourceTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the event configuration based on resource types.
module Amazonka.IoTWireless.GetEventConfigurationByResourceTypes
  ( -- * Creating a Request
    GetEventConfigurationByResourceTypes (..),
    newGetEventConfigurationByResourceTypes,

    -- * Destructuring the Response
    GetEventConfigurationByResourceTypesResponse (..),
    newGetEventConfigurationByResourceTypesResponse,

    -- * Response Lenses
    getEventConfigurationByResourceTypesResponse_deviceRegistrationState,
    getEventConfigurationByResourceTypesResponse_connectionStatus,
    getEventConfigurationByResourceTypesResponse_messageDeliveryStatus,
    getEventConfigurationByResourceTypesResponse_join,
    getEventConfigurationByResourceTypesResponse_proximity,
    getEventConfigurationByResourceTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEventConfigurationByResourceTypes' smart constructor.
data GetEventConfigurationByResourceTypes = GetEventConfigurationByResourceTypes'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventConfigurationByResourceTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetEventConfigurationByResourceTypes ::
  GetEventConfigurationByResourceTypes
newGetEventConfigurationByResourceTypes =
  GetEventConfigurationByResourceTypes'

instance
  Core.AWSRequest
    GetEventConfigurationByResourceTypes
  where
  type
    AWSResponse GetEventConfigurationByResourceTypes =
      GetEventConfigurationByResourceTypesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventConfigurationByResourceTypesResponse'
            Prelude.<$> (x Core..?> "DeviceRegistrationState")
              Prelude.<*> (x Core..?> "ConnectionStatus")
              Prelude.<*> (x Core..?> "MessageDeliveryStatus")
              Prelude.<*> (x Core..?> "Join")
              Prelude.<*> (x Core..?> "Proximity")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetEventConfigurationByResourceTypes
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetEventConfigurationByResourceTypes
  where
  rnf _ = ()

instance
  Core.ToHeaders
    GetEventConfigurationByResourceTypes
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    GetEventConfigurationByResourceTypes
  where
  toPath =
    Prelude.const
      "/event-configurations-resource-types"

instance
  Core.ToQuery
    GetEventConfigurationByResourceTypes
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEventConfigurationByResourceTypesResponse' smart constructor.
data GetEventConfigurationByResourceTypesResponse = GetEventConfigurationByResourceTypesResponse'
  { -- | Resource type event configuration for the device registration state
    -- event.
    deviceRegistrationState :: Prelude.Maybe DeviceRegistrationStateResourceTypeEventConfiguration,
    -- | Resource type event configuration for the connection status event.
    connectionStatus :: Prelude.Maybe ConnectionStatusResourceTypeEventConfiguration,
    -- | Resource type event configuration object for the message delivery status
    -- event.
    messageDeliveryStatus :: Prelude.Maybe MessageDeliveryStatusResourceTypeEventConfiguration,
    -- | Resource type event configuration for the join event.
    join :: Prelude.Maybe JoinResourceTypeEventConfiguration,
    -- | Resource type event configuration for the proximity event.
    proximity :: Prelude.Maybe ProximityResourceTypeEventConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventConfigurationByResourceTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceRegistrationState', 'getEventConfigurationByResourceTypesResponse_deviceRegistrationState' - Resource type event configuration for the device registration state
-- event.
--
-- 'connectionStatus', 'getEventConfigurationByResourceTypesResponse_connectionStatus' - Resource type event configuration for the connection status event.
--
-- 'messageDeliveryStatus', 'getEventConfigurationByResourceTypesResponse_messageDeliveryStatus' - Resource type event configuration object for the message delivery status
-- event.
--
-- 'join', 'getEventConfigurationByResourceTypesResponse_join' - Resource type event configuration for the join event.
--
-- 'proximity', 'getEventConfigurationByResourceTypesResponse_proximity' - Resource type event configuration for the proximity event.
--
-- 'httpStatus', 'getEventConfigurationByResourceTypesResponse_httpStatus' - The response's http status code.
newGetEventConfigurationByResourceTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEventConfigurationByResourceTypesResponse
newGetEventConfigurationByResourceTypesResponse
  pHttpStatus_ =
    GetEventConfigurationByResourceTypesResponse'
      { deviceRegistrationState =
          Prelude.Nothing,
        connectionStatus =
          Prelude.Nothing,
        messageDeliveryStatus =
          Prelude.Nothing,
        join = Prelude.Nothing,
        proximity = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Resource type event configuration for the device registration state
-- event.
getEventConfigurationByResourceTypesResponse_deviceRegistrationState :: Lens.Lens' GetEventConfigurationByResourceTypesResponse (Prelude.Maybe DeviceRegistrationStateResourceTypeEventConfiguration)
getEventConfigurationByResourceTypesResponse_deviceRegistrationState = Lens.lens (\GetEventConfigurationByResourceTypesResponse' {deviceRegistrationState} -> deviceRegistrationState) (\s@GetEventConfigurationByResourceTypesResponse' {} a -> s {deviceRegistrationState = a} :: GetEventConfigurationByResourceTypesResponse)

-- | Resource type event configuration for the connection status event.
getEventConfigurationByResourceTypesResponse_connectionStatus :: Lens.Lens' GetEventConfigurationByResourceTypesResponse (Prelude.Maybe ConnectionStatusResourceTypeEventConfiguration)
getEventConfigurationByResourceTypesResponse_connectionStatus = Lens.lens (\GetEventConfigurationByResourceTypesResponse' {connectionStatus} -> connectionStatus) (\s@GetEventConfigurationByResourceTypesResponse' {} a -> s {connectionStatus = a} :: GetEventConfigurationByResourceTypesResponse)

-- | Resource type event configuration object for the message delivery status
-- event.
getEventConfigurationByResourceTypesResponse_messageDeliveryStatus :: Lens.Lens' GetEventConfigurationByResourceTypesResponse (Prelude.Maybe MessageDeliveryStatusResourceTypeEventConfiguration)
getEventConfigurationByResourceTypesResponse_messageDeliveryStatus = Lens.lens (\GetEventConfigurationByResourceTypesResponse' {messageDeliveryStatus} -> messageDeliveryStatus) (\s@GetEventConfigurationByResourceTypesResponse' {} a -> s {messageDeliveryStatus = a} :: GetEventConfigurationByResourceTypesResponse)

-- | Resource type event configuration for the join event.
getEventConfigurationByResourceTypesResponse_join :: Lens.Lens' GetEventConfigurationByResourceTypesResponse (Prelude.Maybe JoinResourceTypeEventConfiguration)
getEventConfigurationByResourceTypesResponse_join = Lens.lens (\GetEventConfigurationByResourceTypesResponse' {join} -> join) (\s@GetEventConfigurationByResourceTypesResponse' {} a -> s {join = a} :: GetEventConfigurationByResourceTypesResponse)

-- | Resource type event configuration for the proximity event.
getEventConfigurationByResourceTypesResponse_proximity :: Lens.Lens' GetEventConfigurationByResourceTypesResponse (Prelude.Maybe ProximityResourceTypeEventConfiguration)
getEventConfigurationByResourceTypesResponse_proximity = Lens.lens (\GetEventConfigurationByResourceTypesResponse' {proximity} -> proximity) (\s@GetEventConfigurationByResourceTypesResponse' {} a -> s {proximity = a} :: GetEventConfigurationByResourceTypesResponse)

-- | The response's http status code.
getEventConfigurationByResourceTypesResponse_httpStatus :: Lens.Lens' GetEventConfigurationByResourceTypesResponse Prelude.Int
getEventConfigurationByResourceTypesResponse_httpStatus = Lens.lens (\GetEventConfigurationByResourceTypesResponse' {httpStatus} -> httpStatus) (\s@GetEventConfigurationByResourceTypesResponse' {} a -> s {httpStatus = a} :: GetEventConfigurationByResourceTypesResponse)

instance
  Prelude.NFData
    GetEventConfigurationByResourceTypesResponse
  where
  rnf GetEventConfigurationByResourceTypesResponse' {..} =
    Prelude.rnf deviceRegistrationState
      `Prelude.seq` Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf messageDeliveryStatus
      `Prelude.seq` Prelude.rnf join
      `Prelude.seq` Prelude.rnf proximity
      `Prelude.seq` Prelude.rnf httpStatus
