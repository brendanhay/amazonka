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
-- Module      : Amazonka.IoTWireless.UpdateEventConfigurationByResourceTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the event configuration based on resource types.
module Amazonka.IoTWireless.UpdateEventConfigurationByResourceTypes
  ( -- * Creating a Request
    UpdateEventConfigurationByResourceTypes (..),
    newUpdateEventConfigurationByResourceTypes,

    -- * Request Lenses
    updateEventConfigurationByResourceTypes_deviceRegistrationState,
    updateEventConfigurationByResourceTypes_connectionStatus,
    updateEventConfigurationByResourceTypes_messageDeliveryStatus,
    updateEventConfigurationByResourceTypes_join,
    updateEventConfigurationByResourceTypes_proximity,

    -- * Destructuring the Response
    UpdateEventConfigurationByResourceTypesResponse (..),
    newUpdateEventConfigurationByResourceTypesResponse,

    -- * Response Lenses
    updateEventConfigurationByResourceTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEventConfigurationByResourceTypes' smart constructor.
data UpdateEventConfigurationByResourceTypes = UpdateEventConfigurationByResourceTypes'
  { -- | Device registration state resource type event configuration object for
    -- enabling and disabling wireless gateway topic.
    deviceRegistrationState :: Prelude.Maybe DeviceRegistrationStateResourceTypeEventConfiguration,
    -- | Connection status resource type event configuration object for enabling
    -- and disabling wireless gateway topic.
    connectionStatus :: Prelude.Maybe ConnectionStatusResourceTypeEventConfiguration,
    -- | Message delivery status resource type event configuration object for
    -- enabling and disabling wireless device topic.
    messageDeliveryStatus :: Prelude.Maybe MessageDeliveryStatusResourceTypeEventConfiguration,
    -- | Join resource type event configuration object for enabling and disabling
    -- wireless device topic.
    join :: Prelude.Maybe JoinResourceTypeEventConfiguration,
    -- | Proximity resource type event configuration object for enabling and
    -- disabling wireless gateway topic.
    proximity :: Prelude.Maybe ProximityResourceTypeEventConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventConfigurationByResourceTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceRegistrationState', 'updateEventConfigurationByResourceTypes_deviceRegistrationState' - Device registration state resource type event configuration object for
-- enabling and disabling wireless gateway topic.
--
-- 'connectionStatus', 'updateEventConfigurationByResourceTypes_connectionStatus' - Connection status resource type event configuration object for enabling
-- and disabling wireless gateway topic.
--
-- 'messageDeliveryStatus', 'updateEventConfigurationByResourceTypes_messageDeliveryStatus' - Message delivery status resource type event configuration object for
-- enabling and disabling wireless device topic.
--
-- 'join', 'updateEventConfigurationByResourceTypes_join' - Join resource type event configuration object for enabling and disabling
-- wireless device topic.
--
-- 'proximity', 'updateEventConfigurationByResourceTypes_proximity' - Proximity resource type event configuration object for enabling and
-- disabling wireless gateway topic.
newUpdateEventConfigurationByResourceTypes ::
  UpdateEventConfigurationByResourceTypes
newUpdateEventConfigurationByResourceTypes =
  UpdateEventConfigurationByResourceTypes'
    { deviceRegistrationState =
        Prelude.Nothing,
      connectionStatus = Prelude.Nothing,
      messageDeliveryStatus =
        Prelude.Nothing,
      join = Prelude.Nothing,
      proximity = Prelude.Nothing
    }

-- | Device registration state resource type event configuration object for
-- enabling and disabling wireless gateway topic.
updateEventConfigurationByResourceTypes_deviceRegistrationState :: Lens.Lens' UpdateEventConfigurationByResourceTypes (Prelude.Maybe DeviceRegistrationStateResourceTypeEventConfiguration)
updateEventConfigurationByResourceTypes_deviceRegistrationState = Lens.lens (\UpdateEventConfigurationByResourceTypes' {deviceRegistrationState} -> deviceRegistrationState) (\s@UpdateEventConfigurationByResourceTypes' {} a -> s {deviceRegistrationState = a} :: UpdateEventConfigurationByResourceTypes)

-- | Connection status resource type event configuration object for enabling
-- and disabling wireless gateway topic.
updateEventConfigurationByResourceTypes_connectionStatus :: Lens.Lens' UpdateEventConfigurationByResourceTypes (Prelude.Maybe ConnectionStatusResourceTypeEventConfiguration)
updateEventConfigurationByResourceTypes_connectionStatus = Lens.lens (\UpdateEventConfigurationByResourceTypes' {connectionStatus} -> connectionStatus) (\s@UpdateEventConfigurationByResourceTypes' {} a -> s {connectionStatus = a} :: UpdateEventConfigurationByResourceTypes)

-- | Message delivery status resource type event configuration object for
-- enabling and disabling wireless device topic.
updateEventConfigurationByResourceTypes_messageDeliveryStatus :: Lens.Lens' UpdateEventConfigurationByResourceTypes (Prelude.Maybe MessageDeliveryStatusResourceTypeEventConfiguration)
updateEventConfigurationByResourceTypes_messageDeliveryStatus = Lens.lens (\UpdateEventConfigurationByResourceTypes' {messageDeliveryStatus} -> messageDeliveryStatus) (\s@UpdateEventConfigurationByResourceTypes' {} a -> s {messageDeliveryStatus = a} :: UpdateEventConfigurationByResourceTypes)

-- | Join resource type event configuration object for enabling and disabling
-- wireless device topic.
updateEventConfigurationByResourceTypes_join :: Lens.Lens' UpdateEventConfigurationByResourceTypes (Prelude.Maybe JoinResourceTypeEventConfiguration)
updateEventConfigurationByResourceTypes_join = Lens.lens (\UpdateEventConfigurationByResourceTypes' {join} -> join) (\s@UpdateEventConfigurationByResourceTypes' {} a -> s {join = a} :: UpdateEventConfigurationByResourceTypes)

-- | Proximity resource type event configuration object for enabling and
-- disabling wireless gateway topic.
updateEventConfigurationByResourceTypes_proximity :: Lens.Lens' UpdateEventConfigurationByResourceTypes (Prelude.Maybe ProximityResourceTypeEventConfiguration)
updateEventConfigurationByResourceTypes_proximity = Lens.lens (\UpdateEventConfigurationByResourceTypes' {proximity} -> proximity) (\s@UpdateEventConfigurationByResourceTypes' {} a -> s {proximity = a} :: UpdateEventConfigurationByResourceTypes)

instance
  Core.AWSRequest
    UpdateEventConfigurationByResourceTypes
  where
  type
    AWSResponse
      UpdateEventConfigurationByResourceTypes =
      UpdateEventConfigurationByResourceTypesResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEventConfigurationByResourceTypesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateEventConfigurationByResourceTypes
  where
  hashWithSalt
    _salt
    UpdateEventConfigurationByResourceTypes' {..} =
      _salt
        `Prelude.hashWithSalt` deviceRegistrationState
        `Prelude.hashWithSalt` connectionStatus
        `Prelude.hashWithSalt` messageDeliveryStatus
        `Prelude.hashWithSalt` join
        `Prelude.hashWithSalt` proximity

instance
  Prelude.NFData
    UpdateEventConfigurationByResourceTypes
  where
  rnf UpdateEventConfigurationByResourceTypes' {..} =
    Prelude.rnf deviceRegistrationState
      `Prelude.seq` Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf messageDeliveryStatus
      `Prelude.seq` Prelude.rnf join
      `Prelude.seq` Prelude.rnf proximity

instance
  Core.ToHeaders
    UpdateEventConfigurationByResourceTypes
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToJSON
    UpdateEventConfigurationByResourceTypes
  where
  toJSON UpdateEventConfigurationByResourceTypes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DeviceRegistrationState" Core..=)
              Prelude.<$> deviceRegistrationState,
            ("ConnectionStatus" Core..=)
              Prelude.<$> connectionStatus,
            ("MessageDeliveryStatus" Core..=)
              Prelude.<$> messageDeliveryStatus,
            ("Join" Core..=) Prelude.<$> join,
            ("Proximity" Core..=) Prelude.<$> proximity
          ]
      )

instance
  Core.ToPath
    UpdateEventConfigurationByResourceTypes
  where
  toPath =
    Prelude.const
      "/event-configurations-resource-types"

instance
  Core.ToQuery
    UpdateEventConfigurationByResourceTypes
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEventConfigurationByResourceTypesResponse' smart constructor.
data UpdateEventConfigurationByResourceTypesResponse = UpdateEventConfigurationByResourceTypesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventConfigurationByResourceTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEventConfigurationByResourceTypesResponse_httpStatus' - The response's http status code.
newUpdateEventConfigurationByResourceTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEventConfigurationByResourceTypesResponse
newUpdateEventConfigurationByResourceTypesResponse
  pHttpStatus_ =
    UpdateEventConfigurationByResourceTypesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateEventConfigurationByResourceTypesResponse_httpStatus :: Lens.Lens' UpdateEventConfigurationByResourceTypesResponse Prelude.Int
updateEventConfigurationByResourceTypesResponse_httpStatus = Lens.lens (\UpdateEventConfigurationByResourceTypesResponse' {httpStatus} -> httpStatus) (\s@UpdateEventConfigurationByResourceTypesResponse' {} a -> s {httpStatus = a} :: UpdateEventConfigurationByResourceTypesResponse)

instance
  Prelude.NFData
    UpdateEventConfigurationByResourceTypesResponse
  where
  rnf
    UpdateEventConfigurationByResourceTypesResponse' {..} =
      Prelude.rnf httpStatus
