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
-- Module      : Amazonka.IoTWireless.UpdateResourceEventConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the event configuration for a particular resource identifier.
module Amazonka.IoTWireless.UpdateResourceEventConfiguration
  ( -- * Creating a Request
    UpdateResourceEventConfiguration (..),
    newUpdateResourceEventConfiguration,

    -- * Request Lenses
    updateResourceEventConfiguration_deviceRegistrationState,
    updateResourceEventConfiguration_connectionStatus,
    updateResourceEventConfiguration_messageDeliveryStatus,
    updateResourceEventConfiguration_join,
    updateResourceEventConfiguration_proximity,
    updateResourceEventConfiguration_partnerType,
    updateResourceEventConfiguration_identifier,
    updateResourceEventConfiguration_identifierType,

    -- * Destructuring the Response
    UpdateResourceEventConfigurationResponse (..),
    newUpdateResourceEventConfigurationResponse,

    -- * Response Lenses
    updateResourceEventConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateResourceEventConfiguration' smart constructor.
data UpdateResourceEventConfiguration = UpdateResourceEventConfiguration'
  { -- | Event configuration for the device registration state event.
    deviceRegistrationState :: Prelude.Maybe DeviceRegistrationStateEventConfiguration,
    -- | Event configuration for the connection status event.
    connectionStatus :: Prelude.Maybe ConnectionStatusEventConfiguration,
    -- | Event configuration for the message delivery status event.
    messageDeliveryStatus :: Prelude.Maybe MessageDeliveryStatusEventConfiguration,
    -- | Event configuration for the join event.
    join :: Prelude.Maybe JoinEventConfiguration,
    -- | Event configuration for the proximity event.
    proximity :: Prelude.Maybe ProximityEventConfiguration,
    -- | Partner type of the resource if the identifier type is
    -- @PartnerAccountId@
    partnerType :: Prelude.Maybe EventNotificationPartnerType,
    -- | Resource identifier to opt in for event messaging.
    identifier :: Prelude.Text,
    -- | Identifier type of the particular resource identifier for event
    -- configuration.
    identifierType :: IdentifierType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceRegistrationState', 'updateResourceEventConfiguration_deviceRegistrationState' - Event configuration for the device registration state event.
--
-- 'connectionStatus', 'updateResourceEventConfiguration_connectionStatus' - Event configuration for the connection status event.
--
-- 'messageDeliveryStatus', 'updateResourceEventConfiguration_messageDeliveryStatus' - Event configuration for the message delivery status event.
--
-- 'join', 'updateResourceEventConfiguration_join' - Event configuration for the join event.
--
-- 'proximity', 'updateResourceEventConfiguration_proximity' - Event configuration for the proximity event.
--
-- 'partnerType', 'updateResourceEventConfiguration_partnerType' - Partner type of the resource if the identifier type is
-- @PartnerAccountId@
--
-- 'identifier', 'updateResourceEventConfiguration_identifier' - Resource identifier to opt in for event messaging.
--
-- 'identifierType', 'updateResourceEventConfiguration_identifierType' - Identifier type of the particular resource identifier for event
-- configuration.
newUpdateResourceEventConfiguration ::
  -- | 'identifier'
  Prelude.Text ->
  -- | 'identifierType'
  IdentifierType ->
  UpdateResourceEventConfiguration
newUpdateResourceEventConfiguration
  pIdentifier_
  pIdentifierType_ =
    UpdateResourceEventConfiguration'
      { deviceRegistrationState =
          Prelude.Nothing,
        connectionStatus = Prelude.Nothing,
        messageDeliveryStatus = Prelude.Nothing,
        join = Prelude.Nothing,
        proximity = Prelude.Nothing,
        partnerType = Prelude.Nothing,
        identifier = pIdentifier_,
        identifierType = pIdentifierType_
      }

-- | Event configuration for the device registration state event.
updateResourceEventConfiguration_deviceRegistrationState :: Lens.Lens' UpdateResourceEventConfiguration (Prelude.Maybe DeviceRegistrationStateEventConfiguration)
updateResourceEventConfiguration_deviceRegistrationState = Lens.lens (\UpdateResourceEventConfiguration' {deviceRegistrationState} -> deviceRegistrationState) (\s@UpdateResourceEventConfiguration' {} a -> s {deviceRegistrationState = a} :: UpdateResourceEventConfiguration)

-- | Event configuration for the connection status event.
updateResourceEventConfiguration_connectionStatus :: Lens.Lens' UpdateResourceEventConfiguration (Prelude.Maybe ConnectionStatusEventConfiguration)
updateResourceEventConfiguration_connectionStatus = Lens.lens (\UpdateResourceEventConfiguration' {connectionStatus} -> connectionStatus) (\s@UpdateResourceEventConfiguration' {} a -> s {connectionStatus = a} :: UpdateResourceEventConfiguration)

-- | Event configuration for the message delivery status event.
updateResourceEventConfiguration_messageDeliveryStatus :: Lens.Lens' UpdateResourceEventConfiguration (Prelude.Maybe MessageDeliveryStatusEventConfiguration)
updateResourceEventConfiguration_messageDeliveryStatus = Lens.lens (\UpdateResourceEventConfiguration' {messageDeliveryStatus} -> messageDeliveryStatus) (\s@UpdateResourceEventConfiguration' {} a -> s {messageDeliveryStatus = a} :: UpdateResourceEventConfiguration)

-- | Event configuration for the join event.
updateResourceEventConfiguration_join :: Lens.Lens' UpdateResourceEventConfiguration (Prelude.Maybe JoinEventConfiguration)
updateResourceEventConfiguration_join = Lens.lens (\UpdateResourceEventConfiguration' {join} -> join) (\s@UpdateResourceEventConfiguration' {} a -> s {join = a} :: UpdateResourceEventConfiguration)

-- | Event configuration for the proximity event.
updateResourceEventConfiguration_proximity :: Lens.Lens' UpdateResourceEventConfiguration (Prelude.Maybe ProximityEventConfiguration)
updateResourceEventConfiguration_proximity = Lens.lens (\UpdateResourceEventConfiguration' {proximity} -> proximity) (\s@UpdateResourceEventConfiguration' {} a -> s {proximity = a} :: UpdateResourceEventConfiguration)

-- | Partner type of the resource if the identifier type is
-- @PartnerAccountId@
updateResourceEventConfiguration_partnerType :: Lens.Lens' UpdateResourceEventConfiguration (Prelude.Maybe EventNotificationPartnerType)
updateResourceEventConfiguration_partnerType = Lens.lens (\UpdateResourceEventConfiguration' {partnerType} -> partnerType) (\s@UpdateResourceEventConfiguration' {} a -> s {partnerType = a} :: UpdateResourceEventConfiguration)

-- | Resource identifier to opt in for event messaging.
updateResourceEventConfiguration_identifier :: Lens.Lens' UpdateResourceEventConfiguration Prelude.Text
updateResourceEventConfiguration_identifier = Lens.lens (\UpdateResourceEventConfiguration' {identifier} -> identifier) (\s@UpdateResourceEventConfiguration' {} a -> s {identifier = a} :: UpdateResourceEventConfiguration)

-- | Identifier type of the particular resource identifier for event
-- configuration.
updateResourceEventConfiguration_identifierType :: Lens.Lens' UpdateResourceEventConfiguration IdentifierType
updateResourceEventConfiguration_identifierType = Lens.lens (\UpdateResourceEventConfiguration' {identifierType} -> identifierType) (\s@UpdateResourceEventConfiguration' {} a -> s {identifierType = a} :: UpdateResourceEventConfiguration)

instance
  Core.AWSRequest
    UpdateResourceEventConfiguration
  where
  type
    AWSResponse UpdateResourceEventConfiguration =
      UpdateResourceEventConfigurationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateResourceEventConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateResourceEventConfiguration
  where
  hashWithSalt
    _salt
    UpdateResourceEventConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` deviceRegistrationState
        `Prelude.hashWithSalt` connectionStatus
        `Prelude.hashWithSalt` messageDeliveryStatus
        `Prelude.hashWithSalt` join
        `Prelude.hashWithSalt` proximity
        `Prelude.hashWithSalt` partnerType
        `Prelude.hashWithSalt` identifier
        `Prelude.hashWithSalt` identifierType

instance
  Prelude.NFData
    UpdateResourceEventConfiguration
  where
  rnf UpdateResourceEventConfiguration' {..} =
    Prelude.rnf deviceRegistrationState
      `Prelude.seq` Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf messageDeliveryStatus
      `Prelude.seq` Prelude.rnf join
      `Prelude.seq` Prelude.rnf proximity
      `Prelude.seq` Prelude.rnf partnerType
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf identifierType

instance
  Data.ToHeaders
    UpdateResourceEventConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateResourceEventConfiguration where
  toJSON UpdateResourceEventConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeviceRegistrationState" Data..=)
              Prelude.<$> deviceRegistrationState,
            ("ConnectionStatus" Data..=)
              Prelude.<$> connectionStatus,
            ("MessageDeliveryStatus" Data..=)
              Prelude.<$> messageDeliveryStatus,
            ("Join" Data..=) Prelude.<$> join,
            ("Proximity" Data..=) Prelude.<$> proximity
          ]
      )

instance Data.ToPath UpdateResourceEventConfiguration where
  toPath UpdateResourceEventConfiguration' {..} =
    Prelude.mconcat
      ["/event-configurations/", Data.toBS identifier]

instance
  Data.ToQuery
    UpdateResourceEventConfiguration
  where
  toQuery UpdateResourceEventConfiguration' {..} =
    Prelude.mconcat
      [ "partnerType" Data.=: partnerType,
        "identifierType" Data.=: identifierType
      ]

-- | /See:/ 'newUpdateResourceEventConfigurationResponse' smart constructor.
data UpdateResourceEventConfigurationResponse = UpdateResourceEventConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceEventConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateResourceEventConfigurationResponse_httpStatus' - The response's http status code.
newUpdateResourceEventConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResourceEventConfigurationResponse
newUpdateResourceEventConfigurationResponse
  pHttpStatus_ =
    UpdateResourceEventConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateResourceEventConfigurationResponse_httpStatus :: Lens.Lens' UpdateResourceEventConfigurationResponse Prelude.Int
updateResourceEventConfigurationResponse_httpStatus = Lens.lens (\UpdateResourceEventConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateResourceEventConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateResourceEventConfigurationResponse)

instance
  Prelude.NFData
    UpdateResourceEventConfigurationResponse
  where
  rnf UpdateResourceEventConfigurationResponse' {..} =
    Prelude.rnf httpStatus
