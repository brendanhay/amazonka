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
-- Module      : Amazonka.IoTWireless.GetResourceEventConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the event configuration for a particular resource identifier.
module Amazonka.IoTWireless.GetResourceEventConfiguration
  ( -- * Creating a Request
    GetResourceEventConfiguration (..),
    newGetResourceEventConfiguration,

    -- * Request Lenses
    getResourceEventConfiguration_partnerType,
    getResourceEventConfiguration_identifier,
    getResourceEventConfiguration_identifierType,

    -- * Destructuring the Response
    GetResourceEventConfigurationResponse (..),
    newGetResourceEventConfigurationResponse,

    -- * Response Lenses
    getResourceEventConfigurationResponse_connectionStatus,
    getResourceEventConfigurationResponse_deviceRegistrationState,
    getResourceEventConfigurationResponse_join,
    getResourceEventConfigurationResponse_messageDeliveryStatus,
    getResourceEventConfigurationResponse_proximity,
    getResourceEventConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceEventConfiguration' smart constructor.
data GetResourceEventConfiguration = GetResourceEventConfiguration'
  { -- | Partner type of the resource if the identifier type is
    -- @PartnerAccountId@.
    partnerType :: Prelude.Maybe EventNotificationPartnerType,
    -- | Resource identifier to opt in for event messaging.
    identifier :: Prelude.Text,
    -- | Identifier type of the particular resource identifier for event
    -- configuration.
    identifierType :: IdentifierType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partnerType', 'getResourceEventConfiguration_partnerType' - Partner type of the resource if the identifier type is
-- @PartnerAccountId@.
--
-- 'identifier', 'getResourceEventConfiguration_identifier' - Resource identifier to opt in for event messaging.
--
-- 'identifierType', 'getResourceEventConfiguration_identifierType' - Identifier type of the particular resource identifier for event
-- configuration.
newGetResourceEventConfiguration ::
  -- | 'identifier'
  Prelude.Text ->
  -- | 'identifierType'
  IdentifierType ->
  GetResourceEventConfiguration
newGetResourceEventConfiguration
  pIdentifier_
  pIdentifierType_ =
    GetResourceEventConfiguration'
      { partnerType =
          Prelude.Nothing,
        identifier = pIdentifier_,
        identifierType = pIdentifierType_
      }

-- | Partner type of the resource if the identifier type is
-- @PartnerAccountId@.
getResourceEventConfiguration_partnerType :: Lens.Lens' GetResourceEventConfiguration (Prelude.Maybe EventNotificationPartnerType)
getResourceEventConfiguration_partnerType = Lens.lens (\GetResourceEventConfiguration' {partnerType} -> partnerType) (\s@GetResourceEventConfiguration' {} a -> s {partnerType = a} :: GetResourceEventConfiguration)

-- | Resource identifier to opt in for event messaging.
getResourceEventConfiguration_identifier :: Lens.Lens' GetResourceEventConfiguration Prelude.Text
getResourceEventConfiguration_identifier = Lens.lens (\GetResourceEventConfiguration' {identifier} -> identifier) (\s@GetResourceEventConfiguration' {} a -> s {identifier = a} :: GetResourceEventConfiguration)

-- | Identifier type of the particular resource identifier for event
-- configuration.
getResourceEventConfiguration_identifierType :: Lens.Lens' GetResourceEventConfiguration IdentifierType
getResourceEventConfiguration_identifierType = Lens.lens (\GetResourceEventConfiguration' {identifierType} -> identifierType) (\s@GetResourceEventConfiguration' {} a -> s {identifierType = a} :: GetResourceEventConfiguration)

instance
  Core.AWSRequest
    GetResourceEventConfiguration
  where
  type
    AWSResponse GetResourceEventConfiguration =
      GetResourceEventConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceEventConfigurationResponse'
            Prelude.<$> (x Data..?> "ConnectionStatus")
            Prelude.<*> (x Data..?> "DeviceRegistrationState")
            Prelude.<*> (x Data..?> "Join")
            Prelude.<*> (x Data..?> "MessageDeliveryStatus")
            Prelude.<*> (x Data..?> "Proximity")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetResourceEventConfiguration
  where
  hashWithSalt _salt GetResourceEventConfiguration' {..} =
    _salt `Prelude.hashWithSalt` partnerType
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` identifierType

instance Prelude.NFData GetResourceEventConfiguration where
  rnf GetResourceEventConfiguration' {..} =
    Prelude.rnf partnerType
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf identifierType

instance Data.ToHeaders GetResourceEventConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetResourceEventConfiguration where
  toPath GetResourceEventConfiguration' {..} =
    Prelude.mconcat
      ["/event-configurations/", Data.toBS identifier]

instance Data.ToQuery GetResourceEventConfiguration where
  toQuery GetResourceEventConfiguration' {..} =
    Prelude.mconcat
      [ "partnerType" Data.=: partnerType,
        "identifierType" Data.=: identifierType
      ]

-- | /See:/ 'newGetResourceEventConfigurationResponse' smart constructor.
data GetResourceEventConfigurationResponse = GetResourceEventConfigurationResponse'
  { -- | Event configuration for the connection status event.
    connectionStatus :: Prelude.Maybe ConnectionStatusEventConfiguration,
    -- | Event configuration for the device registration state event.
    deviceRegistrationState :: Prelude.Maybe DeviceRegistrationStateEventConfiguration,
    -- | Event configuration for the join event.
    join :: Prelude.Maybe JoinEventConfiguration,
    -- | Event configuration for the message delivery status event.
    messageDeliveryStatus :: Prelude.Maybe MessageDeliveryStatusEventConfiguration,
    -- | Event configuration for the proximity event.
    proximity :: Prelude.Maybe ProximityEventConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceEventConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionStatus', 'getResourceEventConfigurationResponse_connectionStatus' - Event configuration for the connection status event.
--
-- 'deviceRegistrationState', 'getResourceEventConfigurationResponse_deviceRegistrationState' - Event configuration for the device registration state event.
--
-- 'join', 'getResourceEventConfigurationResponse_join' - Event configuration for the join event.
--
-- 'messageDeliveryStatus', 'getResourceEventConfigurationResponse_messageDeliveryStatus' - Event configuration for the message delivery status event.
--
-- 'proximity', 'getResourceEventConfigurationResponse_proximity' - Event configuration for the proximity event.
--
-- 'httpStatus', 'getResourceEventConfigurationResponse_httpStatus' - The response's http status code.
newGetResourceEventConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceEventConfigurationResponse
newGetResourceEventConfigurationResponse pHttpStatus_ =
  GetResourceEventConfigurationResponse'
    { connectionStatus =
        Prelude.Nothing,
      deviceRegistrationState =
        Prelude.Nothing,
      join = Prelude.Nothing,
      messageDeliveryStatus =
        Prelude.Nothing,
      proximity = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Event configuration for the connection status event.
getResourceEventConfigurationResponse_connectionStatus :: Lens.Lens' GetResourceEventConfigurationResponse (Prelude.Maybe ConnectionStatusEventConfiguration)
getResourceEventConfigurationResponse_connectionStatus = Lens.lens (\GetResourceEventConfigurationResponse' {connectionStatus} -> connectionStatus) (\s@GetResourceEventConfigurationResponse' {} a -> s {connectionStatus = a} :: GetResourceEventConfigurationResponse)

-- | Event configuration for the device registration state event.
getResourceEventConfigurationResponse_deviceRegistrationState :: Lens.Lens' GetResourceEventConfigurationResponse (Prelude.Maybe DeviceRegistrationStateEventConfiguration)
getResourceEventConfigurationResponse_deviceRegistrationState = Lens.lens (\GetResourceEventConfigurationResponse' {deviceRegistrationState} -> deviceRegistrationState) (\s@GetResourceEventConfigurationResponse' {} a -> s {deviceRegistrationState = a} :: GetResourceEventConfigurationResponse)

-- | Event configuration for the join event.
getResourceEventConfigurationResponse_join :: Lens.Lens' GetResourceEventConfigurationResponse (Prelude.Maybe JoinEventConfiguration)
getResourceEventConfigurationResponse_join = Lens.lens (\GetResourceEventConfigurationResponse' {join} -> join) (\s@GetResourceEventConfigurationResponse' {} a -> s {join = a} :: GetResourceEventConfigurationResponse)

-- | Event configuration for the message delivery status event.
getResourceEventConfigurationResponse_messageDeliveryStatus :: Lens.Lens' GetResourceEventConfigurationResponse (Prelude.Maybe MessageDeliveryStatusEventConfiguration)
getResourceEventConfigurationResponse_messageDeliveryStatus = Lens.lens (\GetResourceEventConfigurationResponse' {messageDeliveryStatus} -> messageDeliveryStatus) (\s@GetResourceEventConfigurationResponse' {} a -> s {messageDeliveryStatus = a} :: GetResourceEventConfigurationResponse)

-- | Event configuration for the proximity event.
getResourceEventConfigurationResponse_proximity :: Lens.Lens' GetResourceEventConfigurationResponse (Prelude.Maybe ProximityEventConfiguration)
getResourceEventConfigurationResponse_proximity = Lens.lens (\GetResourceEventConfigurationResponse' {proximity} -> proximity) (\s@GetResourceEventConfigurationResponse' {} a -> s {proximity = a} :: GetResourceEventConfigurationResponse)

-- | The response's http status code.
getResourceEventConfigurationResponse_httpStatus :: Lens.Lens' GetResourceEventConfigurationResponse Prelude.Int
getResourceEventConfigurationResponse_httpStatus = Lens.lens (\GetResourceEventConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetResourceEventConfigurationResponse' {} a -> s {httpStatus = a} :: GetResourceEventConfigurationResponse)

instance
  Prelude.NFData
    GetResourceEventConfigurationResponse
  where
  rnf GetResourceEventConfigurationResponse' {..} =
    Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf deviceRegistrationState
      `Prelude.seq` Prelude.rnf join
      `Prelude.seq` Prelude.rnf messageDeliveryStatus
      `Prelude.seq` Prelude.rnf proximity
      `Prelude.seq` Prelude.rnf httpStatus
