{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MqttContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MqttContext where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the MQTT context to use for the test authorizer request
--
-- /See:/ 'newMqttContext' smart constructor.
data MqttContext = MqttContext'
  { -- | The value of the @clientId@ key in an MQTT authorization request.
    clientId :: Core.Maybe Core.Text,
    -- | The value of the @password@ key in an MQTT authorization request.
    password :: Core.Maybe Core.Base64,
    -- | The value of the @username@ key in an MQTT authorization request.
    username :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MqttContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'mqttContext_clientId' - The value of the @clientId@ key in an MQTT authorization request.
--
-- 'password', 'mqttContext_password' - The value of the @password@ key in an MQTT authorization request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'username', 'mqttContext_username' - The value of the @username@ key in an MQTT authorization request.
newMqttContext ::
  MqttContext
newMqttContext =
  MqttContext'
    { clientId = Core.Nothing,
      password = Core.Nothing,
      username = Core.Nothing
    }

-- | The value of the @clientId@ key in an MQTT authorization request.
mqttContext_clientId :: Lens.Lens' MqttContext (Core.Maybe Core.Text)
mqttContext_clientId = Lens.lens (\MqttContext' {clientId} -> clientId) (\s@MqttContext' {} a -> s {clientId = a} :: MqttContext)

-- | The value of the @password@ key in an MQTT authorization request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
mqttContext_password :: Lens.Lens' MqttContext (Core.Maybe Core.ByteString)
mqttContext_password = Lens.lens (\MqttContext' {password} -> password) (\s@MqttContext' {} a -> s {password = a} :: MqttContext) Core.. Lens.mapping Core._Base64

-- | The value of the @username@ key in an MQTT authorization request.
mqttContext_username :: Lens.Lens' MqttContext (Core.Maybe Core.Text)
mqttContext_username = Lens.lens (\MqttContext' {username} -> username) (\s@MqttContext' {} a -> s {username = a} :: MqttContext)

instance Core.Hashable MqttContext

instance Core.NFData MqttContext

instance Core.ToJSON MqttContext where
  toJSON MqttContext' {..} =
    Core.object
      ( Core.catMaybes
          [ ("clientId" Core..=) Core.<$> clientId,
            ("password" Core..=) Core.<$> password,
            ("username" Core..=) Core.<$> username
          ]
      )
