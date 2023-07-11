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
-- Module      : Amazonka.IoT.Types.MqttContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.MqttContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the MQTT context to use for the test authorizer request
--
-- /See:/ 'newMqttContext' smart constructor.
data MqttContext = MqttContext'
  { -- | The value of the @clientId@ key in an MQTT authorization request.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The value of the @password@ key in an MQTT authorization request.
    password :: Prelude.Maybe Data.Base64,
    -- | The value of the @username@ key in an MQTT authorization request.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { clientId = Prelude.Nothing,
      password = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The value of the @clientId@ key in an MQTT authorization request.
mqttContext_clientId :: Lens.Lens' MqttContext (Prelude.Maybe Prelude.Text)
mqttContext_clientId = Lens.lens (\MqttContext' {clientId} -> clientId) (\s@MqttContext' {} a -> s {clientId = a} :: MqttContext)

-- | The value of the @password@ key in an MQTT authorization request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
mqttContext_password :: Lens.Lens' MqttContext (Prelude.Maybe Prelude.ByteString)
mqttContext_password = Lens.lens (\MqttContext' {password} -> password) (\s@MqttContext' {} a -> s {password = a} :: MqttContext) Prelude.. Lens.mapping Data._Base64

-- | The value of the @username@ key in an MQTT authorization request.
mqttContext_username :: Lens.Lens' MqttContext (Prelude.Maybe Prelude.Text)
mqttContext_username = Lens.lens (\MqttContext' {username} -> username) (\s@MqttContext' {} a -> s {username = a} :: MqttContext)

instance Prelude.Hashable MqttContext where
  hashWithSalt _salt MqttContext' {..} =
    _salt
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` username

instance Prelude.NFData MqttContext where
  rnf MqttContext' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf username

instance Data.ToJSON MqttContext where
  toJSON MqttContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientId" Data..=) Prelude.<$> clientId,
            ("password" Data..=) Prelude.<$> password,
            ("username" Data..=) Prelude.<$> username
          ]
      )
