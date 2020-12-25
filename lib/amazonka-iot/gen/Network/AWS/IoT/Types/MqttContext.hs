{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MqttContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MqttContext
  ( MqttContext (..),

    -- * Smart constructor
    mkMqttContext,

    -- * Lenses
    mcClientId,
    mcPassword,
    mcUsername,
  )
where

import qualified Network.AWS.IoT.Types.MqttClientId as Types
import qualified Network.AWS.IoT.Types.MqttUsername as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the MQTT context to use for the test authorizer request
--
-- /See:/ 'mkMqttContext' smart constructor.
data MqttContext = MqttContext'
  { -- | The value of the @clientId@ key in an MQTT authorization request.
    clientId :: Core.Maybe Types.MqttClientId,
    -- | The value of the @password@ key in an MQTT authorization request.
    password :: Core.Maybe Core.Base64,
    -- | The value of the @username@ key in an MQTT authorization request.
    username :: Core.Maybe Types.MqttUsername
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MqttContext' value with any optional fields omitted.
mkMqttContext ::
  MqttContext
mkMqttContext =
  MqttContext'
    { clientId = Core.Nothing,
      password = Core.Nothing,
      username = Core.Nothing
    }

-- | The value of the @clientId@ key in an MQTT authorization request.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClientId :: Lens.Lens' MqttContext (Core.Maybe Types.MqttClientId)
mcClientId = Lens.field @"clientId"
{-# DEPRECATED mcClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The value of the @password@ key in an MQTT authorization request.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcPassword :: Lens.Lens' MqttContext (Core.Maybe Core.Base64)
mcPassword = Lens.field @"password"
{-# DEPRECATED mcPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The value of the @username@ key in an MQTT authorization request.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcUsername :: Lens.Lens' MqttContext (Core.Maybe Types.MqttUsername)
mcUsername = Lens.field @"username"
{-# DEPRECATED mcUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Core.FromJSON MqttContext where
  toJSON MqttContext {..} =
    Core.object
      ( Core.catMaybes
          [ ("clientId" Core..=) Core.<$> clientId,
            ("password" Core..=) Core.<$> password,
            ("username" Core..=) Core.<$> username
          ]
      )
