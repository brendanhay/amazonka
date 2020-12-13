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
    mcUsername,
    mcPassword,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the MQTT context to use for the test authorizer request
--
-- /See:/ 'mkMqttContext' smart constructor.
data MqttContext = MqttContext'
  { -- | The value of the @clientId@ key in an MQTT authorization request.
    clientId :: Lude.Maybe Lude.Text,
    -- | The value of the @username@ key in an MQTT authorization request.
    username :: Lude.Maybe Lude.Text,
    -- | The value of the @password@ key in an MQTT authorization request.
    password :: Lude.Maybe Lude.Base64
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MqttContext' with the minimum fields required to make a request.
--
-- * 'clientId' - The value of the @clientId@ key in an MQTT authorization request.
-- * 'username' - The value of the @username@ key in an MQTT authorization request.
-- * 'password' - The value of the @password@ key in an MQTT authorization request.
mkMqttContext ::
  MqttContext
mkMqttContext =
  MqttContext'
    { clientId = Lude.Nothing,
      username = Lude.Nothing,
      password = Lude.Nothing
    }

-- | The value of the @clientId@ key in an MQTT authorization request.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClientId :: Lens.Lens' MqttContext (Lude.Maybe Lude.Text)
mcClientId = Lens.lens (clientId :: MqttContext -> Lude.Maybe Lude.Text) (\s a -> s {clientId = a} :: MqttContext)
{-# DEPRECATED mcClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The value of the @username@ key in an MQTT authorization request.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcUsername :: Lens.Lens' MqttContext (Lude.Maybe Lude.Text)
mcUsername = Lens.lens (username :: MqttContext -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: MqttContext)
{-# DEPRECATED mcUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The value of the @password@ key in an MQTT authorization request.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcPassword :: Lens.Lens' MqttContext (Lude.Maybe Lude.Base64)
mcPassword = Lens.lens (password :: MqttContext -> Lude.Maybe Lude.Base64) (\s a -> s {password = a} :: MqttContext)
{-# DEPRECATED mcPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Lude.ToJSON MqttContext where
  toJSON MqttContext' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("clientId" Lude..=) Lude.<$> clientId,
            ("username" Lude..=) Lude.<$> username,
            ("password" Lude..=) Lude.<$> password
          ]
      )
