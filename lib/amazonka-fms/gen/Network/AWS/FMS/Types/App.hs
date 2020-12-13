{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.App
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.App
  ( App (..),

    -- * Smart constructor
    mkApp,

    -- * Lenses
    aAppName,
    aProtocol,
    aPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An individual AWS Firewall Manager application.
--
-- /See:/ 'mkApp' smart constructor.
data App = App'
  { -- | The application's name.
    appName :: Lude.Text,
    -- | The IP protocol name or number. The name can be one of @tcp@ , @udp@ , or @icmp@ . For information on possible numbers, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> .
    protocol :: Lude.Text,
    -- | The application's port number, for example @80@ .
    port :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'App' with the minimum fields required to make a request.
--
-- * 'appName' - The application's name.
-- * 'protocol' - The IP protocol name or number. The name can be one of @tcp@ , @udp@ , or @icmp@ . For information on possible numbers, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> .
-- * 'port' - The application's port number, for example @80@ .
mkApp ::
  -- | 'appName'
  Lude.Text ->
  -- | 'protocol'
  Lude.Text ->
  -- | 'port'
  Lude.Natural ->
  App
mkApp pAppName_ pProtocol_ pPort_ =
  App' {appName = pAppName_, protocol = pProtocol_, port = pPort_}

-- | The application's name.
--
-- /Note:/ Consider using 'appName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAppName :: Lens.Lens' App Lude.Text
aAppName = Lens.lens (appName :: App -> Lude.Text) (\s a -> s {appName = a} :: App)
{-# DEPRECATED aAppName "Use generic-lens or generic-optics with 'appName' instead." #-}

-- | The IP protocol name or number. The name can be one of @tcp@ , @udp@ , or @icmp@ . For information on possible numbers, see <https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers> .
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aProtocol :: Lens.Lens' App Lude.Text
aProtocol = Lens.lens (protocol :: App -> Lude.Text) (\s a -> s {protocol = a} :: App)
{-# DEPRECATED aProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The application's port number, for example @80@ .
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPort :: Lens.Lens' App Lude.Natural
aPort = Lens.lens (port :: App -> Lude.Natural) (\s a -> s {port = a} :: App)
{-# DEPRECATED aPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON App where
  parseJSON =
    Lude.withObject
      "App"
      ( \x ->
          App'
            Lude.<$> (x Lude..: "AppName")
            Lude.<*> (x Lude..: "Protocol")
            Lude.<*> (x Lude..: "Port")
      )

instance Lude.ToJSON App where
  toJSON App' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AppName" Lude..= appName),
            Lude.Just ("Protocol" Lude..= protocol),
            Lude.Just ("Port" Lude..= port)
          ]
      )
