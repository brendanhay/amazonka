{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.RemotePortDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.RemotePortDetails
  ( RemotePortDetails (..),

    -- * Smart constructor
    mkRemotePortDetails,

    -- * Lenses
    rpdPortName,
    rpdPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the remote port.
--
-- /See:/ 'mkRemotePortDetails' smart constructor.
data RemotePortDetails = RemotePortDetails'
  { -- | The port name of the remote connection.
    portName :: Lude.Maybe Lude.Text,
    -- | The port number of the remote connection.
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemotePortDetails' with the minimum fields required to make a request.
--
-- * 'portName' - The port name of the remote connection.
-- * 'port' - The port number of the remote connection.
mkRemotePortDetails ::
  RemotePortDetails
mkRemotePortDetails =
  RemotePortDetails' {portName = Lude.Nothing, port = Lude.Nothing}

-- | The port name of the remote connection.
--
-- /Note:/ Consider using 'portName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpdPortName :: Lens.Lens' RemotePortDetails (Lude.Maybe Lude.Text)
rpdPortName = Lens.lens (portName :: RemotePortDetails -> Lude.Maybe Lude.Text) (\s a -> s {portName = a} :: RemotePortDetails)
{-# DEPRECATED rpdPortName "Use generic-lens or generic-optics with 'portName' instead." #-}

-- | The port number of the remote connection.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpdPort :: Lens.Lens' RemotePortDetails (Lude.Maybe Lude.Int)
rpdPort = Lens.lens (port :: RemotePortDetails -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: RemotePortDetails)
{-# DEPRECATED rpdPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON RemotePortDetails where
  parseJSON =
    Lude.withObject
      "RemotePortDetails"
      ( \x ->
          RemotePortDetails'
            Lude.<$> (x Lude..:? "portName") Lude.<*> (x Lude..:? "port")
      )
