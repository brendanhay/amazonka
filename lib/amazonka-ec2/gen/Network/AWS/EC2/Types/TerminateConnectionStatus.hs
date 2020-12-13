{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TerminateConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TerminateConnectionStatus
  ( TerminateConnectionStatus (..),

    -- * Smart constructor
    mkTerminateConnectionStatus,

    -- * Lenses
    tcsCurrentStatus,
    tcsConnectionId,
    tcsPreviousStatus,
  )
where

import Network.AWS.EC2.Types.ClientVPNConnectionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a terminated Client VPN endpoint client connection.
--
-- /See:/ 'mkTerminateConnectionStatus' smart constructor.
data TerminateConnectionStatus = TerminateConnectionStatus'
  { -- | A message about the status of the client connection, if applicable.
    currentStatus :: Lude.Maybe ClientVPNConnectionStatus,
    -- | The ID of the client connection.
    connectionId :: Lude.Maybe Lude.Text,
    -- | The state of the client connection.
    previousStatus :: Lude.Maybe ClientVPNConnectionStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateConnectionStatus' with the minimum fields required to make a request.
--
-- * 'currentStatus' - A message about the status of the client connection, if applicable.
-- * 'connectionId' - The ID of the client connection.
-- * 'previousStatus' - The state of the client connection.
mkTerminateConnectionStatus ::
  TerminateConnectionStatus
mkTerminateConnectionStatus =
  TerminateConnectionStatus'
    { currentStatus = Lude.Nothing,
      connectionId = Lude.Nothing,
      previousStatus = Lude.Nothing
    }

-- | A message about the status of the client connection, if applicable.
--
-- /Note:/ Consider using 'currentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsCurrentStatus :: Lens.Lens' TerminateConnectionStatus (Lude.Maybe ClientVPNConnectionStatus)
tcsCurrentStatus = Lens.lens (currentStatus :: TerminateConnectionStatus -> Lude.Maybe ClientVPNConnectionStatus) (\s a -> s {currentStatus = a} :: TerminateConnectionStatus)
{-# DEPRECATED tcsCurrentStatus "Use generic-lens or generic-optics with 'currentStatus' instead." #-}

-- | The ID of the client connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsConnectionId :: Lens.Lens' TerminateConnectionStatus (Lude.Maybe Lude.Text)
tcsConnectionId = Lens.lens (connectionId :: TerminateConnectionStatus -> Lude.Maybe Lude.Text) (\s a -> s {connectionId = a} :: TerminateConnectionStatus)
{-# DEPRECATED tcsConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The state of the client connection.
--
-- /Note:/ Consider using 'previousStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsPreviousStatus :: Lens.Lens' TerminateConnectionStatus (Lude.Maybe ClientVPNConnectionStatus)
tcsPreviousStatus = Lens.lens (previousStatus :: TerminateConnectionStatus -> Lude.Maybe ClientVPNConnectionStatus) (\s a -> s {previousStatus = a} :: TerminateConnectionStatus)
{-# DEPRECATED tcsPreviousStatus "Use generic-lens or generic-optics with 'previousStatus' instead." #-}

instance Lude.FromXML TerminateConnectionStatus where
  parseXML x =
    TerminateConnectionStatus'
      Lude.<$> (x Lude..@? "currentStatus")
      Lude.<*> (x Lude..@? "connectionId")
      Lude.<*> (x Lude..@? "previousStatus")
