{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TerminateConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TerminateConnectionStatus
  ( TerminateConnectionStatus (..)
  -- * Smart constructor
  , mkTerminateConnectionStatus
  -- * Lenses
  , tcsConnectionId
  , tcsCurrentStatus
  , tcsPreviousStatus
  ) where

import qualified Network.AWS.EC2.Types.ClientVpnConnectionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a terminated Client VPN endpoint client connection.
--
-- /See:/ 'mkTerminateConnectionStatus' smart constructor.
data TerminateConnectionStatus = TerminateConnectionStatus'
  { connectionId :: Core.Maybe Core.Text
    -- ^ The ID of the client connection.
  , currentStatus :: Core.Maybe Types.ClientVpnConnectionStatus
    -- ^ A message about the status of the client connection, if applicable.
  , previousStatus :: Core.Maybe Types.ClientVpnConnectionStatus
    -- ^ The state of the client connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateConnectionStatus' value with any optional fields omitted.
mkTerminateConnectionStatus
    :: TerminateConnectionStatus
mkTerminateConnectionStatus
  = TerminateConnectionStatus'{connectionId = Core.Nothing,
                               currentStatus = Core.Nothing, previousStatus = Core.Nothing}

-- | The ID of the client connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsConnectionId :: Lens.Lens' TerminateConnectionStatus (Core.Maybe Core.Text)
tcsConnectionId = Lens.field @"connectionId"
{-# INLINEABLE tcsConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

-- | A message about the status of the client connection, if applicable.
--
-- /Note:/ Consider using 'currentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsCurrentStatus :: Lens.Lens' TerminateConnectionStatus (Core.Maybe Types.ClientVpnConnectionStatus)
tcsCurrentStatus = Lens.field @"currentStatus"
{-# INLINEABLE tcsCurrentStatus #-}
{-# DEPRECATED currentStatus "Use generic-lens or generic-optics with 'currentStatus' instead"  #-}

-- | The state of the client connection.
--
-- /Note:/ Consider using 'previousStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsPreviousStatus :: Lens.Lens' TerminateConnectionStatus (Core.Maybe Types.ClientVpnConnectionStatus)
tcsPreviousStatus = Lens.field @"previousStatus"
{-# INLINEABLE tcsPreviousStatus #-}
{-# DEPRECATED previousStatus "Use generic-lens or generic-optics with 'previousStatus' instead"  #-}

instance Core.FromXML TerminateConnectionStatus where
        parseXML x
          = TerminateConnectionStatus' Core.<$>
              (x Core..@? "connectionId") Core.<*> x Core..@? "currentStatus"
                Core.<*> x Core..@? "previousStatus"
