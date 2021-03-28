{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVpnConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVpnConnectionStatus
  ( ClientVpnConnectionStatus (..)
  -- * Smart constructor
  , mkClientVpnConnectionStatus
  -- * Lenses
  , cvcsCode
  , cvcsMessage
  ) where

import qualified Network.AWS.EC2.Types.ClientVpnConnectionStatusCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the status of a client connection.
--
-- /See:/ 'mkClientVpnConnectionStatus' smart constructor.
data ClientVpnConnectionStatus = ClientVpnConnectionStatus'
  { code :: Core.Maybe Types.ClientVpnConnectionStatusCode
    -- ^ The state of the client connection.
  , message :: Core.Maybe Core.Text
    -- ^ A message about the status of the client connection, if applicable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientVpnConnectionStatus' value with any optional fields omitted.
mkClientVpnConnectionStatus
    :: ClientVpnConnectionStatus
mkClientVpnConnectionStatus
  = ClientVpnConnectionStatus'{code = Core.Nothing,
                               message = Core.Nothing}

-- | The state of the client connection.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcsCode :: Lens.Lens' ClientVpnConnectionStatus (Core.Maybe Types.ClientVpnConnectionStatusCode)
cvcsCode = Lens.field @"code"
{-# INLINEABLE cvcsCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | A message about the status of the client connection, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcsMessage :: Lens.Lens' ClientVpnConnectionStatus (Core.Maybe Core.Text)
cvcsMessage = Lens.field @"message"
{-# INLINEABLE cvcsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML ClientVpnConnectionStatus where
        parseXML x
          = ClientVpnConnectionStatus' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
