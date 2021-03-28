{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVpnRouteStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVpnRouteStatus
  ( ClientVpnRouteStatus (..)
  -- * Smart constructor
  , mkClientVpnRouteStatus
  -- * Lenses
  , cvrsCode
  , cvrsMessage
  ) where

import qualified Network.AWS.EC2.Types.ClientVpnRouteStatusCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the state of a Client VPN endpoint route.
--
-- /See:/ 'mkClientVpnRouteStatus' smart constructor.
data ClientVpnRouteStatus = ClientVpnRouteStatus'
  { code :: Core.Maybe Types.ClientVpnRouteStatusCode
    -- ^ The state of the Client VPN endpoint route.
  , message :: Core.Maybe Core.Text
    -- ^ A message about the status of the Client VPN endpoint route, if applicable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientVpnRouteStatus' value with any optional fields omitted.
mkClientVpnRouteStatus
    :: ClientVpnRouteStatus
mkClientVpnRouteStatus
  = ClientVpnRouteStatus'{code = Core.Nothing,
                          message = Core.Nothing}

-- | The state of the Client VPN endpoint route.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsCode :: Lens.Lens' ClientVpnRouteStatus (Core.Maybe Types.ClientVpnRouteStatusCode)
cvrsCode = Lens.field @"code"
{-# INLINEABLE cvrsCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | A message about the status of the Client VPN endpoint route, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsMessage :: Lens.Lens' ClientVpnRouteStatus (Core.Maybe Core.Text)
cvrsMessage = Lens.field @"message"
{-# INLINEABLE cvrsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML ClientVpnRouteStatus where
        parseXML x
          = ClientVpnRouteStatus' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
