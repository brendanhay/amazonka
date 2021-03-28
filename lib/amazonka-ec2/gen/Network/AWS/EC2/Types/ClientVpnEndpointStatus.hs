{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVpnEndpointStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVpnEndpointStatus
  ( ClientVpnEndpointStatus (..)
  -- * Smart constructor
  , mkClientVpnEndpointStatus
  -- * Lenses
  , cvesCode
  , cvesMessage
  ) where

import qualified Network.AWS.EC2.Types.ClientVpnEndpointStatusCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the state of a Client VPN endpoint.
--
-- /See:/ 'mkClientVpnEndpointStatus' smart constructor.
data ClientVpnEndpointStatus = ClientVpnEndpointStatus'
  { code :: Core.Maybe Types.ClientVpnEndpointStatusCode
    -- ^ The state of the Client VPN endpoint. Possible states include:
--
--
--     * @pending-associate@ - The Client VPN endpoint has been created but no target networks have been associated. The Client VPN endpoint cannot accept connections.
--
--
--     * @available@ - The Client VPN endpoint has been created and a target network has been associated. The Client VPN endpoint can accept connections.
--
--
--     * @deleting@ - The Client VPN endpoint is being deleted. The Client VPN endpoint cannot accept connections.
--
--
--     * @deleted@ - The Client VPN endpoint has been deleted. The Client VPN endpoint cannot accept connections.
--
--
  , message :: Core.Maybe Core.Text
    -- ^ A message about the status of the Client VPN endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientVpnEndpointStatus' value with any optional fields omitted.
mkClientVpnEndpointStatus
    :: ClientVpnEndpointStatus
mkClientVpnEndpointStatus
  = ClientVpnEndpointStatus'{code = Core.Nothing,
                             message = Core.Nothing}

-- | The state of the Client VPN endpoint. Possible states include:
--
--
--     * @pending-associate@ - The Client VPN endpoint has been created but no target networks have been associated. The Client VPN endpoint cannot accept connections.
--
--
--     * @available@ - The Client VPN endpoint has been created and a target network has been associated. The Client VPN endpoint can accept connections.
--
--
--     * @deleting@ - The Client VPN endpoint is being deleted. The Client VPN endpoint cannot accept connections.
--
--
--     * @deleted@ - The Client VPN endpoint has been deleted. The Client VPN endpoint cannot accept connections.
--
--
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvesCode :: Lens.Lens' ClientVpnEndpointStatus (Core.Maybe Types.ClientVpnEndpointStatusCode)
cvesCode = Lens.field @"code"
{-# INLINEABLE cvesCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | A message about the status of the Client VPN endpoint.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvesMessage :: Lens.Lens' ClientVpnEndpointStatus (Core.Maybe Core.Text)
cvesMessage = Lens.field @"message"
{-# INLINEABLE cvesMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML ClientVpnEndpointStatus where
        parseXML x
          = ClientVpnEndpointStatus' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
