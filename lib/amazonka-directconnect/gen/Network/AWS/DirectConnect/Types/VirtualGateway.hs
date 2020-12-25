{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.VirtualGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.VirtualGateway
  ( VirtualGateway (..),

    -- * Smart constructor
    mkVirtualGateway,

    -- * Lenses
    vgVirtualGatewayId,
    vgVirtualGatewayState,
  )
where

import qualified Network.AWS.DirectConnect.Types.VirtualGatewayId as Types
import qualified Network.AWS.DirectConnect.Types.VirtualGatewayState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a virtual private gateway for a private virtual interface.
--
-- /See:/ 'mkVirtualGateway' smart constructor.
data VirtualGateway = VirtualGateway'
  { -- | The ID of the virtual private gateway.
    virtualGatewayId :: Core.Maybe Types.VirtualGatewayId,
    -- | The state of the virtual private gateway. The following are the possible values:
    --
    --
    --     * @pending@ : Initial state after creating the virtual private gateway.
    --
    --
    --     * @available@ : Ready for use by a private virtual interface.
    --
    --
    --     * @deleting@ : Initial state after deleting the virtual private gateway.
    --
    --
    --     * @deleted@ : The virtual private gateway is deleted. The private virtual interface is unable to send traffic over this gateway.
    virtualGatewayState :: Core.Maybe Types.VirtualGatewayState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VirtualGateway' value with any optional fields omitted.
mkVirtualGateway ::
  VirtualGateway
mkVirtualGateway =
  VirtualGateway'
    { virtualGatewayId = Core.Nothing,
      virtualGatewayState = Core.Nothing
    }

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgVirtualGatewayId :: Lens.Lens' VirtualGateway (Core.Maybe Types.VirtualGatewayId)
vgVirtualGatewayId = Lens.field @"virtualGatewayId"
{-# DEPRECATED vgVirtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead." #-}

-- | The state of the virtual private gateway. The following are the possible values:
--
--
--     * @pending@ : Initial state after creating the virtual private gateway.
--
--
--     * @available@ : Ready for use by a private virtual interface.
--
--
--     * @deleting@ : Initial state after deleting the virtual private gateway.
--
--
--     * @deleted@ : The virtual private gateway is deleted. The private virtual interface is unable to send traffic over this gateway.
--
--
--
-- /Note:/ Consider using 'virtualGatewayState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgVirtualGatewayState :: Lens.Lens' VirtualGateway (Core.Maybe Types.VirtualGatewayState)
vgVirtualGatewayState = Lens.field @"virtualGatewayState"
{-# DEPRECATED vgVirtualGatewayState "Use generic-lens or generic-optics with 'virtualGatewayState' instead." #-}

instance Core.FromJSON VirtualGateway where
  parseJSON =
    Core.withObject "VirtualGateway" Core.$
      \x ->
        VirtualGateway'
          Core.<$> (x Core..:? "virtualGatewayId")
          Core.<*> (x Core..:? "virtualGatewayState")
