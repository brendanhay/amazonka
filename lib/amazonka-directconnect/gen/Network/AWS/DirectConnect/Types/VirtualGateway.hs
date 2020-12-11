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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a virtual private gateway for a private virtual interface.
--
-- /See:/ 'mkVirtualGateway' smart constructor.
data VirtualGateway = VirtualGateway'
  { virtualGatewayId ::
      Lude.Maybe Lude.Text,
    virtualGatewayState :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VirtualGateway' with the minimum fields required to make a request.
--
-- * 'virtualGatewayId' - The ID of the virtual private gateway.
-- * 'virtualGatewayState' - The state of the virtual private gateway. The following are the possible values:
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
mkVirtualGateway ::
  VirtualGateway
mkVirtualGateway =
  VirtualGateway'
    { virtualGatewayId = Lude.Nothing,
      virtualGatewayState = Lude.Nothing
    }

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vgVirtualGatewayId :: Lens.Lens' VirtualGateway (Lude.Maybe Lude.Text)
vgVirtualGatewayId = Lens.lens (virtualGatewayId :: VirtualGateway -> Lude.Maybe Lude.Text) (\s a -> s {virtualGatewayId = a} :: VirtualGateway)
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
vgVirtualGatewayState :: Lens.Lens' VirtualGateway (Lude.Maybe Lude.Text)
vgVirtualGatewayState = Lens.lens (virtualGatewayState :: VirtualGateway -> Lude.Maybe Lude.Text) (\s a -> s {virtualGatewayState = a} :: VirtualGateway)
{-# DEPRECATED vgVirtualGatewayState "Use generic-lens or generic-optics with 'virtualGatewayState' instead." #-}

instance Lude.FromJSON VirtualGateway where
  parseJSON =
    Lude.withObject
      "VirtualGateway"
      ( \x ->
          VirtualGateway'
            Lude.<$> (x Lude..:? "virtualGatewayId")
            Lude.<*> (x Lude..:? "virtualGatewayState")
      )
