{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PropagatingVGW
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PropagatingVGW
  ( PropagatingVGW (..),

    -- * Smart constructor
    mkPropagatingVGW,

    -- * Lenses
    pvGatewayId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a virtual private gateway propagating route.
--
-- /See:/ 'mkPropagatingVGW' smart constructor.
newtype PropagatingVGW = PropagatingVGW'
  { -- | The ID of the virtual private gateway.
    gatewayId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PropagatingVGW' with the minimum fields required to make a request.
--
-- * 'gatewayId' - The ID of the virtual private gateway.
mkPropagatingVGW ::
  PropagatingVGW
mkPropagatingVGW = PropagatingVGW' {gatewayId = Lude.Nothing}

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvGatewayId :: Lens.Lens' PropagatingVGW (Lude.Maybe Lude.Text)
pvGatewayId = Lens.lens (gatewayId :: PropagatingVGW -> Lude.Maybe Lude.Text) (\s a -> s {gatewayId = a} :: PropagatingVGW)
{-# DEPRECATED pvGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

instance Lude.FromXML PropagatingVGW where
  parseXML x = PropagatingVGW' Lude.<$> (x Lude..@? "gatewayId")
