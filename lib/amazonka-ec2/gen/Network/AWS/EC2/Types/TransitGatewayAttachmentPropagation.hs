{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayAttachmentPropagation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayAttachmentPropagation
  ( TransitGatewayAttachmentPropagation (..),

    -- * Smart constructor
    mkTransitGatewayAttachmentPropagation,

    -- * Lenses
    tgapState,
    tgapTransitGatewayRouteTableId,
  )
where

import Network.AWS.EC2.Types.TransitGatewayPropagationState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a propagation route table.
--
-- /See:/ 'mkTransitGatewayAttachmentPropagation' smart constructor.
data TransitGatewayAttachmentPropagation = TransitGatewayAttachmentPropagation'
  { state ::
      Lude.Maybe
        TransitGatewayPropagationState,
    transitGatewayRouteTableId ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayAttachmentPropagation' with the minimum fields required to make a request.
--
-- * 'state' - The state of the propagation route table.
-- * 'transitGatewayRouteTableId' - The ID of the propagation route table.
mkTransitGatewayAttachmentPropagation ::
  TransitGatewayAttachmentPropagation
mkTransitGatewayAttachmentPropagation =
  TransitGatewayAttachmentPropagation'
    { state = Lude.Nothing,
      transitGatewayRouteTableId = Lude.Nothing
    }

-- | The state of the propagation route table.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgapState :: Lens.Lens' TransitGatewayAttachmentPropagation (Lude.Maybe TransitGatewayPropagationState)
tgapState = Lens.lens (state :: TransitGatewayAttachmentPropagation -> Lude.Maybe TransitGatewayPropagationState) (\s a -> s {state = a} :: TransitGatewayAttachmentPropagation)
{-# DEPRECATED tgapState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the propagation route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgapTransitGatewayRouteTableId :: Lens.Lens' TransitGatewayAttachmentPropagation (Lude.Maybe Lude.Text)
tgapTransitGatewayRouteTableId = Lens.lens (transitGatewayRouteTableId :: TransitGatewayAttachmentPropagation -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayRouteTableId = a} :: TransitGatewayAttachmentPropagation)
{-# DEPRECATED tgapTransitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead." #-}

instance Lude.FromXML TransitGatewayAttachmentPropagation where
  parseXML x =
    TransitGatewayAttachmentPropagation'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "transitGatewayRouteTableId")
