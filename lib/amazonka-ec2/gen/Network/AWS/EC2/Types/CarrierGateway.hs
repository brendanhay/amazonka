-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CarrierGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CarrierGateway
  ( CarrierGateway (..),

    -- * Smart constructor
    mkCarrierGateway,

    -- * Lenses
    cgState,
    cgVPCId,
    cgOwnerId,
    cgTags,
    cgCarrierGatewayId,
  )
where

import Network.AWS.EC2.Types.CarrierGatewayState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a carrier gateway.
--
-- /See:/ 'mkCarrierGateway' smart constructor.
data CarrierGateway = CarrierGateway'
  { state ::
      Lude.Maybe CarrierGatewayState,
    vpcId :: Lude.Maybe Lude.Text,
    ownerId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    carrierGatewayId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CarrierGateway' with the minimum fields required to make a request.
--
-- * 'carrierGatewayId' - The ID of the carrier gateway.
-- * 'ownerId' - The AWS account ID of the owner of the carrier gateway.
-- * 'state' - The state of the carrier gateway.
-- * 'tags' - The tags assigned to the carrier gateway.
-- * 'vpcId' - The ID of the VPC associated with the carrier gateway.
mkCarrierGateway ::
  CarrierGateway
mkCarrierGateway =
  CarrierGateway'
    { state = Lude.Nothing,
      vpcId = Lude.Nothing,
      ownerId = Lude.Nothing,
      tags = Lude.Nothing,
      carrierGatewayId = Lude.Nothing
    }

-- | The state of the carrier gateway.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgState :: Lens.Lens' CarrierGateway (Lude.Maybe CarrierGatewayState)
cgState = Lens.lens (state :: CarrierGateway -> Lude.Maybe CarrierGatewayState) (\s a -> s {state = a} :: CarrierGateway)
{-# DEPRECATED cgState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the VPC associated with the carrier gateway.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgVPCId :: Lens.Lens' CarrierGateway (Lude.Maybe Lude.Text)
cgVPCId = Lens.lens (vpcId :: CarrierGateway -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: CarrierGateway)
{-# DEPRECATED cgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The AWS account ID of the owner of the carrier gateway.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgOwnerId :: Lens.Lens' CarrierGateway (Lude.Maybe Lude.Text)
cgOwnerId = Lens.lens (ownerId :: CarrierGateway -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: CarrierGateway)
{-# DEPRECATED cgOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The tags assigned to the carrier gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgTags :: Lens.Lens' CarrierGateway (Lude.Maybe [Tag])
cgTags = Lens.lens (tags :: CarrierGateway -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CarrierGateway)
{-# DEPRECATED cgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the carrier gateway.
--
-- /Note:/ Consider using 'carrierGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgCarrierGatewayId :: Lens.Lens' CarrierGateway (Lude.Maybe Lude.Text)
cgCarrierGatewayId = Lens.lens (carrierGatewayId :: CarrierGateway -> Lude.Maybe Lude.Text) (\s a -> s {carrierGatewayId = a} :: CarrierGateway)
{-# DEPRECATED cgCarrierGatewayId "Use generic-lens or generic-optics with 'carrierGatewayId' instead." #-}

instance Lude.FromXML CarrierGateway where
  parseXML x =
    CarrierGateway'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "carrierGatewayId")
