-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPeeringAttachment
  ( TransitGatewayPeeringAttachment (..),

    -- * Smart constructor
    mkTransitGatewayPeeringAttachment,

    -- * Lenses
    tgpaCreationTime,
    tgpaRequesterTgwInfo,
    tgpaStatus,
    tgpaState,
    tgpaAccepterTgwInfo,
    tgpaTransitGatewayAttachmentId,
    tgpaTags,
  )
where

import Network.AWS.EC2.Types.PeeringAttachmentStatus
import Network.AWS.EC2.Types.PeeringTgwInfo
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the transit gateway peering attachment.
--
-- /See:/ 'mkTransitGatewayPeeringAttachment' smart constructor.
data TransitGatewayPeeringAttachment = TransitGatewayPeeringAttachment'
  { creationTime ::
      Lude.Maybe Lude.ISO8601,
    requesterTgwInfo ::
      Lude.Maybe PeeringTgwInfo,
    status ::
      Lude.Maybe
        PeeringAttachmentStatus,
    state ::
      Lude.Maybe
        TransitGatewayAttachmentState,
    accepterTgwInfo ::
      Lude.Maybe PeeringTgwInfo,
    transitGatewayAttachmentId ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayPeeringAttachment' with the minimum fields required to make a request.
--
-- * 'accepterTgwInfo' - Information about the accepter transit gateway.
-- * 'creationTime' - The time the transit gateway peering attachment was created.
-- * 'requesterTgwInfo' - Information about the requester transit gateway.
-- * 'state' - The state of the transit gateway peering attachment. Note that the @initiating@ state has been deprecated.
-- * 'status' - The status of the transit gateway peering attachment.
-- * 'tags' - The tags for the transit gateway peering attachment.
-- * 'transitGatewayAttachmentId' - The ID of the transit gateway peering attachment.
mkTransitGatewayPeeringAttachment ::
  TransitGatewayPeeringAttachment
mkTransitGatewayPeeringAttachment =
  TransitGatewayPeeringAttachment'
    { creationTime = Lude.Nothing,
      requesterTgwInfo = Lude.Nothing,
      status = Lude.Nothing,
      state = Lude.Nothing,
      accepterTgwInfo = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The time the transit gateway peering attachment was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaCreationTime :: Lens.Lens' TransitGatewayPeeringAttachment (Lude.Maybe Lude.ISO8601)
tgpaCreationTime = Lens.lens (creationTime :: TransitGatewayPeeringAttachment -> Lude.Maybe Lude.ISO8601) (\s a -> s {creationTime = a} :: TransitGatewayPeeringAttachment)
{-# DEPRECATED tgpaCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Information about the requester transit gateway.
--
-- /Note:/ Consider using 'requesterTgwInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaRequesterTgwInfo :: Lens.Lens' TransitGatewayPeeringAttachment (Lude.Maybe PeeringTgwInfo)
tgpaRequesterTgwInfo = Lens.lens (requesterTgwInfo :: TransitGatewayPeeringAttachment -> Lude.Maybe PeeringTgwInfo) (\s a -> s {requesterTgwInfo = a} :: TransitGatewayPeeringAttachment)
{-# DEPRECATED tgpaRequesterTgwInfo "Use generic-lens or generic-optics with 'requesterTgwInfo' instead." #-}

-- | The status of the transit gateway peering attachment.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaStatus :: Lens.Lens' TransitGatewayPeeringAttachment (Lude.Maybe PeeringAttachmentStatus)
tgpaStatus = Lens.lens (status :: TransitGatewayPeeringAttachment -> Lude.Maybe PeeringAttachmentStatus) (\s a -> s {status = a} :: TransitGatewayPeeringAttachment)
{-# DEPRECATED tgpaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The state of the transit gateway peering attachment. Note that the @initiating@ state has been deprecated.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaState :: Lens.Lens' TransitGatewayPeeringAttachment (Lude.Maybe TransitGatewayAttachmentState)
tgpaState = Lens.lens (state :: TransitGatewayPeeringAttachment -> Lude.Maybe TransitGatewayAttachmentState) (\s a -> s {state = a} :: TransitGatewayPeeringAttachment)
{-# DEPRECATED tgpaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Information about the accepter transit gateway.
--
-- /Note:/ Consider using 'accepterTgwInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaAccepterTgwInfo :: Lens.Lens' TransitGatewayPeeringAttachment (Lude.Maybe PeeringTgwInfo)
tgpaAccepterTgwInfo = Lens.lens (accepterTgwInfo :: TransitGatewayPeeringAttachment -> Lude.Maybe PeeringTgwInfo) (\s a -> s {accepterTgwInfo = a} :: TransitGatewayPeeringAttachment)
{-# DEPRECATED tgpaAccepterTgwInfo "Use generic-lens or generic-optics with 'accepterTgwInfo' instead." #-}

-- | The ID of the transit gateway peering attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayPeeringAttachment (Lude.Maybe Lude.Text)
tgpaTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: TransitGatewayPeeringAttachment -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: TransitGatewayPeeringAttachment)
{-# DEPRECATED tgpaTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

-- | The tags for the transit gateway peering attachment.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpaTags :: Lens.Lens' TransitGatewayPeeringAttachment (Lude.Maybe [Tag])
tgpaTags = Lens.lens (tags :: TransitGatewayPeeringAttachment -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TransitGatewayPeeringAttachment)
{-# DEPRECATED tgpaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML TransitGatewayPeeringAttachment where
  parseXML x =
    TransitGatewayPeeringAttachment'
      Lude.<$> (x Lude..@? "creationTime")
      Lude.<*> (x Lude..@? "requesterTgwInfo")
      Lude.<*> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "accepterTgwInfo")
      Lude.<*> (x Lude..@? "transitGatewayAttachmentId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
