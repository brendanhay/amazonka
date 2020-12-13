{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusItem
  ( VolumeStatusItem (..),

    -- * Smart constructor
    mkVolumeStatusItem,

    -- * Lenses
    vsiVolumeStatus,
    vsiActions,
    vsiOutpostARN,
    vsiEvents,
    vsiAvailabilityZone,
    vsiVolumeId,
    vsiAttachmentStatuses,
  )
where

import Network.AWS.EC2.Types.VolumeStatusAction
import Network.AWS.EC2.Types.VolumeStatusAttachmentStatus
import Network.AWS.EC2.Types.VolumeStatusEvent
import Network.AWS.EC2.Types.VolumeStatusInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the volume status.
--
-- /See:/ 'mkVolumeStatusItem' smart constructor.
data VolumeStatusItem = VolumeStatusItem'
  { -- | The volume status.
    volumeStatus :: Lude.Maybe VolumeStatusInfo,
    -- | The details of the operation.
    actions :: Lude.Maybe [VolumeStatusAction],
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostARN :: Lude.Maybe Lude.Text,
    -- | A list of events associated with the volume.
    events :: Lude.Maybe [VolumeStatusEvent],
    -- | The Availability Zone of the volume.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The volume ID.
    volumeId :: Lude.Maybe Lude.Text,
    -- | Information about the instances to which the volume is attached.
    attachmentStatuses :: Lude.Maybe [VolumeStatusAttachmentStatus]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VolumeStatusItem' with the minimum fields required to make a request.
--
-- * 'volumeStatus' - The volume status.
-- * 'actions' - The details of the operation.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost.
-- * 'events' - A list of events associated with the volume.
-- * 'availabilityZone' - The Availability Zone of the volume.
-- * 'volumeId' - The volume ID.
-- * 'attachmentStatuses' - Information about the instances to which the volume is attached.
mkVolumeStatusItem ::
  VolumeStatusItem
mkVolumeStatusItem =
  VolumeStatusItem'
    { volumeStatus = Lude.Nothing,
      actions = Lude.Nothing,
      outpostARN = Lude.Nothing,
      events = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      volumeId = Lude.Nothing,
      attachmentStatuses = Lude.Nothing
    }

-- | The volume status.
--
-- /Note:/ Consider using 'volumeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiVolumeStatus :: Lens.Lens' VolumeStatusItem (Lude.Maybe VolumeStatusInfo)
vsiVolumeStatus = Lens.lens (volumeStatus :: VolumeStatusItem -> Lude.Maybe VolumeStatusInfo) (\s a -> s {volumeStatus = a} :: VolumeStatusItem)
{-# DEPRECATED vsiVolumeStatus "Use generic-lens or generic-optics with 'volumeStatus' instead." #-}

-- | The details of the operation.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiActions :: Lens.Lens' VolumeStatusItem (Lude.Maybe [VolumeStatusAction])
vsiActions = Lens.lens (actions :: VolumeStatusItem -> Lude.Maybe [VolumeStatusAction]) (\s a -> s {actions = a} :: VolumeStatusItem)
{-# DEPRECATED vsiActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiOutpostARN :: Lens.Lens' VolumeStatusItem (Lude.Maybe Lude.Text)
vsiOutpostARN = Lens.lens (outpostARN :: VolumeStatusItem -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: VolumeStatusItem)
{-# DEPRECATED vsiOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | A list of events associated with the volume.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiEvents :: Lens.Lens' VolumeStatusItem (Lude.Maybe [VolumeStatusEvent])
vsiEvents = Lens.lens (events :: VolumeStatusItem -> Lude.Maybe [VolumeStatusEvent]) (\s a -> s {events = a} :: VolumeStatusItem)
{-# DEPRECATED vsiEvents "Use generic-lens or generic-optics with 'events' instead." #-}

-- | The Availability Zone of the volume.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiAvailabilityZone :: Lens.Lens' VolumeStatusItem (Lude.Maybe Lude.Text)
vsiAvailabilityZone = Lens.lens (availabilityZone :: VolumeStatusItem -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: VolumeStatusItem)
{-# DEPRECATED vsiAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The volume ID.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiVolumeId :: Lens.Lens' VolumeStatusItem (Lude.Maybe Lude.Text)
vsiVolumeId = Lens.lens (volumeId :: VolumeStatusItem -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: VolumeStatusItem)
{-# DEPRECATED vsiVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | Information about the instances to which the volume is attached.
--
-- /Note:/ Consider using 'attachmentStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsiAttachmentStatuses :: Lens.Lens' VolumeStatusItem (Lude.Maybe [VolumeStatusAttachmentStatus])
vsiAttachmentStatuses = Lens.lens (attachmentStatuses :: VolumeStatusItem -> Lude.Maybe [VolumeStatusAttachmentStatus]) (\s a -> s {attachmentStatuses = a} :: VolumeStatusItem)
{-# DEPRECATED vsiAttachmentStatuses "Use generic-lens or generic-optics with 'attachmentStatuses' instead." #-}

instance Lude.FromXML VolumeStatusItem where
  parseXML x =
    VolumeStatusItem'
      Lude.<$> (x Lude..@? "volumeStatus")
      Lude.<*> ( x Lude..@? "actionsSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "outpostArn")
      Lude.<*> ( x Lude..@? "eventsSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "volumeId")
      Lude.<*> ( x Lude..@? "attachmentStatuses" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
