{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VolumeStatusAction
import Network.AWS.EC2.Types.VolumeStatusAttachmentStatus
import Network.AWS.EC2.Types.VolumeStatusEvent
import Network.AWS.EC2.Types.VolumeStatusInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the volume status.
--
-- /See:/ 'newVolumeStatusItem' smart constructor.
data VolumeStatusItem = VolumeStatusItem'
  { -- | The volume status.
    volumeStatus :: Prelude.Maybe VolumeStatusInfo,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The volume ID.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | The details of the operation.
    actions :: Prelude.Maybe [VolumeStatusAction],
    -- | A list of events associated with the volume.
    events :: Prelude.Maybe [VolumeStatusEvent],
    -- | The Availability Zone of the volume.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Information about the instances to which the volume is attached.
    attachmentStatuses :: Prelude.Maybe [VolumeStatusAttachmentStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VolumeStatusItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeStatus', 'volumeStatusItem_volumeStatus' - The volume status.
--
-- 'outpostArn', 'volumeStatusItem_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'volumeId', 'volumeStatusItem_volumeId' - The volume ID.
--
-- 'actions', 'volumeStatusItem_actions' - The details of the operation.
--
-- 'events', 'volumeStatusItem_events' - A list of events associated with the volume.
--
-- 'availabilityZone', 'volumeStatusItem_availabilityZone' - The Availability Zone of the volume.
--
-- 'attachmentStatuses', 'volumeStatusItem_attachmentStatuses' - Information about the instances to which the volume is attached.
newVolumeStatusItem ::
  VolumeStatusItem
newVolumeStatusItem =
  VolumeStatusItem'
    { volumeStatus = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      actions = Prelude.Nothing,
      events = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      attachmentStatuses = Prelude.Nothing
    }

-- | The volume status.
volumeStatusItem_volumeStatus :: Lens.Lens' VolumeStatusItem (Prelude.Maybe VolumeStatusInfo)
volumeStatusItem_volumeStatus = Lens.lens (\VolumeStatusItem' {volumeStatus} -> volumeStatus) (\s@VolumeStatusItem' {} a -> s {volumeStatus = a} :: VolumeStatusItem)

-- | The Amazon Resource Name (ARN) of the Outpost.
volumeStatusItem_outpostArn :: Lens.Lens' VolumeStatusItem (Prelude.Maybe Prelude.Text)
volumeStatusItem_outpostArn = Lens.lens (\VolumeStatusItem' {outpostArn} -> outpostArn) (\s@VolumeStatusItem' {} a -> s {outpostArn = a} :: VolumeStatusItem)

-- | The volume ID.
volumeStatusItem_volumeId :: Lens.Lens' VolumeStatusItem (Prelude.Maybe Prelude.Text)
volumeStatusItem_volumeId = Lens.lens (\VolumeStatusItem' {volumeId} -> volumeId) (\s@VolumeStatusItem' {} a -> s {volumeId = a} :: VolumeStatusItem)

-- | The details of the operation.
volumeStatusItem_actions :: Lens.Lens' VolumeStatusItem (Prelude.Maybe [VolumeStatusAction])
volumeStatusItem_actions = Lens.lens (\VolumeStatusItem' {actions} -> actions) (\s@VolumeStatusItem' {} a -> s {actions = a} :: VolumeStatusItem) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of events associated with the volume.
volumeStatusItem_events :: Lens.Lens' VolumeStatusItem (Prelude.Maybe [VolumeStatusEvent])
volumeStatusItem_events = Lens.lens (\VolumeStatusItem' {events} -> events) (\s@VolumeStatusItem' {} a -> s {events = a} :: VolumeStatusItem) Prelude.. Lens.mapping Prelude._Coerce

-- | The Availability Zone of the volume.
volumeStatusItem_availabilityZone :: Lens.Lens' VolumeStatusItem (Prelude.Maybe Prelude.Text)
volumeStatusItem_availabilityZone = Lens.lens (\VolumeStatusItem' {availabilityZone} -> availabilityZone) (\s@VolumeStatusItem' {} a -> s {availabilityZone = a} :: VolumeStatusItem)

-- | Information about the instances to which the volume is attached.
volumeStatusItem_attachmentStatuses :: Lens.Lens' VolumeStatusItem (Prelude.Maybe [VolumeStatusAttachmentStatus])
volumeStatusItem_attachmentStatuses = Lens.lens (\VolumeStatusItem' {attachmentStatuses} -> attachmentStatuses) (\s@VolumeStatusItem' {} a -> s {attachmentStatuses = a} :: VolumeStatusItem) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML VolumeStatusItem where
  parseXML x =
    VolumeStatusItem'
      Prelude.<$> (x Prelude..@? "volumeStatus")
      Prelude.<*> (x Prelude..@? "outpostArn")
      Prelude.<*> (x Prelude..@? "volumeId")
      Prelude.<*> ( x Prelude..@? "actionsSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "eventsSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> ( x Prelude..@? "attachmentStatuses"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable VolumeStatusItem

instance Prelude.NFData VolumeStatusItem
