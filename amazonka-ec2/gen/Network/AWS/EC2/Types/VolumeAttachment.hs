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
-- Module      : Network.AWS.EC2.Types.VolumeAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeAttachment where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VolumeAttachmentState
import qualified Network.AWS.Lens as Lens

-- | Describes volume attachment details.
--
-- /See:/ 'newVolumeAttachment' smart constructor.
data VolumeAttachment = VolumeAttachment'
  { -- | The ID of the instance.
    instanceId :: Core.Maybe Core.Text,
    -- | The time stamp when the attachment initiated.
    attachTime :: Core.Maybe Core.ISO8601,
    -- | The device name.
    device :: Core.Maybe Core.Text,
    -- | The ID of the volume.
    volumeId :: Core.Maybe Core.Text,
    -- | The attachment state of the volume.
    state :: Core.Maybe VolumeAttachmentState,
    -- | Indicates whether the EBS volume is deleted on instance termination.
    deleteOnTermination :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VolumeAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'volumeAttachment_instanceId' - The ID of the instance.
--
-- 'attachTime', 'volumeAttachment_attachTime' - The time stamp when the attachment initiated.
--
-- 'device', 'volumeAttachment_device' - The device name.
--
-- 'volumeId', 'volumeAttachment_volumeId' - The ID of the volume.
--
-- 'state', 'volumeAttachment_state' - The attachment state of the volume.
--
-- 'deleteOnTermination', 'volumeAttachment_deleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
newVolumeAttachment ::
  VolumeAttachment
newVolumeAttachment =
  VolumeAttachment'
    { instanceId = Core.Nothing,
      attachTime = Core.Nothing,
      device = Core.Nothing,
      volumeId = Core.Nothing,
      state = Core.Nothing,
      deleteOnTermination = Core.Nothing
    }

-- | The ID of the instance.
volumeAttachment_instanceId :: Lens.Lens' VolumeAttachment (Core.Maybe Core.Text)
volumeAttachment_instanceId = Lens.lens (\VolumeAttachment' {instanceId} -> instanceId) (\s@VolumeAttachment' {} a -> s {instanceId = a} :: VolumeAttachment)

-- | The time stamp when the attachment initiated.
volumeAttachment_attachTime :: Lens.Lens' VolumeAttachment (Core.Maybe Core.UTCTime)
volumeAttachment_attachTime = Lens.lens (\VolumeAttachment' {attachTime} -> attachTime) (\s@VolumeAttachment' {} a -> s {attachTime = a} :: VolumeAttachment) Core.. Lens.mapping Core._Time

-- | The device name.
volumeAttachment_device :: Lens.Lens' VolumeAttachment (Core.Maybe Core.Text)
volumeAttachment_device = Lens.lens (\VolumeAttachment' {device} -> device) (\s@VolumeAttachment' {} a -> s {device = a} :: VolumeAttachment)

-- | The ID of the volume.
volumeAttachment_volumeId :: Lens.Lens' VolumeAttachment (Core.Maybe Core.Text)
volumeAttachment_volumeId = Lens.lens (\VolumeAttachment' {volumeId} -> volumeId) (\s@VolumeAttachment' {} a -> s {volumeId = a} :: VolumeAttachment)

-- | The attachment state of the volume.
volumeAttachment_state :: Lens.Lens' VolumeAttachment (Core.Maybe VolumeAttachmentState)
volumeAttachment_state = Lens.lens (\VolumeAttachment' {state} -> state) (\s@VolumeAttachment' {} a -> s {state = a} :: VolumeAttachment)

-- | Indicates whether the EBS volume is deleted on instance termination.
volumeAttachment_deleteOnTermination :: Lens.Lens' VolumeAttachment (Core.Maybe Core.Bool)
volumeAttachment_deleteOnTermination = Lens.lens (\VolumeAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@VolumeAttachment' {} a -> s {deleteOnTermination = a} :: VolumeAttachment)

instance Core.FromXML VolumeAttachment where
  parseXML x =
    VolumeAttachment'
      Core.<$> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "attachTime")
      Core.<*> (x Core..@? "device")
      Core.<*> (x Core..@? "volumeId")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "deleteOnTermination")

instance Core.Hashable VolumeAttachment

instance Core.NFData VolumeAttachment
