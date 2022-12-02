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
-- Module      : Amazonka.EC2.Types.VolumeAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VolumeAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.VolumeAttachmentState
import qualified Amazonka.Prelude as Prelude

-- | Describes volume attachment details.
--
-- /See:/ 'newVolumeAttachment' smart constructor.
data VolumeAttachment = VolumeAttachment'
  { -- | Indicates whether the EBS volume is deleted on instance termination.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The device name.
    device :: Prelude.Maybe Prelude.Text,
    -- | The attachment state of the volume.
    state :: Prelude.Maybe VolumeAttachmentState,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The time stamp when the attachment initiated.
    attachTime :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the volume.
    volumeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteOnTermination', 'volumeAttachment_deleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
--
-- 'device', 'volumeAttachment_device' - The device name.
--
-- 'state', 'volumeAttachment_state' - The attachment state of the volume.
--
-- 'instanceId', 'volumeAttachment_instanceId' - The ID of the instance.
--
-- 'attachTime', 'volumeAttachment_attachTime' - The time stamp when the attachment initiated.
--
-- 'volumeId', 'volumeAttachment_volumeId' - The ID of the volume.
newVolumeAttachment ::
  VolumeAttachment
newVolumeAttachment =
  VolumeAttachment'
    { deleteOnTermination =
        Prelude.Nothing,
      device = Prelude.Nothing,
      state = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      attachTime = Prelude.Nothing,
      volumeId = Prelude.Nothing
    }

-- | Indicates whether the EBS volume is deleted on instance termination.
volumeAttachment_deleteOnTermination :: Lens.Lens' VolumeAttachment (Prelude.Maybe Prelude.Bool)
volumeAttachment_deleteOnTermination = Lens.lens (\VolumeAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@VolumeAttachment' {} a -> s {deleteOnTermination = a} :: VolumeAttachment)

-- | The device name.
volumeAttachment_device :: Lens.Lens' VolumeAttachment (Prelude.Maybe Prelude.Text)
volumeAttachment_device = Lens.lens (\VolumeAttachment' {device} -> device) (\s@VolumeAttachment' {} a -> s {device = a} :: VolumeAttachment)

-- | The attachment state of the volume.
volumeAttachment_state :: Lens.Lens' VolumeAttachment (Prelude.Maybe VolumeAttachmentState)
volumeAttachment_state = Lens.lens (\VolumeAttachment' {state} -> state) (\s@VolumeAttachment' {} a -> s {state = a} :: VolumeAttachment)

-- | The ID of the instance.
volumeAttachment_instanceId :: Lens.Lens' VolumeAttachment (Prelude.Maybe Prelude.Text)
volumeAttachment_instanceId = Lens.lens (\VolumeAttachment' {instanceId} -> instanceId) (\s@VolumeAttachment' {} a -> s {instanceId = a} :: VolumeAttachment)

-- | The time stamp when the attachment initiated.
volumeAttachment_attachTime :: Lens.Lens' VolumeAttachment (Prelude.Maybe Prelude.UTCTime)
volumeAttachment_attachTime = Lens.lens (\VolumeAttachment' {attachTime} -> attachTime) (\s@VolumeAttachment' {} a -> s {attachTime = a} :: VolumeAttachment) Prelude.. Lens.mapping Data._Time

-- | The ID of the volume.
volumeAttachment_volumeId :: Lens.Lens' VolumeAttachment (Prelude.Maybe Prelude.Text)
volumeAttachment_volumeId = Lens.lens (\VolumeAttachment' {volumeId} -> volumeId) (\s@VolumeAttachment' {} a -> s {volumeId = a} :: VolumeAttachment)

instance Data.FromXML VolumeAttachment where
  parseXML x =
    VolumeAttachment'
      Prelude.<$> (x Data..@? "deleteOnTermination")
      Prelude.<*> (x Data..@? "device")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "instanceId")
      Prelude.<*> (x Data..@? "attachTime")
      Prelude.<*> (x Data..@? "volumeId")

instance Prelude.Hashable VolumeAttachment where
  hashWithSalt _salt VolumeAttachment' {..} =
    _salt `Prelude.hashWithSalt` deleteOnTermination
      `Prelude.hashWithSalt` device
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` attachTime
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData VolumeAttachment where
  rnf VolumeAttachment' {..} =
    Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf device
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf attachTime
      `Prelude.seq` Prelude.rnf volumeId
