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
-- Module      : Network.AWS.EC2.Types.EbsInstanceBlockDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EbsInstanceBlockDevice where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AttachmentStatus
import qualified Network.AWS.Lens as Lens

-- | Describes a parameter used to set up an EBS volume in a block device
-- mapping.
--
-- /See:/ 'newEbsInstanceBlockDevice' smart constructor.
data EbsInstanceBlockDevice = EbsInstanceBlockDevice'
  { -- | The attachment state.
    status :: Core.Maybe AttachmentStatus,
    -- | The time stamp when the attachment initiated.
    attachTime :: Core.Maybe Core.ISO8601,
    -- | The ID of the EBS volume.
    volumeId :: Core.Maybe Core.Text,
    -- | Indicates whether the volume is deleted on instance termination.
    deleteOnTermination :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EbsInstanceBlockDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'ebsInstanceBlockDevice_status' - The attachment state.
--
-- 'attachTime', 'ebsInstanceBlockDevice_attachTime' - The time stamp when the attachment initiated.
--
-- 'volumeId', 'ebsInstanceBlockDevice_volumeId' - The ID of the EBS volume.
--
-- 'deleteOnTermination', 'ebsInstanceBlockDevice_deleteOnTermination' - Indicates whether the volume is deleted on instance termination.
newEbsInstanceBlockDevice ::
  EbsInstanceBlockDevice
newEbsInstanceBlockDevice =
  EbsInstanceBlockDevice'
    { status = Core.Nothing,
      attachTime = Core.Nothing,
      volumeId = Core.Nothing,
      deleteOnTermination = Core.Nothing
    }

-- | The attachment state.
ebsInstanceBlockDevice_status :: Lens.Lens' EbsInstanceBlockDevice (Core.Maybe AttachmentStatus)
ebsInstanceBlockDevice_status = Lens.lens (\EbsInstanceBlockDevice' {status} -> status) (\s@EbsInstanceBlockDevice' {} a -> s {status = a} :: EbsInstanceBlockDevice)

-- | The time stamp when the attachment initiated.
ebsInstanceBlockDevice_attachTime :: Lens.Lens' EbsInstanceBlockDevice (Core.Maybe Core.UTCTime)
ebsInstanceBlockDevice_attachTime = Lens.lens (\EbsInstanceBlockDevice' {attachTime} -> attachTime) (\s@EbsInstanceBlockDevice' {} a -> s {attachTime = a} :: EbsInstanceBlockDevice) Core.. Lens.mapping Core._Time

-- | The ID of the EBS volume.
ebsInstanceBlockDevice_volumeId :: Lens.Lens' EbsInstanceBlockDevice (Core.Maybe Core.Text)
ebsInstanceBlockDevice_volumeId = Lens.lens (\EbsInstanceBlockDevice' {volumeId} -> volumeId) (\s@EbsInstanceBlockDevice' {} a -> s {volumeId = a} :: EbsInstanceBlockDevice)

-- | Indicates whether the volume is deleted on instance termination.
ebsInstanceBlockDevice_deleteOnTermination :: Lens.Lens' EbsInstanceBlockDevice (Core.Maybe Core.Bool)
ebsInstanceBlockDevice_deleteOnTermination = Lens.lens (\EbsInstanceBlockDevice' {deleteOnTermination} -> deleteOnTermination) (\s@EbsInstanceBlockDevice' {} a -> s {deleteOnTermination = a} :: EbsInstanceBlockDevice)

instance Core.FromXML EbsInstanceBlockDevice where
  parseXML x =
    EbsInstanceBlockDevice'
      Core.<$> (x Core..@? "status")
      Core.<*> (x Core..@? "attachTime")
      Core.<*> (x Core..@? "volumeId")
      Core.<*> (x Core..@? "deleteOnTermination")

instance Core.Hashable EbsInstanceBlockDevice

instance Core.NFData EbsInstanceBlockDevice
