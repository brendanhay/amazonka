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
-- Module      : Network.AWS.StorageGateway.Types.Disk
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.Disk where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a gateway\'s local disk.
--
-- /See:/ 'newDisk' smart constructor.
data Disk = Disk'
  { -- | The iSCSI qualified name (IQN) that is defined for a disk. This field is
    -- not included in the response if the local disk is not defined as an
    -- iSCSI target. The format of this field is
    -- /targetIqn::LUNNumber::region-volumeId/.
    diskAllocationResource :: Core.Maybe Core.Text,
    -- | A value that represents the status of a local disk.
    diskStatus :: Core.Maybe Core.Text,
    -- | The local disk size in bytes.
    diskSizeInBytes :: Core.Maybe Core.Integer,
    diskAttributeList :: Core.Maybe [Core.Text],
    -- | The path of a local disk in the gateway virtual machine (VM).
    diskPath :: Core.Maybe Core.Text,
    -- | The unique device ID or other distinguishing data that identifies a
    -- local disk.
    diskId :: Core.Maybe Core.Text,
    diskAllocationType :: Core.Maybe Core.Text,
    -- | The device node of a local disk as assigned by the virtualization
    -- environment.
    diskNode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Disk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskAllocationResource', 'disk_diskAllocationResource' - The iSCSI qualified name (IQN) that is defined for a disk. This field is
-- not included in the response if the local disk is not defined as an
-- iSCSI target. The format of this field is
-- /targetIqn::LUNNumber::region-volumeId/.
--
-- 'diskStatus', 'disk_diskStatus' - A value that represents the status of a local disk.
--
-- 'diskSizeInBytes', 'disk_diskSizeInBytes' - The local disk size in bytes.
--
-- 'diskAttributeList', 'disk_diskAttributeList' - Undocumented member.
--
-- 'diskPath', 'disk_diskPath' - The path of a local disk in the gateway virtual machine (VM).
--
-- 'diskId', 'disk_diskId' - The unique device ID or other distinguishing data that identifies a
-- local disk.
--
-- 'diskAllocationType', 'disk_diskAllocationType' - Undocumented member.
--
-- 'diskNode', 'disk_diskNode' - The device node of a local disk as assigned by the virtualization
-- environment.
newDisk ::
  Disk
newDisk =
  Disk'
    { diskAllocationResource = Core.Nothing,
      diskStatus = Core.Nothing,
      diskSizeInBytes = Core.Nothing,
      diskAttributeList = Core.Nothing,
      diskPath = Core.Nothing,
      diskId = Core.Nothing,
      diskAllocationType = Core.Nothing,
      diskNode = Core.Nothing
    }

-- | The iSCSI qualified name (IQN) that is defined for a disk. This field is
-- not included in the response if the local disk is not defined as an
-- iSCSI target. The format of this field is
-- /targetIqn::LUNNumber::region-volumeId/.
disk_diskAllocationResource :: Lens.Lens' Disk (Core.Maybe Core.Text)
disk_diskAllocationResource = Lens.lens (\Disk' {diskAllocationResource} -> diskAllocationResource) (\s@Disk' {} a -> s {diskAllocationResource = a} :: Disk)

-- | A value that represents the status of a local disk.
disk_diskStatus :: Lens.Lens' Disk (Core.Maybe Core.Text)
disk_diskStatus = Lens.lens (\Disk' {diskStatus} -> diskStatus) (\s@Disk' {} a -> s {diskStatus = a} :: Disk)

-- | The local disk size in bytes.
disk_diskSizeInBytes :: Lens.Lens' Disk (Core.Maybe Core.Integer)
disk_diskSizeInBytes = Lens.lens (\Disk' {diskSizeInBytes} -> diskSizeInBytes) (\s@Disk' {} a -> s {diskSizeInBytes = a} :: Disk)

-- | Undocumented member.
disk_diskAttributeList :: Lens.Lens' Disk (Core.Maybe [Core.Text])
disk_diskAttributeList = Lens.lens (\Disk' {diskAttributeList} -> diskAttributeList) (\s@Disk' {} a -> s {diskAttributeList = a} :: Disk) Core.. Lens.mapping Lens._Coerce

-- | The path of a local disk in the gateway virtual machine (VM).
disk_diskPath :: Lens.Lens' Disk (Core.Maybe Core.Text)
disk_diskPath = Lens.lens (\Disk' {diskPath} -> diskPath) (\s@Disk' {} a -> s {diskPath = a} :: Disk)

-- | The unique device ID or other distinguishing data that identifies a
-- local disk.
disk_diskId :: Lens.Lens' Disk (Core.Maybe Core.Text)
disk_diskId = Lens.lens (\Disk' {diskId} -> diskId) (\s@Disk' {} a -> s {diskId = a} :: Disk)

-- | Undocumented member.
disk_diskAllocationType :: Lens.Lens' Disk (Core.Maybe Core.Text)
disk_diskAllocationType = Lens.lens (\Disk' {diskAllocationType} -> diskAllocationType) (\s@Disk' {} a -> s {diskAllocationType = a} :: Disk)

-- | The device node of a local disk as assigned by the virtualization
-- environment.
disk_diskNode :: Lens.Lens' Disk (Core.Maybe Core.Text)
disk_diskNode = Lens.lens (\Disk' {diskNode} -> diskNode) (\s@Disk' {} a -> s {diskNode = a} :: Disk)

instance Core.FromJSON Disk where
  parseJSON =
    Core.withObject
      "Disk"
      ( \x ->
          Disk'
            Core.<$> (x Core..:? "DiskAllocationResource")
            Core.<*> (x Core..:? "DiskStatus")
            Core.<*> (x Core..:? "DiskSizeInBytes")
            Core.<*> (x Core..:? "DiskAttributeList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "DiskPath")
            Core.<*> (x Core..:? "DiskId")
            Core.<*> (x Core..:? "DiskAllocationType")
            Core.<*> (x Core..:? "DiskNode")
      )

instance Core.Hashable Disk

instance Core.NFData Disk
