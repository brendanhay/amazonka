{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.Disk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.Disk
  ( Disk (..),

    -- * Smart constructor
    mkDisk,

    -- * Lenses
    dDiskAllocationResource,
    dDiskAllocationType,
    dDiskNode,
    dDiskPath,
    dDiskSizeInBytes,
    dDiskStatus,
    dDiskId,
    dDiskAttributeList,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a gateway's local disk.
--
-- /See:/ 'mkDisk' smart constructor.
data Disk = Disk'
  { diskAllocationResource :: Lude.Maybe Lude.Text,
    diskAllocationType :: Lude.Maybe Lude.Text,
    diskNode :: Lude.Maybe Lude.Text,
    diskPath :: Lude.Maybe Lude.Text,
    diskSizeInBytes :: Lude.Maybe Lude.Integer,
    diskStatus :: Lude.Maybe Lude.Text,
    diskId :: Lude.Maybe Lude.Text,
    diskAttributeList :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Disk' with the minimum fields required to make a request.
--
-- * 'diskAllocationResource' - The iSCSI qualified name (IQN) that is defined for a disk. This field is not included in the response if the local disk is not defined as an iSCSI target. The format of this field is /targetIqn::LUNNumber::region-volumeId/ .
-- * 'diskAllocationType' - Undocumented field.
-- * 'diskAttributeList' - Undocumented field.
-- * 'diskId' - The unique device ID or other distinguishing data that identifies a local disk.
-- * 'diskNode' - The device node of a local disk as assigned by the virtualization environment.
-- * 'diskPath' - The path of a local disk in the gateway virtual machine (VM).
-- * 'diskSizeInBytes' - The local disk size in bytes.
-- * 'diskStatus' - A value that represents the status of a local disk.
mkDisk ::
  Disk
mkDisk =
  Disk'
    { diskAllocationResource = Lude.Nothing,
      diskAllocationType = Lude.Nothing,
      diskNode = Lude.Nothing,
      diskPath = Lude.Nothing,
      diskSizeInBytes = Lude.Nothing,
      diskStatus = Lude.Nothing,
      diskId = Lude.Nothing,
      diskAttributeList = Lude.Nothing
    }

-- | The iSCSI qualified name (IQN) that is defined for a disk. This field is not included in the response if the local disk is not defined as an iSCSI target. The format of this field is /targetIqn::LUNNumber::region-volumeId/ .
--
-- /Note:/ Consider using 'diskAllocationResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDiskAllocationResource :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dDiskAllocationResource = Lens.lens (diskAllocationResource :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {diskAllocationResource = a} :: Disk)
{-# DEPRECATED dDiskAllocationResource "Use generic-lens or generic-optics with 'diskAllocationResource' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'diskAllocationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDiskAllocationType :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dDiskAllocationType = Lens.lens (diskAllocationType :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {diskAllocationType = a} :: Disk)
{-# DEPRECATED dDiskAllocationType "Use generic-lens or generic-optics with 'diskAllocationType' instead." #-}

-- | The device node of a local disk as assigned by the virtualization environment.
--
-- /Note:/ Consider using 'diskNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDiskNode :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dDiskNode = Lens.lens (diskNode :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {diskNode = a} :: Disk)
{-# DEPRECATED dDiskNode "Use generic-lens or generic-optics with 'diskNode' instead." #-}

-- | The path of a local disk in the gateway virtual machine (VM).
--
-- /Note:/ Consider using 'diskPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDiskPath :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dDiskPath = Lens.lens (diskPath :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {diskPath = a} :: Disk)
{-# DEPRECATED dDiskPath "Use generic-lens or generic-optics with 'diskPath' instead." #-}

-- | The local disk size in bytes.
--
-- /Note:/ Consider using 'diskSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDiskSizeInBytes :: Lens.Lens' Disk (Lude.Maybe Lude.Integer)
dDiskSizeInBytes = Lens.lens (diskSizeInBytes :: Disk -> Lude.Maybe Lude.Integer) (\s a -> s {diskSizeInBytes = a} :: Disk)
{-# DEPRECATED dDiskSizeInBytes "Use generic-lens or generic-optics with 'diskSizeInBytes' instead." #-}

-- | A value that represents the status of a local disk.
--
-- /Note:/ Consider using 'diskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDiskStatus :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dDiskStatus = Lens.lens (diskStatus :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {diskStatus = a} :: Disk)
{-# DEPRECATED dDiskStatus "Use generic-lens or generic-optics with 'diskStatus' instead." #-}

-- | The unique device ID or other distinguishing data that identifies a local disk.
--
-- /Note:/ Consider using 'diskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDiskId :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dDiskId = Lens.lens (diskId :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {diskId = a} :: Disk)
{-# DEPRECATED dDiskId "Use generic-lens or generic-optics with 'diskId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'diskAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDiskAttributeList :: Lens.Lens' Disk (Lude.Maybe [Lude.Text])
dDiskAttributeList = Lens.lens (diskAttributeList :: Disk -> Lude.Maybe [Lude.Text]) (\s a -> s {diskAttributeList = a} :: Disk)
{-# DEPRECATED dDiskAttributeList "Use generic-lens or generic-optics with 'diskAttributeList' instead." #-}

instance Lude.FromJSON Disk where
  parseJSON =
    Lude.withObject
      "Disk"
      ( \x ->
          Disk'
            Lude.<$> (x Lude..:? "DiskAllocationResource")
            Lude.<*> (x Lude..:? "DiskAllocationType")
            Lude.<*> (x Lude..:? "DiskNode")
            Lude.<*> (x Lude..:? "DiskPath")
            Lude.<*> (x Lude..:? "DiskSizeInBytes")
            Lude.<*> (x Lude..:? "DiskStatus")
            Lude.<*> (x Lude..:? "DiskId")
            Lude.<*> (x Lude..:? "DiskAttributeList" Lude..!= Lude.mempty)
      )
