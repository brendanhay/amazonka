{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateDiskSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a block storage disk. You can use snapshots for backups, to make copies of disks, and to save data before shutting down a Lightsail instance.
--
-- You can take a snapshot of an attached disk that is in use; however, snapshots only capture data that has been written to your disk at the time the snapshot command is issued. This may exclude any data that has been cached by any applications or the operating system. If you can pause any file systems on the disk long enough to take a snapshot, your snapshot should be complete. Nevertheless, if you cannot pause all file writes to the disk, you should unmount the disk from within the Lightsail instance, issue the create disk snapshot command, and then remount the disk to ensure a consistent and complete snapshot. You may remount and use your disk while the snapshot status is pending.
-- You can also use this operation to create a snapshot of an instance's system volume. You might want to do this, for example, to recover data from the system volume of a botched instance or to create a backup of the system volume like you would for a block storage disk. To create a snapshot of a system volume, just define the @instance name@ parameter when issuing the snapshot command, and a snapshot of the defined instance's system volume will be created. After the snapshot is available, you can create a block storage disk from the snapshot and attach it to a running instance to access the data on the disk.
-- The @create disk snapshot@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateDiskSnapshot
  ( -- * Creating a request
    CreateDiskSnapshot (..),
    mkCreateDiskSnapshot,

    -- ** Request lenses
    cdsDiskName,
    cdsInstanceName,
    cdsTags,
    cdsDiskSnapshotName,

    -- * Destructuring the response
    CreateDiskSnapshotResponse (..),
    mkCreateDiskSnapshotResponse,

    -- ** Response lenses
    cdsrsOperations,
    cdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDiskSnapshot' smart constructor.
data CreateDiskSnapshot = CreateDiskSnapshot'
  { diskName ::
      Lude.Maybe Lude.Text,
    instanceName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    diskSnapshotName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDiskSnapshot' with the minimum fields required to make a request.
--
-- * 'diskName' - The unique name of the source disk (e.g., @Disk-Virginia-1@ ).
-- * 'diskSnapshotName' - The name of the destination disk snapshot (e.g., @my-disk-snapshot@ ) based on the source disk.
-- * 'instanceName' - The unique name of the source instance (e.g., @Amazon_Linux-512MB-Virginia-1@ ). When this is defined, a snapshot of the instance's system volume is created.
-- * 'tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
mkCreateDiskSnapshot ::
  -- | 'diskSnapshotName'
  Lude.Text ->
  CreateDiskSnapshot
mkCreateDiskSnapshot pDiskSnapshotName_ =
  CreateDiskSnapshot'
    { diskName = Lude.Nothing,
      instanceName = Lude.Nothing,
      tags = Lude.Nothing,
      diskSnapshotName = pDiskSnapshotName_
    }

-- | The unique name of the source disk (e.g., @Disk-Virginia-1@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDiskName :: Lens.Lens' CreateDiskSnapshot (Lude.Maybe Lude.Text)
cdsDiskName = Lens.lens (diskName :: CreateDiskSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {diskName = a} :: CreateDiskSnapshot)
{-# DEPRECATED cdsDiskName "Use generic-lens or generic-optics with 'diskName' instead." #-}

-- | The unique name of the source instance (e.g., @Amazon_Linux-512MB-Virginia-1@ ). When this is defined, a snapshot of the instance's system volume is created.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsInstanceName :: Lens.Lens' CreateDiskSnapshot (Lude.Maybe Lude.Text)
cdsInstanceName = Lens.lens (instanceName :: CreateDiskSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {instanceName = a} :: CreateDiskSnapshot)
{-# DEPRECATED cdsInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTags :: Lens.Lens' CreateDiskSnapshot (Lude.Maybe [Tag])
cdsTags = Lens.lens (tags :: CreateDiskSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDiskSnapshot)
{-# DEPRECATED cdsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the destination disk snapshot (e.g., @my-disk-snapshot@ ) based on the source disk.
--
-- /Note:/ Consider using 'diskSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDiskSnapshotName :: Lens.Lens' CreateDiskSnapshot Lude.Text
cdsDiskSnapshotName = Lens.lens (diskSnapshotName :: CreateDiskSnapshot -> Lude.Text) (\s a -> s {diskSnapshotName = a} :: CreateDiskSnapshot)
{-# DEPRECATED cdsDiskSnapshotName "Use generic-lens or generic-optics with 'diskSnapshotName' instead." #-}

instance Lude.AWSRequest CreateDiskSnapshot where
  type Rs CreateDiskSnapshot = CreateDiskSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDiskSnapshotResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDiskSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CreateDiskSnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDiskSnapshot where
  toJSON CreateDiskSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("diskName" Lude..=) Lude.<$> diskName,
            ("instanceName" Lude..=) Lude.<$> instanceName,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("diskSnapshotName" Lude..= diskSnapshotName)
          ]
      )

instance Lude.ToPath CreateDiskSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDiskSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDiskSnapshotResponse' smart constructor.
data CreateDiskSnapshotResponse = CreateDiskSnapshotResponse'
  { operations ::
      Lude.Maybe [Operation],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDiskSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateDiskSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDiskSnapshotResponse
mkCreateDiskSnapshotResponse pResponseStatus_ =
  CreateDiskSnapshotResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrsOperations :: Lens.Lens' CreateDiskSnapshotResponse (Lude.Maybe [Operation])
cdsrsOperations = Lens.lens (operations :: CreateDiskSnapshotResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateDiskSnapshotResponse)
{-# DEPRECATED cdsrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsrsResponseStatus :: Lens.Lens' CreateDiskSnapshotResponse Lude.Int
cdsrsResponseStatus = Lens.lens (responseStatus :: CreateDiskSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDiskSnapshotResponse)
{-# DEPRECATED cdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
