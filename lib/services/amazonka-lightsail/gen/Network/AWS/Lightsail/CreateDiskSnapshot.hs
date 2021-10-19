{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateDiskSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a block storage disk. You can use snapshots for
-- backups, to make copies of disks, and to save data before shutting down
-- a Lightsail instance.
--
-- You can take a snapshot of an attached disk that is in use; however,
-- snapshots only capture data that has been written to your disk at the
-- time the snapshot command is issued. This may exclude any data that has
-- been cached by any applications or the operating system. If you can
-- pause any file systems on the disk long enough to take a snapshot, your
-- snapshot should be complete. Nevertheless, if you cannot pause all file
-- writes to the disk, you should unmount the disk from within the
-- Lightsail instance, issue the create disk snapshot command, and then
-- remount the disk to ensure a consistent and complete snapshot. You may
-- remount and use your disk while the snapshot status is pending.
--
-- You can also use this operation to create a snapshot of an instance\'s
-- system volume. You might want to do this, for example, to recover data
-- from the system volume of a botched instance or to create a backup of
-- the system volume like you would for a block storage disk. To create a
-- snapshot of a system volume, just define the @instance name@ parameter
-- when issuing the snapshot command, and a snapshot of the defined
-- instance\'s system volume will be created. After the snapshot is
-- available, you can create a block storage disk from the snapshot and
-- attach it to a running instance to access the data on the disk.
--
-- The @create disk snapshot@ operation supports tag-based access control
-- via request tags. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Network.AWS.Lightsail.CreateDiskSnapshot
  ( -- * Creating a Request
    CreateDiskSnapshot (..),
    newCreateDiskSnapshot,

    -- * Request Lenses
    createDiskSnapshot_diskName,
    createDiskSnapshot_instanceName,
    createDiskSnapshot_tags,
    createDiskSnapshot_diskSnapshotName,

    -- * Destructuring the Response
    CreateDiskSnapshotResponse (..),
    newCreateDiskSnapshotResponse,

    -- * Response Lenses
    createDiskSnapshotResponse_operations,
    createDiskSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDiskSnapshot' smart constructor.
data CreateDiskSnapshot = CreateDiskSnapshot'
  { -- | The unique name of the source disk (e.g., @Disk-Virginia-1@).
    --
    -- This parameter cannot be defined together with the @instance name@
    -- parameter. The @disk name@ and @instance name@ parameters are mutually
    -- exclusive.
    diskName :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the source instance (e.g.,
    -- @Amazon_Linux-512MB-Virginia-1@). When this is defined, a snapshot of
    -- the instance\'s system volume is created.
    --
    -- This parameter cannot be defined together with the @disk name@
    -- parameter. The @instance name@ and @disk name@ parameters are mutually
    -- exclusive.
    instanceName :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the destination disk snapshot (e.g., @my-disk-snapshot@)
    -- based on the source disk.
    diskSnapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDiskSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskName', 'createDiskSnapshot_diskName' - The unique name of the source disk (e.g., @Disk-Virginia-1@).
--
-- This parameter cannot be defined together with the @instance name@
-- parameter. The @disk name@ and @instance name@ parameters are mutually
-- exclusive.
--
-- 'instanceName', 'createDiskSnapshot_instanceName' - The unique name of the source instance (e.g.,
-- @Amazon_Linux-512MB-Virginia-1@). When this is defined, a snapshot of
-- the instance\'s system volume is created.
--
-- This parameter cannot be defined together with the @disk name@
-- parameter. The @instance name@ and @disk name@ parameters are mutually
-- exclusive.
--
-- 'tags', 'createDiskSnapshot_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'diskSnapshotName', 'createDiskSnapshot_diskSnapshotName' - The name of the destination disk snapshot (e.g., @my-disk-snapshot@)
-- based on the source disk.
newCreateDiskSnapshot ::
  -- | 'diskSnapshotName'
  Prelude.Text ->
  CreateDiskSnapshot
newCreateDiskSnapshot pDiskSnapshotName_ =
  CreateDiskSnapshot'
    { diskName = Prelude.Nothing,
      instanceName = Prelude.Nothing,
      tags = Prelude.Nothing,
      diskSnapshotName = pDiskSnapshotName_
    }

-- | The unique name of the source disk (e.g., @Disk-Virginia-1@).
--
-- This parameter cannot be defined together with the @instance name@
-- parameter. The @disk name@ and @instance name@ parameters are mutually
-- exclusive.
createDiskSnapshot_diskName :: Lens.Lens' CreateDiskSnapshot (Prelude.Maybe Prelude.Text)
createDiskSnapshot_diskName = Lens.lens (\CreateDiskSnapshot' {diskName} -> diskName) (\s@CreateDiskSnapshot' {} a -> s {diskName = a} :: CreateDiskSnapshot)

-- | The unique name of the source instance (e.g.,
-- @Amazon_Linux-512MB-Virginia-1@). When this is defined, a snapshot of
-- the instance\'s system volume is created.
--
-- This parameter cannot be defined together with the @disk name@
-- parameter. The @instance name@ and @disk name@ parameters are mutually
-- exclusive.
createDiskSnapshot_instanceName :: Lens.Lens' CreateDiskSnapshot (Prelude.Maybe Prelude.Text)
createDiskSnapshot_instanceName = Lens.lens (\CreateDiskSnapshot' {instanceName} -> instanceName) (\s@CreateDiskSnapshot' {} a -> s {instanceName = a} :: CreateDiskSnapshot)

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createDiskSnapshot_tags :: Lens.Lens' CreateDiskSnapshot (Prelude.Maybe [Tag])
createDiskSnapshot_tags = Lens.lens (\CreateDiskSnapshot' {tags} -> tags) (\s@CreateDiskSnapshot' {} a -> s {tags = a} :: CreateDiskSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The name of the destination disk snapshot (e.g., @my-disk-snapshot@)
-- based on the source disk.
createDiskSnapshot_diskSnapshotName :: Lens.Lens' CreateDiskSnapshot Prelude.Text
createDiskSnapshot_diskSnapshotName = Lens.lens (\CreateDiskSnapshot' {diskSnapshotName} -> diskSnapshotName) (\s@CreateDiskSnapshot' {} a -> s {diskSnapshotName = a} :: CreateDiskSnapshot)

instance Core.AWSRequest CreateDiskSnapshot where
  type
    AWSResponse CreateDiskSnapshot =
      CreateDiskSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDiskSnapshotResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDiskSnapshot

instance Prelude.NFData CreateDiskSnapshot

instance Core.ToHeaders CreateDiskSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateDiskSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDiskSnapshot where
  toJSON CreateDiskSnapshot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("diskName" Core..=) Prelude.<$> diskName,
            ("instanceName" Core..=) Prelude.<$> instanceName,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("diskSnapshotName" Core..= diskSnapshotName)
          ]
      )

instance Core.ToPath CreateDiskSnapshot where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDiskSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDiskSnapshotResponse' smart constructor.
data CreateDiskSnapshotResponse = CreateDiskSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDiskSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createDiskSnapshotResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createDiskSnapshotResponse_httpStatus' - The response's http status code.
newCreateDiskSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDiskSnapshotResponse
newCreateDiskSnapshotResponse pHttpStatus_ =
  CreateDiskSnapshotResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createDiskSnapshotResponse_operations :: Lens.Lens' CreateDiskSnapshotResponse (Prelude.Maybe [Operation])
createDiskSnapshotResponse_operations = Lens.lens (\CreateDiskSnapshotResponse' {operations} -> operations) (\s@CreateDiskSnapshotResponse' {} a -> s {operations = a} :: CreateDiskSnapshotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createDiskSnapshotResponse_httpStatus :: Lens.Lens' CreateDiskSnapshotResponse Prelude.Int
createDiskSnapshotResponse_httpStatus = Lens.lens (\CreateDiskSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateDiskSnapshotResponse' {} a -> s {httpStatus = a} :: CreateDiskSnapshotResponse)

instance Prelude.NFData CreateDiskSnapshotResponse
