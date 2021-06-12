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
-- Module      : Network.AWS.Lightsail.CreateDiskFromSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a block storage disk from a manual or automatic snapshot of a
-- disk. The resulting disk can be attached to an Amazon Lightsail instance
-- in the same Availability Zone (e.g., @us-east-2a@).
--
-- The @create disk from snapshot@ operation supports tag-based access
-- control via request tags and resource tags applied to the resource
-- identified by @disk snapshot name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.CreateDiskFromSnapshot
  ( -- * Creating a Request
    CreateDiskFromSnapshot (..),
    newCreateDiskFromSnapshot,

    -- * Request Lenses
    createDiskFromSnapshot_sourceDiskName,
    createDiskFromSnapshot_restoreDate,
    createDiskFromSnapshot_addOns,
    createDiskFromSnapshot_useLatestRestorableAutoSnapshot,
    createDiskFromSnapshot_tags,
    createDiskFromSnapshot_diskSnapshotName,
    createDiskFromSnapshot_diskName,
    createDiskFromSnapshot_availabilityZone,
    createDiskFromSnapshot_sizeInGb,

    -- * Destructuring the Response
    CreateDiskFromSnapshotResponse (..),
    newCreateDiskFromSnapshotResponse,

    -- * Response Lenses
    createDiskFromSnapshotResponse_operations,
    createDiskFromSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDiskFromSnapshot' smart constructor.
data CreateDiskFromSnapshot = CreateDiskFromSnapshot'
  { -- | The name of the source disk from which the source automatic snapshot was
    -- created.
    --
    -- Constraints:
    --
    -- -   This parameter cannot be defined together with the
    --     @disk snapshot name@ parameter. The @source disk name@ and
    --     @disk snapshot name@ parameters are mutually exclusive.
    --
    -- -   Define this parameter only when creating a new disk from an
    --     automatic snapshot. For more information, see the
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
    sourceDiskName :: Core.Maybe Core.Text,
    -- | The date of the automatic snapshot to use for the new disk. Use the
    -- @get auto snapshots@ operation to identify the dates of the available
    -- automatic snapshots.
    --
    -- Constraints:
    --
    -- -   Must be specified in @YYYY-MM-DD@ format.
    --
    -- -   This parameter cannot be defined together with the
    --     @use latest restorable auto snapshot@ parameter. The @restore date@
    --     and @use latest restorable auto snapshot@ parameters are mutually
    --     exclusive.
    --
    -- -   Define this parameter only when creating a new disk from an
    --     automatic snapshot. For more information, see the
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
    restoreDate :: Core.Maybe Core.Text,
    -- | An array of objects that represent the add-ons to enable for the new
    -- disk.
    addOns :: Core.Maybe [AddOnRequest],
    -- | A Boolean value to indicate whether to use the latest available
    -- automatic snapshot.
    --
    -- Constraints:
    --
    -- -   This parameter cannot be defined together with the @restore date@
    --     parameter. The @use latest restorable auto snapshot@ and
    --     @restore date@ parameters are mutually exclusive.
    --
    -- -   Define this parameter only when creating a new disk from an
    --     automatic snapshot. For more information, see the
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
    useLatestRestorableAutoSnapshot :: Core.Maybe Core.Bool,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Core.Maybe [Tag],
    -- | The name of the disk snapshot (e.g., @my-snapshot@) from which to create
    -- the new storage disk.
    --
    -- Constraint:
    --
    -- -   This parameter cannot be defined together with the
    --     @source disk name@ parameter. The @disk snapshot name@ and
    --     @source disk name@ parameters are mutually exclusive.
    diskSnapshotName :: Core.Maybe Core.Text,
    -- | The unique Lightsail disk name (e.g., @my-disk@).
    diskName :: Core.Text,
    -- | The Availability Zone where you want to create the disk (e.g.,
    -- @us-east-2a@). Choose the same Availability Zone as the Lightsail
    -- instance where you want to create the disk.
    --
    -- Use the GetRegions operation to list the Availability Zones where
    -- Lightsail is currently available.
    availabilityZone :: Core.Text,
    -- | The size of the disk in GB (e.g., @32@).
    sizeInGb :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDiskFromSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceDiskName', 'createDiskFromSnapshot_sourceDiskName' - The name of the source disk from which the source automatic snapshot was
-- created.
--
-- Constraints:
--
-- -   This parameter cannot be defined together with the
--     @disk snapshot name@ parameter. The @source disk name@ and
--     @disk snapshot name@ parameters are mutually exclusive.
--
-- -   Define this parameter only when creating a new disk from an
--     automatic snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
--
-- 'restoreDate', 'createDiskFromSnapshot_restoreDate' - The date of the automatic snapshot to use for the new disk. Use the
-- @get auto snapshots@ operation to identify the dates of the available
-- automatic snapshots.
--
-- Constraints:
--
-- -   Must be specified in @YYYY-MM-DD@ format.
--
-- -   This parameter cannot be defined together with the
--     @use latest restorable auto snapshot@ parameter. The @restore date@
--     and @use latest restorable auto snapshot@ parameters are mutually
--     exclusive.
--
-- -   Define this parameter only when creating a new disk from an
--     automatic snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
--
-- 'addOns', 'createDiskFromSnapshot_addOns' - An array of objects that represent the add-ons to enable for the new
-- disk.
--
-- 'useLatestRestorableAutoSnapshot', 'createDiskFromSnapshot_useLatestRestorableAutoSnapshot' - A Boolean value to indicate whether to use the latest available
-- automatic snapshot.
--
-- Constraints:
--
-- -   This parameter cannot be defined together with the @restore date@
--     parameter. The @use latest restorable auto snapshot@ and
--     @restore date@ parameters are mutually exclusive.
--
-- -   Define this parameter only when creating a new disk from an
--     automatic snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
--
-- 'tags', 'createDiskFromSnapshot_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'diskSnapshotName', 'createDiskFromSnapshot_diskSnapshotName' - The name of the disk snapshot (e.g., @my-snapshot@) from which to create
-- the new storage disk.
--
-- Constraint:
--
-- -   This parameter cannot be defined together with the
--     @source disk name@ parameter. The @disk snapshot name@ and
--     @source disk name@ parameters are mutually exclusive.
--
-- 'diskName', 'createDiskFromSnapshot_diskName' - The unique Lightsail disk name (e.g., @my-disk@).
--
-- 'availabilityZone', 'createDiskFromSnapshot_availabilityZone' - The Availability Zone where you want to create the disk (e.g.,
-- @us-east-2a@). Choose the same Availability Zone as the Lightsail
-- instance where you want to create the disk.
--
-- Use the GetRegions operation to list the Availability Zones where
-- Lightsail is currently available.
--
-- 'sizeInGb', 'createDiskFromSnapshot_sizeInGb' - The size of the disk in GB (e.g., @32@).
newCreateDiskFromSnapshot ::
  -- | 'diskName'
  Core.Text ->
  -- | 'availabilityZone'
  Core.Text ->
  -- | 'sizeInGb'
  Core.Int ->
  CreateDiskFromSnapshot
newCreateDiskFromSnapshot
  pDiskName_
  pAvailabilityZone_
  pSizeInGb_ =
    CreateDiskFromSnapshot'
      { sourceDiskName =
          Core.Nothing,
        restoreDate = Core.Nothing,
        addOns = Core.Nothing,
        useLatestRestorableAutoSnapshot = Core.Nothing,
        tags = Core.Nothing,
        diskSnapshotName = Core.Nothing,
        diskName = pDiskName_,
        availabilityZone = pAvailabilityZone_,
        sizeInGb = pSizeInGb_
      }

-- | The name of the source disk from which the source automatic snapshot was
-- created.
--
-- Constraints:
--
-- -   This parameter cannot be defined together with the
--     @disk snapshot name@ parameter. The @source disk name@ and
--     @disk snapshot name@ parameters are mutually exclusive.
--
-- -   Define this parameter only when creating a new disk from an
--     automatic snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
createDiskFromSnapshot_sourceDiskName :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe Core.Text)
createDiskFromSnapshot_sourceDiskName = Lens.lens (\CreateDiskFromSnapshot' {sourceDiskName} -> sourceDiskName) (\s@CreateDiskFromSnapshot' {} a -> s {sourceDiskName = a} :: CreateDiskFromSnapshot)

-- | The date of the automatic snapshot to use for the new disk. Use the
-- @get auto snapshots@ operation to identify the dates of the available
-- automatic snapshots.
--
-- Constraints:
--
-- -   Must be specified in @YYYY-MM-DD@ format.
--
-- -   This parameter cannot be defined together with the
--     @use latest restorable auto snapshot@ parameter. The @restore date@
--     and @use latest restorable auto snapshot@ parameters are mutually
--     exclusive.
--
-- -   Define this parameter only when creating a new disk from an
--     automatic snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
createDiskFromSnapshot_restoreDate :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe Core.Text)
createDiskFromSnapshot_restoreDate = Lens.lens (\CreateDiskFromSnapshot' {restoreDate} -> restoreDate) (\s@CreateDiskFromSnapshot' {} a -> s {restoreDate = a} :: CreateDiskFromSnapshot)

-- | An array of objects that represent the add-ons to enable for the new
-- disk.
createDiskFromSnapshot_addOns :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe [AddOnRequest])
createDiskFromSnapshot_addOns = Lens.lens (\CreateDiskFromSnapshot' {addOns} -> addOns) (\s@CreateDiskFromSnapshot' {} a -> s {addOns = a} :: CreateDiskFromSnapshot) Core.. Lens.mapping Lens._Coerce

-- | A Boolean value to indicate whether to use the latest available
-- automatic snapshot.
--
-- Constraints:
--
-- -   This parameter cannot be defined together with the @restore date@
--     parameter. The @use latest restorable auto snapshot@ and
--     @restore date@ parameters are mutually exclusive.
--
-- -   Define this parameter only when creating a new disk from an
--     automatic snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide>.
createDiskFromSnapshot_useLatestRestorableAutoSnapshot :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe Core.Bool)
createDiskFromSnapshot_useLatestRestorableAutoSnapshot = Lens.lens (\CreateDiskFromSnapshot' {useLatestRestorableAutoSnapshot} -> useLatestRestorableAutoSnapshot) (\s@CreateDiskFromSnapshot' {} a -> s {useLatestRestorableAutoSnapshot = a} :: CreateDiskFromSnapshot)

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createDiskFromSnapshot_tags :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe [Tag])
createDiskFromSnapshot_tags = Lens.lens (\CreateDiskFromSnapshot' {tags} -> tags) (\s@CreateDiskFromSnapshot' {} a -> s {tags = a} :: CreateDiskFromSnapshot) Core.. Lens.mapping Lens._Coerce

-- | The name of the disk snapshot (e.g., @my-snapshot@) from which to create
-- the new storage disk.
--
-- Constraint:
--
-- -   This parameter cannot be defined together with the
--     @source disk name@ parameter. The @disk snapshot name@ and
--     @source disk name@ parameters are mutually exclusive.
createDiskFromSnapshot_diskSnapshotName :: Lens.Lens' CreateDiskFromSnapshot (Core.Maybe Core.Text)
createDiskFromSnapshot_diskSnapshotName = Lens.lens (\CreateDiskFromSnapshot' {diskSnapshotName} -> diskSnapshotName) (\s@CreateDiskFromSnapshot' {} a -> s {diskSnapshotName = a} :: CreateDiskFromSnapshot)

-- | The unique Lightsail disk name (e.g., @my-disk@).
createDiskFromSnapshot_diskName :: Lens.Lens' CreateDiskFromSnapshot Core.Text
createDiskFromSnapshot_diskName = Lens.lens (\CreateDiskFromSnapshot' {diskName} -> diskName) (\s@CreateDiskFromSnapshot' {} a -> s {diskName = a} :: CreateDiskFromSnapshot)

-- | The Availability Zone where you want to create the disk (e.g.,
-- @us-east-2a@). Choose the same Availability Zone as the Lightsail
-- instance where you want to create the disk.
--
-- Use the GetRegions operation to list the Availability Zones where
-- Lightsail is currently available.
createDiskFromSnapshot_availabilityZone :: Lens.Lens' CreateDiskFromSnapshot Core.Text
createDiskFromSnapshot_availabilityZone = Lens.lens (\CreateDiskFromSnapshot' {availabilityZone} -> availabilityZone) (\s@CreateDiskFromSnapshot' {} a -> s {availabilityZone = a} :: CreateDiskFromSnapshot)

-- | The size of the disk in GB (e.g., @32@).
createDiskFromSnapshot_sizeInGb :: Lens.Lens' CreateDiskFromSnapshot Core.Int
createDiskFromSnapshot_sizeInGb = Lens.lens (\CreateDiskFromSnapshot' {sizeInGb} -> sizeInGb) (\s@CreateDiskFromSnapshot' {} a -> s {sizeInGb = a} :: CreateDiskFromSnapshot)

instance Core.AWSRequest CreateDiskFromSnapshot where
  type
    AWSResponse CreateDiskFromSnapshot =
      CreateDiskFromSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDiskFromSnapshotResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDiskFromSnapshot

instance Core.NFData CreateDiskFromSnapshot

instance Core.ToHeaders CreateDiskFromSnapshot where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateDiskFromSnapshot" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDiskFromSnapshot where
  toJSON CreateDiskFromSnapshot' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sourceDiskName" Core..=) Core.<$> sourceDiskName,
            ("restoreDate" Core..=) Core.<$> restoreDate,
            ("addOns" Core..=) Core.<$> addOns,
            ("useLatestRestorableAutoSnapshot" Core..=)
              Core.<$> useLatestRestorableAutoSnapshot,
            ("tags" Core..=) Core.<$> tags,
            ("diskSnapshotName" Core..=)
              Core.<$> diskSnapshotName,
            Core.Just ("diskName" Core..= diskName),
            Core.Just
              ("availabilityZone" Core..= availabilityZone),
            Core.Just ("sizeInGb" Core..= sizeInGb)
          ]
      )

instance Core.ToPath CreateDiskFromSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery CreateDiskFromSnapshot where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDiskFromSnapshotResponse' smart constructor.
data CreateDiskFromSnapshotResponse = CreateDiskFromSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDiskFromSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createDiskFromSnapshotResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createDiskFromSnapshotResponse_httpStatus' - The response's http status code.
newCreateDiskFromSnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDiskFromSnapshotResponse
newCreateDiskFromSnapshotResponse pHttpStatus_ =
  CreateDiskFromSnapshotResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createDiskFromSnapshotResponse_operations :: Lens.Lens' CreateDiskFromSnapshotResponse (Core.Maybe [Operation])
createDiskFromSnapshotResponse_operations = Lens.lens (\CreateDiskFromSnapshotResponse' {operations} -> operations) (\s@CreateDiskFromSnapshotResponse' {} a -> s {operations = a} :: CreateDiskFromSnapshotResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createDiskFromSnapshotResponse_httpStatus :: Lens.Lens' CreateDiskFromSnapshotResponse Core.Int
createDiskFromSnapshotResponse_httpStatus = Lens.lens (\CreateDiskFromSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateDiskFromSnapshotResponse' {} a -> s {httpStatus = a} :: CreateDiskFromSnapshotResponse)

instance Core.NFData CreateDiskFromSnapshotResponse
