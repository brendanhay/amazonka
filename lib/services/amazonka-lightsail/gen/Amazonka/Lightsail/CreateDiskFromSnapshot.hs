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
-- Module      : Amazonka.Lightsail.CreateDiskFromSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.CreateDiskFromSnapshot
  ( -- * Creating a Request
    CreateDiskFromSnapshot (..),
    newCreateDiskFromSnapshot,

    -- * Request Lenses
    createDiskFromSnapshot_addOns,
    createDiskFromSnapshot_diskSnapshotName,
    createDiskFromSnapshot_restoreDate,
    createDiskFromSnapshot_sourceDiskName,
    createDiskFromSnapshot_tags,
    createDiskFromSnapshot_useLatestRestorableAutoSnapshot,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDiskFromSnapshot' smart constructor.
data CreateDiskFromSnapshot = CreateDiskFromSnapshot'
  { -- | An array of objects that represent the add-ons to enable for the new
    -- disk.
    addOns :: Prelude.Maybe [AddOnRequest],
    -- | The name of the disk snapshot (e.g., @my-snapshot@) from which to create
    -- the new storage disk.
    --
    -- Constraint:
    --
    -- -   This parameter cannot be defined together with the
    --     @source disk name@ parameter. The @disk snapshot name@ and
    --     @source disk name@ parameters are mutually exclusive.
    diskSnapshotName :: Prelude.Maybe Prelude.Text,
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
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Amazon Lightsail Developer Guide>.
    restoreDate :: Prelude.Maybe Prelude.Text,
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
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Amazon Lightsail Developer Guide>.
    sourceDiskName :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Prelude.Maybe [Tag],
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
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Amazon Lightsail Developer Guide>.
    useLatestRestorableAutoSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The unique Lightsail disk name (e.g., @my-disk@).
    diskName :: Prelude.Text,
    -- | The Availability Zone where you want to create the disk (e.g.,
    -- @us-east-2a@). Choose the same Availability Zone as the Lightsail
    -- instance where you want to create the disk.
    --
    -- Use the GetRegions operation to list the Availability Zones where
    -- Lightsail is currently available.
    availabilityZone :: Prelude.Text,
    -- | The size of the disk in GB (e.g., @32@).
    sizeInGb :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDiskFromSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addOns', 'createDiskFromSnapshot_addOns' - An array of objects that represent the add-ons to enable for the new
-- disk.
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
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Amazon Lightsail Developer Guide>.
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
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Amazon Lightsail Developer Guide>.
--
-- 'tags', 'createDiskFromSnapshot_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
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
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Amazon Lightsail Developer Guide>.
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
  Prelude.Text ->
  -- | 'availabilityZone'
  Prelude.Text ->
  -- | 'sizeInGb'
  Prelude.Int ->
  CreateDiskFromSnapshot
newCreateDiskFromSnapshot
  pDiskName_
  pAvailabilityZone_
  pSizeInGb_ =
    CreateDiskFromSnapshot'
      { addOns = Prelude.Nothing,
        diskSnapshotName = Prelude.Nothing,
        restoreDate = Prelude.Nothing,
        sourceDiskName = Prelude.Nothing,
        tags = Prelude.Nothing,
        useLatestRestorableAutoSnapshot = Prelude.Nothing,
        diskName = pDiskName_,
        availabilityZone = pAvailabilityZone_,
        sizeInGb = pSizeInGb_
      }

-- | An array of objects that represent the add-ons to enable for the new
-- disk.
createDiskFromSnapshot_addOns :: Lens.Lens' CreateDiskFromSnapshot (Prelude.Maybe [AddOnRequest])
createDiskFromSnapshot_addOns = Lens.lens (\CreateDiskFromSnapshot' {addOns} -> addOns) (\s@CreateDiskFromSnapshot' {} a -> s {addOns = a} :: CreateDiskFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The name of the disk snapshot (e.g., @my-snapshot@) from which to create
-- the new storage disk.
--
-- Constraint:
--
-- -   This parameter cannot be defined together with the
--     @source disk name@ parameter. The @disk snapshot name@ and
--     @source disk name@ parameters are mutually exclusive.
createDiskFromSnapshot_diskSnapshotName :: Lens.Lens' CreateDiskFromSnapshot (Prelude.Maybe Prelude.Text)
createDiskFromSnapshot_diskSnapshotName = Lens.lens (\CreateDiskFromSnapshot' {diskSnapshotName} -> diskSnapshotName) (\s@CreateDiskFromSnapshot' {} a -> s {diskSnapshotName = a} :: CreateDiskFromSnapshot)

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
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Amazon Lightsail Developer Guide>.
createDiskFromSnapshot_restoreDate :: Lens.Lens' CreateDiskFromSnapshot (Prelude.Maybe Prelude.Text)
createDiskFromSnapshot_restoreDate = Lens.lens (\CreateDiskFromSnapshot' {restoreDate} -> restoreDate) (\s@CreateDiskFromSnapshot' {} a -> s {restoreDate = a} :: CreateDiskFromSnapshot)

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
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Amazon Lightsail Developer Guide>.
createDiskFromSnapshot_sourceDiskName :: Lens.Lens' CreateDiskFromSnapshot (Prelude.Maybe Prelude.Text)
createDiskFromSnapshot_sourceDiskName = Lens.lens (\CreateDiskFromSnapshot' {sourceDiskName} -> sourceDiskName) (\s@CreateDiskFromSnapshot' {} a -> s {sourceDiskName = a} :: CreateDiskFromSnapshot)

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createDiskFromSnapshot_tags :: Lens.Lens' CreateDiskFromSnapshot (Prelude.Maybe [Tag])
createDiskFromSnapshot_tags = Lens.lens (\CreateDiskFromSnapshot' {tags} -> tags) (\s@CreateDiskFromSnapshot' {} a -> s {tags = a} :: CreateDiskFromSnapshot) Prelude.. Lens.mapping Lens.coerced

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
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Amazon Lightsail Developer Guide>.
createDiskFromSnapshot_useLatestRestorableAutoSnapshot :: Lens.Lens' CreateDiskFromSnapshot (Prelude.Maybe Prelude.Bool)
createDiskFromSnapshot_useLatestRestorableAutoSnapshot = Lens.lens (\CreateDiskFromSnapshot' {useLatestRestorableAutoSnapshot} -> useLatestRestorableAutoSnapshot) (\s@CreateDiskFromSnapshot' {} a -> s {useLatestRestorableAutoSnapshot = a} :: CreateDiskFromSnapshot)

-- | The unique Lightsail disk name (e.g., @my-disk@).
createDiskFromSnapshot_diskName :: Lens.Lens' CreateDiskFromSnapshot Prelude.Text
createDiskFromSnapshot_diskName = Lens.lens (\CreateDiskFromSnapshot' {diskName} -> diskName) (\s@CreateDiskFromSnapshot' {} a -> s {diskName = a} :: CreateDiskFromSnapshot)

-- | The Availability Zone where you want to create the disk (e.g.,
-- @us-east-2a@). Choose the same Availability Zone as the Lightsail
-- instance where you want to create the disk.
--
-- Use the GetRegions operation to list the Availability Zones where
-- Lightsail is currently available.
createDiskFromSnapshot_availabilityZone :: Lens.Lens' CreateDiskFromSnapshot Prelude.Text
createDiskFromSnapshot_availabilityZone = Lens.lens (\CreateDiskFromSnapshot' {availabilityZone} -> availabilityZone) (\s@CreateDiskFromSnapshot' {} a -> s {availabilityZone = a} :: CreateDiskFromSnapshot)

-- | The size of the disk in GB (e.g., @32@).
createDiskFromSnapshot_sizeInGb :: Lens.Lens' CreateDiskFromSnapshot Prelude.Int
createDiskFromSnapshot_sizeInGb = Lens.lens (\CreateDiskFromSnapshot' {sizeInGb} -> sizeInGb) (\s@CreateDiskFromSnapshot' {} a -> s {sizeInGb = a} :: CreateDiskFromSnapshot)

instance Core.AWSRequest CreateDiskFromSnapshot where
  type
    AWSResponse CreateDiskFromSnapshot =
      CreateDiskFromSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDiskFromSnapshotResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDiskFromSnapshot where
  hashWithSalt _salt CreateDiskFromSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` addOns
      `Prelude.hashWithSalt` diskSnapshotName
      `Prelude.hashWithSalt` restoreDate
      `Prelude.hashWithSalt` sourceDiskName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` useLatestRestorableAutoSnapshot
      `Prelude.hashWithSalt` diskName
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` sizeInGb

instance Prelude.NFData CreateDiskFromSnapshot where
  rnf CreateDiskFromSnapshot' {..} =
    Prelude.rnf addOns
      `Prelude.seq` Prelude.rnf diskSnapshotName
      `Prelude.seq` Prelude.rnf restoreDate
      `Prelude.seq` Prelude.rnf sourceDiskName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf useLatestRestorableAutoSnapshot
      `Prelude.seq` Prelude.rnf diskName
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf sizeInGb

instance Data.ToHeaders CreateDiskFromSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.CreateDiskFromSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDiskFromSnapshot where
  toJSON CreateDiskFromSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addOns" Data..=) Prelude.<$> addOns,
            ("diskSnapshotName" Data..=)
              Prelude.<$> diskSnapshotName,
            ("restoreDate" Data..=) Prelude.<$> restoreDate,
            ("sourceDiskName" Data..=)
              Prelude.<$> sourceDiskName,
            ("tags" Data..=) Prelude.<$> tags,
            ("useLatestRestorableAutoSnapshot" Data..=)
              Prelude.<$> useLatestRestorableAutoSnapshot,
            Prelude.Just ("diskName" Data..= diskName),
            Prelude.Just
              ("availabilityZone" Data..= availabilityZone),
            Prelude.Just ("sizeInGb" Data..= sizeInGb)
          ]
      )

instance Data.ToPath CreateDiskFromSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDiskFromSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDiskFromSnapshotResponse' smart constructor.
data CreateDiskFromSnapshotResponse = CreateDiskFromSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateDiskFromSnapshotResponse
newCreateDiskFromSnapshotResponse pHttpStatus_ =
  CreateDiskFromSnapshotResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createDiskFromSnapshotResponse_operations :: Lens.Lens' CreateDiskFromSnapshotResponse (Prelude.Maybe [Operation])
createDiskFromSnapshotResponse_operations = Lens.lens (\CreateDiskFromSnapshotResponse' {operations} -> operations) (\s@CreateDiskFromSnapshotResponse' {} a -> s {operations = a} :: CreateDiskFromSnapshotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createDiskFromSnapshotResponse_httpStatus :: Lens.Lens' CreateDiskFromSnapshotResponse Prelude.Int
createDiskFromSnapshotResponse_httpStatus = Lens.lens (\CreateDiskFromSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateDiskFromSnapshotResponse' {} a -> s {httpStatus = a} :: CreateDiskFromSnapshotResponse)

instance
  Prelude.NFData
    CreateDiskFromSnapshotResponse
  where
  rnf CreateDiskFromSnapshotResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
