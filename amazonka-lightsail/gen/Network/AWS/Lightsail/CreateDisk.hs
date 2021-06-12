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
-- Module      : Network.AWS.Lightsail.CreateDisk
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a block storage disk that can be attached to an Amazon Lightsail
-- instance in the same Availability Zone (e.g., @us-east-2a@).
--
-- The @create disk@ operation supports tag-based access control via
-- request tags. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.CreateDisk
  ( -- * Creating a Request
    CreateDisk (..),
    newCreateDisk,

    -- * Request Lenses
    createDisk_addOns,
    createDisk_tags,
    createDisk_diskName,
    createDisk_availabilityZone,
    createDisk_sizeInGb,

    -- * Destructuring the Response
    CreateDiskResponse (..),
    newCreateDiskResponse,

    -- * Response Lenses
    createDiskResponse_operations,
    createDiskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDisk' smart constructor.
data CreateDisk = CreateDisk'
  { -- | An array of objects that represent the add-ons to enable for the new
    -- disk.
    addOns :: Core.Maybe [AddOnRequest],
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Core.Maybe [Tag],
    -- | The unique Lightsail disk name (e.g., @my-disk@).
    diskName :: Core.Text,
    -- | The Availability Zone where you want to create the disk (e.g.,
    -- @us-east-2a@). Use the same Availability Zone as the Lightsail instance
    -- to which you want to attach the disk.
    --
    -- Use the @get regions@ operation to list the Availability Zones where
    -- Lightsail is currently available.
    availabilityZone :: Core.Text,
    -- | The size of the disk in GB (e.g., @32@).
    sizeInGb :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addOns', 'createDisk_addOns' - An array of objects that represent the add-ons to enable for the new
-- disk.
--
-- 'tags', 'createDisk_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'diskName', 'createDisk_diskName' - The unique Lightsail disk name (e.g., @my-disk@).
--
-- 'availabilityZone', 'createDisk_availabilityZone' - The Availability Zone where you want to create the disk (e.g.,
-- @us-east-2a@). Use the same Availability Zone as the Lightsail instance
-- to which you want to attach the disk.
--
-- Use the @get regions@ operation to list the Availability Zones where
-- Lightsail is currently available.
--
-- 'sizeInGb', 'createDisk_sizeInGb' - The size of the disk in GB (e.g., @32@).
newCreateDisk ::
  -- | 'diskName'
  Core.Text ->
  -- | 'availabilityZone'
  Core.Text ->
  -- | 'sizeInGb'
  Core.Int ->
  CreateDisk
newCreateDisk
  pDiskName_
  pAvailabilityZone_
  pSizeInGb_ =
    CreateDisk'
      { addOns = Core.Nothing,
        tags = Core.Nothing,
        diskName = pDiskName_,
        availabilityZone = pAvailabilityZone_,
        sizeInGb = pSizeInGb_
      }

-- | An array of objects that represent the add-ons to enable for the new
-- disk.
createDisk_addOns :: Lens.Lens' CreateDisk (Core.Maybe [AddOnRequest])
createDisk_addOns = Lens.lens (\CreateDisk' {addOns} -> addOns) (\s@CreateDisk' {} a -> s {addOns = a} :: CreateDisk) Core.. Lens.mapping Lens._Coerce

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createDisk_tags :: Lens.Lens' CreateDisk (Core.Maybe [Tag])
createDisk_tags = Lens.lens (\CreateDisk' {tags} -> tags) (\s@CreateDisk' {} a -> s {tags = a} :: CreateDisk) Core.. Lens.mapping Lens._Coerce

-- | The unique Lightsail disk name (e.g., @my-disk@).
createDisk_diskName :: Lens.Lens' CreateDisk Core.Text
createDisk_diskName = Lens.lens (\CreateDisk' {diskName} -> diskName) (\s@CreateDisk' {} a -> s {diskName = a} :: CreateDisk)

-- | The Availability Zone where you want to create the disk (e.g.,
-- @us-east-2a@). Use the same Availability Zone as the Lightsail instance
-- to which you want to attach the disk.
--
-- Use the @get regions@ operation to list the Availability Zones where
-- Lightsail is currently available.
createDisk_availabilityZone :: Lens.Lens' CreateDisk Core.Text
createDisk_availabilityZone = Lens.lens (\CreateDisk' {availabilityZone} -> availabilityZone) (\s@CreateDisk' {} a -> s {availabilityZone = a} :: CreateDisk)

-- | The size of the disk in GB (e.g., @32@).
createDisk_sizeInGb :: Lens.Lens' CreateDisk Core.Int
createDisk_sizeInGb = Lens.lens (\CreateDisk' {sizeInGb} -> sizeInGb) (\s@CreateDisk' {} a -> s {sizeInGb = a} :: CreateDisk)

instance Core.AWSRequest CreateDisk where
  type AWSResponse CreateDisk = CreateDiskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDiskResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDisk

instance Core.NFData CreateDisk

instance Core.ToHeaders CreateDisk where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Lightsail_20161128.CreateDisk" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDisk where
  toJSON CreateDisk' {..} =
    Core.object
      ( Core.catMaybes
          [ ("addOns" Core..=) Core.<$> addOns,
            ("tags" Core..=) Core.<$> tags,
            Core.Just ("diskName" Core..= diskName),
            Core.Just
              ("availabilityZone" Core..= availabilityZone),
            Core.Just ("sizeInGb" Core..= sizeInGb)
          ]
      )

instance Core.ToPath CreateDisk where
  toPath = Core.const "/"

instance Core.ToQuery CreateDisk where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDiskResponse' smart constructor.
data CreateDiskResponse = CreateDiskResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDiskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createDiskResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createDiskResponse_httpStatus' - The response's http status code.
newCreateDiskResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDiskResponse
newCreateDiskResponse pHttpStatus_ =
  CreateDiskResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createDiskResponse_operations :: Lens.Lens' CreateDiskResponse (Core.Maybe [Operation])
createDiskResponse_operations = Lens.lens (\CreateDiskResponse' {operations} -> operations) (\s@CreateDiskResponse' {} a -> s {operations = a} :: CreateDiskResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createDiskResponse_httpStatus :: Lens.Lens' CreateDiskResponse Core.Int
createDiskResponse_httpStatus = Lens.lens (\CreateDiskResponse' {httpStatus} -> httpStatus) (\s@CreateDiskResponse' {} a -> s {httpStatus = a} :: CreateDiskResponse)

instance Core.NFData CreateDiskResponse
