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
-- Module      : Amazonka.Lightsail.DeleteDisk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified block storage disk. The disk must be in the
-- @available@ state (not attached to a Lightsail instance).
--
-- The disk may remain in the @deleting@ state for several minutes.
--
-- The @delete disk@ operation supports tag-based access control via
-- resource tags applied to the resource identified by @disk name@. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.DeleteDisk
  ( -- * Creating a Request
    DeleteDisk (..),
    newDeleteDisk,

    -- * Request Lenses
    deleteDisk_forceDeleteAddOns,
    deleteDisk_diskName,

    -- * Destructuring the Response
    DeleteDiskResponse (..),
    newDeleteDiskResponse,

    -- * Response Lenses
    deleteDiskResponse_operations,
    deleteDiskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDisk' smart constructor.
data DeleteDisk = DeleteDisk'
  { -- | A Boolean value to indicate whether to delete all add-ons for the disk.
    forceDeleteAddOns :: Prelude.Maybe Prelude.Bool,
    -- | The unique name of the disk you want to delete (e.g., @my-disk@).
    diskName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDisk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceDeleteAddOns', 'deleteDisk_forceDeleteAddOns' - A Boolean value to indicate whether to delete all add-ons for the disk.
--
-- 'diskName', 'deleteDisk_diskName' - The unique name of the disk you want to delete (e.g., @my-disk@).
newDeleteDisk ::
  -- | 'diskName'
  Prelude.Text ->
  DeleteDisk
newDeleteDisk pDiskName_ =
  DeleteDisk'
    { forceDeleteAddOns = Prelude.Nothing,
      diskName = pDiskName_
    }

-- | A Boolean value to indicate whether to delete all add-ons for the disk.
deleteDisk_forceDeleteAddOns :: Lens.Lens' DeleteDisk (Prelude.Maybe Prelude.Bool)
deleteDisk_forceDeleteAddOns = Lens.lens (\DeleteDisk' {forceDeleteAddOns} -> forceDeleteAddOns) (\s@DeleteDisk' {} a -> s {forceDeleteAddOns = a} :: DeleteDisk)

-- | The unique name of the disk you want to delete (e.g., @my-disk@).
deleteDisk_diskName :: Lens.Lens' DeleteDisk Prelude.Text
deleteDisk_diskName = Lens.lens (\DeleteDisk' {diskName} -> diskName) (\s@DeleteDisk' {} a -> s {diskName = a} :: DeleteDisk)

instance Core.AWSRequest DeleteDisk where
  type AWSResponse DeleteDisk = DeleteDiskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDiskResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDisk where
  hashWithSalt _salt DeleteDisk' {..} =
    _salt `Prelude.hashWithSalt` forceDeleteAddOns
      `Prelude.hashWithSalt` diskName

instance Prelude.NFData DeleteDisk where
  rnf DeleteDisk' {..} =
    Prelude.rnf forceDeleteAddOns
      `Prelude.seq` Prelude.rnf diskName

instance Data.ToHeaders DeleteDisk where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.DeleteDisk" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDisk where
  toJSON DeleteDisk' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("forceDeleteAddOns" Data..=)
              Prelude.<$> forceDeleteAddOns,
            Prelude.Just ("diskName" Data..= diskName)
          ]
      )

instance Data.ToPath DeleteDisk where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDisk where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDiskResponse' smart constructor.
data DeleteDiskResponse = DeleteDiskResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDiskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteDiskResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteDiskResponse_httpStatus' - The response's http status code.
newDeleteDiskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDiskResponse
newDeleteDiskResponse pHttpStatus_ =
  DeleteDiskResponse'
    { operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteDiskResponse_operations :: Lens.Lens' DeleteDiskResponse (Prelude.Maybe [Operation])
deleteDiskResponse_operations = Lens.lens (\DeleteDiskResponse' {operations} -> operations) (\s@DeleteDiskResponse' {} a -> s {operations = a} :: DeleteDiskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteDiskResponse_httpStatus :: Lens.Lens' DeleteDiskResponse Prelude.Int
deleteDiskResponse_httpStatus = Lens.lens (\DeleteDiskResponse' {httpStatus} -> httpStatus) (\s@DeleteDiskResponse' {} a -> s {httpStatus = a} :: DeleteDiskResponse)

instance Prelude.NFData DeleteDiskResponse where
  rnf DeleteDiskResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
