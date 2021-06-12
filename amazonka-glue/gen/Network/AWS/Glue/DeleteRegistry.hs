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
-- Module      : Network.AWS.Glue.DeleteRegistry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the entire registry including schema and all of its versions. To
-- get the status of the delete operation, you can call the @GetRegistry@
-- API after the asynchronous call. Deleting a registry will disable all
-- online operations for the registry such as the @UpdateRegistry@,
-- @CreateSchema@, @UpdateSchema@, and @RegisterSchemaVersion@ APIs.
module Network.AWS.Glue.DeleteRegistry
  ( -- * Creating a Request
    DeleteRegistry (..),
    newDeleteRegistry,

    -- * Request Lenses
    deleteRegistry_registryId,

    -- * Destructuring the Response
    DeleteRegistryResponse (..),
    newDeleteRegistryResponse,

    -- * Response Lenses
    deleteRegistryResponse_status,
    deleteRegistryResponse_registryName,
    deleteRegistryResponse_registryArn,
    deleteRegistryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRegistry' smart constructor.
data DeleteRegistry = DeleteRegistry'
  { -- | This is a wrapper structure that may contain the registry name and
    -- Amazon Resource Name (ARN).
    registryId :: RegistryId
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRegistry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'deleteRegistry_registryId' - This is a wrapper structure that may contain the registry name and
-- Amazon Resource Name (ARN).
newDeleteRegistry ::
  -- | 'registryId'
  RegistryId ->
  DeleteRegistry
newDeleteRegistry pRegistryId_ =
  DeleteRegistry' {registryId = pRegistryId_}

-- | This is a wrapper structure that may contain the registry name and
-- Amazon Resource Name (ARN).
deleteRegistry_registryId :: Lens.Lens' DeleteRegistry RegistryId
deleteRegistry_registryId = Lens.lens (\DeleteRegistry' {registryId} -> registryId) (\s@DeleteRegistry' {} a -> s {registryId = a} :: DeleteRegistry)

instance Core.AWSRequest DeleteRegistry where
  type
    AWSResponse DeleteRegistry =
      DeleteRegistryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRegistryResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "RegistryName")
            Core.<*> (x Core..?> "RegistryArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRegistry

instance Core.NFData DeleteRegistry

instance Core.ToHeaders DeleteRegistry where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.DeleteRegistry" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRegistry where
  toJSON DeleteRegistry' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RegistryId" Core..= registryId)]
      )

instance Core.ToPath DeleteRegistry where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRegistry where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteRegistryResponse' smart constructor.
data DeleteRegistryResponse = DeleteRegistryResponse'
  { -- | The status of the registry. A successful operation will return the
    -- @Deleting@ status.
    status :: Core.Maybe RegistryStatus,
    -- | The name of the registry being deleted.
    registryName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the registry being deleted.
    registryArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRegistryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'deleteRegistryResponse_status' - The status of the registry. A successful operation will return the
-- @Deleting@ status.
--
-- 'registryName', 'deleteRegistryResponse_registryName' - The name of the registry being deleted.
--
-- 'registryArn', 'deleteRegistryResponse_registryArn' - The Amazon Resource Name (ARN) of the registry being deleted.
--
-- 'httpStatus', 'deleteRegistryResponse_httpStatus' - The response's http status code.
newDeleteRegistryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteRegistryResponse
newDeleteRegistryResponse pHttpStatus_ =
  DeleteRegistryResponse'
    { status = Core.Nothing,
      registryName = Core.Nothing,
      registryArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the registry. A successful operation will return the
-- @Deleting@ status.
deleteRegistryResponse_status :: Lens.Lens' DeleteRegistryResponse (Core.Maybe RegistryStatus)
deleteRegistryResponse_status = Lens.lens (\DeleteRegistryResponse' {status} -> status) (\s@DeleteRegistryResponse' {} a -> s {status = a} :: DeleteRegistryResponse)

-- | The name of the registry being deleted.
deleteRegistryResponse_registryName :: Lens.Lens' DeleteRegistryResponse (Core.Maybe Core.Text)
deleteRegistryResponse_registryName = Lens.lens (\DeleteRegistryResponse' {registryName} -> registryName) (\s@DeleteRegistryResponse' {} a -> s {registryName = a} :: DeleteRegistryResponse)

-- | The Amazon Resource Name (ARN) of the registry being deleted.
deleteRegistryResponse_registryArn :: Lens.Lens' DeleteRegistryResponse (Core.Maybe Core.Text)
deleteRegistryResponse_registryArn = Lens.lens (\DeleteRegistryResponse' {registryArn} -> registryArn) (\s@DeleteRegistryResponse' {} a -> s {registryArn = a} :: DeleteRegistryResponse)

-- | The response's http status code.
deleteRegistryResponse_httpStatus :: Lens.Lens' DeleteRegistryResponse Core.Int
deleteRegistryResponse_httpStatus = Lens.lens (\DeleteRegistryResponse' {httpStatus} -> httpStatus) (\s@DeleteRegistryResponse' {} a -> s {httpStatus = a} :: DeleteRegistryResponse)

instance Core.NFData DeleteRegistryResponse
