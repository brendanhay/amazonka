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
-- Module      : Network.AWS.WorkMail.DeleteMailboxPermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes permissions granted to a member (user or group).
module Network.AWS.WorkMail.DeleteMailboxPermissions
  ( -- * Creating a Request
    DeleteMailboxPermissions (..),
    newDeleteMailboxPermissions,

    -- * Request Lenses
    deleteMailboxPermissions_organizationId,
    deleteMailboxPermissions_entityId,
    deleteMailboxPermissions_granteeId,

    -- * Destructuring the Response
    DeleteMailboxPermissionsResponse (..),
    newDeleteMailboxPermissionsResponse,

    -- * Response Lenses
    deleteMailboxPermissionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDeleteMailboxPermissions' smart constructor.
data DeleteMailboxPermissions = DeleteMailboxPermissions'
  { -- | The identifier of the organization under which the member (user or
    -- group) exists.
    organizationId :: Core.Text,
    -- | The identifier of the member (user or group) that owns the mailbox.
    entityId :: Core.Text,
    -- | The identifier of the member (user or group) for which to delete granted
    -- permissions.
    granteeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMailboxPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deleteMailboxPermissions_organizationId' - The identifier of the organization under which the member (user or
-- group) exists.
--
-- 'entityId', 'deleteMailboxPermissions_entityId' - The identifier of the member (user or group) that owns the mailbox.
--
-- 'granteeId', 'deleteMailboxPermissions_granteeId' - The identifier of the member (user or group) for which to delete granted
-- permissions.
newDeleteMailboxPermissions ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'entityId'
  Core.Text ->
  -- | 'granteeId'
  Core.Text ->
  DeleteMailboxPermissions
newDeleteMailboxPermissions
  pOrganizationId_
  pEntityId_
  pGranteeId_ =
    DeleteMailboxPermissions'
      { organizationId =
          pOrganizationId_,
        entityId = pEntityId_,
        granteeId = pGranteeId_
      }

-- | The identifier of the organization under which the member (user or
-- group) exists.
deleteMailboxPermissions_organizationId :: Lens.Lens' DeleteMailboxPermissions Core.Text
deleteMailboxPermissions_organizationId = Lens.lens (\DeleteMailboxPermissions' {organizationId} -> organizationId) (\s@DeleteMailboxPermissions' {} a -> s {organizationId = a} :: DeleteMailboxPermissions)

-- | The identifier of the member (user or group) that owns the mailbox.
deleteMailboxPermissions_entityId :: Lens.Lens' DeleteMailboxPermissions Core.Text
deleteMailboxPermissions_entityId = Lens.lens (\DeleteMailboxPermissions' {entityId} -> entityId) (\s@DeleteMailboxPermissions' {} a -> s {entityId = a} :: DeleteMailboxPermissions)

-- | The identifier of the member (user or group) for which to delete granted
-- permissions.
deleteMailboxPermissions_granteeId :: Lens.Lens' DeleteMailboxPermissions Core.Text
deleteMailboxPermissions_granteeId = Lens.lens (\DeleteMailboxPermissions' {granteeId} -> granteeId) (\s@DeleteMailboxPermissions' {} a -> s {granteeId = a} :: DeleteMailboxPermissions)

instance Core.AWSRequest DeleteMailboxPermissions where
  type
    AWSResponse DeleteMailboxPermissions =
      DeleteMailboxPermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMailboxPermissionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteMailboxPermissions

instance Core.NFData DeleteMailboxPermissions

instance Core.ToHeaders DeleteMailboxPermissions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.DeleteMailboxPermissions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteMailboxPermissions where
  toJSON DeleteMailboxPermissions' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId),
            Core.Just ("GranteeId" Core..= granteeId)
          ]
      )

instance Core.ToPath DeleteMailboxPermissions where
  toPath = Core.const "/"

instance Core.ToQuery DeleteMailboxPermissions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteMailboxPermissionsResponse' smart constructor.
data DeleteMailboxPermissionsResponse = DeleteMailboxPermissionsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMailboxPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMailboxPermissionsResponse_httpStatus' - The response's http status code.
newDeleteMailboxPermissionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteMailboxPermissionsResponse
newDeleteMailboxPermissionsResponse pHttpStatus_ =
  DeleteMailboxPermissionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteMailboxPermissionsResponse_httpStatus :: Lens.Lens' DeleteMailboxPermissionsResponse Core.Int
deleteMailboxPermissionsResponse_httpStatus = Lens.lens (\DeleteMailboxPermissionsResponse' {httpStatus} -> httpStatus) (\s@DeleteMailboxPermissionsResponse' {} a -> s {httpStatus = a} :: DeleteMailboxPermissionsResponse)

instance Core.NFData DeleteMailboxPermissionsResponse
