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
-- Module      : Network.AWS.WorkMail.PutMailboxPermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets permissions for a user, group, or resource. This replaces any
-- pre-existing permissions.
module Network.AWS.WorkMail.PutMailboxPermissions
  ( -- * Creating a Request
    PutMailboxPermissions (..),
    newPutMailboxPermissions,

    -- * Request Lenses
    putMailboxPermissions_organizationId,
    putMailboxPermissions_entityId,
    putMailboxPermissions_granteeId,
    putMailboxPermissions_permissionValues,

    -- * Destructuring the Response
    PutMailboxPermissionsResponse (..),
    newPutMailboxPermissionsResponse,

    -- * Response Lenses
    putMailboxPermissionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newPutMailboxPermissions' smart constructor.
data PutMailboxPermissions = PutMailboxPermissions'
  { -- | The identifier of the organization under which the user, group, or
    -- resource exists.
    organizationId :: Core.Text,
    -- | The identifier of the user, group, or resource for which to update
    -- mailbox permissions.
    entityId :: Core.Text,
    -- | The identifier of the user, group, or resource to which to grant the
    -- permissions.
    granteeId :: Core.Text,
    -- | The permissions granted to the grantee. SEND_AS allows the grantee to
    -- send email as the owner of the mailbox (the grantee is not mentioned on
    -- these emails). SEND_ON_BEHALF allows the grantee to send email on behalf
    -- of the owner of the mailbox (the grantee is not mentioned as the
    -- physical sender of these emails). FULL_ACCESS allows the grantee full
    -- access to the mailbox, irrespective of other folder-level permissions
    -- set on the mailbox.
    permissionValues :: [PermissionType]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutMailboxPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'putMailboxPermissions_organizationId' - The identifier of the organization under which the user, group, or
-- resource exists.
--
-- 'entityId', 'putMailboxPermissions_entityId' - The identifier of the user, group, or resource for which to update
-- mailbox permissions.
--
-- 'granteeId', 'putMailboxPermissions_granteeId' - The identifier of the user, group, or resource to which to grant the
-- permissions.
--
-- 'permissionValues', 'putMailboxPermissions_permissionValues' - The permissions granted to the grantee. SEND_AS allows the grantee to
-- send email as the owner of the mailbox (the grantee is not mentioned on
-- these emails). SEND_ON_BEHALF allows the grantee to send email on behalf
-- of the owner of the mailbox (the grantee is not mentioned as the
-- physical sender of these emails). FULL_ACCESS allows the grantee full
-- access to the mailbox, irrespective of other folder-level permissions
-- set on the mailbox.
newPutMailboxPermissions ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'entityId'
  Core.Text ->
  -- | 'granteeId'
  Core.Text ->
  PutMailboxPermissions
newPutMailboxPermissions
  pOrganizationId_
  pEntityId_
  pGranteeId_ =
    PutMailboxPermissions'
      { organizationId =
          pOrganizationId_,
        entityId = pEntityId_,
        granteeId = pGranteeId_,
        permissionValues = Core.mempty
      }

-- | The identifier of the organization under which the user, group, or
-- resource exists.
putMailboxPermissions_organizationId :: Lens.Lens' PutMailboxPermissions Core.Text
putMailboxPermissions_organizationId = Lens.lens (\PutMailboxPermissions' {organizationId} -> organizationId) (\s@PutMailboxPermissions' {} a -> s {organizationId = a} :: PutMailboxPermissions)

-- | The identifier of the user, group, or resource for which to update
-- mailbox permissions.
putMailboxPermissions_entityId :: Lens.Lens' PutMailboxPermissions Core.Text
putMailboxPermissions_entityId = Lens.lens (\PutMailboxPermissions' {entityId} -> entityId) (\s@PutMailboxPermissions' {} a -> s {entityId = a} :: PutMailboxPermissions)

-- | The identifier of the user, group, or resource to which to grant the
-- permissions.
putMailboxPermissions_granteeId :: Lens.Lens' PutMailboxPermissions Core.Text
putMailboxPermissions_granteeId = Lens.lens (\PutMailboxPermissions' {granteeId} -> granteeId) (\s@PutMailboxPermissions' {} a -> s {granteeId = a} :: PutMailboxPermissions)

-- | The permissions granted to the grantee. SEND_AS allows the grantee to
-- send email as the owner of the mailbox (the grantee is not mentioned on
-- these emails). SEND_ON_BEHALF allows the grantee to send email on behalf
-- of the owner of the mailbox (the grantee is not mentioned as the
-- physical sender of these emails). FULL_ACCESS allows the grantee full
-- access to the mailbox, irrespective of other folder-level permissions
-- set on the mailbox.
putMailboxPermissions_permissionValues :: Lens.Lens' PutMailboxPermissions [PermissionType]
putMailboxPermissions_permissionValues = Lens.lens (\PutMailboxPermissions' {permissionValues} -> permissionValues) (\s@PutMailboxPermissions' {} a -> s {permissionValues = a} :: PutMailboxPermissions) Core.. Lens._Coerce

instance Core.AWSRequest PutMailboxPermissions where
  type
    AWSResponse PutMailboxPermissions =
      PutMailboxPermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutMailboxPermissionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutMailboxPermissions

instance Core.NFData PutMailboxPermissions

instance Core.ToHeaders PutMailboxPermissions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.PutMailboxPermissions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutMailboxPermissions where
  toJSON PutMailboxPermissions' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId),
            Core.Just ("GranteeId" Core..= granteeId),
            Core.Just
              ("PermissionValues" Core..= permissionValues)
          ]
      )

instance Core.ToPath PutMailboxPermissions where
  toPath = Core.const "/"

instance Core.ToQuery PutMailboxPermissions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutMailboxPermissionsResponse' smart constructor.
data PutMailboxPermissionsResponse = PutMailboxPermissionsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutMailboxPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putMailboxPermissionsResponse_httpStatus' - The response's http status code.
newPutMailboxPermissionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutMailboxPermissionsResponse
newPutMailboxPermissionsResponse pHttpStatus_ =
  PutMailboxPermissionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putMailboxPermissionsResponse_httpStatus :: Lens.Lens' PutMailboxPermissionsResponse Core.Int
putMailboxPermissionsResponse_httpStatus = Lens.lens (\PutMailboxPermissionsResponse' {httpStatus} -> httpStatus) (\s@PutMailboxPermissionsResponse' {} a -> s {httpStatus = a} :: PutMailboxPermissionsResponse)

instance Core.NFData PutMailboxPermissionsResponse
