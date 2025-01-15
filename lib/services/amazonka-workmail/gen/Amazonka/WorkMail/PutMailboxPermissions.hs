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
-- Module      : Amazonka.WorkMail.PutMailboxPermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets permissions for a user, group, or resource. This replaces any
-- pre-existing permissions.
module Amazonka.WorkMail.PutMailboxPermissions
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newPutMailboxPermissions' smart constructor.
data PutMailboxPermissions = PutMailboxPermissions'
  { -- | The identifier of the organization under which the user, group, or
    -- resource exists.
    organizationId :: Prelude.Text,
    -- | The identifier of the user, group, or resource for which to update
    -- mailbox permissions.
    entityId :: Prelude.Text,
    -- | The identifier of the user, group, or resource to which to grant the
    -- permissions.
    granteeId :: Prelude.Text,
    -- | The permissions granted to the grantee. SEND_AS allows the grantee to
    -- send email as the owner of the mailbox (the grantee is not mentioned on
    -- these emails). SEND_ON_BEHALF allows the grantee to send email on behalf
    -- of the owner of the mailbox (the grantee is not mentioned as the
    -- physical sender of these emails). FULL_ACCESS allows the grantee full
    -- access to the mailbox, irrespective of other folder-level permissions
    -- set on the mailbox.
    permissionValues :: [PermissionType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  -- | 'granteeId'
  Prelude.Text ->
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
        permissionValues = Prelude.mempty
      }

-- | The identifier of the organization under which the user, group, or
-- resource exists.
putMailboxPermissions_organizationId :: Lens.Lens' PutMailboxPermissions Prelude.Text
putMailboxPermissions_organizationId = Lens.lens (\PutMailboxPermissions' {organizationId} -> organizationId) (\s@PutMailboxPermissions' {} a -> s {organizationId = a} :: PutMailboxPermissions)

-- | The identifier of the user, group, or resource for which to update
-- mailbox permissions.
putMailboxPermissions_entityId :: Lens.Lens' PutMailboxPermissions Prelude.Text
putMailboxPermissions_entityId = Lens.lens (\PutMailboxPermissions' {entityId} -> entityId) (\s@PutMailboxPermissions' {} a -> s {entityId = a} :: PutMailboxPermissions)

-- | The identifier of the user, group, or resource to which to grant the
-- permissions.
putMailboxPermissions_granteeId :: Lens.Lens' PutMailboxPermissions Prelude.Text
putMailboxPermissions_granteeId = Lens.lens (\PutMailboxPermissions' {granteeId} -> granteeId) (\s@PutMailboxPermissions' {} a -> s {granteeId = a} :: PutMailboxPermissions)

-- | The permissions granted to the grantee. SEND_AS allows the grantee to
-- send email as the owner of the mailbox (the grantee is not mentioned on
-- these emails). SEND_ON_BEHALF allows the grantee to send email on behalf
-- of the owner of the mailbox (the grantee is not mentioned as the
-- physical sender of these emails). FULL_ACCESS allows the grantee full
-- access to the mailbox, irrespective of other folder-level permissions
-- set on the mailbox.
putMailboxPermissions_permissionValues :: Lens.Lens' PutMailboxPermissions [PermissionType]
putMailboxPermissions_permissionValues = Lens.lens (\PutMailboxPermissions' {permissionValues} -> permissionValues) (\s@PutMailboxPermissions' {} a -> s {permissionValues = a} :: PutMailboxPermissions) Prelude.. Lens.coerced

instance Core.AWSRequest PutMailboxPermissions where
  type
    AWSResponse PutMailboxPermissions =
      PutMailboxPermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutMailboxPermissionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutMailboxPermissions where
  hashWithSalt _salt PutMailboxPermissions' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` granteeId
      `Prelude.hashWithSalt` permissionValues

instance Prelude.NFData PutMailboxPermissions where
  rnf PutMailboxPermissions' {..} =
    Prelude.rnf organizationId `Prelude.seq`
      Prelude.rnf entityId `Prelude.seq`
        Prelude.rnf granteeId `Prelude.seq`
          Prelude.rnf permissionValues

instance Data.ToHeaders PutMailboxPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.PutMailboxPermissions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutMailboxPermissions where
  toJSON PutMailboxPermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("EntityId" Data..= entityId),
            Prelude.Just ("GranteeId" Data..= granteeId),
            Prelude.Just
              ("PermissionValues" Data..= permissionValues)
          ]
      )

instance Data.ToPath PutMailboxPermissions where
  toPath = Prelude.const "/"

instance Data.ToQuery PutMailboxPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutMailboxPermissionsResponse' smart constructor.
data PutMailboxPermissionsResponse = PutMailboxPermissionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutMailboxPermissionsResponse
newPutMailboxPermissionsResponse pHttpStatus_ =
  PutMailboxPermissionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putMailboxPermissionsResponse_httpStatus :: Lens.Lens' PutMailboxPermissionsResponse Prelude.Int
putMailboxPermissionsResponse_httpStatus = Lens.lens (\PutMailboxPermissionsResponse' {httpStatus} -> httpStatus) (\s@PutMailboxPermissionsResponse' {} a -> s {httpStatus = a} :: PutMailboxPermissionsResponse)

instance Prelude.NFData PutMailboxPermissionsResponse where
  rnf PutMailboxPermissionsResponse' {..} =
    Prelude.rnf httpStatus
