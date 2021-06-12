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
-- Module      : Network.AWS.WorkMail.DeleteAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove one or more specified aliases from a set of aliases for a given
-- user.
module Network.AWS.WorkMail.DeleteAlias
  ( -- * Creating a Request
    DeleteAlias (..),
    newDeleteAlias,

    -- * Request Lenses
    deleteAlias_organizationId,
    deleteAlias_entityId,
    deleteAlias_alias,

    -- * Destructuring the Response
    DeleteAliasResponse (..),
    newDeleteAliasResponse,

    -- * Response Lenses
    deleteAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDeleteAlias' smart constructor.
data DeleteAlias = DeleteAlias'
  { -- | The identifier for the organization under which the user exists.
    organizationId :: Core.Text,
    -- | The identifier for the member (user or group) from which to have the
    -- aliases removed.
    entityId :: Core.Text,
    -- | The aliases to be removed from the user\'s set of aliases. Duplicate
    -- entries in the list are collapsed into single entries (the list is
    -- transformed into a set).
    alias :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deleteAlias_organizationId' - The identifier for the organization under which the user exists.
--
-- 'entityId', 'deleteAlias_entityId' - The identifier for the member (user or group) from which to have the
-- aliases removed.
--
-- 'alias', 'deleteAlias_alias' - The aliases to be removed from the user\'s set of aliases. Duplicate
-- entries in the list are collapsed into single entries (the list is
-- transformed into a set).
newDeleteAlias ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'entityId'
  Core.Text ->
  -- | 'alias'
  Core.Text ->
  DeleteAlias
newDeleteAlias pOrganizationId_ pEntityId_ pAlias_ =
  DeleteAlias'
    { organizationId = pOrganizationId_,
      entityId = pEntityId_,
      alias = pAlias_
    }

-- | The identifier for the organization under which the user exists.
deleteAlias_organizationId :: Lens.Lens' DeleteAlias Core.Text
deleteAlias_organizationId = Lens.lens (\DeleteAlias' {organizationId} -> organizationId) (\s@DeleteAlias' {} a -> s {organizationId = a} :: DeleteAlias)

-- | The identifier for the member (user or group) from which to have the
-- aliases removed.
deleteAlias_entityId :: Lens.Lens' DeleteAlias Core.Text
deleteAlias_entityId = Lens.lens (\DeleteAlias' {entityId} -> entityId) (\s@DeleteAlias' {} a -> s {entityId = a} :: DeleteAlias)

-- | The aliases to be removed from the user\'s set of aliases. Duplicate
-- entries in the list are collapsed into single entries (the list is
-- transformed into a set).
deleteAlias_alias :: Lens.Lens' DeleteAlias Core.Text
deleteAlias_alias = Lens.lens (\DeleteAlias' {alias} -> alias) (\s@DeleteAlias' {} a -> s {alias = a} :: DeleteAlias)

instance Core.AWSRequest DeleteAlias where
  type AWSResponse DeleteAlias = DeleteAliasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAliasResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteAlias

instance Core.NFData DeleteAlias

instance Core.ToHeaders DeleteAlias where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("WorkMailService.DeleteAlias" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAlias where
  toJSON DeleteAlias' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId),
            Core.Just ("Alias" Core..= alias)
          ]
      )

instance Core.ToPath DeleteAlias where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAlias where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAliasResponse_httpStatus' - The response's http status code.
newDeleteAliasResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteAliasResponse
newDeleteAliasResponse pHttpStatus_ =
  DeleteAliasResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteAliasResponse_httpStatus :: Lens.Lens' DeleteAliasResponse Core.Int
deleteAliasResponse_httpStatus = Lens.lens (\DeleteAliasResponse' {httpStatus} -> httpStatus) (\s@DeleteAliasResponse' {} a -> s {httpStatus = a} :: DeleteAliasResponse)

instance Core.NFData DeleteAliasResponse
