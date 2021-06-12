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
-- Module      : Network.AWS.IoT.DeleteRoleAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a role alias
module Network.AWS.IoT.DeleteRoleAlias
  ( -- * Creating a Request
    DeleteRoleAlias (..),
    newDeleteRoleAlias,

    -- * Request Lenses
    deleteRoleAlias_roleAlias,

    -- * Destructuring the Response
    DeleteRoleAliasResponse (..),
    newDeleteRoleAliasResponse,

    -- * Response Lenses
    deleteRoleAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRoleAlias' smart constructor.
data DeleteRoleAlias = DeleteRoleAlias'
  { -- | The role alias to delete.
    roleAlias :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRoleAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleAlias', 'deleteRoleAlias_roleAlias' - The role alias to delete.
newDeleteRoleAlias ::
  -- | 'roleAlias'
  Core.Text ->
  DeleteRoleAlias
newDeleteRoleAlias pRoleAlias_ =
  DeleteRoleAlias' {roleAlias = pRoleAlias_}

-- | The role alias to delete.
deleteRoleAlias_roleAlias :: Lens.Lens' DeleteRoleAlias Core.Text
deleteRoleAlias_roleAlias = Lens.lens (\DeleteRoleAlias' {roleAlias} -> roleAlias) (\s@DeleteRoleAlias' {} a -> s {roleAlias = a} :: DeleteRoleAlias)

instance Core.AWSRequest DeleteRoleAlias where
  type
    AWSResponse DeleteRoleAlias =
      DeleteRoleAliasResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRoleAliasResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRoleAlias

instance Core.NFData DeleteRoleAlias

instance Core.ToHeaders DeleteRoleAlias where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteRoleAlias where
  toPath DeleteRoleAlias' {..} =
    Core.mconcat
      ["/role-aliases/", Core.toBS roleAlias]

instance Core.ToQuery DeleteRoleAlias where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteRoleAliasResponse' smart constructor.
data DeleteRoleAliasResponse = DeleteRoleAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRoleAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRoleAliasResponse_httpStatus' - The response's http status code.
newDeleteRoleAliasResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteRoleAliasResponse
newDeleteRoleAliasResponse pHttpStatus_ =
  DeleteRoleAliasResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteRoleAliasResponse_httpStatus :: Lens.Lens' DeleteRoleAliasResponse Core.Int
deleteRoleAliasResponse_httpStatus = Lens.lens (\DeleteRoleAliasResponse' {httpStatus} -> httpStatus) (\s@DeleteRoleAliasResponse' {} a -> s {httpStatus = a} :: DeleteRoleAliasResponse)

instance Core.NFData DeleteRoleAliasResponse
