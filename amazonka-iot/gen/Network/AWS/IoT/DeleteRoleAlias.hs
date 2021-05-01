{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRoleAlias' smart constructor.
data DeleteRoleAlias = DeleteRoleAlias'
  { -- | The role alias to delete.
    roleAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteRoleAlias
newDeleteRoleAlias pRoleAlias_ =
  DeleteRoleAlias' {roleAlias = pRoleAlias_}

-- | The role alias to delete.
deleteRoleAlias_roleAlias :: Lens.Lens' DeleteRoleAlias Prelude.Text
deleteRoleAlias_roleAlias = Lens.lens (\DeleteRoleAlias' {roleAlias} -> roleAlias) (\s@DeleteRoleAlias' {} a -> s {roleAlias = a} :: DeleteRoleAlias)

instance Prelude.AWSRequest DeleteRoleAlias where
  type Rs DeleteRoleAlias = DeleteRoleAliasResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRoleAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRoleAlias

instance Prelude.NFData DeleteRoleAlias

instance Prelude.ToHeaders DeleteRoleAlias where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteRoleAlias where
  toPath DeleteRoleAlias' {..} =
    Prelude.mconcat
      ["/role-aliases/", Prelude.toBS roleAlias]

instance Prelude.ToQuery DeleteRoleAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRoleAliasResponse' smart constructor.
data DeleteRoleAliasResponse = DeleteRoleAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteRoleAliasResponse
newDeleteRoleAliasResponse pHttpStatus_ =
  DeleteRoleAliasResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteRoleAliasResponse_httpStatus :: Lens.Lens' DeleteRoleAliasResponse Prelude.Int
deleteRoleAliasResponse_httpStatus = Lens.lens (\DeleteRoleAliasResponse' {httpStatus} -> httpStatus) (\s@DeleteRoleAliasResponse' {} a -> s {httpStatus = a} :: DeleteRoleAliasResponse)

instance Prelude.NFData DeleteRoleAliasResponse
