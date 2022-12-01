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
-- Module      : Amazonka.IoT.DeleteRoleAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a role alias
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteRoleAlias>
-- action.
module Amazonka.IoT.DeleteRoleAlias
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRoleAlias' smart constructor.
data DeleteRoleAlias = DeleteRoleAlias'
  { -- | The role alias to delete.
    roleAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteRoleAlias where
  type
    AWSResponse DeleteRoleAlias =
      DeleteRoleAliasResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRoleAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRoleAlias where
  hashWithSalt _salt DeleteRoleAlias' {..} =
    _salt `Prelude.hashWithSalt` roleAlias

instance Prelude.NFData DeleteRoleAlias where
  rnf DeleteRoleAlias' {..} = Prelude.rnf roleAlias

instance Core.ToHeaders DeleteRoleAlias where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteRoleAlias where
  toPath DeleteRoleAlias' {..} =
    Prelude.mconcat
      ["/role-aliases/", Core.toBS roleAlias]

instance Core.ToQuery DeleteRoleAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRoleAliasResponse' smart constructor.
data DeleteRoleAliasResponse = DeleteRoleAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteRoleAliasResponse where
  rnf DeleteRoleAliasResponse' {..} =
    Prelude.rnf httpStatus
