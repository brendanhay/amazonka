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
-- Module      : Amazonka.FinSpaceData.GetPermissionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a specific permission group.
module Amazonka.FinSpaceData.GetPermissionGroup
  ( -- * Creating a Request
    GetPermissionGroup (..),
    newGetPermissionGroup,

    -- * Request Lenses
    getPermissionGroup_permissionGroupId,

    -- * Destructuring the Response
    GetPermissionGroupResponse (..),
    newGetPermissionGroupResponse,

    -- * Response Lenses
    getPermissionGroupResponse_permissionGroup,
    getPermissionGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPermissionGroup' smart constructor.
data GetPermissionGroup = GetPermissionGroup'
  { -- | The unique identifier for the permission group.
    permissionGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPermissionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionGroupId', 'getPermissionGroup_permissionGroupId' - The unique identifier for the permission group.
newGetPermissionGroup ::
  -- | 'permissionGroupId'
  Prelude.Text ->
  GetPermissionGroup
newGetPermissionGroup pPermissionGroupId_ =
  GetPermissionGroup'
    { permissionGroupId =
        pPermissionGroupId_
    }

-- | The unique identifier for the permission group.
getPermissionGroup_permissionGroupId :: Lens.Lens' GetPermissionGroup Prelude.Text
getPermissionGroup_permissionGroupId = Lens.lens (\GetPermissionGroup' {permissionGroupId} -> permissionGroupId) (\s@GetPermissionGroup' {} a -> s {permissionGroupId = a} :: GetPermissionGroup)

instance Core.AWSRequest GetPermissionGroup where
  type
    AWSResponse GetPermissionGroup =
      GetPermissionGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPermissionGroupResponse'
            Prelude.<$> (x Data..?> "permissionGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPermissionGroup where
  hashWithSalt _salt GetPermissionGroup' {..} =
    _salt `Prelude.hashWithSalt` permissionGroupId

instance Prelude.NFData GetPermissionGroup where
  rnf GetPermissionGroup' {..} =
    Prelude.rnf permissionGroupId

instance Data.ToHeaders GetPermissionGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetPermissionGroup where
  toPath GetPermissionGroup' {..} =
    Prelude.mconcat
      ["/permission-group/", Data.toBS permissionGroupId]

instance Data.ToQuery GetPermissionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPermissionGroupResponse' smart constructor.
data GetPermissionGroupResponse = GetPermissionGroupResponse'
  { permissionGroup :: Prelude.Maybe PermissionGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPermissionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionGroup', 'getPermissionGroupResponse_permissionGroup' - Undocumented member.
--
-- 'httpStatus', 'getPermissionGroupResponse_httpStatus' - The response's http status code.
newGetPermissionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPermissionGroupResponse
newGetPermissionGroupResponse pHttpStatus_ =
  GetPermissionGroupResponse'
    { permissionGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getPermissionGroupResponse_permissionGroup :: Lens.Lens' GetPermissionGroupResponse (Prelude.Maybe PermissionGroup)
getPermissionGroupResponse_permissionGroup = Lens.lens (\GetPermissionGroupResponse' {permissionGroup} -> permissionGroup) (\s@GetPermissionGroupResponse' {} a -> s {permissionGroup = a} :: GetPermissionGroupResponse)

-- | The response's http status code.
getPermissionGroupResponse_httpStatus :: Lens.Lens' GetPermissionGroupResponse Prelude.Int
getPermissionGroupResponse_httpStatus = Lens.lens (\GetPermissionGroupResponse' {httpStatus} -> httpStatus) (\s@GetPermissionGroupResponse' {} a -> s {httpStatus = a} :: GetPermissionGroupResponse)

instance Prelude.NFData GetPermissionGroupResponse where
  rnf GetPermissionGroupResponse' {..} =
    Prelude.rnf permissionGroup `Prelude.seq`
      Prelude.rnf httpStatus
