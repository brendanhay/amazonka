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
-- Module      : Amazonka.Greengrass.GetAssociatedRole
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the role associated with a particular group.
module Amazonka.Greengrass.GetAssociatedRole
  ( -- * Creating a Request
    GetAssociatedRole (..),
    newGetAssociatedRole,

    -- * Request Lenses
    getAssociatedRole_groupId,

    -- * Destructuring the Response
    GetAssociatedRoleResponse (..),
    newGetAssociatedRoleResponse,

    -- * Response Lenses
    getAssociatedRoleResponse_associatedAt,
    getAssociatedRoleResponse_roleArn,
    getAssociatedRoleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAssociatedRole' smart constructor.
data GetAssociatedRole = GetAssociatedRole'
  { -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssociatedRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'getAssociatedRole_groupId' - The ID of the Greengrass group.
newGetAssociatedRole ::
  -- | 'groupId'
  Prelude.Text ->
  GetAssociatedRole
newGetAssociatedRole pGroupId_ =
  GetAssociatedRole' {groupId = pGroupId_}

-- | The ID of the Greengrass group.
getAssociatedRole_groupId :: Lens.Lens' GetAssociatedRole Prelude.Text
getAssociatedRole_groupId = Lens.lens (\GetAssociatedRole' {groupId} -> groupId) (\s@GetAssociatedRole' {} a -> s {groupId = a} :: GetAssociatedRole)

instance Core.AWSRequest GetAssociatedRole where
  type
    AWSResponse GetAssociatedRole =
      GetAssociatedRoleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssociatedRoleResponse'
            Prelude.<$> (x Data..?> "AssociatedAt")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAssociatedRole where
  hashWithSalt _salt GetAssociatedRole' {..} =
    _salt `Prelude.hashWithSalt` groupId

instance Prelude.NFData GetAssociatedRole where
  rnf GetAssociatedRole' {..} = Prelude.rnf groupId

instance Data.ToHeaders GetAssociatedRole where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAssociatedRole where
  toPath GetAssociatedRole' {..} =
    Prelude.mconcat
      ["/greengrass/groups/", Data.toBS groupId, "/role"]

instance Data.ToQuery GetAssociatedRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssociatedRoleResponse' smart constructor.
data GetAssociatedRoleResponse = GetAssociatedRoleResponse'
  { -- | The time when the role was associated with the group.
    associatedAt :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role that is associated with the group.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssociatedRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedAt', 'getAssociatedRoleResponse_associatedAt' - The time when the role was associated with the group.
--
-- 'roleArn', 'getAssociatedRoleResponse_roleArn' - The ARN of the role that is associated with the group.
--
-- 'httpStatus', 'getAssociatedRoleResponse_httpStatus' - The response's http status code.
newGetAssociatedRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssociatedRoleResponse
newGetAssociatedRoleResponse pHttpStatus_ =
  GetAssociatedRoleResponse'
    { associatedAt =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time when the role was associated with the group.
getAssociatedRoleResponse_associatedAt :: Lens.Lens' GetAssociatedRoleResponse (Prelude.Maybe Prelude.Text)
getAssociatedRoleResponse_associatedAt = Lens.lens (\GetAssociatedRoleResponse' {associatedAt} -> associatedAt) (\s@GetAssociatedRoleResponse' {} a -> s {associatedAt = a} :: GetAssociatedRoleResponse)

-- | The ARN of the role that is associated with the group.
getAssociatedRoleResponse_roleArn :: Lens.Lens' GetAssociatedRoleResponse (Prelude.Maybe Prelude.Text)
getAssociatedRoleResponse_roleArn = Lens.lens (\GetAssociatedRoleResponse' {roleArn} -> roleArn) (\s@GetAssociatedRoleResponse' {} a -> s {roleArn = a} :: GetAssociatedRoleResponse)

-- | The response's http status code.
getAssociatedRoleResponse_httpStatus :: Lens.Lens' GetAssociatedRoleResponse Prelude.Int
getAssociatedRoleResponse_httpStatus = Lens.lens (\GetAssociatedRoleResponse' {httpStatus} -> httpStatus) (\s@GetAssociatedRoleResponse' {} a -> s {httpStatus = a} :: GetAssociatedRoleResponse)

instance Prelude.NFData GetAssociatedRoleResponse where
  rnf GetAssociatedRoleResponse' {..} =
    Prelude.rnf associatedAt
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf httpStatus
