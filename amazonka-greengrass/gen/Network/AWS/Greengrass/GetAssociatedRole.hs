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
-- Module      : Network.AWS.Greengrass.GetAssociatedRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the role associated with a particular group.
module Network.AWS.Greengrass.GetAssociatedRole
  ( -- * Creating a Request
    GetAssociatedRole (..),
    newGetAssociatedRole,

    -- * Request Lenses
    getAssociatedRole_groupId,

    -- * Destructuring the Response
    GetAssociatedRoleResponse (..),
    newGetAssociatedRoleResponse,

    -- * Response Lenses
    getAssociatedRoleResponse_roleArn,
    getAssociatedRoleResponse_associatedAt,
    getAssociatedRoleResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAssociatedRole' smart constructor.
data GetAssociatedRole = GetAssociatedRole'
  { -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetAssociatedRole where
  type Rs GetAssociatedRole = GetAssociatedRoleResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssociatedRoleResponse'
            Prelude.<$> (x Prelude..?> "RoleArn")
            Prelude.<*> (x Prelude..?> "AssociatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAssociatedRole

instance Prelude.NFData GetAssociatedRole

instance Prelude.ToHeaders GetAssociatedRole where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetAssociatedRole where
  toPath GetAssociatedRole' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Prelude.toBS groupId,
        "/role"
      ]

instance Prelude.ToQuery GetAssociatedRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssociatedRoleResponse' smart constructor.
data GetAssociatedRoleResponse = GetAssociatedRoleResponse'
  { -- | The ARN of the role that is associated with the group.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The time when the role was associated with the group.
    associatedAt :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetAssociatedRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'getAssociatedRoleResponse_roleArn' - The ARN of the role that is associated with the group.
--
-- 'associatedAt', 'getAssociatedRoleResponse_associatedAt' - The time when the role was associated with the group.
--
-- 'httpStatus', 'getAssociatedRoleResponse_httpStatus' - The response's http status code.
newGetAssociatedRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssociatedRoleResponse
newGetAssociatedRoleResponse pHttpStatus_ =
  GetAssociatedRoleResponse'
    { roleArn =
        Prelude.Nothing,
      associatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the role that is associated with the group.
getAssociatedRoleResponse_roleArn :: Lens.Lens' GetAssociatedRoleResponse (Prelude.Maybe Prelude.Text)
getAssociatedRoleResponse_roleArn = Lens.lens (\GetAssociatedRoleResponse' {roleArn} -> roleArn) (\s@GetAssociatedRoleResponse' {} a -> s {roleArn = a} :: GetAssociatedRoleResponse)

-- | The time when the role was associated with the group.
getAssociatedRoleResponse_associatedAt :: Lens.Lens' GetAssociatedRoleResponse (Prelude.Maybe Prelude.Text)
getAssociatedRoleResponse_associatedAt = Lens.lens (\GetAssociatedRoleResponse' {associatedAt} -> associatedAt) (\s@GetAssociatedRoleResponse' {} a -> s {associatedAt = a} :: GetAssociatedRoleResponse)

-- | The response's http status code.
getAssociatedRoleResponse_httpStatus :: Lens.Lens' GetAssociatedRoleResponse Prelude.Int
getAssociatedRoleResponse_httpStatus = Lens.lens (\GetAssociatedRoleResponse' {httpStatus} -> httpStatus) (\s@GetAssociatedRoleResponse' {} a -> s {httpStatus = a} :: GetAssociatedRoleResponse)

instance Prelude.NFData GetAssociatedRoleResponse
