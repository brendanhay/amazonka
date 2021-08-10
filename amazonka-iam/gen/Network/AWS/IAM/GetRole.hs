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
-- Module      : Network.AWS.IAM.GetRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified role, including the role\'s
-- path, GUID, ARN, and the role\'s trust policy that grants permission to
-- assume the role. For more information about roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with roles>.
--
-- Policies returned by this operation are URL-encoded compliant with
-- <https://tools.ietf.org/html/rfc3986 RFC 3986>. You can use a URL
-- decoding method to convert the policy back to plain JSON text. For
-- example, if you use Java, you can use the @decode@ method of the
-- @java.net.URLDecoder@ utility class in the Java SDK. Other languages and
-- SDKs provide similar functionality.
module Network.AWS.IAM.GetRole
  ( -- * Creating a Request
    GetRole (..),
    newGetRole,

    -- * Request Lenses
    getRole_roleName,

    -- * Destructuring the Response
    GetRoleResponse (..),
    newGetRoleResponse,

    -- * Response Lenses
    getRoleResponse_httpStatus,
    getRoleResponse_role,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRole' smart constructor.
data GetRole = GetRole'
  { -- | The name of the IAM role to get information about.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'getRole_roleName' - The name of the IAM role to get information about.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newGetRole ::
  -- | 'roleName'
  Prelude.Text ->
  GetRole
newGetRole pRoleName_ =
  GetRole' {roleName = pRoleName_}

-- | The name of the IAM role to get information about.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
getRole_roleName :: Lens.Lens' GetRole Prelude.Text
getRole_roleName = Lens.lens (\GetRole' {roleName} -> roleName) (\s@GetRole' {} a -> s {roleName = a} :: GetRole)

instance Core.AWSRequest GetRole where
  type AWSResponse GetRole = GetRoleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetRoleResult"
      ( \s h x ->
          GetRoleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "Role")
      )

instance Prelude.Hashable GetRole

instance Prelude.NFData GetRole

instance Core.ToHeaders GetRole where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetRole where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRole where
  toQuery GetRole' {..} =
    Prelude.mconcat
      [ "Action" Core.=: ("GetRole" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Core.=: roleName
      ]

-- | Contains the response to a successful GetRole request.
--
-- /See:/ 'newGetRoleResponse' smart constructor.
data GetRoleResponse = GetRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing details about the IAM role.
    role' :: Role
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getRoleResponse_httpStatus' - The response's http status code.
--
-- 'role'', 'getRoleResponse_role' - A structure containing details about the IAM role.
newGetRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'role''
  Role ->
  GetRoleResponse
newGetRoleResponse pHttpStatus_ pRole_ =
  GetRoleResponse'
    { httpStatus = pHttpStatus_,
      role' = pRole_
    }

-- | The response's http status code.
getRoleResponse_httpStatus :: Lens.Lens' GetRoleResponse Prelude.Int
getRoleResponse_httpStatus = Lens.lens (\GetRoleResponse' {httpStatus} -> httpStatus) (\s@GetRoleResponse' {} a -> s {httpStatus = a} :: GetRoleResponse)

-- | A structure containing details about the IAM role.
getRoleResponse_role :: Lens.Lens' GetRoleResponse Role
getRoleResponse_role = Lens.lens (\GetRoleResponse' {role'} -> role') (\s@GetRoleResponse' {} a -> s {role' = a} :: GetRoleResponse)

instance Prelude.NFData GetRoleResponse
