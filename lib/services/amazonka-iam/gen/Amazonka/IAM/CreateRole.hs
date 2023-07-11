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
-- Module      : Amazonka.IAM.CreateRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new role for your Amazon Web Services account. For more
-- information about roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html IAM roles>.
-- For information about quotas for role names and the number of roles you
-- can create, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS quotas>
-- in the /IAM User Guide/.
module Amazonka.IAM.CreateRole
  ( -- * Creating a Request
    CreateRole (..),
    newCreateRole,

    -- * Request Lenses
    createRole_description,
    createRole_maxSessionDuration,
    createRole_path,
    createRole_permissionsBoundary,
    createRole_tags,
    createRole_roleName,
    createRole_assumeRolePolicyDocument,

    -- * Destructuring the Response
    CreateRoleResponse (..),
    newCreateRoleResponse,

    -- * Response Lenses
    createRoleResponse_httpStatus,
    createRoleResponse_role,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRole' smart constructor.
data CreateRole = CreateRole'
  { -- | A description of the role.
    description :: Prelude.Maybe Prelude.Text,
    -- | The maximum session duration (in seconds) that you want to set for the
    -- specified role. If you do not specify a value for this setting, the
    -- default value of one hour is applied. This setting can have a value from
    -- 1 hour to 12 hours.
    --
    -- Anyone who assumes the role from the CLI or API can use the
    -- @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter
    -- to request a longer session. The @MaxSessionDuration@ setting determines
    -- the maximum duration that can be requested using the @DurationSeconds@
    -- parameter. If users don\'t specify a value for the @DurationSeconds@
    -- parameter, their security credentials are valid for one hour by default.
    -- This applies when you use the @AssumeRole*@ API operations or the
    -- @assume-role*@ CLI operations but does not apply when you use those
    -- operations to create a console URL. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM roles>
    -- in the /IAM User Guide/.
    maxSessionDuration :: Prelude.Maybe Prelude.Natural,
    -- | The path to the role. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
    -- in the /IAM User Guide/.
    --
    -- This parameter is optional. If it is not included, it defaults to a
    -- slash (\/).
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of either a forward slash (\/) by itself or a string that
    -- must begin and end with forward slashes. In addition, it can contain any
    -- ASCII character from the ! (@\\u0021@) through the DEL character
    -- (@\\u007F@), including most punctuation characters, digits, and upper
    -- and lowercased letters.
    path :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the policy that is used to set the permissions boundary for
    -- the role.
    permissionsBoundary :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that you want to attach to the new role. Each tag
    -- consists of a key name and an associated value. For more information
    -- about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    --
    -- If any one of the tags is invalid or if you exceed the allowed maximum
    -- number of tags, then the entire request fails and the resource is not
    -- created.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the role to create.
    --
    -- IAM user, group, role, and policy names must be unique within the
    -- account. Names are not distinguished by case. For example, you cannot
    -- create resources named both \"MyResource\" and \"myresource\".
    roleName :: Prelude.Text,
    -- | The trust relationship policy document that grants an entity permission
    -- to assume the role.
    --
    -- In IAM, you must provide a JSON policy that has been converted to a
    -- string. However, for CloudFormation templates formatted in YAML, you can
    -- provide the policy in JSON or YAML format. CloudFormation always
    -- converts a YAML policy to JSON format before submitting it to IAM.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
    -- this parameter is a string of characters consisting of the following:
    --
    -- -   Any printable ASCII character ranging from the space character
    --     (@\\u0020@) through the end of the ASCII character range
    --
    -- -   The printable characters in the Basic Latin and Latin-1 Supplement
    --     character set (through @\\u00FF@)
    --
    -- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
    --     carriage return (@\\u000D@)
    --
    -- Upon success, the response includes the same trust policy in JSON
    -- format.
    assumeRolePolicyDocument :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createRole_description' - A description of the role.
--
-- 'maxSessionDuration', 'createRole_maxSessionDuration' - The maximum session duration (in seconds) that you want to set for the
-- specified role. If you do not specify a value for this setting, the
-- default value of one hour is applied. This setting can have a value from
-- 1 hour to 12 hours.
--
-- Anyone who assumes the role from the CLI or API can use the
-- @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter
-- to request a longer session. The @MaxSessionDuration@ setting determines
-- the maximum duration that can be requested using the @DurationSeconds@
-- parameter. If users don\'t specify a value for the @DurationSeconds@
-- parameter, their security credentials are valid for one hour by default.
-- This applies when you use the @AssumeRole*@ API operations or the
-- @assume-role*@ CLI operations but does not apply when you use those
-- operations to create a console URL. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM roles>
-- in the /IAM User Guide/.
--
-- 'path', 'createRole_path' - The path to the role. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /IAM User Guide/.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- 'permissionsBoundary', 'createRole_permissionsBoundary' - The ARN of the policy that is used to set the permissions boundary for
-- the role.
--
-- 'tags', 'createRole_tags' - A list of tags that you want to attach to the new role. Each tag
-- consists of a key name and an associated value. For more information
-- about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
--
-- 'roleName', 'createRole_roleName' - The name of the role to create.
--
-- IAM user, group, role, and policy names must be unique within the
-- account. Names are not distinguished by case. For example, you cannot
-- create resources named both \"MyResource\" and \"myresource\".
--
-- 'assumeRolePolicyDocument', 'createRole_assumeRolePolicyDocument' - The trust relationship policy document that grants an entity permission
-- to assume the role.
--
-- In IAM, you must provide a JSON policy that has been converted to a
-- string. However, for CloudFormation templates formatted in YAML, you can
-- provide the policy in JSON or YAML format. CloudFormation always
-- converts a YAML policy to JSON format before submitting it to IAM.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
--
-- Upon success, the response includes the same trust policy in JSON
-- format.
newCreateRole ::
  -- | 'roleName'
  Prelude.Text ->
  -- | 'assumeRolePolicyDocument'
  Prelude.Text ->
  CreateRole
newCreateRole pRoleName_ pAssumeRolePolicyDocument_ =
  CreateRole'
    { description = Prelude.Nothing,
      maxSessionDuration = Prelude.Nothing,
      path = Prelude.Nothing,
      permissionsBoundary = Prelude.Nothing,
      tags = Prelude.Nothing,
      roleName = pRoleName_,
      assumeRolePolicyDocument =
        pAssumeRolePolicyDocument_
    }

-- | A description of the role.
createRole_description :: Lens.Lens' CreateRole (Prelude.Maybe Prelude.Text)
createRole_description = Lens.lens (\CreateRole' {description} -> description) (\s@CreateRole' {} a -> s {description = a} :: CreateRole)

-- | The maximum session duration (in seconds) that you want to set for the
-- specified role. If you do not specify a value for this setting, the
-- default value of one hour is applied. This setting can have a value from
-- 1 hour to 12 hours.
--
-- Anyone who assumes the role from the CLI or API can use the
-- @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter
-- to request a longer session. The @MaxSessionDuration@ setting determines
-- the maximum duration that can be requested using the @DurationSeconds@
-- parameter. If users don\'t specify a value for the @DurationSeconds@
-- parameter, their security credentials are valid for one hour by default.
-- This applies when you use the @AssumeRole*@ API operations or the
-- @assume-role*@ CLI operations but does not apply when you use those
-- operations to create a console URL. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM roles>
-- in the /IAM User Guide/.
createRole_maxSessionDuration :: Lens.Lens' CreateRole (Prelude.Maybe Prelude.Natural)
createRole_maxSessionDuration = Lens.lens (\CreateRole' {maxSessionDuration} -> maxSessionDuration) (\s@CreateRole' {} a -> s {maxSessionDuration = a} :: CreateRole)

-- | The path to the role. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /IAM User Guide/.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
createRole_path :: Lens.Lens' CreateRole (Prelude.Maybe Prelude.Text)
createRole_path = Lens.lens (\CreateRole' {path} -> path) (\s@CreateRole' {} a -> s {path = a} :: CreateRole)

-- | The ARN of the policy that is used to set the permissions boundary for
-- the role.
createRole_permissionsBoundary :: Lens.Lens' CreateRole (Prelude.Maybe Prelude.Text)
createRole_permissionsBoundary = Lens.lens (\CreateRole' {permissionsBoundary} -> permissionsBoundary) (\s@CreateRole' {} a -> s {permissionsBoundary = a} :: CreateRole)

-- | A list of tags that you want to attach to the new role. Each tag
-- consists of a key name and an associated value. For more information
-- about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
createRole_tags :: Lens.Lens' CreateRole (Prelude.Maybe [Tag])
createRole_tags = Lens.lens (\CreateRole' {tags} -> tags) (\s@CreateRole' {} a -> s {tags = a} :: CreateRole) Prelude.. Lens.mapping Lens.coerced

-- | The name of the role to create.
--
-- IAM user, group, role, and policy names must be unique within the
-- account. Names are not distinguished by case. For example, you cannot
-- create resources named both \"MyResource\" and \"myresource\".
createRole_roleName :: Lens.Lens' CreateRole Prelude.Text
createRole_roleName = Lens.lens (\CreateRole' {roleName} -> roleName) (\s@CreateRole' {} a -> s {roleName = a} :: CreateRole)

-- | The trust relationship policy document that grants an entity permission
-- to assume the role.
--
-- In IAM, you must provide a JSON policy that has been converted to a
-- string. However, for CloudFormation templates formatted in YAML, you can
-- provide the policy in JSON or YAML format. CloudFormation always
-- converts a YAML policy to JSON format before submitting it to IAM.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
--
-- Upon success, the response includes the same trust policy in JSON
-- format.
createRole_assumeRolePolicyDocument :: Lens.Lens' CreateRole Prelude.Text
createRole_assumeRolePolicyDocument = Lens.lens (\CreateRole' {assumeRolePolicyDocument} -> assumeRolePolicyDocument) (\s@CreateRole' {} a -> s {assumeRolePolicyDocument = a} :: CreateRole)

instance Core.AWSRequest CreateRole where
  type AWSResponse CreateRole = CreateRoleResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateRoleResult"
      ( \s h x ->
          CreateRoleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "Role")
      )

instance Prelude.Hashable CreateRole where
  hashWithSalt _salt CreateRole' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` maxSessionDuration
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` permissionsBoundary
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` assumeRolePolicyDocument

instance Prelude.NFData CreateRole where
  rnf CreateRole' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf maxSessionDuration
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf permissionsBoundary
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf assumeRolePolicyDocument

instance Data.ToHeaders CreateRole where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateRole where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateRole where
  toQuery CreateRole' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateRole" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Description" Data.=: description,
        "MaxSessionDuration" Data.=: maxSessionDuration,
        "Path" Data.=: path,
        "PermissionsBoundary" Data.=: permissionsBoundary,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "RoleName" Data.=: roleName,
        "AssumeRolePolicyDocument"
          Data.=: assumeRolePolicyDocument
      ]

-- | Contains the response to a successful CreateRole request.
--
-- /See:/ 'newCreateRoleResponse' smart constructor.
data CreateRoleResponse = CreateRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing details about the new role.
    role' :: Role
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createRoleResponse_httpStatus' - The response's http status code.
--
-- 'role'', 'createRoleResponse_role' - A structure containing details about the new role.
newCreateRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'role''
  Role ->
  CreateRoleResponse
newCreateRoleResponse pHttpStatus_ pRole_ =
  CreateRoleResponse'
    { httpStatus = pHttpStatus_,
      role' = pRole_
    }

-- | The response's http status code.
createRoleResponse_httpStatus :: Lens.Lens' CreateRoleResponse Prelude.Int
createRoleResponse_httpStatus = Lens.lens (\CreateRoleResponse' {httpStatus} -> httpStatus) (\s@CreateRoleResponse' {} a -> s {httpStatus = a} :: CreateRoleResponse)

-- | A structure containing details about the new role.
createRoleResponse_role :: Lens.Lens' CreateRoleResponse Role
createRoleResponse_role = Lens.lens (\CreateRoleResponse' {role'} -> role') (\s@CreateRoleResponse' {} a -> s {role' = a} :: CreateRoleResponse)

instance Prelude.NFData CreateRoleResponse where
  rnf CreateRoleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf role'
