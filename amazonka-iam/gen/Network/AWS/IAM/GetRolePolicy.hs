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
-- Module      : Network.AWS.IAM.GetRolePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified inline policy document that is embedded with the
-- specified IAM role.
--
-- Policies returned by this operation are URL-encoded compliant with
-- <https://tools.ietf.org/html/rfc3986 RFC 3986>. You can use a URL
-- decoding method to convert the policy back to plain JSON text. For
-- example, if you use Java, you can use the @decode@ method of the
-- @java.net.URLDecoder@ utility class in the Java SDK. Other languages and
-- SDKs provide similar functionality.
--
-- An IAM role can also have managed policies attached to it. To retrieve a
-- managed policy document that is attached to a role, use GetPolicy to
-- determine the policy\'s default version, then use GetPolicyVersion to
-- retrieve the policy document.
--
-- For more information about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- For more information about roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using roles to delegate permissions and federate identities>.
module Network.AWS.IAM.GetRolePolicy
  ( -- * Creating a Request
    GetRolePolicy (..),
    newGetRolePolicy,

    -- * Request Lenses
    getRolePolicy_roleName,
    getRolePolicy_policyName,

    -- * Destructuring the Response
    GetRolePolicyResponse (..),
    newGetRolePolicyResponse,

    -- * Response Lenses
    getRolePolicyResponse_httpStatus,
    getRolePolicyResponse_roleName,
    getRolePolicyResponse_policyName,
    getRolePolicyResponse_policyDocument,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRolePolicy' smart constructor.
data GetRolePolicy = GetRolePolicy'
  { -- | The name of the role associated with the policy.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text,
    -- | The name of the policy document to get.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetRolePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'getRolePolicy_roleName' - The name of the role associated with the policy.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyName', 'getRolePolicy_policyName' - The name of the policy document to get.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newGetRolePolicy ::
  -- | 'roleName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  GetRolePolicy
newGetRolePolicy pRoleName_ pPolicyName_ =
  GetRolePolicy'
    { roleName = pRoleName_,
      policyName = pPolicyName_
    }

-- | The name of the role associated with the policy.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
getRolePolicy_roleName :: Lens.Lens' GetRolePolicy Prelude.Text
getRolePolicy_roleName = Lens.lens (\GetRolePolicy' {roleName} -> roleName) (\s@GetRolePolicy' {} a -> s {roleName = a} :: GetRolePolicy)

-- | The name of the policy document to get.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
getRolePolicy_policyName :: Lens.Lens' GetRolePolicy Prelude.Text
getRolePolicy_policyName = Lens.lens (\GetRolePolicy' {policyName} -> policyName) (\s@GetRolePolicy' {} a -> s {policyName = a} :: GetRolePolicy)

instance Prelude.AWSRequest GetRolePolicy where
  type Rs GetRolePolicy = GetRolePolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetRolePolicyResult"
      ( \s h x ->
          GetRolePolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "RoleName")
            Prelude.<*> (x Prelude..@ "PolicyName")
            Prelude.<*> (x Prelude..@ "PolicyDocument")
      )

instance Prelude.Hashable GetRolePolicy

instance Prelude.NFData GetRolePolicy

instance Prelude.ToHeaders GetRolePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetRolePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetRolePolicy where
  toQuery GetRolePolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("GetRolePolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Prelude.=: roleName,
        "PolicyName" Prelude.=: policyName
      ]

-- | Contains the response to a successful GetRolePolicy request.
--
-- /See:/ 'newGetRolePolicyResponse' smart constructor.
data GetRolePolicyResponse = GetRolePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The role the policy is associated with.
    roleName :: Prelude.Text,
    -- | The name of the policy.
    policyName :: Prelude.Text,
    -- | The policy document.
    --
    -- IAM stores policies in JSON format. However, resources that were created
    -- using AWS CloudFormation templates can be formatted in YAML. AWS
    -- CloudFormation always converts a YAML policy to JSON format before
    -- submitting it to IAM.
    policyDocument :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetRolePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getRolePolicyResponse_httpStatus' - The response's http status code.
--
-- 'roleName', 'getRolePolicyResponse_roleName' - The role the policy is associated with.
--
-- 'policyName', 'getRolePolicyResponse_policyName' - The name of the policy.
--
-- 'policyDocument', 'getRolePolicyResponse_policyDocument' - The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created
-- using AWS CloudFormation templates can be formatted in YAML. AWS
-- CloudFormation always converts a YAML policy to JSON format before
-- submitting it to IAM.
newGetRolePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'roleName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  GetRolePolicyResponse
newGetRolePolicyResponse
  pHttpStatus_
  pRoleName_
  pPolicyName_
  pPolicyDocument_ =
    GetRolePolicyResponse'
      { httpStatus = pHttpStatus_,
        roleName = pRoleName_,
        policyName = pPolicyName_,
        policyDocument = pPolicyDocument_
      }

-- | The response's http status code.
getRolePolicyResponse_httpStatus :: Lens.Lens' GetRolePolicyResponse Prelude.Int
getRolePolicyResponse_httpStatus = Lens.lens (\GetRolePolicyResponse' {httpStatus} -> httpStatus) (\s@GetRolePolicyResponse' {} a -> s {httpStatus = a} :: GetRolePolicyResponse)

-- | The role the policy is associated with.
getRolePolicyResponse_roleName :: Lens.Lens' GetRolePolicyResponse Prelude.Text
getRolePolicyResponse_roleName = Lens.lens (\GetRolePolicyResponse' {roleName} -> roleName) (\s@GetRolePolicyResponse' {} a -> s {roleName = a} :: GetRolePolicyResponse)

-- | The name of the policy.
getRolePolicyResponse_policyName :: Lens.Lens' GetRolePolicyResponse Prelude.Text
getRolePolicyResponse_policyName = Lens.lens (\GetRolePolicyResponse' {policyName} -> policyName) (\s@GetRolePolicyResponse' {} a -> s {policyName = a} :: GetRolePolicyResponse)

-- | The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created
-- using AWS CloudFormation templates can be formatted in YAML. AWS
-- CloudFormation always converts a YAML policy to JSON format before
-- submitting it to IAM.
getRolePolicyResponse_policyDocument :: Lens.Lens' GetRolePolicyResponse Prelude.Text
getRolePolicyResponse_policyDocument = Lens.lens (\GetRolePolicyResponse' {policyDocument} -> policyDocument) (\s@GetRolePolicyResponse' {} a -> s {policyDocument = a} :: GetRolePolicyResponse)

instance Prelude.NFData GetRolePolicyResponse
