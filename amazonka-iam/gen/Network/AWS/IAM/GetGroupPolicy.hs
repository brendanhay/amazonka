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
-- Module      : Network.AWS.IAM.GetGroupPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified inline policy document that is embedded in the
-- specified IAM group.
--
-- Policies returned by this operation are URL-encoded compliant with
-- <https://tools.ietf.org/html/rfc3986 RFC 3986>. You can use a URL
-- decoding method to convert the policy back to plain JSON text. For
-- example, if you use Java, you can use the @decode@ method of the
-- @java.net.URLDecoder@ utility class in the Java SDK. Other languages and
-- SDKs provide similar functionality.
--
-- An IAM group can also have managed policies attached to it. To retrieve
-- a managed policy document that is attached to a group, use GetPolicy to
-- determine the policy\'s default version, then use GetPolicyVersion to
-- retrieve the policy document.
--
-- For more information about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.GetGroupPolicy
  ( -- * Creating a Request
    GetGroupPolicy (..),
    newGetGroupPolicy,

    -- * Request Lenses
    getGroupPolicy_groupName,
    getGroupPolicy_policyName,

    -- * Destructuring the Response
    GetGroupPolicyResponse (..),
    newGetGroupPolicyResponse,

    -- * Response Lenses
    getGroupPolicyResponse_httpStatus,
    getGroupPolicyResponse_groupName,
    getGroupPolicyResponse_policyName,
    getGroupPolicyResponse_policyDocument,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGroupPolicy' smart constructor.
data GetGroupPolicy = GetGroupPolicy'
  { -- | The name of the group the policy is associated with.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    groupName :: Prelude.Text,
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
-- Create a value of 'GetGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'getGroupPolicy_groupName' - The name of the group the policy is associated with.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyName', 'getGroupPolicy_policyName' - The name of the policy document to get.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newGetGroupPolicy ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  GetGroupPolicy
newGetGroupPolicy pGroupName_ pPolicyName_ =
  GetGroupPolicy'
    { groupName = pGroupName_,
      policyName = pPolicyName_
    }

-- | The name of the group the policy is associated with.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
getGroupPolicy_groupName :: Lens.Lens' GetGroupPolicy Prelude.Text
getGroupPolicy_groupName = Lens.lens (\GetGroupPolicy' {groupName} -> groupName) (\s@GetGroupPolicy' {} a -> s {groupName = a} :: GetGroupPolicy)

-- | The name of the policy document to get.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
getGroupPolicy_policyName :: Lens.Lens' GetGroupPolicy Prelude.Text
getGroupPolicy_policyName = Lens.lens (\GetGroupPolicy' {policyName} -> policyName) (\s@GetGroupPolicy' {} a -> s {policyName = a} :: GetGroupPolicy)

instance Prelude.AWSRequest GetGroupPolicy where
  type Rs GetGroupPolicy = GetGroupPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetGroupPolicyResult"
      ( \s h x ->
          GetGroupPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "GroupName")
            Prelude.<*> (x Prelude..@ "PolicyName")
            Prelude.<*> (x Prelude..@ "PolicyDocument")
      )

instance Prelude.Hashable GetGroupPolicy

instance Prelude.NFData GetGroupPolicy

instance Prelude.ToHeaders GetGroupPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetGroupPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetGroupPolicy where
  toQuery GetGroupPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("GetGroupPolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "GroupName" Prelude.=: groupName,
        "PolicyName" Prelude.=: policyName
      ]

-- | Contains the response to a successful GetGroupPolicy request.
--
-- /See:/ 'newGetGroupPolicyResponse' smart constructor.
data GetGroupPolicyResponse = GetGroupPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The group the policy is associated with.
    groupName :: Prelude.Text,
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
-- Create a value of 'GetGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getGroupPolicyResponse_httpStatus' - The response's http status code.
--
-- 'groupName', 'getGroupPolicyResponse_groupName' - The group the policy is associated with.
--
-- 'policyName', 'getGroupPolicyResponse_policyName' - The name of the policy.
--
-- 'policyDocument', 'getGroupPolicyResponse_policyDocument' - The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created
-- using AWS CloudFormation templates can be formatted in YAML. AWS
-- CloudFormation always converts a YAML policy to JSON format before
-- submitting it to IAM.
newGetGroupPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'groupName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  GetGroupPolicyResponse
newGetGroupPolicyResponse
  pHttpStatus_
  pGroupName_
  pPolicyName_
  pPolicyDocument_ =
    GetGroupPolicyResponse'
      { httpStatus = pHttpStatus_,
        groupName = pGroupName_,
        policyName = pPolicyName_,
        policyDocument = pPolicyDocument_
      }

-- | The response's http status code.
getGroupPolicyResponse_httpStatus :: Lens.Lens' GetGroupPolicyResponse Prelude.Int
getGroupPolicyResponse_httpStatus = Lens.lens (\GetGroupPolicyResponse' {httpStatus} -> httpStatus) (\s@GetGroupPolicyResponse' {} a -> s {httpStatus = a} :: GetGroupPolicyResponse)

-- | The group the policy is associated with.
getGroupPolicyResponse_groupName :: Lens.Lens' GetGroupPolicyResponse Prelude.Text
getGroupPolicyResponse_groupName = Lens.lens (\GetGroupPolicyResponse' {groupName} -> groupName) (\s@GetGroupPolicyResponse' {} a -> s {groupName = a} :: GetGroupPolicyResponse)

-- | The name of the policy.
getGroupPolicyResponse_policyName :: Lens.Lens' GetGroupPolicyResponse Prelude.Text
getGroupPolicyResponse_policyName = Lens.lens (\GetGroupPolicyResponse' {policyName} -> policyName) (\s@GetGroupPolicyResponse' {} a -> s {policyName = a} :: GetGroupPolicyResponse)

-- | The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created
-- using AWS CloudFormation templates can be formatted in YAML. AWS
-- CloudFormation always converts a YAML policy to JSON format before
-- submitting it to IAM.
getGroupPolicyResponse_policyDocument :: Lens.Lens' GetGroupPolicyResponse Prelude.Text
getGroupPolicyResponse_policyDocument = Lens.lens (\GetGroupPolicyResponse' {policyDocument} -> policyDocument) (\s@GetGroupPolicyResponse' {} a -> s {policyDocument = a} :: GetGroupPolicyResponse)

instance Prelude.NFData GetGroupPolicyResponse
