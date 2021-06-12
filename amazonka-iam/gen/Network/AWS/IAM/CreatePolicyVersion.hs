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
-- Module      : Network.AWS.IAM.CreatePolicyVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the specified managed policy. To update a
-- managed policy, you create a new policy version. A managed policy can
-- have up to five versions. If the policy has five versions, you must
-- delete an existing version using DeletePolicyVersion before you create a
-- new version.
--
-- Optionally, you can set the new version as the policy\'s default
-- version. The default version is the version that is in effect for the
-- IAM users, groups, and roles to which the policy is attached.
--
-- For more information about managed policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.CreatePolicyVersion
  ( -- * Creating a Request
    CreatePolicyVersion (..),
    newCreatePolicyVersion,

    -- * Request Lenses
    createPolicyVersion_setAsDefault,
    createPolicyVersion_policyArn,
    createPolicyVersion_policyDocument,

    -- * Destructuring the Response
    CreatePolicyVersionResponse (..),
    newCreatePolicyVersionResponse,

    -- * Response Lenses
    createPolicyVersionResponse_policyVersion,
    createPolicyVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreatePolicyVersion' smart constructor.
data CreatePolicyVersion = CreatePolicyVersion'
  { -- | Specifies whether to set this version as the policy\'s default version.
    --
    -- When this parameter is @true@, the new policy version becomes the
    -- operative version. That is, it becomes the version that is in effect for
    -- the IAM users, groups, and roles that the policy is attached to.
    --
    -- For more information about managed policy versions, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
    -- in the /IAM User Guide/.
    setAsDefault :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the IAM policy to which you want to
    -- add a new version.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Core.Text,
    -- | The JSON policy document that you want to use as the content for this
    -- new version of the policy.
    --
    -- You must provide policies in JSON format in IAM. However, for AWS
    -- CloudFormation templates formatted in YAML, you can provide the policy
    -- in JSON or YAML format. AWS CloudFormation always converts a YAML policy
    -- to JSON format before submitting it to IAM.
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
    policyDocument :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setAsDefault', 'createPolicyVersion_setAsDefault' - Specifies whether to set this version as the policy\'s default version.
--
-- When this parameter is @true@, the new policy version becomes the
-- operative version. That is, it becomes the version that is in effect for
-- the IAM users, groups, and roles that the policy is attached to.
--
-- For more information about managed policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
--
-- 'policyArn', 'createPolicyVersion_policyArn' - The Amazon Resource Name (ARN) of the IAM policy to which you want to
-- add a new version.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'policyDocument', 'createPolicyVersion_policyDocument' - The JSON policy document that you want to use as the content for this
-- new version of the policy.
--
-- You must provide policies in JSON format in IAM. However, for AWS
-- CloudFormation templates formatted in YAML, you can provide the policy
-- in JSON or YAML format. AWS CloudFormation always converts a YAML policy
-- to JSON format before submitting it to IAM.
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
newCreatePolicyVersion ::
  -- | 'policyArn'
  Core.Text ->
  -- | 'policyDocument'
  Core.Text ->
  CreatePolicyVersion
newCreatePolicyVersion pPolicyArn_ pPolicyDocument_ =
  CreatePolicyVersion'
    { setAsDefault = Core.Nothing,
      policyArn = pPolicyArn_,
      policyDocument = pPolicyDocument_
    }

-- | Specifies whether to set this version as the policy\'s default version.
--
-- When this parameter is @true@, the new policy version becomes the
-- operative version. That is, it becomes the version that is in effect for
-- the IAM users, groups, and roles that the policy is attached to.
--
-- For more information about managed policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
createPolicyVersion_setAsDefault :: Lens.Lens' CreatePolicyVersion (Core.Maybe Core.Bool)
createPolicyVersion_setAsDefault = Lens.lens (\CreatePolicyVersion' {setAsDefault} -> setAsDefault) (\s@CreatePolicyVersion' {} a -> s {setAsDefault = a} :: CreatePolicyVersion)

-- | The Amazon Resource Name (ARN) of the IAM policy to which you want to
-- add a new version.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
createPolicyVersion_policyArn :: Lens.Lens' CreatePolicyVersion Core.Text
createPolicyVersion_policyArn = Lens.lens (\CreatePolicyVersion' {policyArn} -> policyArn) (\s@CreatePolicyVersion' {} a -> s {policyArn = a} :: CreatePolicyVersion)

-- | The JSON policy document that you want to use as the content for this
-- new version of the policy.
--
-- You must provide policies in JSON format in IAM. However, for AWS
-- CloudFormation templates formatted in YAML, you can provide the policy
-- in JSON or YAML format. AWS CloudFormation always converts a YAML policy
-- to JSON format before submitting it to IAM.
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
createPolicyVersion_policyDocument :: Lens.Lens' CreatePolicyVersion Core.Text
createPolicyVersion_policyDocument = Lens.lens (\CreatePolicyVersion' {policyDocument} -> policyDocument) (\s@CreatePolicyVersion' {} a -> s {policyDocument = a} :: CreatePolicyVersion)

instance Core.AWSRequest CreatePolicyVersion where
  type
    AWSResponse CreatePolicyVersion =
      CreatePolicyVersionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreatePolicyVersionResult"
      ( \s h x ->
          CreatePolicyVersionResponse'
            Core.<$> (x Core..@? "PolicyVersion")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePolicyVersion

instance Core.NFData CreatePolicyVersion

instance Core.ToHeaders CreatePolicyVersion where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreatePolicyVersion where
  toPath = Core.const "/"

instance Core.ToQuery CreatePolicyVersion where
  toQuery CreatePolicyVersion' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreatePolicyVersion" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "SetAsDefault" Core.=: setAsDefault,
        "PolicyArn" Core.=: policyArn,
        "PolicyDocument" Core.=: policyDocument
      ]

-- | Contains the response to a successful CreatePolicyVersion request.
--
-- /See:/ 'newCreatePolicyVersionResponse' smart constructor.
data CreatePolicyVersionResponse = CreatePolicyVersionResponse'
  { -- | A structure containing details about the new policy version.
    policyVersion :: Core.Maybe PolicyVersion,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePolicyVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyVersion', 'createPolicyVersionResponse_policyVersion' - A structure containing details about the new policy version.
--
-- 'httpStatus', 'createPolicyVersionResponse_httpStatus' - The response's http status code.
newCreatePolicyVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePolicyVersionResponse
newCreatePolicyVersionResponse pHttpStatus_ =
  CreatePolicyVersionResponse'
    { policyVersion =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure containing details about the new policy version.
createPolicyVersionResponse_policyVersion :: Lens.Lens' CreatePolicyVersionResponse (Core.Maybe PolicyVersion)
createPolicyVersionResponse_policyVersion = Lens.lens (\CreatePolicyVersionResponse' {policyVersion} -> policyVersion) (\s@CreatePolicyVersionResponse' {} a -> s {policyVersion = a} :: CreatePolicyVersionResponse)

-- | The response's http status code.
createPolicyVersionResponse_httpStatus :: Lens.Lens' CreatePolicyVersionResponse Core.Int
createPolicyVersionResponse_httpStatus = Lens.lens (\CreatePolicyVersionResponse' {httpStatus} -> httpStatus) (\s@CreatePolicyVersionResponse' {} a -> s {httpStatus = a} :: CreatePolicyVersionResponse)

instance Core.NFData CreatePolicyVersionResponse
