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
-- Module      : Network.AWS.IAM.GetPolicyVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified version of the specified
-- managed policy, including the policy document.
--
-- Policies returned by this operation are URL-encoded compliant with
-- <https://tools.ietf.org/html/rfc3986 RFC 3986>. You can use a URL
-- decoding method to convert the policy back to plain JSON text. For
-- example, if you use Java, you can use the @decode@ method of the
-- @java.net.URLDecoder@ utility class in the Java SDK. Other languages and
-- SDKs provide similar functionality.
--
-- To list the available versions for a policy, use ListPolicyVersions.
--
-- This operation retrieves information about managed policies. To retrieve
-- information about an inline policy that is embedded in a user, group, or
-- role, use GetUserPolicy, GetGroupPolicy, or GetRolePolicy.
--
-- For more information about the types of policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- For more information about managed policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.GetPolicyVersion
  ( -- * Creating a Request
    GetPolicyVersion (..),
    newGetPolicyVersion,

    -- * Request Lenses
    getPolicyVersion_policyArn,
    getPolicyVersion_versionId,

    -- * Destructuring the Response
    GetPolicyVersionResponse (..),
    newGetPolicyVersionResponse,

    -- * Response Lenses
    getPolicyVersionResponse_policyVersion,
    getPolicyVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPolicyVersion' smart constructor.
data GetPolicyVersion = GetPolicyVersion'
  { -- | The Amazon Resource Name (ARN) of the managed policy that you want
    -- information about.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Core.Text,
    -- | Identifies the policy version to retrieve.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consists of the lowercase letter \'v\' followed by one or two
    -- digits, and optionally followed by a period \'.\' and a string of
    -- letters and digits.
    versionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyArn', 'getPolicyVersion_policyArn' - The Amazon Resource Name (ARN) of the managed policy that you want
-- information about.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'versionId', 'getPolicyVersion_versionId' - Identifies the policy version to retrieve.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consists of the lowercase letter \'v\' followed by one or two
-- digits, and optionally followed by a period \'.\' and a string of
-- letters and digits.
newGetPolicyVersion ::
  -- | 'policyArn'
  Core.Text ->
  -- | 'versionId'
  Core.Text ->
  GetPolicyVersion
newGetPolicyVersion pPolicyArn_ pVersionId_ =
  GetPolicyVersion'
    { policyArn = pPolicyArn_,
      versionId = pVersionId_
    }

-- | The Amazon Resource Name (ARN) of the managed policy that you want
-- information about.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
getPolicyVersion_policyArn :: Lens.Lens' GetPolicyVersion Core.Text
getPolicyVersion_policyArn = Lens.lens (\GetPolicyVersion' {policyArn} -> policyArn) (\s@GetPolicyVersion' {} a -> s {policyArn = a} :: GetPolicyVersion)

-- | Identifies the policy version to retrieve.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consists of the lowercase letter \'v\' followed by one or two
-- digits, and optionally followed by a period \'.\' and a string of
-- letters and digits.
getPolicyVersion_versionId :: Lens.Lens' GetPolicyVersion Core.Text
getPolicyVersion_versionId = Lens.lens (\GetPolicyVersion' {versionId} -> versionId) (\s@GetPolicyVersion' {} a -> s {versionId = a} :: GetPolicyVersion)

instance Core.AWSRequest GetPolicyVersion where
  type
    AWSResponse GetPolicyVersion =
      GetPolicyVersionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetPolicyVersionResult"
      ( \s h x ->
          GetPolicyVersionResponse'
            Core.<$> (x Core..@? "PolicyVersion")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPolicyVersion

instance Core.NFData GetPolicyVersion

instance Core.ToHeaders GetPolicyVersion where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetPolicyVersion where
  toPath = Core.const "/"

instance Core.ToQuery GetPolicyVersion where
  toQuery GetPolicyVersion' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetPolicyVersion" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "PolicyArn" Core.=: policyArn,
        "VersionId" Core.=: versionId
      ]

-- | Contains the response to a successful GetPolicyVersion request.
--
-- /See:/ 'newGetPolicyVersionResponse' smart constructor.
data GetPolicyVersionResponse = GetPolicyVersionResponse'
  { -- | A structure containing details about the policy version.
    policyVersion :: Core.Maybe PolicyVersion,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPolicyVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyVersion', 'getPolicyVersionResponse_policyVersion' - A structure containing details about the policy version.
--
-- 'httpStatus', 'getPolicyVersionResponse_httpStatus' - The response's http status code.
newGetPolicyVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPolicyVersionResponse
newGetPolicyVersionResponse pHttpStatus_ =
  GetPolicyVersionResponse'
    { policyVersion =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure containing details about the policy version.
getPolicyVersionResponse_policyVersion :: Lens.Lens' GetPolicyVersionResponse (Core.Maybe PolicyVersion)
getPolicyVersionResponse_policyVersion = Lens.lens (\GetPolicyVersionResponse' {policyVersion} -> policyVersion) (\s@GetPolicyVersionResponse' {} a -> s {policyVersion = a} :: GetPolicyVersionResponse)

-- | The response's http status code.
getPolicyVersionResponse_httpStatus :: Lens.Lens' GetPolicyVersionResponse Core.Int
getPolicyVersionResponse_httpStatus = Lens.lens (\GetPolicyVersionResponse' {httpStatus} -> httpStatus) (\s@GetPolicyVersionResponse' {} a -> s {httpStatus = a} :: GetPolicyVersionResponse)

instance Core.NFData GetPolicyVersionResponse
