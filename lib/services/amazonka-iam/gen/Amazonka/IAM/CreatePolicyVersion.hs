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
-- Module      : Amazonka.IAM.CreatePolicyVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.IAM.CreatePolicyVersion
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    setAsDefault :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the IAM policy to which you want to
    -- add a new version.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    policyArn :: Prelude.Text,
    -- | The JSON policy document that you want to use as the content for this
    -- new version of the policy.
    --
    -- You must provide policies in JSON format in IAM. However, for
    -- CloudFormation templates formatted in YAML, you can provide the policy
    -- in JSON or YAML format. CloudFormation always converts a YAML policy to
    -- JSON format before submitting it to IAM.
    --
    -- The maximum length of the policy document that you can pass in this
    -- operation, including whitespace, is listed below. To view the maximum
    -- character counts of a managed policy with no whitespaces, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
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
    policyDocument :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- in the /Amazon Web Services General Reference/.
--
-- 'policyDocument', 'createPolicyVersion_policyDocument' - The JSON policy document that you want to use as the content for this
-- new version of the policy.
--
-- You must provide policies in JSON format in IAM. However, for
-- CloudFormation templates formatted in YAML, you can provide the policy
-- in JSON or YAML format. CloudFormation always converts a YAML policy to
-- JSON format before submitting it to IAM.
--
-- The maximum length of the policy document that you can pass in this
-- operation, including whitespace, is listed below. To view the maximum
-- character counts of a managed policy with no whitespaces, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
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
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  CreatePolicyVersion
newCreatePolicyVersion pPolicyArn_ pPolicyDocument_ =
  CreatePolicyVersion'
    { setAsDefault =
        Prelude.Nothing,
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
createPolicyVersion_setAsDefault :: Lens.Lens' CreatePolicyVersion (Prelude.Maybe Prelude.Bool)
createPolicyVersion_setAsDefault = Lens.lens (\CreatePolicyVersion' {setAsDefault} -> setAsDefault) (\s@CreatePolicyVersion' {} a -> s {setAsDefault = a} :: CreatePolicyVersion)

-- | The Amazon Resource Name (ARN) of the IAM policy to which you want to
-- add a new version.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
createPolicyVersion_policyArn :: Lens.Lens' CreatePolicyVersion Prelude.Text
createPolicyVersion_policyArn = Lens.lens (\CreatePolicyVersion' {policyArn} -> policyArn) (\s@CreatePolicyVersion' {} a -> s {policyArn = a} :: CreatePolicyVersion)

-- | The JSON policy document that you want to use as the content for this
-- new version of the policy.
--
-- You must provide policies in JSON format in IAM. However, for
-- CloudFormation templates formatted in YAML, you can provide the policy
-- in JSON or YAML format. CloudFormation always converts a YAML policy to
-- JSON format before submitting it to IAM.
--
-- The maximum length of the policy document that you can pass in this
-- operation, including whitespace, is listed below. To view the maximum
-- character counts of a managed policy with no whitespaces, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-quotas-entity-length IAM and STS character quotas>.
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
createPolicyVersion_policyDocument :: Lens.Lens' CreatePolicyVersion Prelude.Text
createPolicyVersion_policyDocument = Lens.lens (\CreatePolicyVersion' {policyDocument} -> policyDocument) (\s@CreatePolicyVersion' {} a -> s {policyDocument = a} :: CreatePolicyVersion)

instance Core.AWSRequest CreatePolicyVersion where
  type
    AWSResponse CreatePolicyVersion =
      CreatePolicyVersionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreatePolicyVersionResult"
      ( \s h x ->
          CreatePolicyVersionResponse'
            Prelude.<$> (x Data..@? "PolicyVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePolicyVersion where
  hashWithSalt _salt CreatePolicyVersion' {..} =
    _salt
      `Prelude.hashWithSalt` setAsDefault
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` policyDocument

instance Prelude.NFData CreatePolicyVersion where
  rnf CreatePolicyVersion' {..} =
    Prelude.rnf setAsDefault
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf policyDocument

instance Data.ToHeaders CreatePolicyVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreatePolicyVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePolicyVersion where
  toQuery CreatePolicyVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreatePolicyVersion" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "SetAsDefault" Data.=: setAsDefault,
        "PolicyArn" Data.=: policyArn,
        "PolicyDocument" Data.=: policyDocument
      ]

-- | Contains the response to a successful CreatePolicyVersion request.
--
-- /See:/ 'newCreatePolicyVersionResponse' smart constructor.
data CreatePolicyVersionResponse = CreatePolicyVersionResponse'
  { -- | A structure containing details about the new policy version.
    policyVersion :: Prelude.Maybe PolicyVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreatePolicyVersionResponse
newCreatePolicyVersionResponse pHttpStatus_ =
  CreatePolicyVersionResponse'
    { policyVersion =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure containing details about the new policy version.
createPolicyVersionResponse_policyVersion :: Lens.Lens' CreatePolicyVersionResponse (Prelude.Maybe PolicyVersion)
createPolicyVersionResponse_policyVersion = Lens.lens (\CreatePolicyVersionResponse' {policyVersion} -> policyVersion) (\s@CreatePolicyVersionResponse' {} a -> s {policyVersion = a} :: CreatePolicyVersionResponse)

-- | The response's http status code.
createPolicyVersionResponse_httpStatus :: Lens.Lens' CreatePolicyVersionResponse Prelude.Int
createPolicyVersionResponse_httpStatus = Lens.lens (\CreatePolicyVersionResponse' {httpStatus} -> httpStatus) (\s@CreatePolicyVersionResponse' {} a -> s {httpStatus = a} :: CreatePolicyVersionResponse)

instance Prelude.NFData CreatePolicyVersionResponse where
  rnf CreatePolicyVersionResponse' {..} =
    Prelude.rnf policyVersion
      `Prelude.seq` Prelude.rnf httpStatus
