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
-- Module      : Amazonka.IAM.PutRolePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates an inline policy document that is embedded in the
-- specified IAM role.
--
-- When you embed an inline policy in a role, the inline policy is used as
-- part of the role\'s access (permissions) policy. The role\'s trust
-- policy is created at the same time as the role, using CreateRole. You
-- can update a role\'s trust policy using UpdateAssumeRolePolicy. For more
-- information about IAM roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using roles to delegate permissions and federate identities>.
--
-- A role can also have a managed policy attached to it. To attach a
-- managed policy to a role, use AttachRolePolicy. To create a new managed
-- policy, use CreatePolicy. For information about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- For information about the maximum number of inline policies that you can
-- embed with a role, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS quotas>
-- in the /IAM User Guide/.
--
-- Because policy documents can be large, you should use POST rather than
-- GET when calling @PutRolePolicy@. For general information about using
-- the Query API with IAM, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making query requests>
-- in the /IAM User Guide/.
module Amazonka.IAM.PutRolePolicy
  ( -- * Creating a Request
    PutRolePolicy (..),
    newPutRolePolicy,

    -- * Request Lenses
    putRolePolicy_roleName,
    putRolePolicy_policyName,
    putRolePolicy_policyDocument,

    -- * Destructuring the Response
    PutRolePolicyResponse (..),
    newPutRolePolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRolePolicy' smart constructor.
data PutRolePolicy = PutRolePolicy'
  { -- | The name of the role to associate the policy with.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text,
    -- | The name of the policy document.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    policyName :: Prelude.Text,
    -- | The policy document.
    --
    -- You must provide policies in JSON format in IAM. However, for
    -- CloudFormation templates formatted in YAML, you can provide the policy
    -- in JSON or YAML format. CloudFormation always converts a YAML policy to
    -- JSON format before submitting it to IAM.
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
-- Create a value of 'PutRolePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'putRolePolicy_roleName' - The name of the role to associate the policy with.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyName', 'putRolePolicy_policyName' - The name of the policy document.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyDocument', 'putRolePolicy_policyDocument' - The policy document.
--
-- You must provide policies in JSON format in IAM. However, for
-- CloudFormation templates formatted in YAML, you can provide the policy
-- in JSON or YAML format. CloudFormation always converts a YAML policy to
-- JSON format before submitting it to IAM.
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
newPutRolePolicy ::
  -- | 'roleName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  PutRolePolicy
newPutRolePolicy
  pRoleName_
  pPolicyName_
  pPolicyDocument_ =
    PutRolePolicy'
      { roleName = pRoleName_,
        policyName = pPolicyName_,
        policyDocument = pPolicyDocument_
      }

-- | The name of the role to associate the policy with.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
putRolePolicy_roleName :: Lens.Lens' PutRolePolicy Prelude.Text
putRolePolicy_roleName = Lens.lens (\PutRolePolicy' {roleName} -> roleName) (\s@PutRolePolicy' {} a -> s {roleName = a} :: PutRolePolicy)

-- | The name of the policy document.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
putRolePolicy_policyName :: Lens.Lens' PutRolePolicy Prelude.Text
putRolePolicy_policyName = Lens.lens (\PutRolePolicy' {policyName} -> policyName) (\s@PutRolePolicy' {} a -> s {policyName = a} :: PutRolePolicy)

-- | The policy document.
--
-- You must provide policies in JSON format in IAM. However, for
-- CloudFormation templates formatted in YAML, you can provide the policy
-- in JSON or YAML format. CloudFormation always converts a YAML policy to
-- JSON format before submitting it to IAM.
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
putRolePolicy_policyDocument :: Lens.Lens' PutRolePolicy Prelude.Text
putRolePolicy_policyDocument = Lens.lens (\PutRolePolicy' {policyDocument} -> policyDocument) (\s@PutRolePolicy' {} a -> s {policyDocument = a} :: PutRolePolicy)

instance Core.AWSRequest PutRolePolicy where
  type
    AWSResponse PutRolePolicy =
      PutRolePolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull PutRolePolicyResponse'

instance Prelude.Hashable PutRolePolicy where
  hashWithSalt _salt PutRolePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyDocument

instance Prelude.NFData PutRolePolicy where
  rnf PutRolePolicy' {..} =
    Prelude.rnf roleName `Prelude.seq`
      Prelude.rnf policyName `Prelude.seq`
        Prelude.rnf policyDocument

instance Data.ToHeaders PutRolePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PutRolePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutRolePolicy where
  toQuery PutRolePolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PutRolePolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Data.=: roleName,
        "PolicyName" Data.=: policyName,
        "PolicyDocument" Data.=: policyDocument
      ]

-- | /See:/ 'newPutRolePolicyResponse' smart constructor.
data PutRolePolicyResponse = PutRolePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRolePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutRolePolicyResponse ::
  PutRolePolicyResponse
newPutRolePolicyResponse = PutRolePolicyResponse'

instance Prelude.NFData PutRolePolicyResponse where
  rnf _ = ()
