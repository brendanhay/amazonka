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
-- Module      : Network.AWS.IAM.UpdateAssumeRolePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the policy that grants an IAM entity permission to assume a
-- role. This is typically referred to as the \"role trust policy\". For
-- more information about roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using roles to delegate permissions and federate identities>.
module Network.AWS.IAM.UpdateAssumeRolePolicy
  ( -- * Creating a Request
    UpdateAssumeRolePolicy (..),
    newUpdateAssumeRolePolicy,

    -- * Request Lenses
    updateAssumeRolePolicy_roleName,
    updateAssumeRolePolicy_policyDocument,

    -- * Destructuring the Response
    UpdateAssumeRolePolicyResponse (..),
    newUpdateAssumeRolePolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAssumeRolePolicy' smart constructor.
data UpdateAssumeRolePolicy = UpdateAssumeRolePolicy'
  { -- | The name of the role to update with the new policy.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text,
    -- | The policy that grants an entity permission to assume the role.
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
    policyDocument :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssumeRolePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'updateAssumeRolePolicy_roleName' - The name of the role to update with the new policy.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyDocument', 'updateAssumeRolePolicy_policyDocument' - The policy that grants an entity permission to assume the role.
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
newUpdateAssumeRolePolicy ::
  -- | 'roleName'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  UpdateAssumeRolePolicy
newUpdateAssumeRolePolicy pRoleName_ pPolicyDocument_ =
  UpdateAssumeRolePolicy'
    { roleName = pRoleName_,
      policyDocument = pPolicyDocument_
    }

-- | The name of the role to update with the new policy.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
updateAssumeRolePolicy_roleName :: Lens.Lens' UpdateAssumeRolePolicy Prelude.Text
updateAssumeRolePolicy_roleName = Lens.lens (\UpdateAssumeRolePolicy' {roleName} -> roleName) (\s@UpdateAssumeRolePolicy' {} a -> s {roleName = a} :: UpdateAssumeRolePolicy)

-- | The policy that grants an entity permission to assume the role.
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
updateAssumeRolePolicy_policyDocument :: Lens.Lens' UpdateAssumeRolePolicy Prelude.Text
updateAssumeRolePolicy_policyDocument = Lens.lens (\UpdateAssumeRolePolicy' {policyDocument} -> policyDocument) (\s@UpdateAssumeRolePolicy' {} a -> s {policyDocument = a} :: UpdateAssumeRolePolicy)

instance Prelude.AWSRequest UpdateAssumeRolePolicy where
  type
    Rs UpdateAssumeRolePolicy =
      UpdateAssumeRolePolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UpdateAssumeRolePolicyResponse'

instance Prelude.Hashable UpdateAssumeRolePolicy

instance Prelude.NFData UpdateAssumeRolePolicy

instance Prelude.ToHeaders UpdateAssumeRolePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UpdateAssumeRolePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateAssumeRolePolicy where
  toQuery UpdateAssumeRolePolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UpdateAssumeRolePolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Prelude.=: roleName,
        "PolicyDocument" Prelude.=: policyDocument
      ]

-- | /See:/ 'newUpdateAssumeRolePolicyResponse' smart constructor.
data UpdateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssumeRolePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateAssumeRolePolicyResponse ::
  UpdateAssumeRolePolicyResponse
newUpdateAssumeRolePolicyResponse =
  UpdateAssumeRolePolicyResponse'

instance
  Prelude.NFData
    UpdateAssumeRolePolicyResponse
