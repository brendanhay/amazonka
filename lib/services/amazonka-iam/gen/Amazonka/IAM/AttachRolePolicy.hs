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
-- Module      : Amazonka.IAM.AttachRolePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified managed policy to the specified IAM role. When
-- you attach a managed policy to a role, the managed policy becomes part
-- of the role\'s permission (access) policy.
--
-- You cannot use a managed policy as the role\'s trust policy. The role\'s
-- trust policy is created at the same time as the role, using CreateRole.
-- You can update a role\'s trust policy using UpdateAssumeRolePolicy.
--
-- Use this operation to attach a /managed/ policy to a role. To embed an
-- inline policy in a role, use PutRolePolicy. For more information about
-- policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- As a best practice, you can validate your IAM policies. To learn more,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_policy-validator.html Validating IAM policies>
-- in the /IAM User Guide/.
module Amazonka.IAM.AttachRolePolicy
  ( -- * Creating a Request
    AttachRolePolicy (..),
    newAttachRolePolicy,

    -- * Request Lenses
    attachRolePolicy_roleName,
    attachRolePolicy_policyArn,

    -- * Destructuring the Response
    AttachRolePolicyResponse (..),
    newAttachRolePolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachRolePolicy' smart constructor.
data AttachRolePolicy = AttachRolePolicy'
  { -- | The name (friendly name, not ARN) of the role to attach the policy to.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM policy you want to attach.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachRolePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'attachRolePolicy_roleName' - The name (friendly name, not ARN) of the role to attach the policy to.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyArn', 'attachRolePolicy_policyArn' - The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
newAttachRolePolicy ::
  -- | 'roleName'
  Prelude.Text ->
  -- | 'policyArn'
  Prelude.Text ->
  AttachRolePolicy
newAttachRolePolicy pRoleName_ pPolicyArn_ =
  AttachRolePolicy'
    { roleName = pRoleName_,
      policyArn = pPolicyArn_
    }

-- | The name (friendly name, not ARN) of the role to attach the policy to.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
attachRolePolicy_roleName :: Lens.Lens' AttachRolePolicy Prelude.Text
attachRolePolicy_roleName = Lens.lens (\AttachRolePolicy' {roleName} -> roleName) (\s@AttachRolePolicy' {} a -> s {roleName = a} :: AttachRolePolicy)

-- | The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
attachRolePolicy_policyArn :: Lens.Lens' AttachRolePolicy Prelude.Text
attachRolePolicy_policyArn = Lens.lens (\AttachRolePolicy' {policyArn} -> policyArn) (\s@AttachRolePolicy' {} a -> s {policyArn = a} :: AttachRolePolicy)

instance Core.AWSRequest AttachRolePolicy where
  type
    AWSResponse AttachRolePolicy =
      AttachRolePolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull AttachRolePolicyResponse'

instance Prelude.Hashable AttachRolePolicy where
  hashWithSalt _salt AttachRolePolicy' {..} =
    _salt `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData AttachRolePolicy where
  rnf AttachRolePolicy' {..} =
    Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf policyArn

instance Data.ToHeaders AttachRolePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AttachRolePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery AttachRolePolicy where
  toQuery AttachRolePolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AttachRolePolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Data.=: roleName,
        "PolicyArn" Data.=: policyArn
      ]

-- | /See:/ 'newAttachRolePolicyResponse' smart constructor.
data AttachRolePolicyResponse = AttachRolePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachRolePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAttachRolePolicyResponse ::
  AttachRolePolicyResponse
newAttachRolePolicyResponse =
  AttachRolePolicyResponse'

instance Prelude.NFData AttachRolePolicyResponse where
  rnf _ = ()
