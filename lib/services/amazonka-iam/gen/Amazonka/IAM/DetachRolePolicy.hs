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
-- Module      : Amazonka.IAM.DetachRolePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified role.
--
-- A role can also have inline policies embedded with it. To delete an
-- inline policy, use DeleteRolePolicy. For information about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
module Amazonka.IAM.DetachRolePolicy
  ( -- * Creating a Request
    DetachRolePolicy (..),
    newDetachRolePolicy,

    -- * Request Lenses
    detachRolePolicy_roleName,
    detachRolePolicy_policyArn,

    -- * Destructuring the Response
    DetachRolePolicyResponse (..),
    newDetachRolePolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetachRolePolicy' smart constructor.
data DetachRolePolicy = DetachRolePolicy'
  { -- | The name (friendly name, not ARN) of the IAM role to detach the policy
    -- from.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachRolePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'detachRolePolicy_roleName' - The name (friendly name, not ARN) of the IAM role to detach the policy
-- from.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyArn', 'detachRolePolicy_policyArn' - The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
newDetachRolePolicy ::
  -- | 'roleName'
  Prelude.Text ->
  -- | 'policyArn'
  Prelude.Text ->
  DetachRolePolicy
newDetachRolePolicy pRoleName_ pPolicyArn_ =
  DetachRolePolicy'
    { roleName = pRoleName_,
      policyArn = pPolicyArn_
    }

-- | The name (friendly name, not ARN) of the IAM role to detach the policy
-- from.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
detachRolePolicy_roleName :: Lens.Lens' DetachRolePolicy Prelude.Text
detachRolePolicy_roleName = Lens.lens (\DetachRolePolicy' {roleName} -> roleName) (\s@DetachRolePolicy' {} a -> s {roleName = a} :: DetachRolePolicy)

-- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
detachRolePolicy_policyArn :: Lens.Lens' DetachRolePolicy Prelude.Text
detachRolePolicy_policyArn = Lens.lens (\DetachRolePolicy' {policyArn} -> policyArn) (\s@DetachRolePolicy' {} a -> s {policyArn = a} :: DetachRolePolicy)

instance Core.AWSRequest DetachRolePolicy where
  type
    AWSResponse DetachRolePolicy =
      DetachRolePolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DetachRolePolicyResponse'

instance Prelude.Hashable DetachRolePolicy where
  hashWithSalt _salt DetachRolePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData DetachRolePolicy where
  rnf DetachRolePolicy' {..} =
    Prelude.rnf roleName `Prelude.seq`
      Prelude.rnf policyArn

instance Data.ToHeaders DetachRolePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DetachRolePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DetachRolePolicy where
  toQuery DetachRolePolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DetachRolePolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Data.=: roleName,
        "PolicyArn" Data.=: policyArn
      ]

-- | /See:/ 'newDetachRolePolicyResponse' smart constructor.
data DetachRolePolicyResponse = DetachRolePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachRolePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDetachRolePolicyResponse ::
  DetachRolePolicyResponse
newDetachRolePolicyResponse =
  DetachRolePolicyResponse'

instance Prelude.NFData DetachRolePolicyResponse where
  rnf _ = ()
