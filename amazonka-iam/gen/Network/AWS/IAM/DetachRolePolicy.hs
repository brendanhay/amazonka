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
-- Module      : Network.AWS.IAM.DetachRolePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified role.
--
-- A role can also have inline policies embedded with it. To delete an
-- inline policy, use DeleteRolePolicy. For information about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.DetachRolePolicy
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    -- in the /AWS General Reference/.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- in the /AWS General Reference/.
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
-- in the /AWS General Reference/.
detachRolePolicy_policyArn :: Lens.Lens' DetachRolePolicy Prelude.Text
detachRolePolicy_policyArn = Lens.lens (\DetachRolePolicy' {policyArn} -> policyArn) (\s@DetachRolePolicy' {} a -> s {policyArn = a} :: DetachRolePolicy)

instance Prelude.AWSRequest DetachRolePolicy where
  type Rs DetachRolePolicy = DetachRolePolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DetachRolePolicyResponse'

instance Prelude.Hashable DetachRolePolicy

instance Prelude.NFData DetachRolePolicy

instance Prelude.ToHeaders DetachRolePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DetachRolePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DetachRolePolicy where
  toQuery DetachRolePolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DetachRolePolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Prelude.=: roleName,
        "PolicyArn" Prelude.=: policyArn
      ]

-- | /See:/ 'newDetachRolePolicyResponse' smart constructor.
data DetachRolePolicyResponse = DetachRolePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachRolePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDetachRolePolicyResponse ::
  DetachRolePolicyResponse
newDetachRolePolicyResponse =
  DetachRolePolicyResponse'

instance Prelude.NFData DetachRolePolicyResponse
