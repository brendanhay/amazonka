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
-- Module      : Network.AWS.IAM.DeletePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified managed policy.
--
-- Before you can delete a managed policy, you must first detach the policy
-- from all users, groups, and roles that it is attached to. In addition,
-- you must delete all the policy\'s versions. The following steps describe
-- the process for deleting a managed policy:
--
-- -   Detach the policy from all users, groups, and roles that the policy
--     is attached to, using DetachUserPolicy, DetachGroupPolicy, or
--     DetachRolePolicy. To list all the users, groups, and roles that a
--     policy is attached to, use ListEntitiesForPolicy.
--
-- -   Delete all versions of the policy using DeletePolicyVersion. To list
--     the policy\'s versions, use ListPolicyVersions. You cannot use
--     DeletePolicyVersion to delete the version that is marked as the
--     default version. You delete the policy\'s default version in the
--     next step of the process.
--
-- -   Delete the policy (this automatically deletes the policy\'s default
--     version) using this operation.
--
-- For information about managed policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.DeletePolicy
  ( -- * Creating a Request
    DeletePolicy (..),
    newDeletePolicy,

    -- * Request Lenses
    deletePolicy_policyArn,

    -- * Destructuring the Response
    DeletePolicyResponse (..),
    newDeletePolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { -- | The Amazon Resource Name (ARN) of the IAM policy you want to delete.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyArn', 'deletePolicy_policyArn' - The Amazon Resource Name (ARN) of the IAM policy you want to delete.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newDeletePolicy ::
  -- | 'policyArn'
  Prelude.Text ->
  DeletePolicy
newDeletePolicy pPolicyArn_ =
  DeletePolicy' {policyArn = pPolicyArn_}

-- | The Amazon Resource Name (ARN) of the IAM policy you want to delete.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
deletePolicy_policyArn :: Lens.Lens' DeletePolicy Prelude.Text
deletePolicy_policyArn = Lens.lens (\DeletePolicy' {policyArn} -> policyArn) (\s@DeletePolicy' {} a -> s {policyArn = a} :: DeletePolicy)

instance Prelude.AWSRequest DeletePolicy where
  type Rs DeletePolicy = DeletePolicyResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeletePolicyResponse'

instance Prelude.Hashable DeletePolicy

instance Prelude.NFData DeletePolicy

instance Prelude.ToHeaders DeletePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeletePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeletePolicy where
  toQuery DeletePolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeletePolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "PolicyArn" Prelude.=: policyArn
      ]

-- | /See:/ 'newDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePolicyResponse ::
  DeletePolicyResponse
newDeletePolicyResponse = DeletePolicyResponse'

instance Prelude.NFData DeletePolicyResponse
