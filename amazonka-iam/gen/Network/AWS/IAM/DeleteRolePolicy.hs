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
-- Module      : Network.AWS.IAM.DeleteRolePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified
-- IAM role.
--
-- A role can also have managed policies attached to it. To detach a
-- managed policy from a role, use DetachRolePolicy. For more information
-- about policies, refer to
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.DeleteRolePolicy
  ( -- * Creating a Request
    DeleteRolePolicy (..),
    newDeleteRolePolicy,

    -- * Request Lenses
    deleteRolePolicy_roleName,
    deleteRolePolicy_policyName,

    -- * Destructuring the Response
    DeleteRolePolicyResponse (..),
    newDeleteRolePolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRolePolicy' smart constructor.
data DeleteRolePolicy = DeleteRolePolicy'
  { -- | The name (friendly name, not ARN) identifying the role that the policy
    -- is embedded in.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text,
    -- | The name of the inline policy to delete from the specified IAM role.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRolePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'deleteRolePolicy_roleName' - The name (friendly name, not ARN) identifying the role that the policy
-- is embedded in.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyName', 'deleteRolePolicy_policyName' - The name of the inline policy to delete from the specified IAM role.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newDeleteRolePolicy ::
  -- | 'roleName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  DeleteRolePolicy
newDeleteRolePolicy pRoleName_ pPolicyName_ =
  DeleteRolePolicy'
    { roleName = pRoleName_,
      policyName = pPolicyName_
    }

-- | The name (friendly name, not ARN) identifying the role that the policy
-- is embedded in.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteRolePolicy_roleName :: Lens.Lens' DeleteRolePolicy Prelude.Text
deleteRolePolicy_roleName = Lens.lens (\DeleteRolePolicy' {roleName} -> roleName) (\s@DeleteRolePolicy' {} a -> s {roleName = a} :: DeleteRolePolicy)

-- | The name of the inline policy to delete from the specified IAM role.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteRolePolicy_policyName :: Lens.Lens' DeleteRolePolicy Prelude.Text
deleteRolePolicy_policyName = Lens.lens (\DeleteRolePolicy' {policyName} -> policyName) (\s@DeleteRolePolicy' {} a -> s {policyName = a} :: DeleteRolePolicy)

instance Prelude.AWSRequest DeleteRolePolicy where
  type Rs DeleteRolePolicy = DeleteRolePolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteRolePolicyResponse'

instance Prelude.Hashable DeleteRolePolicy

instance Prelude.NFData DeleteRolePolicy

instance Prelude.ToHeaders DeleteRolePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteRolePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRolePolicy where
  toQuery DeleteRolePolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteRolePolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "RoleName" Prelude.=: roleName,
        "PolicyName" Prelude.=: policyName
      ]

-- | /See:/ 'newDeleteRolePolicyResponse' smart constructor.
data DeleteRolePolicyResponse = DeleteRolePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRolePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRolePolicyResponse ::
  DeleteRolePolicyResponse
newDeleteRolePolicyResponse =
  DeleteRolePolicyResponse'

instance Prelude.NFData DeleteRolePolicyResponse
