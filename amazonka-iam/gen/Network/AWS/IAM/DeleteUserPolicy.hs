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
-- Module      : Network.AWS.IAM.DeleteUserPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified
-- IAM user.
--
-- A user can also have managed policies attached to it. To detach a
-- managed policy from a user, use DetachUserPolicy. For more information
-- about policies, refer to
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.DeleteUserPolicy
  ( -- * Creating a Request
    DeleteUserPolicy (..),
    newDeleteUserPolicy,

    -- * Request Lenses
    deleteUserPolicy_userName,
    deleteUserPolicy_policyName,

    -- * Destructuring the Response
    DeleteUserPolicyResponse (..),
    newDeleteUserPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUserPolicy' smart constructor.
data DeleteUserPolicy = DeleteUserPolicy'
  { -- | The name (friendly name, not ARN) identifying the user that the policy
    -- is embedded in.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text,
    -- | The name identifying the policy document to delete.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'deleteUserPolicy_userName' - The name (friendly name, not ARN) identifying the user that the policy
-- is embedded in.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyName', 'deleteUserPolicy_policyName' - The name identifying the policy document to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newDeleteUserPolicy ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  DeleteUserPolicy
newDeleteUserPolicy pUserName_ pPolicyName_ =
  DeleteUserPolicy'
    { userName = pUserName_,
      policyName = pPolicyName_
    }

-- | The name (friendly name, not ARN) identifying the user that the policy
-- is embedded in.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteUserPolicy_userName :: Lens.Lens' DeleteUserPolicy Prelude.Text
deleteUserPolicy_userName = Lens.lens (\DeleteUserPolicy' {userName} -> userName) (\s@DeleteUserPolicy' {} a -> s {userName = a} :: DeleteUserPolicy)

-- | The name identifying the policy document to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteUserPolicy_policyName :: Lens.Lens' DeleteUserPolicy Prelude.Text
deleteUserPolicy_policyName = Lens.lens (\DeleteUserPolicy' {policyName} -> policyName) (\s@DeleteUserPolicy' {} a -> s {policyName = a} :: DeleteUserPolicy)

instance Prelude.AWSRequest DeleteUserPolicy where
  type Rs DeleteUserPolicy = DeleteUserPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteUserPolicyResponse'

instance Prelude.Hashable DeleteUserPolicy

instance Prelude.NFData DeleteUserPolicy

instance Prelude.ToHeaders DeleteUserPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteUserPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteUserPolicy where
  toQuery DeleteUserPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteUserPolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName,
        "PolicyName" Prelude.=: policyName
      ]

-- | /See:/ 'newDeleteUserPolicyResponse' smart constructor.
data DeleteUserPolicyResponse = DeleteUserPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserPolicyResponse ::
  DeleteUserPolicyResponse
newDeleteUserPolicyResponse =
  DeleteUserPolicyResponse'

instance Prelude.NFData DeleteUserPolicyResponse
