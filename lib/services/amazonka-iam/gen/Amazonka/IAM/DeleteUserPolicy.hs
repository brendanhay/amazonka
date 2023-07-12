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
-- Module      : Amazonka.IAM.DeleteUserPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.IAM.DeleteUserPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteUserPolicy where
  type
    AWSResponse DeleteUserPolicy =
      DeleteUserPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteUserPolicyResponse'

instance Prelude.Hashable DeleteUserPolicy where
  hashWithSalt _salt DeleteUserPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData DeleteUserPolicy where
  rnf DeleteUserPolicy' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf policyName

instance Data.ToHeaders DeleteUserPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteUserPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteUserPolicy where
  toQuery DeleteUserPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteUserPolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Data.=: userName,
        "PolicyName" Data.=: policyName
      ]

-- | /See:/ 'newDeleteUserPolicyResponse' smart constructor.
data DeleteUserPolicyResponse = DeleteUserPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserPolicyResponse ::
  DeleteUserPolicyResponse
newDeleteUserPolicyResponse =
  DeleteUserPolicyResponse'

instance Prelude.NFData DeleteUserPolicyResponse where
  rnf _ = ()
