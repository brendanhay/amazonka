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
-- Module      : Amazonka.IAM.DeleteGroupPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified
-- IAM group.
--
-- A group can also have managed policies attached to it. To detach a
-- managed policy from a group, use DetachGroupPolicy. For more information
-- about policies, refer to
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
module Amazonka.IAM.DeleteGroupPolicy
  ( -- * Creating a Request
    DeleteGroupPolicy (..),
    newDeleteGroupPolicy,

    -- * Request Lenses
    deleteGroupPolicy_groupName,
    deleteGroupPolicy_policyName,

    -- * Destructuring the Response
    DeleteGroupPolicyResponse (..),
    newDeleteGroupPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGroupPolicy' smart constructor.
data DeleteGroupPolicy = DeleteGroupPolicy'
  { -- | The name (friendly name, not ARN) identifying the group that the policy
    -- is embedded in.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    groupName :: Prelude.Text,
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
-- Create a value of 'DeleteGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'deleteGroupPolicy_groupName' - The name (friendly name, not ARN) identifying the group that the policy
-- is embedded in.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyName', 'deleteGroupPolicy_policyName' - The name identifying the policy document to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newDeleteGroupPolicy ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  DeleteGroupPolicy
newDeleteGroupPolicy pGroupName_ pPolicyName_ =
  DeleteGroupPolicy'
    { groupName = pGroupName_,
      policyName = pPolicyName_
    }

-- | The name (friendly name, not ARN) identifying the group that the policy
-- is embedded in.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteGroupPolicy_groupName :: Lens.Lens' DeleteGroupPolicy Prelude.Text
deleteGroupPolicy_groupName = Lens.lens (\DeleteGroupPolicy' {groupName} -> groupName) (\s@DeleteGroupPolicy' {} a -> s {groupName = a} :: DeleteGroupPolicy)

-- | The name identifying the policy document to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
deleteGroupPolicy_policyName :: Lens.Lens' DeleteGroupPolicy Prelude.Text
deleteGroupPolicy_policyName = Lens.lens (\DeleteGroupPolicy' {policyName} -> policyName) (\s@DeleteGroupPolicy' {} a -> s {policyName = a} :: DeleteGroupPolicy)

instance Core.AWSRequest DeleteGroupPolicy where
  type
    AWSResponse DeleteGroupPolicy =
      DeleteGroupPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteGroupPolicyResponse'

instance Prelude.Hashable DeleteGroupPolicy where
  hashWithSalt _salt DeleteGroupPolicy' {..} =
    _salt `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData DeleteGroupPolicy where
  rnf DeleteGroupPolicy' {..} =
    Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf policyName

instance Data.ToHeaders DeleteGroupPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteGroupPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteGroupPolicy where
  toQuery DeleteGroupPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteGroupPolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "GroupName" Data.=: groupName,
        "PolicyName" Data.=: policyName
      ]

-- | /See:/ 'newDeleteGroupPolicyResponse' smart constructor.
data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteGroupPolicyResponse ::
  DeleteGroupPolicyResponse
newDeleteGroupPolicyResponse =
  DeleteGroupPolicyResponse'

instance Prelude.NFData DeleteGroupPolicyResponse where
  rnf _ = ()
