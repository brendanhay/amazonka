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
-- Module      : Network.AWS.IAM.DetachGroupPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified IAM group.
--
-- A group can also have inline policies embedded with it. To delete an
-- inline policy, use DeleteGroupPolicy. For information about policies,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.DetachGroupPolicy
  ( -- * Creating a Request
    DetachGroupPolicy (..),
    newDetachGroupPolicy,

    -- * Request Lenses
    detachGroupPolicy_groupName,
    detachGroupPolicy_policyArn,

    -- * Destructuring the Response
    DetachGroupPolicyResponse (..),
    newDetachGroupPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachGroupPolicy' smart constructor.
data DetachGroupPolicy = DetachGroupPolicy'
  { -- | The name (friendly name, not ARN) of the IAM group to detach the policy
    -- from.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    groupName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'detachGroupPolicy_groupName' - The name (friendly name, not ARN) of the IAM group to detach the policy
-- from.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyArn', 'detachGroupPolicy_policyArn' - The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newDetachGroupPolicy ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'policyArn'
  Prelude.Text ->
  DetachGroupPolicy
newDetachGroupPolicy pGroupName_ pPolicyArn_ =
  DetachGroupPolicy'
    { groupName = pGroupName_,
      policyArn = pPolicyArn_
    }

-- | The name (friendly name, not ARN) of the IAM group to detach the policy
-- from.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
detachGroupPolicy_groupName :: Lens.Lens' DetachGroupPolicy Prelude.Text
detachGroupPolicy_groupName = Lens.lens (\DetachGroupPolicy' {groupName} -> groupName) (\s@DetachGroupPolicy' {} a -> s {groupName = a} :: DetachGroupPolicy)

-- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
detachGroupPolicy_policyArn :: Lens.Lens' DetachGroupPolicy Prelude.Text
detachGroupPolicy_policyArn = Lens.lens (\DetachGroupPolicy' {policyArn} -> policyArn) (\s@DetachGroupPolicy' {} a -> s {policyArn = a} :: DetachGroupPolicy)

instance Prelude.AWSRequest DetachGroupPolicy where
  type Rs DetachGroupPolicy = DetachGroupPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DetachGroupPolicyResponse'

instance Prelude.Hashable DetachGroupPolicy

instance Prelude.NFData DetachGroupPolicy

instance Prelude.ToHeaders DetachGroupPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DetachGroupPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DetachGroupPolicy where
  toQuery DetachGroupPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DetachGroupPolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "GroupName" Prelude.=: groupName,
        "PolicyArn" Prelude.=: policyArn
      ]

-- | /See:/ 'newDetachGroupPolicyResponse' smart constructor.
data DetachGroupPolicyResponse = DetachGroupPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDetachGroupPolicyResponse ::
  DetachGroupPolicyResponse
newDetachGroupPolicyResponse =
  DetachGroupPolicyResponse'

instance Prelude.NFData DetachGroupPolicyResponse
