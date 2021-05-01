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
-- Module      : Network.AWS.IAM.DetachUserPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified user.
--
-- A user can also have inline policies embedded with it. To delete an
-- inline policy, use DeleteUserPolicy. For information about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.DetachUserPolicy
  ( -- * Creating a Request
    DetachUserPolicy (..),
    newDetachUserPolicy,

    -- * Request Lenses
    detachUserPolicy_userName,
    detachUserPolicy_policyArn,

    -- * Destructuring the Response
    DetachUserPolicyResponse (..),
    newDetachUserPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachUserPolicy' smart constructor.
data DetachUserPolicy = DetachUserPolicy'
  { -- | The name (friendly name, not ARN) of the IAM user to detach the policy
    -- from.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachUserPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'detachUserPolicy_userName' - The name (friendly name, not ARN) of the IAM user to detach the policy
-- from.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyArn', 'detachUserPolicy_policyArn' - The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newDetachUserPolicy ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'policyArn'
  Prelude.Text ->
  DetachUserPolicy
newDetachUserPolicy pUserName_ pPolicyArn_ =
  DetachUserPolicy'
    { userName = pUserName_,
      policyArn = pPolicyArn_
    }

-- | The name (friendly name, not ARN) of the IAM user to detach the policy
-- from.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
detachUserPolicy_userName :: Lens.Lens' DetachUserPolicy Prelude.Text
detachUserPolicy_userName = Lens.lens (\DetachUserPolicy' {userName} -> userName) (\s@DetachUserPolicy' {} a -> s {userName = a} :: DetachUserPolicy)

-- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
detachUserPolicy_policyArn :: Lens.Lens' DetachUserPolicy Prelude.Text
detachUserPolicy_policyArn = Lens.lens (\DetachUserPolicy' {policyArn} -> policyArn) (\s@DetachUserPolicy' {} a -> s {policyArn = a} :: DetachUserPolicy)

instance Prelude.AWSRequest DetachUserPolicy where
  type Rs DetachUserPolicy = DetachUserPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DetachUserPolicyResponse'

instance Prelude.Hashable DetachUserPolicy

instance Prelude.NFData DetachUserPolicy

instance Prelude.ToHeaders DetachUserPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DetachUserPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DetachUserPolicy where
  toQuery DetachUserPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DetachUserPolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName,
        "PolicyArn" Prelude.=: policyArn
      ]

-- | /See:/ 'newDetachUserPolicyResponse' smart constructor.
data DetachUserPolicyResponse = DetachUserPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachUserPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDetachUserPolicyResponse ::
  DetachUserPolicyResponse
newDetachUserPolicyResponse =
  DetachUserPolicyResponse'

instance Prelude.NFData DetachUserPolicyResponse
