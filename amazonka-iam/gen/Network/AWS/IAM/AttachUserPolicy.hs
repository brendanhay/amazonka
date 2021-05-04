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
-- Module      : Network.AWS.IAM.AttachUserPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified managed policy to the specified user.
--
-- You use this operation to attach a /managed/ policy to a user. To embed
-- an inline policy in a user, use PutUserPolicy.
--
-- For more information about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
module Network.AWS.IAM.AttachUserPolicy
  ( -- * Creating a Request
    AttachUserPolicy (..),
    newAttachUserPolicy,

    -- * Request Lenses
    attachUserPolicy_userName,
    attachUserPolicy_policyArn,

    -- * Destructuring the Response
    AttachUserPolicyResponse (..),
    newAttachUserPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachUserPolicy' smart constructor.
data AttachUserPolicy = AttachUserPolicy'
  { -- | The name (friendly name, not ARN) of the IAM user to attach the policy
    -- to.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM policy you want to attach.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttachUserPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'attachUserPolicy_userName' - The name (friendly name, not ARN) of the IAM user to attach the policy
-- to.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'policyArn', 'attachUserPolicy_policyArn' - The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newAttachUserPolicy ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'policyArn'
  Prelude.Text ->
  AttachUserPolicy
newAttachUserPolicy pUserName_ pPolicyArn_ =
  AttachUserPolicy'
    { userName = pUserName_,
      policyArn = pPolicyArn_
    }

-- | The name (friendly name, not ARN) of the IAM user to attach the policy
-- to.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
attachUserPolicy_userName :: Lens.Lens' AttachUserPolicy Prelude.Text
attachUserPolicy_userName = Lens.lens (\AttachUserPolicy' {userName} -> userName) (\s@AttachUserPolicy' {} a -> s {userName = a} :: AttachUserPolicy)

-- | The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
attachUserPolicy_policyArn :: Lens.Lens' AttachUserPolicy Prelude.Text
attachUserPolicy_policyArn = Lens.lens (\AttachUserPolicy' {policyArn} -> policyArn) (\s@AttachUserPolicy' {} a -> s {policyArn = a} :: AttachUserPolicy)

instance Prelude.AWSRequest AttachUserPolicy where
  type Rs AttachUserPolicy = AttachUserPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull AttachUserPolicyResponse'

instance Prelude.Hashable AttachUserPolicy

instance Prelude.NFData AttachUserPolicy

instance Prelude.ToHeaders AttachUserPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AttachUserPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AttachUserPolicy where
  toQuery AttachUserPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("AttachUserPolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Prelude.=: userName,
        "PolicyArn" Prelude.=: policyArn
      ]

-- | /See:/ 'newAttachUserPolicyResponse' smart constructor.
data AttachUserPolicyResponse = AttachUserPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttachUserPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAttachUserPolicyResponse ::
  AttachUserPolicyResponse
newAttachUserPolicyResponse =
  AttachUserPolicyResponse'

instance Prelude.NFData AttachUserPolicyResponse
