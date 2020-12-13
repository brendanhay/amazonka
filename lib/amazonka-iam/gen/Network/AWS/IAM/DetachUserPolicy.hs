{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DetachUserPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified user.
--
-- A user can also have inline policies embedded with it. To delete an inline policy, use the 'DeleteUserPolicy' API. For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DetachUserPolicy
  ( -- * Creating a request
    DetachUserPolicy (..),
    mkDetachUserPolicy,

    -- ** Request lenses
    dupUserName,
    dupPolicyARN,

    -- * Destructuring the response
    DetachUserPolicyResponse (..),
    mkDetachUserPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachUserPolicy' smart constructor.
data DetachUserPolicy = DetachUserPolicy'
  { -- | The name (friendly name, not ARN) of the IAM user to detach the policy from.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachUserPolicy' with the minimum fields required to make a request.
--
-- * 'userName' - The name (friendly name, not ARN) of the IAM user to detach the policy from.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'policyARN' - The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkDetachUserPolicy ::
  -- | 'userName'
  Lude.Text ->
  -- | 'policyARN'
  Lude.Text ->
  DetachUserPolicy
mkDetachUserPolicy pUserName_ pPolicyARN_ =
  DetachUserPolicy' {userName = pUserName_, policyARN = pPolicyARN_}

-- | The name (friendly name, not ARN) of the IAM user to detach the policy from.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupUserName :: Lens.Lens' DetachUserPolicy Lude.Text
dupUserName = Lens.lens (userName :: DetachUserPolicy -> Lude.Text) (\s a -> s {userName = a} :: DetachUserPolicy)
{-# DEPRECATED dupUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupPolicyARN :: Lens.Lens' DetachUserPolicy Lude.Text
dupPolicyARN = Lens.lens (policyARN :: DetachUserPolicy -> Lude.Text) (\s a -> s {policyARN = a} :: DetachUserPolicy)
{-# DEPRECATED dupPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.AWSRequest DetachUserPolicy where
  type Rs DetachUserPolicy = DetachUserPolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DetachUserPolicyResponse'

instance Lude.ToHeaders DetachUserPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetachUserPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachUserPolicy where
  toQuery DetachUserPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DetachUserPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "PolicyArn" Lude.=: policyARN
      ]

-- | /See:/ 'mkDetachUserPolicyResponse' smart constructor.
data DetachUserPolicyResponse = DetachUserPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachUserPolicyResponse' with the minimum fields required to make a request.
mkDetachUserPolicyResponse ::
  DetachUserPolicyResponse
mkDetachUserPolicyResponse = DetachUserPolicyResponse'
