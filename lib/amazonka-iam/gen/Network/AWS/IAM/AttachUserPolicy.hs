{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AttachUserPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified managed policy to the specified user.
--
-- You use this API to attach a /managed/ policy to a user. To embed an inline policy in a user, use 'PutUserPolicy' .
-- For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.AttachUserPolicy
  ( -- * Creating a request
    AttachUserPolicy (..),
    mkAttachUserPolicy,

    -- ** Request lenses
    aupUserName,
    aupPolicyARN,

    -- * Destructuring the response
    AttachUserPolicyResponse (..),
    mkAttachUserPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachUserPolicy' smart constructor.
data AttachUserPolicy = AttachUserPolicy'
  { userName :: Lude.Text,
    policyARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachUserPolicy' with the minimum fields required to make a request.
--
-- * 'policyARN' - The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
-- * 'userName' - The name (friendly name, not ARN) of the IAM user to attach the policy to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkAttachUserPolicy ::
  -- | 'userName'
  Lude.Text ->
  -- | 'policyARN'
  Lude.Text ->
  AttachUserPolicy
mkAttachUserPolicy pUserName_ pPolicyARN_ =
  AttachUserPolicy' {userName = pUserName_, policyARN = pPolicyARN_}

-- | The name (friendly name, not ARN) of the IAM user to attach the policy to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aupUserName :: Lens.Lens' AttachUserPolicy Lude.Text
aupUserName = Lens.lens (userName :: AttachUserPolicy -> Lude.Text) (\s a -> s {userName = a} :: AttachUserPolicy)
{-# DEPRECATED aupUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aupPolicyARN :: Lens.Lens' AttachUserPolicy Lude.Text
aupPolicyARN = Lens.lens (policyARN :: AttachUserPolicy -> Lude.Text) (\s a -> s {policyARN = a} :: AttachUserPolicy)
{-# DEPRECATED aupPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.AWSRequest AttachUserPolicy where
  type Rs AttachUserPolicy = AttachUserPolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull AttachUserPolicyResponse'

instance Lude.ToHeaders AttachUserPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AttachUserPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachUserPolicy where
  toQuery AttachUserPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AttachUserPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "PolicyArn" Lude.=: policyARN
      ]

-- | /See:/ 'mkAttachUserPolicyResponse' smart constructor.
data AttachUserPolicyResponse = AttachUserPolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachUserPolicyResponse' with the minimum fields required to make a request.
mkAttachUserPolicyResponse ::
  AttachUserPolicyResponse
mkAttachUserPolicyResponse = AttachUserPolicyResponse'
