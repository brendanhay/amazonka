{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AttachGroupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified managed policy to the specified IAM group.
--
-- You use this API to attach a managed policy to a group. To embed an inline policy in a group, use 'PutGroupPolicy' .
-- For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.AttachGroupPolicy
  ( -- * Creating a request
    AttachGroupPolicy (..),
    mkAttachGroupPolicy,

    -- ** Request lenses
    agpGroupName,
    agpPolicyARN,

    -- * Destructuring the response
    AttachGroupPolicyResponse (..),
    mkAttachGroupPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachGroupPolicy' smart constructor.
data AttachGroupPolicy = AttachGroupPolicy'
  { groupName :: Lude.Text,
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

-- | Creates a value of 'AttachGroupPolicy' with the minimum fields required to make a request.
--
-- * 'groupName' - The name (friendly name, not ARN) of the group to attach the policy to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'policyARN' - The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkAttachGroupPolicy ::
  -- | 'groupName'
  Lude.Text ->
  -- | 'policyARN'
  Lude.Text ->
  AttachGroupPolicy
mkAttachGroupPolicy pGroupName_ pPolicyARN_ =
  AttachGroupPolicy'
    { groupName = pGroupName_,
      policyARN = pPolicyARN_
    }

-- | The name (friendly name, not ARN) of the group to attach the policy to.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agpGroupName :: Lens.Lens' AttachGroupPolicy Lude.Text
agpGroupName = Lens.lens (groupName :: AttachGroupPolicy -> Lude.Text) (\s a -> s {groupName = a} :: AttachGroupPolicy)
{-# DEPRECATED agpGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM policy you want to attach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agpPolicyARN :: Lens.Lens' AttachGroupPolicy Lude.Text
agpPolicyARN = Lens.lens (policyARN :: AttachGroupPolicy -> Lude.Text) (\s a -> s {policyARN = a} :: AttachGroupPolicy)
{-# DEPRECATED agpPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.AWSRequest AttachGroupPolicy where
  type Rs AttachGroupPolicy = AttachGroupPolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull AttachGroupPolicyResponse'

instance Lude.ToHeaders AttachGroupPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AttachGroupPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachGroupPolicy where
  toQuery AttachGroupPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AttachGroupPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "GroupName" Lude.=: groupName,
        "PolicyArn" Lude.=: policyARN
      ]

-- | /See:/ 'mkAttachGroupPolicyResponse' smart constructor.
data AttachGroupPolicyResponse = AttachGroupPolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachGroupPolicyResponse' with the minimum fields required to make a request.
mkAttachGroupPolicyResponse ::
  AttachGroupPolicyResponse
mkAttachGroupPolicyResponse = AttachGroupPolicyResponse'
