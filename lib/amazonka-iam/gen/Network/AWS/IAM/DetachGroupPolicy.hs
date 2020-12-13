{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DetachGroupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified managed policy from the specified IAM group.
--
-- A group can also have inline policies embedded with it. To delete an inline policy, use the 'DeleteGroupPolicy' API. For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DetachGroupPolicy
  ( -- * Creating a request
    DetachGroupPolicy (..),
    mkDetachGroupPolicy,

    -- ** Request lenses
    dPolicyARN,
    dGroupName,

    -- * Destructuring the response
    DetachGroupPolicyResponse (..),
    mkDetachGroupPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachGroupPolicy' smart constructor.
data DetachGroupPolicy = DetachGroupPolicy'
  { -- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyARN :: Lude.Text,
    -- | The name (friendly name, not ARN) of the IAM group to detach the policy from.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    groupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachGroupPolicy' with the minimum fields required to make a request.
--
-- * 'policyARN' - The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
-- * 'groupName' - The name (friendly name, not ARN) of the IAM group to detach the policy from.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkDetachGroupPolicy ::
  -- | 'policyARN'
  Lude.Text ->
  -- | 'groupName'
  Lude.Text ->
  DetachGroupPolicy
mkDetachGroupPolicy pPolicyARN_ pGroupName_ =
  DetachGroupPolicy'
    { policyARN = pPolicyARN_,
      groupName = pGroupName_
    }

-- | The Amazon Resource Name (ARN) of the IAM policy you want to detach.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPolicyARN :: Lens.Lens' DetachGroupPolicy Lude.Text
dPolicyARN = Lens.lens (policyARN :: DetachGroupPolicy -> Lude.Text) (\s a -> s {policyARN = a} :: DetachGroupPolicy)
{-# DEPRECATED dPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | The name (friendly name, not ARN) of the IAM group to detach the policy from.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGroupName :: Lens.Lens' DetachGroupPolicy Lude.Text
dGroupName = Lens.lens (groupName :: DetachGroupPolicy -> Lude.Text) (\s a -> s {groupName = a} :: DetachGroupPolicy)
{-# DEPRECATED dGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest DetachGroupPolicy where
  type Rs DetachGroupPolicy = DetachGroupPolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DetachGroupPolicyResponse'

instance Lude.ToHeaders DetachGroupPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetachGroupPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachGroupPolicy where
  toQuery DetachGroupPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DetachGroupPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyArn" Lude.=: policyARN,
        "GroupName" Lude.=: groupName
      ]

-- | /See:/ 'mkDetachGroupPolicyResponse' smart constructor.
data DetachGroupPolicyResponse = DetachGroupPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachGroupPolicyResponse' with the minimum fields required to make a request.
mkDetachGroupPolicyResponse ::
  DetachGroupPolicyResponse
mkDetachGroupPolicyResponse = DetachGroupPolicyResponse'
