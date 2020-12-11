{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeletePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified managed policy.
--
-- Before you can delete a managed policy, you must first detach the policy from all users, groups, and roles that it is attached to. In addition, you must delete all the policy's versions. The following steps describe the process for deleting a managed policy:
--
--     * Detach the policy from all users, groups, and roles that the policy is attached to, using the 'DetachUserPolicy' , 'DetachGroupPolicy' , or 'DetachRolePolicy' API operations. To list all the users, groups, and roles that a policy is attached to, use 'ListEntitiesForPolicy' .
--
--
--     * Delete all versions of the policy using 'DeletePolicyVersion' . To list the policy's versions, use 'ListPolicyVersions' . You cannot use 'DeletePolicyVersion' to delete the version that is marked as the default version. You delete the policy's default version in the next step of the process.
--
--
--     * Delete the policy (this automatically deletes the policy's default version) using this API.
--
--
-- For information about managed policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DeletePolicy
  ( -- * Creating a request
    DeletePolicy (..),
    mkDeletePolicy,

    -- ** Request lenses
    dpPolicyARN,

    -- * Destructuring the response
    DeletePolicyResponse (..),
    mkDeletePolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePolicy' smart constructor.
newtype DeletePolicy = DeletePolicy' {policyARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePolicy' with the minimum fields required to make a request.
--
-- * 'policyARN' - The Amazon Resource Name (ARN) of the IAM policy you want to delete.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkDeletePolicy ::
  -- | 'policyARN'
  Lude.Text ->
  DeletePolicy
mkDeletePolicy pPolicyARN_ = DeletePolicy' {policyARN = pPolicyARN_}

-- | The Amazon Resource Name (ARN) of the IAM policy you want to delete.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPolicyARN :: Lens.Lens' DeletePolicy Lude.Text
dpPolicyARN = Lens.lens (policyARN :: DeletePolicy -> Lude.Text) (\s a -> s {policyARN = a} :: DeletePolicy)
{-# DEPRECATED dpPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.AWSRequest DeletePolicy where
  type Rs DeletePolicy = DeletePolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeletePolicyResponse'

instance Lude.ToHeaders DeletePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeletePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePolicy where
  toQuery DeletePolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeletePolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyArn" Lude.=: policyARN
      ]

-- | /See:/ 'mkDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePolicyResponse' with the minimum fields required to make a request.
mkDeletePolicyResponse ::
  DeletePolicyResponse
mkDeletePolicyResponse = DeletePolicyResponse'
