{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteGroupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified IAM group.
--
-- A group can also have managed policies attached to it. To detach a managed policy from a group, use 'DetachGroupPolicy' . For more information about policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.DeleteGroupPolicy
  ( -- * Creating a request
    DeleteGroupPolicy (..),
    mkDeleteGroupPolicy,

    -- ** Request lenses
    dgpPolicyName,
    dgpGroupName,

    -- * Destructuring the response
    DeleteGroupPolicyResponse (..),
    mkDeleteGroupPolicyResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteGroupPolicy' smart constructor.
data DeleteGroupPolicy = DeleteGroupPolicy'
  { -- | The name identifying the policy document to delete.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    policyName :: Lude.Text,
    -- | The name (friendly name, not ARN) identifying the group that the policy is embedded in.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    groupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGroupPolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name identifying the policy document to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'groupName' - The name (friendly name, not ARN) identifying the group that the policy is embedded in.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkDeleteGroupPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'groupName'
  Lude.Text ->
  DeleteGroupPolicy
mkDeleteGroupPolicy pPolicyName_ pGroupName_ =
  DeleteGroupPolicy'
    { policyName = pPolicyName_,
      groupName = pGroupName_
    }

-- | The name identifying the policy document to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgpPolicyName :: Lens.Lens' DeleteGroupPolicy Lude.Text
dgpPolicyName = Lens.lens (policyName :: DeleteGroupPolicy -> Lude.Text) (\s a -> s {policyName = a} :: DeleteGroupPolicy)
{-# DEPRECATED dgpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The name (friendly name, not ARN) identifying the group that the policy is embedded in.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgpGroupName :: Lens.Lens' DeleteGroupPolicy Lude.Text
dgpGroupName = Lens.lens (groupName :: DeleteGroupPolicy -> Lude.Text) (\s a -> s {groupName = a} :: DeleteGroupPolicy)
{-# DEPRECATED dgpGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest DeleteGroupPolicy where
  type Rs DeleteGroupPolicy = DeleteGroupPolicyResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteGroupPolicyResponse'

instance Lude.ToHeaders DeleteGroupPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteGroupPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteGroupPolicy where
  toQuery DeleteGroupPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteGroupPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyName" Lude.=: policyName,
        "GroupName" Lude.=: groupName
      ]

-- | /See:/ 'mkDeleteGroupPolicyResponse' smart constructor.
data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGroupPolicyResponse' with the minimum fields required to make a request.
mkDeleteGroupPolicyResponse ::
  DeleteGroupPolicyResponse
mkDeleteGroupPolicyResponse = DeleteGroupPolicyResponse'
