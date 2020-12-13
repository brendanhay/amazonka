{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetGroupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified inline policy document that is embedded in the specified IAM group.
--
-- An IAM group can also have managed policies attached to it. To retrieve a managed policy document that is attached to a group, use 'GetPolicy' to determine the policy's default version, then use 'GetPolicyVersion' to retrieve the policy document.
-- For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.GetGroupPolicy
  ( -- * Creating a request
    GetGroupPolicy (..),
    mkGetGroupPolicy,

    -- ** Request lenses
    ggpPolicyName,
    ggpGroupName,

    -- * Destructuring the response
    GetGroupPolicyResponse (..),
    mkGetGroupPolicyResponse,

    -- ** Response lenses
    ggprsPolicyDocument,
    ggprsPolicyName,
    ggprsGroupName,
    ggprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetGroupPolicy' smart constructor.
data GetGroupPolicy = GetGroupPolicy'
  { -- | The name of the policy document to get.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    policyName :: Lude.Text,
    -- | The name of the group the policy is associated with.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    groupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupPolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy document to get.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'groupName' - The name of the group the policy is associated with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkGetGroupPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'groupName'
  Lude.Text ->
  GetGroupPolicy
mkGetGroupPolicy pPolicyName_ pGroupName_ =
  GetGroupPolicy'
    { policyName = pPolicyName_,
      groupName = pGroupName_
    }

-- | The name of the policy document to get.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggpPolicyName :: Lens.Lens' GetGroupPolicy Lude.Text
ggpPolicyName = Lens.lens (policyName :: GetGroupPolicy -> Lude.Text) (\s a -> s {policyName = a} :: GetGroupPolicy)
{-# DEPRECATED ggpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The name of the group the policy is associated with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggpGroupName :: Lens.Lens' GetGroupPolicy Lude.Text
ggpGroupName = Lens.lens (groupName :: GetGroupPolicy -> Lude.Text) (\s a -> s {groupName = a} :: GetGroupPolicy)
{-# DEPRECATED ggpGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest GetGroupPolicy where
  type Rs GetGroupPolicy = GetGroupPolicyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetGroupPolicyResult"
      ( \s h x ->
          GetGroupPolicyResponse'
            Lude.<$> (x Lude..@ "PolicyDocument")
            Lude.<*> (x Lude..@ "PolicyName")
            Lude.<*> (x Lude..@ "GroupName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGroupPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetGroupPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetGroupPolicy where
  toQuery GetGroupPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetGroupPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyName" Lude.=: policyName,
        "GroupName" Lude.=: groupName
      ]

-- | Contains the response to a successful 'GetGroupPolicy' request.
--
-- /See:/ 'mkGetGroupPolicyResponse' smart constructor.
data GetGroupPolicyResponse = GetGroupPolicyResponse'
  { -- | The policy document.
    --
    -- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
    policyDocument :: Lude.Text,
    -- | The name of the policy.
    policyName :: Lude.Text,
    -- | The group the policy is associated with.
    groupName :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGroupPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policyDocument' - The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- * 'policyName' - The name of the policy.
-- * 'groupName' - The group the policy is associated with.
-- * 'responseStatus' - The response status code.
mkGetGroupPolicyResponse ::
  -- | 'policyDocument'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  -- | 'groupName'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  GetGroupPolicyResponse
mkGetGroupPolicyResponse
  pPolicyDocument_
  pPolicyName_
  pGroupName_
  pResponseStatus_ =
    GetGroupPolicyResponse'
      { policyDocument = pPolicyDocument_,
        policyName = pPolicyName_,
        groupName = pGroupName_,
        responseStatus = pResponseStatus_
      }

-- | The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggprsPolicyDocument :: Lens.Lens' GetGroupPolicyResponse Lude.Text
ggprsPolicyDocument = Lens.lens (policyDocument :: GetGroupPolicyResponse -> Lude.Text) (\s a -> s {policyDocument = a} :: GetGroupPolicyResponse)
{-# DEPRECATED ggprsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggprsPolicyName :: Lens.Lens' GetGroupPolicyResponse Lude.Text
ggprsPolicyName = Lens.lens (policyName :: GetGroupPolicyResponse -> Lude.Text) (\s a -> s {policyName = a} :: GetGroupPolicyResponse)
{-# DEPRECATED ggprsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The group the policy is associated with.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggprsGroupName :: Lens.Lens' GetGroupPolicyResponse Lude.Text
ggprsGroupName = Lens.lens (groupName :: GetGroupPolicyResponse -> Lude.Text) (\s a -> s {groupName = a} :: GetGroupPolicyResponse)
{-# DEPRECATED ggprsGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggprsResponseStatus :: Lens.Lens' GetGroupPolicyResponse Lude.Int
ggprsResponseStatus = Lens.lens (responseStatus :: GetGroupPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGroupPolicyResponse)
{-# DEPRECATED ggprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
