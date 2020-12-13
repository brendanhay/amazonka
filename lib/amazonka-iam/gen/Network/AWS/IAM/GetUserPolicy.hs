{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetUserPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified inline policy document that is embedded in the specified IAM user.
--
-- An IAM user can also have managed policies attached to it. To retrieve a managed policy document that is attached to a user, use 'GetPolicy' to determine the policy's default version. Then use 'GetPolicyVersion' to retrieve the policy document.
-- For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.GetUserPolicy
  ( -- * Creating a request
    GetUserPolicy (..),
    mkGetUserPolicy,

    -- ** Request lenses
    gupPolicyName,
    gupUserName,

    -- * Destructuring the response
    GetUserPolicyResponse (..),
    mkGetUserPolicyResponse,

    -- ** Response lenses
    guprsPolicyDocument,
    guprsPolicyName,
    guprsUserName,
    guprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetUserPolicy' smart constructor.
data GetUserPolicy = GetUserPolicy'
  { -- | The name of the policy document to get.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    policyName :: Lude.Text,
    -- | The name of the user who the policy is associated with.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserPolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy document to get.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'userName' - The name of the user who the policy is associated with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkGetUserPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  GetUserPolicy
mkGetUserPolicy pPolicyName_ pUserName_ =
  GetUserPolicy' {policyName = pPolicyName_, userName = pUserName_}

-- | The name of the policy document to get.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupPolicyName :: Lens.Lens' GetUserPolicy Lude.Text
gupPolicyName = Lens.lens (policyName :: GetUserPolicy -> Lude.Text) (\s a -> s {policyName = a} :: GetUserPolicy)
{-# DEPRECATED gupPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The name of the user who the policy is associated with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupUserName :: Lens.Lens' GetUserPolicy Lude.Text
gupUserName = Lens.lens (userName :: GetUserPolicy -> Lude.Text) (\s a -> s {userName = a} :: GetUserPolicy)
{-# DEPRECATED gupUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest GetUserPolicy where
  type Rs GetUserPolicy = GetUserPolicyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetUserPolicyResult"
      ( \s h x ->
          GetUserPolicyResponse'
            Lude.<$> (x Lude..@ "PolicyDocument")
            Lude.<*> (x Lude..@ "PolicyName")
            Lude.<*> (x Lude..@ "UserName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUserPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetUserPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetUserPolicy where
  toQuery GetUserPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetUserPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyName" Lude.=: policyName,
        "UserName" Lude.=: userName
      ]

-- | Contains the response to a successful 'GetUserPolicy' request.
--
-- /See:/ 'mkGetUserPolicyResponse' smart constructor.
data GetUserPolicyResponse = GetUserPolicyResponse'
  { -- | The policy document.
    --
    -- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
    policyDocument :: Lude.Text,
    -- | The name of the policy.
    policyName :: Lude.Text,
    -- | The user the policy is associated with.
    userName :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policyDocument' - The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- * 'policyName' - The name of the policy.
-- * 'userName' - The user the policy is associated with.
-- * 'responseStatus' - The response status code.
mkGetUserPolicyResponse ::
  -- | 'policyDocument'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  GetUserPolicyResponse
mkGetUserPolicyResponse
  pPolicyDocument_
  pPolicyName_
  pUserName_
  pResponseStatus_ =
    GetUserPolicyResponse'
      { policyDocument = pPolicyDocument_,
        policyName = pPolicyName_,
        userName = pUserName_,
        responseStatus = pResponseStatus_
      }

-- | The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprsPolicyDocument :: Lens.Lens' GetUserPolicyResponse Lude.Text
guprsPolicyDocument = Lens.lens (policyDocument :: GetUserPolicyResponse -> Lude.Text) (\s a -> s {policyDocument = a} :: GetUserPolicyResponse)
{-# DEPRECATED guprsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprsPolicyName :: Lens.Lens' GetUserPolicyResponse Lude.Text
guprsPolicyName = Lens.lens (policyName :: GetUserPolicyResponse -> Lude.Text) (\s a -> s {policyName = a} :: GetUserPolicyResponse)
{-# DEPRECATED guprsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The user the policy is associated with.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprsUserName :: Lens.Lens' GetUserPolicyResponse Lude.Text
guprsUserName = Lens.lens (userName :: GetUserPolicyResponse -> Lude.Text) (\s a -> s {userName = a} :: GetUserPolicyResponse)
{-# DEPRECATED guprsUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprsResponseStatus :: Lens.Lens' GetUserPolicyResponse Lude.Int
guprsResponseStatus = Lens.lens (responseStatus :: GetUserPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUserPolicyResponse)
{-# DEPRECATED guprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
