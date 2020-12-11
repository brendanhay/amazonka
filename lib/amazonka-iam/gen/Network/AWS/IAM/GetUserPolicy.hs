{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gupUserName,
    gupPolicyName,

    -- * Destructuring the response
    GetUserPolicyResponse (..),
    mkGetUserPolicyResponse,

    -- ** Response lenses
    guprsResponseStatus,
    guprsUserName,
    guprsPolicyName,
    guprsPolicyDocument,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetUserPolicy' smart constructor.
data GetUserPolicy = GetUserPolicy'
  { userName :: Lude.Text,
    policyName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
  -- | 'userName'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  GetUserPolicy
mkGetUserPolicy pUserName_ pPolicyName_ =
  GetUserPolicy' {userName = pUserName_, policyName = pPolicyName_}

-- | The name of the user who the policy is associated with.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupUserName :: Lens.Lens' GetUserPolicy Lude.Text
gupUserName = Lens.lens (userName :: GetUserPolicy -> Lude.Text) (\s a -> s {userName = a} :: GetUserPolicy)
{-# DEPRECATED gupUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The name of the policy document to get.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupPolicyName :: Lens.Lens' GetUserPolicy Lude.Text
gupPolicyName = Lens.lens (policyName :: GetUserPolicy -> Lude.Text) (\s a -> s {policyName = a} :: GetUserPolicy)
{-# DEPRECATED gupPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Lude.AWSRequest GetUserPolicy where
  type Rs GetUserPolicy = GetUserPolicyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetUserPolicyResult"
      ( \s h x ->
          GetUserPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "UserName")
            Lude.<*> (x Lude..@ "PolicyName")
            Lude.<*> (x Lude..@ "PolicyDocument")
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
        "UserName" Lude.=: userName,
        "PolicyName" Lude.=: policyName
      ]

-- | Contains the response to a successful 'GetUserPolicy' request.
--
-- /See:/ 'mkGetUserPolicyResponse' smart constructor.
data GetUserPolicyResponse = GetUserPolicyResponse'
  { responseStatus ::
      Lude.Int,
    userName :: Lude.Text,
    policyName :: Lude.Text,
    policyDocument :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUserPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policyDocument' - The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- * 'policyName' - The name of the policy.
-- * 'responseStatus' - The response status code.
-- * 'userName' - The user the policy is associated with.
mkGetUserPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'userName'
  Lude.Text ->
  -- | 'policyName'
  Lude.Text ->
  -- | 'policyDocument'
  Lude.Text ->
  GetUserPolicyResponse
mkGetUserPolicyResponse
  pResponseStatus_
  pUserName_
  pPolicyName_
  pPolicyDocument_ =
    GetUserPolicyResponse'
      { responseStatus = pResponseStatus_,
        userName = pUserName_,
        policyName = pPolicyName_,
        policyDocument = pPolicyDocument_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprsResponseStatus :: Lens.Lens' GetUserPolicyResponse Lude.Int
guprsResponseStatus = Lens.lens (responseStatus :: GetUserPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUserPolicyResponse)
{-# DEPRECATED guprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The user the policy is associated with.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprsUserName :: Lens.Lens' GetUserPolicyResponse Lude.Text
guprsUserName = Lens.lens (userName :: GetUserPolicyResponse -> Lude.Text) (\s a -> s {userName = a} :: GetUserPolicyResponse)
{-# DEPRECATED guprsUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprsPolicyName :: Lens.Lens' GetUserPolicyResponse Lude.Text
guprsPolicyName = Lens.lens (policyName :: GetUserPolicyResponse -> Lude.Text) (\s a -> s {policyName = a} :: GetUserPolicyResponse)
{-# DEPRECATED guprsPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The policy document.
--
-- IAM stores policies in JSON format. However, resources that were created using AWS CloudFormation templates can be formatted in YAML. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprsPolicyDocument :: Lens.Lens' GetUserPolicyResponse Lude.Text
guprsPolicyDocument = Lens.lens (policyDocument :: GetUserPolicyResponse -> Lude.Text) (\s a -> s {policyDocument = a} :: GetUserPolicyResponse)
{-# DEPRECATED guprsPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}
