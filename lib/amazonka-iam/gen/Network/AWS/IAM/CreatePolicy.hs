{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreatePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new managed policy for your AWS account.
--
-- This operation creates a policy version with a version identifier of @v1@ and sets v1 as the policy's default version. For more information about policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
-- For more information about managed policies in general, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.CreatePolicy
  ( -- * Creating a request
    CreatePolicy (..),
    mkCreatePolicy,

    -- ** Request lenses
    cpPath,
    cpDescription,
    cpPolicyName,
    cpPolicyDocument,

    -- * Destructuring the response
    CreatePolicyResponse (..),
    mkCreatePolicyResponse,

    -- ** Response lenses
    cprsPolicy,
    cprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePolicy' smart constructor.
data CreatePolicy = CreatePolicy'
  { path :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreatePolicy' with the minimum fields required to make a request.
--
-- * 'description' - A friendly description of the policy.
--
-- Typically used to store information about the permissions defined in the policy. For example, "Grants access to production DynamoDB tables."
-- The policy description is immutable. After a value is assigned, it cannot be changed.
-- * 'path' - The path for the policy.
--
-- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
-- * 'policyDocument' - The JSON policy document that you want to use as the content for the new policy.
--
-- You must provide policies in JSON format in IAM. However, for AWS CloudFormation templates formatted in YAML, you can provide the policy in JSON or YAML format. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
-- * 'policyName' - The friendly name of the policy.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
mkCreatePolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'policyDocument'
  Lude.Text ->
  CreatePolicy
mkCreatePolicy pPolicyName_ pPolicyDocument_ =
  CreatePolicy'
    { path = Lude.Nothing,
      description = Lude.Nothing,
      policyName = pPolicyName_,
      policyDocument = pPolicyDocument_
    }

-- | The path for the policy.
--
-- For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPath :: Lens.Lens' CreatePolicy (Lude.Maybe Lude.Text)
cpPath = Lens.lens (path :: CreatePolicy -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: CreatePolicy)
{-# DEPRECATED cpPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | A friendly description of the policy.
--
-- Typically used to store information about the permissions defined in the policy. For example, "Grants access to production DynamoDB tables."
-- The policy description is immutable. After a value is assigned, it cannot be changed.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreatePolicy (Lude.Maybe Lude.Text)
cpDescription = Lens.lens (description :: CreatePolicy -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreatePolicy)
{-# DEPRECATED cpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The friendly name of the policy.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPolicyName :: Lens.Lens' CreatePolicy Lude.Text
cpPolicyName = Lens.lens (policyName :: CreatePolicy -> Lude.Text) (\s a -> s {policyName = a} :: CreatePolicy)
{-# DEPRECATED cpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The JSON policy document that you want to use as the content for the new policy.
--
-- You must provide policies in JSON format in IAM. However, for AWS CloudFormation templates formatted in YAML, you can provide the policy in JSON or YAML format. AWS CloudFormation always converts a YAML policy to JSON format before submitting it to IAM.
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPolicyDocument :: Lens.Lens' CreatePolicy Lude.Text
cpPolicyDocument = Lens.lens (policyDocument :: CreatePolicy -> Lude.Text) (\s a -> s {policyDocument = a} :: CreatePolicy)
{-# DEPRECATED cpPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

instance Lude.AWSRequest CreatePolicy where
  type Rs CreatePolicy = CreatePolicyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "CreatePolicyResult"
      ( \s h x ->
          CreatePolicyResponse'
            Lude.<$> (x Lude..@? "Policy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreatePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePolicy where
  toQuery CreatePolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreatePolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Path" Lude.=: path,
        "Description" Lude.=: description,
        "PolicyName" Lude.=: policyName,
        "PolicyDocument" Lude.=: policyDocument
      ]

-- | Contains the response to a successful 'CreatePolicy' request.
--
-- /See:/ 'mkCreatePolicyResponse' smart constructor.
data CreatePolicyResponse = CreatePolicyResponse'
  { policy ::
      Lude.Maybe Policy,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - A structure containing details about the new policy.
-- * 'responseStatus' - The response status code.
mkCreatePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePolicyResponse
mkCreatePolicyResponse pResponseStatus_ =
  CreatePolicyResponse'
    { policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure containing details about the new policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsPolicy :: Lens.Lens' CreatePolicyResponse (Lude.Maybe Policy)
cprsPolicy = Lens.lens (policy :: CreatePolicyResponse -> Lude.Maybe Policy) (\s a -> s {policy = a} :: CreatePolicyResponse)
{-# DEPRECATED cprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreatePolicyResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreatePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePolicyResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
