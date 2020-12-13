{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreatePolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the specified managed policy. To update a managed policy, you create a new policy version. A managed policy can have up to five versions. If the policy has five versions, you must delete an existing version using 'DeletePolicyVersion' before you create a new version.
--
-- Optionally, you can set the new version as the policy's default version. The default version is the version that is in effect for the IAM users, groups, and roles to which the policy is attached.
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.CreatePolicyVersion
  ( -- * Creating a request
    CreatePolicyVersion (..),
    mkCreatePolicyVersion,

    -- ** Request lenses
    cpvPolicyDocument,
    cpvSetAsDefault,
    cpvPolicyARN,

    -- * Destructuring the response
    CreatePolicyVersionResponse (..),
    mkCreatePolicyVersionResponse,

    -- ** Response lenses
    cpvrsPolicyVersion,
    cpvrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePolicyVersion' smart constructor.
data CreatePolicyVersion = CreatePolicyVersion'
  { -- | The JSON policy document that you want to use as the content for this new version of the policy.
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
    policyDocument :: Lude.Text,
    -- | Specifies whether to set this version as the policy's default version.
    --
    -- When this parameter is @true@ , the new policy version becomes the operative version. That is, it becomes the version that is in effect for the IAM users, groups, and roles that the policy is attached to.
    -- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
    setAsDefault :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Name (ARN) of the IAM policy to which you want to add a new version.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePolicyVersion' with the minimum fields required to make a request.
--
-- * 'policyDocument' - The JSON policy document that you want to use as the content for this new version of the policy.
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
-- * 'setAsDefault' - Specifies whether to set this version as the policy's default version.
--
-- When this parameter is @true@ , the new policy version becomes the operative version. That is, it becomes the version that is in effect for the IAM users, groups, and roles that the policy is attached to.
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
-- * 'policyARN' - The Amazon Resource Name (ARN) of the IAM policy to which you want to add a new version.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkCreatePolicyVersion ::
  -- | 'policyDocument'
  Lude.Text ->
  -- | 'policyARN'
  Lude.Text ->
  CreatePolicyVersion
mkCreatePolicyVersion pPolicyDocument_ pPolicyARN_ =
  CreatePolicyVersion'
    { policyDocument = pPolicyDocument_,
      setAsDefault = Lude.Nothing,
      policyARN = pPolicyARN_
    }

-- | The JSON policy document that you want to use as the content for this new version of the policy.
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
cpvPolicyDocument :: Lens.Lens' CreatePolicyVersion Lude.Text
cpvPolicyDocument = Lens.lens (policyDocument :: CreatePolicyVersion -> Lude.Text) (\s a -> s {policyDocument = a} :: CreatePolicyVersion)
{-# DEPRECATED cpvPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | Specifies whether to set this version as the policy's default version.
--
-- When this parameter is @true@ , the new policy version becomes the operative version. That is, it becomes the version that is in effect for the IAM users, groups, and roles that the policy is attached to.
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'setAsDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvSetAsDefault :: Lens.Lens' CreatePolicyVersion (Lude.Maybe Lude.Bool)
cpvSetAsDefault = Lens.lens (setAsDefault :: CreatePolicyVersion -> Lude.Maybe Lude.Bool) (\s a -> s {setAsDefault = a} :: CreatePolicyVersion)
{-# DEPRECATED cpvSetAsDefault "Use generic-lens or generic-optics with 'setAsDefault' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM policy to which you want to add a new version.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPolicyARN :: Lens.Lens' CreatePolicyVersion Lude.Text
cpvPolicyARN = Lens.lens (policyARN :: CreatePolicyVersion -> Lude.Text) (\s a -> s {policyARN = a} :: CreatePolicyVersion)
{-# DEPRECATED cpvPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.AWSRequest CreatePolicyVersion where
  type Rs CreatePolicyVersion = CreatePolicyVersionResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "CreatePolicyVersionResult"
      ( \s h x ->
          CreatePolicyVersionResponse'
            Lude.<$> (x Lude..@? "PolicyVersion")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePolicyVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreatePolicyVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePolicyVersion where
  toQuery CreatePolicyVersion' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreatePolicyVersion" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyDocument" Lude.=: policyDocument,
        "SetAsDefault" Lude.=: setAsDefault,
        "PolicyArn" Lude.=: policyARN
      ]

-- | Contains the response to a successful 'CreatePolicyVersion' request.
--
-- /See:/ 'mkCreatePolicyVersionResponse' smart constructor.
data CreatePolicyVersionResponse = CreatePolicyVersionResponse'
  { -- | A structure containing details about the new policy version.
    policyVersion :: Lude.Maybe PolicyVersion,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePolicyVersionResponse' with the minimum fields required to make a request.
--
-- * 'policyVersion' - A structure containing details about the new policy version.
-- * 'responseStatus' - The response status code.
mkCreatePolicyVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePolicyVersionResponse
mkCreatePolicyVersionResponse pResponseStatus_ =
  CreatePolicyVersionResponse'
    { policyVersion = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure containing details about the new policy version.
--
-- /Note:/ Consider using 'policyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrsPolicyVersion :: Lens.Lens' CreatePolicyVersionResponse (Lude.Maybe PolicyVersion)
cpvrsPolicyVersion = Lens.lens (policyVersion :: CreatePolicyVersionResponse -> Lude.Maybe PolicyVersion) (\s a -> s {policyVersion = a} :: CreatePolicyVersionResponse)
{-# DEPRECATED cpvrsPolicyVersion "Use generic-lens or generic-optics with 'policyVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrsResponseStatus :: Lens.Lens' CreatePolicyVersionResponse Lude.Int
cpvrsResponseStatus = Lens.lens (responseStatus :: CreatePolicyVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePolicyVersionResponse)
{-# DEPRECATED cpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
