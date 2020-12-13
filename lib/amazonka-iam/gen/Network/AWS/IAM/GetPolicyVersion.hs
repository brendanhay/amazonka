{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetPolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified version of the specified managed policy, including the policy document.
--
-- To list the available versions for a policy, use 'ListPolicyVersions' .
-- This API retrieves information about managed policies. To retrieve information about an inline policy that is embedded in a user, group, or role, use the 'GetUserPolicy' , 'GetGroupPolicy' , or 'GetRolePolicy' API.
-- For more information about the types of policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- For more information about managed policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.GetPolicyVersion
  ( -- * Creating a request
    GetPolicyVersion (..),
    mkGetPolicyVersion,

    -- ** Request lenses
    gpvVersionId,
    gpvPolicyARN,

    -- * Destructuring the response
    GetPolicyVersionResponse (..),
    mkGetPolicyVersionResponse,

    -- ** Response lenses
    gpvrsPolicyVersion,
    gpvrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPolicyVersion' smart constructor.
data GetPolicyVersion = GetPolicyVersion'
  { -- | Identifies the policy version to retrieve.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consists of the lowercase letter 'v' followed by one or two digits, and optionally followed by a period '.' and a string of letters and digits.
    versionId :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the managed policy that you want information about.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPolicyVersion' with the minimum fields required to make a request.
--
-- * 'versionId' - Identifies the policy version to retrieve.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consists of the lowercase letter 'v' followed by one or two digits, and optionally followed by a period '.' and a string of letters and digits.
-- * 'policyARN' - The Amazon Resource Name (ARN) of the managed policy that you want information about.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkGetPolicyVersion ::
  -- | 'versionId'
  Lude.Text ->
  -- | 'policyARN'
  Lude.Text ->
  GetPolicyVersion
mkGetPolicyVersion pVersionId_ pPolicyARN_ =
  GetPolicyVersion'
    { versionId = pVersionId_,
      policyARN = pPolicyARN_
    }

-- | Identifies the policy version to retrieve.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consists of the lowercase letter 'v' followed by one or two digits, and optionally followed by a period '.' and a string of letters and digits.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvVersionId :: Lens.Lens' GetPolicyVersion Lude.Text
gpvVersionId = Lens.lens (versionId :: GetPolicyVersion -> Lude.Text) (\s a -> s {versionId = a} :: GetPolicyVersion)
{-# DEPRECATED gpvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The Amazon Resource Name (ARN) of the managed policy that you want information about.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvPolicyARN :: Lens.Lens' GetPolicyVersion Lude.Text
gpvPolicyARN = Lens.lens (policyARN :: GetPolicyVersion -> Lude.Text) (\s a -> s {policyARN = a} :: GetPolicyVersion)
{-# DEPRECATED gpvPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.AWSRequest GetPolicyVersion where
  type Rs GetPolicyVersion = GetPolicyVersionResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetPolicyVersionResult"
      ( \s h x ->
          GetPolicyVersionResponse'
            Lude.<$> (x Lude..@? "PolicyVersion")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPolicyVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetPolicyVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPolicyVersion where
  toQuery GetPolicyVersion' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetPolicyVersion" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "VersionId" Lude.=: versionId,
        "PolicyArn" Lude.=: policyARN
      ]

-- | Contains the response to a successful 'GetPolicyVersion' request.
--
-- /See:/ 'mkGetPolicyVersionResponse' smart constructor.
data GetPolicyVersionResponse = GetPolicyVersionResponse'
  { -- | A structure containing details about the policy version.
    policyVersion :: Lude.Maybe PolicyVersion,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPolicyVersionResponse' with the minimum fields required to make a request.
--
-- * 'policyVersion' - A structure containing details about the policy version.
-- * 'responseStatus' - The response status code.
mkGetPolicyVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPolicyVersionResponse
mkGetPolicyVersionResponse pResponseStatus_ =
  GetPolicyVersionResponse'
    { policyVersion = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure containing details about the policy version.
--
-- /Note:/ Consider using 'policyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrsPolicyVersion :: Lens.Lens' GetPolicyVersionResponse (Lude.Maybe PolicyVersion)
gpvrsPolicyVersion = Lens.lens (policyVersion :: GetPolicyVersionResponse -> Lude.Maybe PolicyVersion) (\s a -> s {policyVersion = a} :: GetPolicyVersionResponse)
{-# DEPRECATED gpvrsPolicyVersion "Use generic-lens or generic-optics with 'policyVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpvrsResponseStatus :: Lens.Lens' GetPolicyVersionResponse Lude.Int
gpvrsResponseStatus = Lens.lens (responseStatus :: GetPolicyVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPolicyVersionResponse)
{-# DEPRECATED gpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
