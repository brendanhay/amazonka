{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified managed policy, including the policy's default version and the total number of IAM users, groups, and roles to which the policy is attached. To retrieve the list of the specific users, groups, and roles that the policy is attached to, use the 'ListEntitiesForPolicy' API. This API returns metadata about the policy. To retrieve the actual policy document for a specific version of the policy, use 'GetPolicyVersion' .
--
-- This API retrieves information about managed policies. To retrieve information about an inline policy that is embedded with an IAM user, group, or role, use the 'GetUserPolicy' , 'GetGroupPolicy' , or 'GetRolePolicy' API.
-- For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
module Network.AWS.IAM.GetPolicy
  ( -- * Creating a request
    GetPolicy (..),
    mkGetPolicy,

    -- ** Request lenses
    gpPolicyARN,

    -- * Destructuring the response
    GetPolicyResponse (..),
    mkGetPolicyResponse,

    -- ** Response lenses
    gprsPolicy,
    gprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPolicy' smart constructor.
newtype GetPolicy = GetPolicy'
  { -- | The Amazon Resource Name (ARN) of the managed policy that you want information about.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPolicy' with the minimum fields required to make a request.
--
-- * 'policyARN' - The Amazon Resource Name (ARN) of the managed policy that you want information about.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkGetPolicy ::
  -- | 'policyARN'
  Lude.Text ->
  GetPolicy
mkGetPolicy pPolicyARN_ = GetPolicy' {policyARN = pPolicyARN_}

-- | The Amazon Resource Name (ARN) of the managed policy that you want information about.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPolicyARN :: Lens.Lens' GetPolicy Lude.Text
gpPolicyARN = Lens.lens (policyARN :: GetPolicy -> Lude.Text) (\s a -> s {policyARN = a} :: GetPolicy)
{-# DEPRECATED gpPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Lude.AWSRequest GetPolicy where
  type Rs GetPolicy = GetPolicyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetPolicyResult"
      ( \s h x ->
          GetPolicyResponse'
            Lude.<$> (x Lude..@? "Policy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPolicy where
  toQuery GetPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PolicyArn" Lude.=: policyARN
      ]

-- | Contains the response to a successful 'GetPolicy' request.
--
-- /See:/ 'mkGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { -- | A structure containing details about the policy.
    policy :: Lude.Maybe Policy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - A structure containing details about the policy.
-- * 'responseStatus' - The response status code.
mkGetPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPolicyResponse
mkGetPolicyResponse pResponseStatus_ =
  GetPolicyResponse'
    { policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure containing details about the policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsPolicy :: Lens.Lens' GetPolicyResponse (Lude.Maybe Policy)
gprsPolicy = Lens.lens (policy :: GetPolicyResponse -> Lude.Maybe Policy) (\s a -> s {policy = a} :: GetPolicyResponse)
{-# DEPRECATED gprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsResponseStatus :: Lens.Lens' GetPolicyResponse Lude.Int
gprsResponseStatus = Lens.lens (responseStatus :: GetPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPolicyResponse)
{-# DEPRECATED gprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
