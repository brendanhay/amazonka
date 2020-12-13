{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.AssociateDRTRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the DDoS Response Team (DRT), using the specified role, to access your AWS account to assist with DDoS attack mitigation during potential attacks. This enables the DRT to inspect your AWS WAF configuration and create or update AWS WAF rules and web ACLs.
--
-- You can associate only one @RoleArn@ with your subscription. If you submit an @AssociateDRTRole@ request for an account that already has an associated role, the new @RoleArn@ will replace the existing @RoleArn@ .
-- Prior to making the @AssociateDRTRole@ request, you must attach the <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy> managed policy to the role you will specify in the request. For more information see < https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies> . The role must also trust the service principal @drt.shield.amazonaws.com@ . For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html IAM JSON Policy Elements: Principal> .
-- The DRT will have access only to your AWS WAF and Shield resources. By submitting this request, you authorize the DRT to inspect your AWS WAF and Shield configuration and create and update AWS WAF rules and web ACLs on your behalf. The DRT takes these actions only if explicitly authorized by you.
-- You must have the @iam:PassRole@ permission to make an @AssociateDRTRole@ request. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_passrole.html Granting a User Permissions to Pass a Role to an AWS Service> .
-- To use the services of the DRT and make an @AssociateDRTRole@ request, you must be subscribed to the <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan> or the <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan> .
module Network.AWS.Shield.AssociateDRTRole
  ( -- * Creating a request
    AssociateDRTRole (..),
    mkAssociateDRTRole,

    -- ** Request lenses
    adrtrRoleARN,

    -- * Destructuring the response
    AssociateDRTRoleResponse (..),
    mkAssociateDRTRoleResponse,

    -- ** Response lenses
    adrtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkAssociateDRTRole' smart constructor.
newtype AssociateDRTRole = AssociateDRTRole'
  { -- | The Amazon Resource Name (ARN) of the role the DRT will use to access your AWS account.
    --
    -- Prior to making the @AssociateDRTRole@ request, you must attach the <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy> managed policy to this role. For more information see < https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies> .
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDRTRole' with the minimum fields required to make a request.
--
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role the DRT will use to access your AWS account.
--
-- Prior to making the @AssociateDRTRole@ request, you must attach the <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy> managed policy to this role. For more information see < https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies> .
mkAssociateDRTRole ::
  -- | 'roleARN'
  Lude.Text ->
  AssociateDRTRole
mkAssociateDRTRole pRoleARN_ =
  AssociateDRTRole' {roleARN = pRoleARN_}

-- | The Amazon Resource Name (ARN) of the role the DRT will use to access your AWS account.
--
-- Prior to making the @AssociateDRTRole@ request, you must attach the <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy> managed policy to this role. For more information see < https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrtrRoleARN :: Lens.Lens' AssociateDRTRole Lude.Text
adrtrRoleARN = Lens.lens (roleARN :: AssociateDRTRole -> Lude.Text) (\s a -> s {roleARN = a} :: AssociateDRTRole)
{-# DEPRECATED adrtrRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest AssociateDRTRole where
  type Rs AssociateDRTRole = AssociateDRTRoleResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateDRTRoleResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateDRTRole where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.AssociateDRTRole" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateDRTRole where
  toJSON AssociateDRTRole' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RoleArn" Lude..= roleARN)])

instance Lude.ToPath AssociateDRTRole where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateDRTRole where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateDRTRoleResponse' smart constructor.
newtype AssociateDRTRoleResponse = AssociateDRTRoleResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDRTRoleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateDRTRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateDRTRoleResponse
mkAssociateDRTRoleResponse pResponseStatus_ =
  AssociateDRTRoleResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrtrrsResponseStatus :: Lens.Lens' AssociateDRTRoleResponse Lude.Int
adrtrrsResponseStatus = Lens.lens (responseStatus :: AssociateDRTRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateDRTRoleResponse)
{-# DEPRECATED adrtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
