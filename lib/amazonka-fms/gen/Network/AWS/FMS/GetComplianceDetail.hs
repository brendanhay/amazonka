{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetComplianceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed compliance information about the specified member account. Details include resources that are in and out of compliance with the specified policy. Resources are considered noncompliant for AWS WAF and Shield Advanced policies if the specified policy has not been applied to them. Resources are considered noncompliant for security group policies if they are in scope of the policy, they violate one or more of the policy rules, and remediation is disabled or not possible. Resources are considered noncompliant for Network Firewall policies if a firewall is missing in the VPC, if the firewall endpoint isn't set up in an expected Availability Zone and subnet, if a subnet created by the Firewall Manager doesn't have the expected route table, and for modifications to a firewall policy that violate the Firewall Manager policy's rules.
module Network.AWS.FMS.GetComplianceDetail
  ( -- * Creating a request
    GetComplianceDetail (..),
    mkGetComplianceDetail,

    -- ** Request lenses
    gcdPolicyId,
    gcdMemberAccount,

    -- * Destructuring the response
    GetComplianceDetailResponse (..),
    mkGetComplianceDetailResponse,

    -- ** Response lenses
    gcdrsPolicyComplianceDetail,
    gcdrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetComplianceDetail' smart constructor.
data GetComplianceDetail = GetComplianceDetail'
  { policyId ::
      Lude.Text,
    memberAccount :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetComplianceDetail' with the minimum fields required to make a request.
--
-- * 'memberAccount' - The AWS account that owns the resources that you want to get the details for.
-- * 'policyId' - The ID of the policy that you want to get the details for. @PolicyId@ is returned by @PutPolicy@ and by @ListPolicies@ .
mkGetComplianceDetail ::
  -- | 'policyId'
  Lude.Text ->
  -- | 'memberAccount'
  Lude.Text ->
  GetComplianceDetail
mkGetComplianceDetail pPolicyId_ pMemberAccount_ =
  GetComplianceDetail'
    { policyId = pPolicyId_,
      memberAccount = pMemberAccount_
    }

-- | The ID of the policy that you want to get the details for. @PolicyId@ is returned by @PutPolicy@ and by @ListPolicies@ .
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdPolicyId :: Lens.Lens' GetComplianceDetail Lude.Text
gcdPolicyId = Lens.lens (policyId :: GetComplianceDetail -> Lude.Text) (\s a -> s {policyId = a} :: GetComplianceDetail)
{-# DEPRECATED gcdPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The AWS account that owns the resources that you want to get the details for.
--
-- /Note:/ Consider using 'memberAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdMemberAccount :: Lens.Lens' GetComplianceDetail Lude.Text
gcdMemberAccount = Lens.lens (memberAccount :: GetComplianceDetail -> Lude.Text) (\s a -> s {memberAccount = a} :: GetComplianceDetail)
{-# DEPRECATED gcdMemberAccount "Use generic-lens or generic-optics with 'memberAccount' instead." #-}

instance Lude.AWSRequest GetComplianceDetail where
  type Rs GetComplianceDetail = GetComplianceDetailResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetComplianceDetailResponse'
            Lude.<$> (x Lude..?> "PolicyComplianceDetail")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetComplianceDetail where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.GetComplianceDetail" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetComplianceDetail where
  toJSON GetComplianceDetail' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PolicyId" Lude..= policyId),
            Lude.Just ("MemberAccount" Lude..= memberAccount)
          ]
      )

instance Lude.ToPath GetComplianceDetail where
  toPath = Lude.const "/"

instance Lude.ToQuery GetComplianceDetail where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetComplianceDetailResponse' smart constructor.
data GetComplianceDetailResponse = GetComplianceDetailResponse'
  { policyComplianceDetail ::
      Lude.Maybe PolicyComplianceDetail,
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

-- | Creates a value of 'GetComplianceDetailResponse' with the minimum fields required to make a request.
--
-- * 'policyComplianceDetail' - Information about the resources and the policy that you specified in the @GetComplianceDetail@ request.
-- * 'responseStatus' - The response status code.
mkGetComplianceDetailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetComplianceDetailResponse
mkGetComplianceDetailResponse pResponseStatus_ =
  GetComplianceDetailResponse'
    { policyComplianceDetail =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the resources and the policy that you specified in the @GetComplianceDetail@ request.
--
-- /Note:/ Consider using 'policyComplianceDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrsPolicyComplianceDetail :: Lens.Lens' GetComplianceDetailResponse (Lude.Maybe PolicyComplianceDetail)
gcdrsPolicyComplianceDetail = Lens.lens (policyComplianceDetail :: GetComplianceDetailResponse -> Lude.Maybe PolicyComplianceDetail) (\s a -> s {policyComplianceDetail = a} :: GetComplianceDetailResponse)
{-# DEPRECATED gcdrsPolicyComplianceDetail "Use generic-lens or generic-optics with 'policyComplianceDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdrsResponseStatus :: Lens.Lens' GetComplianceDetailResponse Lude.Int
gcdrsResponseStatus = Lens.lens (responseStatus :: GetComplianceDetailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetComplianceDetailResponse)
{-# DEPRECATED gcdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
