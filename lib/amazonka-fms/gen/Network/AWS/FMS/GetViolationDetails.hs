{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetViolationDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves violations for a resource based on the specified AWS Firewall Manager policy and AWS account.
module Network.AWS.FMS.GetViolationDetails
  ( -- * Creating a request
    GetViolationDetails (..),
    mkGetViolationDetails,

    -- ** Request lenses
    gvdResourceId,
    gvdResourceType,
    gvdPolicyId,
    gvdMemberAccount,

    -- * Destructuring the response
    GetViolationDetailsResponse (..),
    mkGetViolationDetailsResponse,

    -- ** Response lenses
    gvdrsViolationDetail,
    gvdrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetViolationDetails' smart constructor.
data GetViolationDetails = GetViolationDetails'
  { -- | The ID of the resource that has violations.
    resourceId :: Lude.Text,
    -- | The resource type. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Supported resource types are: @AWS::EC2::Instance@ , @AWS::EC2::NetworkInterface@ , @AWS::EC2::SecurityGroup@ , @AWS::NetworkFirewall::FirewallPolicy@ , and @AWS::EC2::Subnet@ .
    resourceType :: Lude.Text,
    -- | The ID of the AWS Firewall Manager policy that you want the details for. This currently only supports security group content audit policies.
    policyId :: Lude.Text,
    -- | The AWS account ID that you want the details for.
    memberAccount :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetViolationDetails' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource that has violations.
-- * 'resourceType' - The resource type. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Supported resource types are: @AWS::EC2::Instance@ , @AWS::EC2::NetworkInterface@ , @AWS::EC2::SecurityGroup@ , @AWS::NetworkFirewall::FirewallPolicy@ , and @AWS::EC2::Subnet@ .
-- * 'policyId' - The ID of the AWS Firewall Manager policy that you want the details for. This currently only supports security group content audit policies.
-- * 'memberAccount' - The AWS account ID that you want the details for.
mkGetViolationDetails ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  Lude.Text ->
  -- | 'policyId'
  Lude.Text ->
  -- | 'memberAccount'
  Lude.Text ->
  GetViolationDetails
mkGetViolationDetails
  pResourceId_
  pResourceType_
  pPolicyId_
  pMemberAccount_ =
    GetViolationDetails'
      { resourceId = pResourceId_,
        resourceType = pResourceType_,
        policyId = pPolicyId_,
        memberAccount = pMemberAccount_
      }

-- | The ID of the resource that has violations.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvdResourceId :: Lens.Lens' GetViolationDetails Lude.Text
gvdResourceId = Lens.lens (resourceId :: GetViolationDetails -> Lude.Text) (\s a -> s {resourceId = a} :: GetViolationDetails)
{-# DEPRECATED gvdResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Supported resource types are: @AWS::EC2::Instance@ , @AWS::EC2::NetworkInterface@ , @AWS::EC2::SecurityGroup@ , @AWS::NetworkFirewall::FirewallPolicy@ , and @AWS::EC2::Subnet@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvdResourceType :: Lens.Lens' GetViolationDetails Lude.Text
gvdResourceType = Lens.lens (resourceType :: GetViolationDetails -> Lude.Text) (\s a -> s {resourceType = a} :: GetViolationDetails)
{-# DEPRECATED gvdResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The ID of the AWS Firewall Manager policy that you want the details for. This currently only supports security group content audit policies.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvdPolicyId :: Lens.Lens' GetViolationDetails Lude.Text
gvdPolicyId = Lens.lens (policyId :: GetViolationDetails -> Lude.Text) (\s a -> s {policyId = a} :: GetViolationDetails)
{-# DEPRECATED gvdPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The AWS account ID that you want the details for.
--
-- /Note:/ Consider using 'memberAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvdMemberAccount :: Lens.Lens' GetViolationDetails Lude.Text
gvdMemberAccount = Lens.lens (memberAccount :: GetViolationDetails -> Lude.Text) (\s a -> s {memberAccount = a} :: GetViolationDetails)
{-# DEPRECATED gvdMemberAccount "Use generic-lens or generic-optics with 'memberAccount' instead." #-}

instance Lude.AWSRequest GetViolationDetails where
  type Rs GetViolationDetails = GetViolationDetailsResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetViolationDetailsResponse'
            Lude.<$> (x Lude..?> "ViolationDetail")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetViolationDetails where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.GetViolationDetails" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetViolationDetails where
  toJSON GetViolationDetails' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("ResourceType" Lude..= resourceType),
            Lude.Just ("PolicyId" Lude..= policyId),
            Lude.Just ("MemberAccount" Lude..= memberAccount)
          ]
      )

instance Lude.ToPath GetViolationDetails where
  toPath = Lude.const "/"

instance Lude.ToQuery GetViolationDetails where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetViolationDetailsResponse' smart constructor.
data GetViolationDetailsResponse = GetViolationDetailsResponse'
  { -- | Violation detail for a resource.
    violationDetail :: Lude.Maybe ViolationDetail,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetViolationDetailsResponse' with the minimum fields required to make a request.
--
-- * 'violationDetail' - Violation detail for a resource.
-- * 'responseStatus' - The response status code.
mkGetViolationDetailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetViolationDetailsResponse
mkGetViolationDetailsResponse pResponseStatus_ =
  GetViolationDetailsResponse'
    { violationDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Violation detail for a resource.
--
-- /Note:/ Consider using 'violationDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvdrsViolationDetail :: Lens.Lens' GetViolationDetailsResponse (Lude.Maybe ViolationDetail)
gvdrsViolationDetail = Lens.lens (violationDetail :: GetViolationDetailsResponse -> Lude.Maybe ViolationDetail) (\s a -> s {violationDetail = a} :: GetViolationDetailsResponse)
{-# DEPRECATED gvdrsViolationDetail "Use generic-lens or generic-optics with 'violationDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvdrsResponseStatus :: Lens.Lens' GetViolationDetailsResponse Lude.Int
gvdrsResponseStatus = Lens.lens (responseStatus :: GetViolationDetailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetViolationDetailsResponse)
{-# DEPRECATED gvdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
