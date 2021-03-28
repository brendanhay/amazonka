{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetViolationDetails (..)
    , mkGetViolationDetails
    -- ** Request lenses
    , gvdPolicyId
    , gvdMemberAccount
    , gvdResourceId
    , gvdResourceType

    -- * Destructuring the response
    , GetViolationDetailsResponse (..)
    , mkGetViolationDetailsResponse
    -- ** Response lenses
    , gvdrrsViolationDetail
    , gvdrrsResponseStatus
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetViolationDetails' smart constructor.
data GetViolationDetails = GetViolationDetails'
  { policyId :: Types.PolicyId
    -- ^ The ID of the AWS Firewall Manager policy that you want the details for. This currently only supports security group content audit policies.
  , memberAccount :: Types.AWSAccountId
    -- ^ The AWS account ID that you want the details for.
  , resourceId :: Types.ResourceId
    -- ^ The ID of the resource that has violations.
  , resourceType :: Types.ResourceType
    -- ^ The resource type. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Supported resource types are: @AWS::EC2::Instance@ , @AWS::EC2::NetworkInterface@ , @AWS::EC2::SecurityGroup@ , @AWS::NetworkFirewall::FirewallPolicy@ , and @AWS::EC2::Subnet@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetViolationDetails' value with any optional fields omitted.
mkGetViolationDetails
    :: Types.PolicyId -- ^ 'policyId'
    -> Types.AWSAccountId -- ^ 'memberAccount'
    -> Types.ResourceId -- ^ 'resourceId'
    -> Types.ResourceType -- ^ 'resourceType'
    -> GetViolationDetails
mkGetViolationDetails policyId memberAccount resourceId
  resourceType
  = GetViolationDetails'{policyId, memberAccount, resourceId,
                         resourceType}

-- | The ID of the AWS Firewall Manager policy that you want the details for. This currently only supports security group content audit policies.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvdPolicyId :: Lens.Lens' GetViolationDetails Types.PolicyId
gvdPolicyId = Lens.field @"policyId"
{-# INLINEABLE gvdPolicyId #-}
{-# DEPRECATED policyId "Use generic-lens or generic-optics with 'policyId' instead"  #-}

-- | The AWS account ID that you want the details for.
--
-- /Note:/ Consider using 'memberAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvdMemberAccount :: Lens.Lens' GetViolationDetails Types.AWSAccountId
gvdMemberAccount = Lens.field @"memberAccount"
{-# INLINEABLE gvdMemberAccount #-}
{-# DEPRECATED memberAccount "Use generic-lens or generic-optics with 'memberAccount' instead"  #-}

-- | The ID of the resource that has violations.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvdResourceId :: Lens.Lens' GetViolationDetails Types.ResourceId
gvdResourceId = Lens.field @"resourceId"
{-# INLINEABLE gvdResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The resource type. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . Supported resource types are: @AWS::EC2::Instance@ , @AWS::EC2::NetworkInterface@ , @AWS::EC2::SecurityGroup@ , @AWS::NetworkFirewall::FirewallPolicy@ , and @AWS::EC2::Subnet@ . 
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvdResourceType :: Lens.Lens' GetViolationDetails Types.ResourceType
gvdResourceType = Lens.field @"resourceType"
{-# INLINEABLE gvdResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.ToQuery GetViolationDetails where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetViolationDetails where
        toHeaders GetViolationDetails{..}
          = Core.pure ("X-Amz-Target", "AWSFMS_20180101.GetViolationDetails")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetViolationDetails where
        toJSON GetViolationDetails{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PolicyId" Core..= policyId),
                  Core.Just ("MemberAccount" Core..= memberAccount),
                  Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("ResourceType" Core..= resourceType)])

instance Core.AWSRequest GetViolationDetails where
        type Rs GetViolationDetails = GetViolationDetailsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetViolationDetailsResponse' Core.<$>
                   (x Core..:? "ViolationDetail") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetViolationDetailsResponse' smart constructor.
data GetViolationDetailsResponse = GetViolationDetailsResponse'
  { violationDetail :: Core.Maybe Types.ViolationDetail
    -- ^ Violation detail for a resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetViolationDetailsResponse' value with any optional fields omitted.
mkGetViolationDetailsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetViolationDetailsResponse
mkGetViolationDetailsResponse responseStatus
  = GetViolationDetailsResponse'{violationDetail = Core.Nothing,
                                 responseStatus}

-- | Violation detail for a resource.
--
-- /Note:/ Consider using 'violationDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvdrrsViolationDetail :: Lens.Lens' GetViolationDetailsResponse (Core.Maybe Types.ViolationDetail)
gvdrrsViolationDetail = Lens.field @"violationDetail"
{-# INLINEABLE gvdrrsViolationDetail #-}
{-# DEPRECATED violationDetail "Use generic-lens or generic-optics with 'violationDetail' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvdrrsResponseStatus :: Lens.Lens' GetViolationDetailsResponse Core.Int
gvdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gvdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
