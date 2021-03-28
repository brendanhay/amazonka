{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AssociateDRTRole (..)
    , mkAssociateDRTRole
    -- ** Request lenses
    , adrtrRoleArn

    -- * Destructuring the response
    , AssociateDRTRoleResponse (..)
    , mkAssociateDRTRoleResponse
    -- ** Response lenses
    , adrtrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkAssociateDRTRole' smart constructor.
newtype AssociateDRTRole = AssociateDRTRole'
  { roleArn :: Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of the role the DRT will use to access your AWS account.
--
-- Prior to making the @AssociateDRTRole@ request, you must attach the <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy> managed policy to this role. For more information see < https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDRTRole' value with any optional fields omitted.
mkAssociateDRTRole
    :: Types.RoleArn -- ^ 'roleArn'
    -> AssociateDRTRole
mkAssociateDRTRole roleArn = AssociateDRTRole'{roleArn}

-- | The Amazon Resource Name (ARN) of the role the DRT will use to access your AWS account.
--
-- Prior to making the @AssociateDRTRole@ request, you must attach the <https://console.aws.amazon.com/iam/home?#/policies/arn:aws:iam::aws:policy/service-role/AWSShieldDRTAccessPolicy AWSShieldDRTAccessPolicy> managed policy to this role. For more information see < https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage-attach-detach.html Attaching and Detaching IAM Policies> .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrtrRoleArn :: Lens.Lens' AssociateDRTRole Types.RoleArn
adrtrRoleArn = Lens.field @"roleArn"
{-# INLINEABLE adrtrRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.ToQuery AssociateDRTRole where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateDRTRole where
        toHeaders AssociateDRTRole{..}
          = Core.pure ("X-Amz-Target", "AWSShield_20160616.AssociateDRTRole")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateDRTRole where
        toJSON AssociateDRTRole{..}
          = Core.object
              (Core.catMaybes [Core.Just ("RoleArn" Core..= roleArn)])

instance Core.AWSRequest AssociateDRTRole where
        type Rs AssociateDRTRole = AssociateDRTRoleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociateDRTRoleResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateDRTRoleResponse' smart constructor.
newtype AssociateDRTRoleResponse = AssociateDRTRoleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDRTRoleResponse' value with any optional fields omitted.
mkAssociateDRTRoleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateDRTRoleResponse
mkAssociateDRTRoleResponse responseStatus
  = AssociateDRTRoleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrtrrrsResponseStatus :: Lens.Lens' AssociateDRTRoleResponse Core.Int
adrtrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE adrtrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
