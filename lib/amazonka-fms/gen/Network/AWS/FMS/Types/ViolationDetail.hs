{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ViolationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.ViolationDetail
  ( ViolationDetail (..)
  -- * Smart constructor
  , mkViolationDetail
  -- * Lenses
  , vdPolicyId
  , vdMemberAccount
  , vdResourceId
  , vdResourceType
  , vdResourceViolations
  , vdResourceDescription
  , vdResourceTags
  ) where

import qualified Network.AWS.FMS.Types.AWSAccountId as Types
import qualified Network.AWS.FMS.Types.LengthBoundedString as Types
import qualified Network.AWS.FMS.Types.PolicyId as Types
import qualified Network.AWS.FMS.Types.ResourceId as Types
import qualified Network.AWS.FMS.Types.ResourceType as Types
import qualified Network.AWS.FMS.Types.ResourceViolation as Types
import qualified Network.AWS.FMS.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Violations for a resource based on the specified AWS Firewall Manager policy and AWS account.
--
-- /See:/ 'mkViolationDetail' smart constructor.
data ViolationDetail = ViolationDetail'
  { policyId :: Types.PolicyId
    -- ^ The ID of the AWS Firewall Manager policy that the violation details were requested for.
  , memberAccount :: Types.AWSAccountId
    -- ^ The AWS account that the violation details were requested for.
  , resourceId :: Types.ResourceId
    -- ^ The resource ID that the violation details were requested for.
  , resourceType :: Types.ResourceType
    -- ^ The resource type that the violation details were requested for.
  , resourceViolations :: [Types.ResourceViolation]
    -- ^ List of violations for the requested resource.
  , resourceDescription :: Core.Maybe Types.LengthBoundedString
    -- ^ Brief description for the requested resource.
  , resourceTags :: Core.Maybe [Types.Tag]
    -- ^ The @ResourceTag@ objects associated with the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ViolationDetail' value with any optional fields omitted.
mkViolationDetail
    :: Types.PolicyId -- ^ 'policyId'
    -> Types.AWSAccountId -- ^ 'memberAccount'
    -> Types.ResourceId -- ^ 'resourceId'
    -> Types.ResourceType -- ^ 'resourceType'
    -> ViolationDetail
mkViolationDetail policyId memberAccount resourceId resourceType
  = ViolationDetail'{policyId, memberAccount, resourceId,
                     resourceType, resourceViolations = Core.mempty,
                     resourceDescription = Core.Nothing, resourceTags = Core.Nothing}

-- | The ID of the AWS Firewall Manager policy that the violation details were requested for.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdPolicyId :: Lens.Lens' ViolationDetail Types.PolicyId
vdPolicyId = Lens.field @"policyId"
{-# INLINEABLE vdPolicyId #-}
{-# DEPRECATED policyId "Use generic-lens or generic-optics with 'policyId' instead"  #-}

-- | The AWS account that the violation details were requested for.
--
-- /Note:/ Consider using 'memberAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdMemberAccount :: Lens.Lens' ViolationDetail Types.AWSAccountId
vdMemberAccount = Lens.field @"memberAccount"
{-# INLINEABLE vdMemberAccount #-}
{-# DEPRECATED memberAccount "Use generic-lens or generic-optics with 'memberAccount' instead"  #-}

-- | The resource ID that the violation details were requested for.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdResourceId :: Lens.Lens' ViolationDetail Types.ResourceId
vdResourceId = Lens.field @"resourceId"
{-# INLINEABLE vdResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The resource type that the violation details were requested for.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdResourceType :: Lens.Lens' ViolationDetail Types.ResourceType
vdResourceType = Lens.field @"resourceType"
{-# INLINEABLE vdResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | List of violations for the requested resource.
--
-- /Note:/ Consider using 'resourceViolations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdResourceViolations :: Lens.Lens' ViolationDetail [Types.ResourceViolation]
vdResourceViolations = Lens.field @"resourceViolations"
{-# INLINEABLE vdResourceViolations #-}
{-# DEPRECATED resourceViolations "Use generic-lens or generic-optics with 'resourceViolations' instead"  #-}

-- | Brief description for the requested resource.
--
-- /Note:/ Consider using 'resourceDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdResourceDescription :: Lens.Lens' ViolationDetail (Core.Maybe Types.LengthBoundedString)
vdResourceDescription = Lens.field @"resourceDescription"
{-# INLINEABLE vdResourceDescription #-}
{-# DEPRECATED resourceDescription "Use generic-lens or generic-optics with 'resourceDescription' instead"  #-}

-- | The @ResourceTag@ objects associated with the resource.
--
-- /Note:/ Consider using 'resourceTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdResourceTags :: Lens.Lens' ViolationDetail (Core.Maybe [Types.Tag])
vdResourceTags = Lens.field @"resourceTags"
{-# INLINEABLE vdResourceTags #-}
{-# DEPRECATED resourceTags "Use generic-lens or generic-optics with 'resourceTags' instead"  #-}

instance Core.FromJSON ViolationDetail where
        parseJSON
          = Core.withObject "ViolationDetail" Core.$
              \ x ->
                ViolationDetail' Core.<$>
                  (x Core..: "PolicyId") Core.<*> x Core..: "MemberAccount" Core.<*>
                    x Core..: "ResourceId"
                    Core.<*> x Core..: "ResourceType"
                    Core.<*> x Core..:? "ResourceViolations" Core..!= Core.mempty
                    Core.<*> x Core..:? "ResourceDescription"
                    Core.<*> x Core..:? "ResourceTags"
