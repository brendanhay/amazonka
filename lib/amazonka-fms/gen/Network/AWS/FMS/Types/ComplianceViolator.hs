{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ComplianceViolator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.ComplianceViolator
  ( ComplianceViolator (..)
  -- * Smart constructor
  , mkComplianceViolator
  -- * Lenses
  , cvResourceId
  , cvResourceType
  , cvViolationReason
  ) where

import qualified Network.AWS.FMS.Types.ResourceId as Types
import qualified Network.AWS.FMS.Types.ResourceType as Types
import qualified Network.AWS.FMS.Types.ViolationReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details of the resource that is not protected by the policy.
--
-- /See:/ 'mkComplianceViolator' smart constructor.
data ComplianceViolator = ComplianceViolator'
  { resourceId :: Core.Maybe Types.ResourceId
    -- ^ The resource ID.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The resource type. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For example: @AWS::ElasticLoadBalancingV2::LoadBalancer@ , @AWS::CloudFront::Distribution@ , or @AWS::NetworkFirewall::FirewallPolicy@ .
  , violationReason :: Core.Maybe Types.ViolationReason
    -- ^ The reason that the resource is not protected by the policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComplianceViolator' value with any optional fields omitted.
mkComplianceViolator
    :: ComplianceViolator
mkComplianceViolator
  = ComplianceViolator'{resourceId = Core.Nothing,
                        resourceType = Core.Nothing, violationReason = Core.Nothing}

-- | The resource ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvResourceId :: Lens.Lens' ComplianceViolator (Core.Maybe Types.ResourceId)
cvResourceId = Lens.field @"resourceId"
{-# INLINEABLE cvResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The resource type. This is in the format shown in the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> . For example: @AWS::ElasticLoadBalancingV2::LoadBalancer@ , @AWS::CloudFront::Distribution@ , or @AWS::NetworkFirewall::FirewallPolicy@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvResourceType :: Lens.Lens' ComplianceViolator (Core.Maybe Types.ResourceType)
cvResourceType = Lens.field @"resourceType"
{-# INLINEABLE cvResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The reason that the resource is not protected by the policy.
--
-- /Note:/ Consider using 'violationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvViolationReason :: Lens.Lens' ComplianceViolator (Core.Maybe Types.ViolationReason)
cvViolationReason = Lens.field @"violationReason"
{-# INLINEABLE cvViolationReason #-}
{-# DEPRECATED violationReason "Use generic-lens or generic-optics with 'violationReason' instead"  #-}

instance Core.FromJSON ComplianceViolator where
        parseJSON
          = Core.withObject "ComplianceViolator" Core.$
              \ x ->
                ComplianceViolator' Core.<$>
                  (x Core..:? "ResourceId") Core.<*> x Core..:? "ResourceType"
                    Core.<*> x Core..:? "ViolationReason"
