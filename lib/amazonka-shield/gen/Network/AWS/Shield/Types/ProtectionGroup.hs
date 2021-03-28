{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.ProtectionGroup
  ( ProtectionGroup (..)
  -- * Smart constructor
  , mkProtectionGroup
  -- * Lenses
  , pgProtectionGroupId
  , pgAggregation
  , pgPattern
  , pgMembers
  , pgResourceType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.ProtectedResourceType as Types
import qualified Network.AWS.Shield.Types.ProtectionGroupAggregation as Types
import qualified Network.AWS.Shield.Types.ProtectionGroupId as Types
import qualified Network.AWS.Shield.Types.ProtectionGroupPattern as Types
import qualified Network.AWS.Shield.Types.ResourceArn as Types

-- | A grouping of protected resources that you and AWS Shield Advanced can monitor as a collective. This resource grouping improves the accuracy of detection and reduces false positives. 
--
-- /See:/ 'mkProtectionGroup' smart constructor.
data ProtectionGroup = ProtectionGroup'
  { protectionGroupId :: Types.ProtectionGroupId
    -- ^ The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it. 
  , aggregation :: Types.ProtectionGroupAggregation
    -- ^ Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.
--
--
--     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.
--
--
--     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.
--
--
--     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
--
--
  , pattern' :: Types.ProtectionGroupPattern
    -- ^ The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
  , members :: [Types.ResourceArn]
    -- ^ The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting. 
  , resourceType :: Core.Maybe Types.ProtectedResourceType
    -- ^ The resource type to include in the protection group. All protected resources of this type are included in the protection group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProtectionGroup' value with any optional fields omitted.
mkProtectionGroup
    :: Types.ProtectionGroupId -- ^ 'protectionGroupId'
    -> Types.ProtectionGroupAggregation -- ^ 'aggregation'
    -> Types.ProtectionGroupPattern -- ^ 'pattern\''
    -> ProtectionGroup
mkProtectionGroup protectionGroupId aggregation pattern'
  = ProtectionGroup'{protectionGroupId, aggregation, pattern',
                     members = Core.mempty, resourceType = Core.Nothing}

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it. 
--
-- /Note:/ Consider using 'protectionGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgProtectionGroupId :: Lens.Lens' ProtectionGroup Types.ProtectionGroupId
pgProtectionGroupId = Lens.field @"protectionGroupId"
{-# INLINEABLE pgProtectionGroupId #-}
{-# DEPRECATED protectionGroupId "Use generic-lens or generic-optics with 'protectionGroupId' instead"  #-}

-- | Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.
--
--
--     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.
--
--
--     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.
--
--
--     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
--
--
--
-- /Note:/ Consider using 'aggregation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgAggregation :: Lens.Lens' ProtectionGroup Types.ProtectionGroupAggregation
pgAggregation = Lens.field @"aggregation"
{-# INLINEABLE pgAggregation #-}
{-# DEPRECATED aggregation "Use generic-lens or generic-optics with 'aggregation' instead"  #-}

-- | The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgPattern :: Lens.Lens' ProtectionGroup Types.ProtectionGroupPattern
pgPattern = Lens.field @"pattern'"
{-# INLINEABLE pgPattern #-}
{-# DEPRECATED pattern' "Use generic-lens or generic-optics with 'pattern'' instead"  #-}

-- | The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting. 
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgMembers :: Lens.Lens' ProtectionGroup [Types.ResourceArn]
pgMembers = Lens.field @"members"
{-# INLINEABLE pgMembers #-}
{-# DEPRECATED members "Use generic-lens or generic-optics with 'members' instead"  #-}

-- | The resource type to include in the protection group. All protected resources of this type are included in the protection group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting. 
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgResourceType :: Lens.Lens' ProtectionGroup (Core.Maybe Types.ProtectedResourceType)
pgResourceType = Lens.field @"resourceType"
{-# INLINEABLE pgResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.FromJSON ProtectionGroup where
        parseJSON
          = Core.withObject "ProtectionGroup" Core.$
              \ x ->
                ProtectionGroup' Core.<$>
                  (x Core..: "ProtectionGroupId") Core.<*> x Core..: "Aggregation"
                    Core.<*> x Core..: "Pattern"
                    Core.<*> x Core..:? "Members" Core..!= Core.mempty
                    Core.<*> x Core..:? "ResourceType"
