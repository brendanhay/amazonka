{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyInstancePlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the placement attributes for a specified instance. You can do the following:
--
--
--     * Modify the affinity between an instance and a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-overview.html Dedicated Host> . When affinity is set to @host@ and the instance is not associated with a specific Dedicated Host, the next time the instance is launched, it is automatically associated with the host on which it lands. If the instance is restarted or rebooted, this relationship persists.
--
--
--     * Change the Dedicated Host with which an instance is associated.
--
--
--     * Change the instance tenancy of an instance from @host@ to @dedicated@ , or from @dedicated@ to @host@ .
--
--
--     * Move an instance to or from a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html placement group> .
--
--
-- At least one attribute for affinity, host ID, tenancy, or placement group name must be specified in the request. Affinity and tenancy can be modified in the same request.
-- To modify the host ID, tenancy, placement group, or partition for an instance, the instance must be in the @stopped@ state.
module Network.AWS.EC2.ModifyInstancePlacement
    (
    -- * Creating a request
      ModifyInstancePlacement (..)
    , mkModifyInstancePlacement
    -- ** Request lenses
    , mipInstanceId
    , mipAffinity
    , mipGroupName
    , mipHostId
    , mipHostResourceGroupArn
    , mipPartitionNumber
    , mipTenancy

    -- * Destructuring the response
    , ModifyInstancePlacementResponse (..)
    , mkModifyInstancePlacementResponse
    -- ** Response lenses
    , miprrsReturn
    , miprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyInstancePlacement' smart constructor.
data ModifyInstancePlacement = ModifyInstancePlacement'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance that you are modifying.
  , affinity :: Core.Maybe Types.Affinity
    -- ^ The affinity setting for the instance.
  , groupName :: Core.Maybe Types.PlacementGroupName
    -- ^ The name of the placement group in which to place the instance. For spread placement groups, the instance must have a tenancy of @default@ . For cluster and partition placement groups, the instance must have a tenancy of @default@ or @dedicated@ .
--
-- To remove an instance from a placement group, specify an empty string ("").
  , hostId :: Core.Maybe Types.DedicatedHostId
    -- ^ The ID of the Dedicated Host with which to associate the instance.
  , hostResourceGroupArn :: Core.Maybe Core.Text
    -- ^ The ARN of the host resource group in which to place the instance.
  , partitionNumber :: Core.Maybe Core.Int
    -- ^ Reserved for future use.
  , tenancy :: Core.Maybe Types.HostTenancy
    -- ^ The tenancy for the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstancePlacement' value with any optional fields omitted.
mkModifyInstancePlacement
    :: Types.InstanceId -- ^ 'instanceId'
    -> ModifyInstancePlacement
mkModifyInstancePlacement instanceId
  = ModifyInstancePlacement'{instanceId, affinity = Core.Nothing,
                             groupName = Core.Nothing, hostId = Core.Nothing,
                             hostResourceGroupArn = Core.Nothing,
                             partitionNumber = Core.Nothing, tenancy = Core.Nothing}

-- | The ID of the instance that you are modifying.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipInstanceId :: Lens.Lens' ModifyInstancePlacement Types.InstanceId
mipInstanceId = Lens.field @"instanceId"
{-# INLINEABLE mipInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The affinity setting for the instance.
--
-- /Note:/ Consider using 'affinity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipAffinity :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Types.Affinity)
mipAffinity = Lens.field @"affinity"
{-# INLINEABLE mipAffinity #-}
{-# DEPRECATED affinity "Use generic-lens or generic-optics with 'affinity' instead"  #-}

-- | The name of the placement group in which to place the instance. For spread placement groups, the instance must have a tenancy of @default@ . For cluster and partition placement groups, the instance must have a tenancy of @default@ or @dedicated@ .
--
-- To remove an instance from a placement group, specify an empty string ("").
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipGroupName :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Types.PlacementGroupName)
mipGroupName = Lens.field @"groupName"
{-# INLINEABLE mipGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The ID of the Dedicated Host with which to associate the instance.
--
-- /Note:/ Consider using 'hostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipHostId :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Types.DedicatedHostId)
mipHostId = Lens.field @"hostId"
{-# INLINEABLE mipHostId #-}
{-# DEPRECATED hostId "Use generic-lens or generic-optics with 'hostId' instead"  #-}

-- | The ARN of the host resource group in which to place the instance.
--
-- /Note:/ Consider using 'hostResourceGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipHostResourceGroupArn :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Core.Text)
mipHostResourceGroupArn = Lens.field @"hostResourceGroupArn"
{-# INLINEABLE mipHostResourceGroupArn #-}
{-# DEPRECATED hostResourceGroupArn "Use generic-lens or generic-optics with 'hostResourceGroupArn' instead"  #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'partitionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipPartitionNumber :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Core.Int)
mipPartitionNumber = Lens.field @"partitionNumber"
{-# INLINEABLE mipPartitionNumber #-}
{-# DEPRECATED partitionNumber "Use generic-lens or generic-optics with 'partitionNumber' instead"  #-}

-- | The tenancy for the instance.
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipTenancy :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Types.HostTenancy)
mipTenancy = Lens.field @"tenancy"
{-# INLINEABLE mipTenancy #-}
{-# DEPRECATED tenancy "Use generic-lens or generic-optics with 'tenancy' instead"  #-}

instance Core.ToQuery ModifyInstancePlacement where
        toQuery ModifyInstancePlacement{..}
          = Core.toQueryPair "Action"
              ("ModifyInstancePlacement" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Affinity") affinity
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GroupName") groupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "HostId") hostId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HostResourceGroupArn")
                hostResourceGroupArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PartitionNumber")
                partitionNumber
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Tenancy") tenancy

instance Core.ToHeaders ModifyInstancePlacement where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyInstancePlacement where
        type Rs ModifyInstancePlacement = ModifyInstancePlacementResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ModifyInstancePlacementResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyInstancePlacementResponse' smart constructor.
data ModifyInstancePlacementResponse = ModifyInstancePlacementResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Is @true@ if the request succeeds, and an error otherwise.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstancePlacementResponse' value with any optional fields omitted.
mkModifyInstancePlacementResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyInstancePlacementResponse
mkModifyInstancePlacementResponse responseStatus
  = ModifyInstancePlacementResponse'{return = Core.Nothing,
                                     responseStatus}

-- | Is @true@ if the request succeeds, and an error otherwise.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miprrsReturn :: Lens.Lens' ModifyInstancePlacementResponse (Core.Maybe Core.Bool)
miprrsReturn = Lens.field @"return"
{-# INLINEABLE miprrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miprrsResponseStatus :: Lens.Lens' ModifyInstancePlacementResponse Core.Int
miprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE miprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
