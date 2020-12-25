{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyInstancePlacement (..),
    mkModifyInstancePlacement,

    -- ** Request lenses
    mipInstanceId,
    mipAffinity,
    mipGroupName,
    mipHostId,
    mipHostResourceGroupArn,
    mipPartitionNumber,
    mipTenancy,

    -- * Destructuring the response
    ModifyInstancePlacementResponse (..),
    mkModifyInstancePlacementResponse,

    -- ** Response lenses
    miprrsReturn,
    miprrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyInstancePlacement' smart constructor.
data ModifyInstancePlacement = ModifyInstancePlacement'
  { -- | The ID of the instance that you are modifying.
    instanceId :: Types.InstanceId,
    -- | The affinity setting for the instance.
    affinity :: Core.Maybe Types.Affinity,
    -- | The name of the placement group in which to place the instance. For spread placement groups, the instance must have a tenancy of @default@ . For cluster and partition placement groups, the instance must have a tenancy of @default@ or @dedicated@ .
    --
    -- To remove an instance from a placement group, specify an empty string ("").
    groupName :: Core.Maybe Types.PlacementGroupName,
    -- | The ID of the Dedicated Host with which to associate the instance.
    hostId :: Core.Maybe Types.DedicatedHostId,
    -- | The ARN of the host resource group in which to place the instance.
    hostResourceGroupArn :: Core.Maybe Types.String,
    -- | Reserved for future use.
    partitionNumber :: Core.Maybe Core.Int,
    -- | The tenancy for the instance.
    tenancy :: Core.Maybe Types.HostTenancy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstancePlacement' value with any optional fields omitted.
mkModifyInstancePlacement ::
  -- | 'instanceId'
  Types.InstanceId ->
  ModifyInstancePlacement
mkModifyInstancePlacement instanceId =
  ModifyInstancePlacement'
    { instanceId,
      affinity = Core.Nothing,
      groupName = Core.Nothing,
      hostId = Core.Nothing,
      hostResourceGroupArn = Core.Nothing,
      partitionNumber = Core.Nothing,
      tenancy = Core.Nothing
    }

-- | The ID of the instance that you are modifying.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipInstanceId :: Lens.Lens' ModifyInstancePlacement Types.InstanceId
mipInstanceId = Lens.field @"instanceId"
{-# DEPRECATED mipInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The affinity setting for the instance.
--
-- /Note:/ Consider using 'affinity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipAffinity :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Types.Affinity)
mipAffinity = Lens.field @"affinity"
{-# DEPRECATED mipAffinity "Use generic-lens or generic-optics with 'affinity' instead." #-}

-- | The name of the placement group in which to place the instance. For spread placement groups, the instance must have a tenancy of @default@ . For cluster and partition placement groups, the instance must have a tenancy of @default@ or @dedicated@ .
--
-- To remove an instance from a placement group, specify an empty string ("").
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipGroupName :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Types.PlacementGroupName)
mipGroupName = Lens.field @"groupName"
{-# DEPRECATED mipGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The ID of the Dedicated Host with which to associate the instance.
--
-- /Note:/ Consider using 'hostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipHostId :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Types.DedicatedHostId)
mipHostId = Lens.field @"hostId"
{-# DEPRECATED mipHostId "Use generic-lens or generic-optics with 'hostId' instead." #-}

-- | The ARN of the host resource group in which to place the instance.
--
-- /Note:/ Consider using 'hostResourceGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipHostResourceGroupArn :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Types.String)
mipHostResourceGroupArn = Lens.field @"hostResourceGroupArn"
{-# DEPRECATED mipHostResourceGroupArn "Use generic-lens or generic-optics with 'hostResourceGroupArn' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'partitionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipPartitionNumber :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Core.Int)
mipPartitionNumber = Lens.field @"partitionNumber"
{-# DEPRECATED mipPartitionNumber "Use generic-lens or generic-optics with 'partitionNumber' instead." #-}

-- | The tenancy for the instance.
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mipTenancy :: Lens.Lens' ModifyInstancePlacement (Core.Maybe Types.HostTenancy)
mipTenancy = Lens.field @"tenancy"
{-# DEPRECATED mipTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

instance Core.AWSRequest ModifyInstancePlacement where
  type Rs ModifyInstancePlacement = ModifyInstancePlacementResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyInstancePlacement")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InstanceId" instanceId)
                Core.<> (Core.toQueryValue "Affinity" Core.<$> affinity)
                Core.<> (Core.toQueryValue "GroupName" Core.<$> groupName)
                Core.<> (Core.toQueryValue "HostId" Core.<$> hostId)
                Core.<> ( Core.toQueryValue "HostResourceGroupArn"
                            Core.<$> hostResourceGroupArn
                        )
                Core.<> (Core.toQueryValue "PartitionNumber" Core.<$> partitionNumber)
                Core.<> (Core.toQueryValue "Tenancy" Core.<$> tenancy)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyInstancePlacementResponse'
            Core.<$> (x Core..@? "return") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyInstancePlacementResponse' smart constructor.
data ModifyInstancePlacementResponse = ModifyInstancePlacementResponse'
  { -- | Is @true@ if the request succeeds, and an error otherwise.
    return :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstancePlacementResponse' value with any optional fields omitted.
mkModifyInstancePlacementResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyInstancePlacementResponse
mkModifyInstancePlacementResponse responseStatus =
  ModifyInstancePlacementResponse'
    { return = Core.Nothing,
      responseStatus
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miprrsReturn :: Lens.Lens' ModifyInstancePlacementResponse (Core.Maybe Core.Bool)
miprrsReturn = Lens.field @"return"
{-# DEPRECATED miprrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miprrsResponseStatus :: Lens.Lens' ModifyInstancePlacementResponse Core.Int
miprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED miprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
