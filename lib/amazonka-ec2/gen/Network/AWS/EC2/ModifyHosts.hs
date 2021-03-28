{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyHosts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the auto-placement setting of a Dedicated Host. When auto-placement is enabled, any instances that you launch with a tenancy of @host@ but without a specific host ID are placed onto any available Dedicated Host in your account that has auto-placement enabled. When auto-placement is disabled, you need to provide a host ID to have the instance launch onto a specific host. If no host ID is provided, the instance is launched onto a suitable host with auto-placement enabled.
--
-- You can also use this API action to modify a Dedicated Host to support either multiple instance types in an instance family, or to support a specific instance type only.
module Network.AWS.EC2.ModifyHosts
    (
    -- * Creating a request
      ModifyHosts (..)
    , mkModifyHosts
    -- ** Request lenses
    , mhHostIds
    , mhAutoPlacement
    , mhHostRecovery
    , mhInstanceFamily
    , mhInstanceType

    -- * Destructuring the response
    , ModifyHostsResponse (..)
    , mkModifyHostsResponse
    -- ** Response lenses
    , mhrrsSuccessful
    , mhrrsUnsuccessful
    , mhrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyHosts' smart constructor.
data ModifyHosts = ModifyHosts'
  { hostIds :: [Types.DedicatedHostId]
    -- ^ The IDs of the Dedicated Hosts to modify.
  , autoPlacement :: Core.Maybe Types.AutoPlacement
    -- ^ Specify whether to enable or disable auto-placement.
  , hostRecovery :: Core.Maybe Types.HostRecovery
    -- ^ Indicates whether to enable or disable host recovery for the Dedicated Host. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
  , instanceFamily :: Core.Maybe Core.Text
    -- ^ Specifies the instance family to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support multiple instance types within its current instance family.
--
-- If you want to modify a Dedicated Host to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
  , instanceType :: Core.Maybe Core.Text
    -- ^ Specifies the instance type to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support only a specific instance type.
--
-- If you want to modify a Dedicated Host to support multiple instance types in its current instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyHosts' value with any optional fields omitted.
mkModifyHosts
    :: ModifyHosts
mkModifyHosts
  = ModifyHosts'{hostIds = Core.mempty, autoPlacement = Core.Nothing,
                 hostRecovery = Core.Nothing, instanceFamily = Core.Nothing,
                 instanceType = Core.Nothing}

-- | The IDs of the Dedicated Hosts to modify.
--
-- /Note:/ Consider using 'hostIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhHostIds :: Lens.Lens' ModifyHosts [Types.DedicatedHostId]
mhHostIds = Lens.field @"hostIds"
{-# INLINEABLE mhHostIds #-}
{-# DEPRECATED hostIds "Use generic-lens or generic-optics with 'hostIds' instead"  #-}

-- | Specify whether to enable or disable auto-placement.
--
-- /Note:/ Consider using 'autoPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhAutoPlacement :: Lens.Lens' ModifyHosts (Core.Maybe Types.AutoPlacement)
mhAutoPlacement = Lens.field @"autoPlacement"
{-# INLINEABLE mhAutoPlacement #-}
{-# DEPRECATED autoPlacement "Use generic-lens or generic-optics with 'autoPlacement' instead"  #-}

-- | Indicates whether to enable or disable host recovery for the Dedicated Host. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'hostRecovery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhHostRecovery :: Lens.Lens' ModifyHosts (Core.Maybe Types.HostRecovery)
mhHostRecovery = Lens.field @"hostRecovery"
{-# INLINEABLE mhHostRecovery #-}
{-# DEPRECATED hostRecovery "Use generic-lens or generic-optics with 'hostRecovery' instead"  #-}

-- | Specifies the instance family to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support multiple instance types within its current instance family.
--
-- If you want to modify a Dedicated Host to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhInstanceFamily :: Lens.Lens' ModifyHosts (Core.Maybe Core.Text)
mhInstanceFamily = Lens.field @"instanceFamily"
{-# INLINEABLE mhInstanceFamily #-}
{-# DEPRECATED instanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead"  #-}

-- | Specifies the instance type to be supported by the Dedicated Host. Specify this parameter to modify a Dedicated Host to support only a specific instance type.
--
-- If you want to modify a Dedicated Host to support multiple instance types in its current instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhInstanceType :: Lens.Lens' ModifyHosts (Core.Maybe Core.Text)
mhInstanceType = Lens.field @"instanceType"
{-# INLINEABLE mhInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

instance Core.ToQuery ModifyHosts where
        toQuery ModifyHosts{..}
          = Core.toQueryPair "Action" ("ModifyHosts" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "HostId" hostIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoPlacement")
                autoPlacement
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HostRecovery")
                hostRecovery
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceFamily")
                instanceFamily
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceType")
                instanceType

instance Core.ToHeaders ModifyHosts where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyHosts where
        type Rs ModifyHosts = ModifyHostsResponse
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
                 ModifyHostsResponse' Core.<$>
                   (x Core..@? "successful" Core..<@> Core.parseXMLList "item")
                     Core.<*>
                     x Core..@? "unsuccessful" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyHostsResponse' smart constructor.
data ModifyHostsResponse = ModifyHostsResponse'
  { successful :: Core.Maybe [Core.Text]
    -- ^ The IDs of the Dedicated Hosts that were successfully modified.
  , unsuccessful :: Core.Maybe [Types.UnsuccessfulItem]
    -- ^ The IDs of the Dedicated Hosts that could not be modified. Check whether the setting you requested can be used.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyHostsResponse' value with any optional fields omitted.
mkModifyHostsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyHostsResponse
mkModifyHostsResponse responseStatus
  = ModifyHostsResponse'{successful = Core.Nothing,
                         unsuccessful = Core.Nothing, responseStatus}

-- | The IDs of the Dedicated Hosts that were successfully modified.
--
-- /Note:/ Consider using 'successful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhrrsSuccessful :: Lens.Lens' ModifyHostsResponse (Core.Maybe [Core.Text])
mhrrsSuccessful = Lens.field @"successful"
{-# INLINEABLE mhrrsSuccessful #-}
{-# DEPRECATED successful "Use generic-lens or generic-optics with 'successful' instead"  #-}

-- | The IDs of the Dedicated Hosts that could not be modified. Check whether the setting you requested can be used.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhrrsUnsuccessful :: Lens.Lens' ModifyHostsResponse (Core.Maybe [Types.UnsuccessfulItem])
mhrrsUnsuccessful = Lens.field @"unsuccessful"
{-# INLINEABLE mhrrsUnsuccessful #-}
{-# DEPRECATED unsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhrrsResponseStatus :: Lens.Lens' ModifyHostsResponse Core.Int
mhrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mhrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
