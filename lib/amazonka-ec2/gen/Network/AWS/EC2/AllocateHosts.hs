{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AllocateHosts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates a Dedicated Host to your account. At a minimum, specify the supported instance type or instance family, the Availability Zone in which to allocate the host, and the number of hosts to allocate.
module Network.AWS.EC2.AllocateHosts
    (
    -- * Creating a request
      AllocateHosts (..)
    , mkAllocateHosts
    -- ** Request lenses
    , ahAvailabilityZone
    , ahQuantity
    , ahAutoPlacement
    , ahClientToken
    , ahHostRecovery
    , ahInstanceFamily
    , ahInstanceType
    , ahTagSpecifications

    -- * Destructuring the response
    , AllocateHostsResponse (..)
    , mkAllocateHostsResponse
    -- ** Response lenses
    , ahrrsHostIds
    , ahrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAllocateHosts' smart constructor.
data AllocateHosts = AllocateHosts'
  { availabilityZone :: Core.Text
    -- ^ The Availability Zone in which to allocate the Dedicated Host.
  , quantity :: Core.Int
    -- ^ The number of Dedicated Hosts to allocate to your account with these parameters.
  , autoPlacement :: Core.Maybe Types.AutoPlacement
    -- ^ Indicates whether the host accepts any untargeted instance launches that match its instance type configuration, or if it only accepts Host tenancy instance launches that specify its unique host ID. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding Instance Placement and Host Affinity> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- Default: @on@ 
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , hostRecovery :: Core.Maybe Types.HostRecovery
    -- ^ Indicates whether to enable or disable host recovery for the Dedicated Host. Host recovery is disabled by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @off@ 
  , instanceFamily :: Core.Maybe Core.Text
    -- ^ Specifies the instance family to be supported by the Dedicated Hosts. If you specify an instance family, the Dedicated Hosts support multiple instance types within that instance family.
--
-- If you want the Dedicated Hosts to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
  , instanceType :: Core.Maybe Core.Text
    -- ^ Specifies the instance type to be supported by the Dedicated Hosts. If you specify an instance type, the Dedicated Hosts support instances of the specified instance type only.
--
-- If you want the Dedicated Hosts to support multiple instance types in a specific instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the Dedicated Host during creation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AllocateHosts' value with any optional fields omitted.
mkAllocateHosts
    :: Core.Text -- ^ 'availabilityZone'
    -> Core.Int -- ^ 'quantity'
    -> AllocateHosts
mkAllocateHosts availabilityZone quantity
  = AllocateHosts'{availabilityZone, quantity,
                   autoPlacement = Core.Nothing, clientToken = Core.Nothing,
                   hostRecovery = Core.Nothing, instanceFamily = Core.Nothing,
                   instanceType = Core.Nothing, tagSpecifications = Core.Nothing}

-- | The Availability Zone in which to allocate the Dedicated Host.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahAvailabilityZone :: Lens.Lens' AllocateHosts Core.Text
ahAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE ahAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The number of Dedicated Hosts to allocate to your account with these parameters.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahQuantity :: Lens.Lens' AllocateHosts Core.Int
ahQuantity = Lens.field @"quantity"
{-# INLINEABLE ahQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | Indicates whether the host accepts any untargeted instance launches that match its instance type configuration, or if it only accepts Host tenancy instance launches that specify its unique host ID. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding Instance Placement and Host Affinity> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- Default: @on@ 
--
-- /Note:/ Consider using 'autoPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahAutoPlacement :: Lens.Lens' AllocateHosts (Core.Maybe Types.AutoPlacement)
ahAutoPlacement = Lens.field @"autoPlacement"
{-# INLINEABLE ahAutoPlacement #-}
{-# DEPRECATED autoPlacement "Use generic-lens or generic-optics with 'autoPlacement' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahClientToken :: Lens.Lens' AllocateHosts (Core.Maybe Core.Text)
ahClientToken = Lens.field @"clientToken"
{-# INLINEABLE ahClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Indicates whether to enable or disable host recovery for the Dedicated Host. Host recovery is disabled by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @off@ 
--
-- /Note:/ Consider using 'hostRecovery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahHostRecovery :: Lens.Lens' AllocateHosts (Core.Maybe Types.HostRecovery)
ahHostRecovery = Lens.field @"hostRecovery"
{-# INLINEABLE ahHostRecovery #-}
{-# DEPRECATED hostRecovery "Use generic-lens or generic-optics with 'hostRecovery' instead"  #-}

-- | Specifies the instance family to be supported by the Dedicated Hosts. If you specify an instance family, the Dedicated Hosts support multiple instance types within that instance family.
--
-- If you want the Dedicated Hosts to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahInstanceFamily :: Lens.Lens' AllocateHosts (Core.Maybe Core.Text)
ahInstanceFamily = Lens.field @"instanceFamily"
{-# INLINEABLE ahInstanceFamily #-}
{-# DEPRECATED instanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead"  #-}

-- | Specifies the instance type to be supported by the Dedicated Hosts. If you specify an instance type, the Dedicated Hosts support instances of the specified instance type only.
--
-- If you want the Dedicated Hosts to support multiple instance types in a specific instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahInstanceType :: Lens.Lens' AllocateHosts (Core.Maybe Core.Text)
ahInstanceType = Lens.field @"instanceType"
{-# INLINEABLE ahInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The tags to apply to the Dedicated Host during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahTagSpecifications :: Lens.Lens' AllocateHosts (Core.Maybe [Types.TagSpecification])
ahTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ahTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery AllocateHosts where
        toQuery AllocateHosts{..}
          = Core.toQueryPair "Action" ("AllocateHosts" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "AvailabilityZone" availabilityZone
              Core.<> Core.toQueryPair "Quantity" quantity
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoPlacement")
                autoPlacement
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HostRecovery")
                hostRecovery
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceFamily")
                instanceFamily
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceType")
                instanceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders AllocateHosts where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AllocateHosts where
        type Rs AllocateHosts = AllocateHostsResponse
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
                 AllocateHostsResponse' Core.<$>
                   (x Core..@? "hostIdSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of AllocateHosts.
--
-- /See:/ 'mkAllocateHostsResponse' smart constructor.
data AllocateHostsResponse = AllocateHostsResponse'
  { hostIds :: Core.Maybe [Core.Text]
    -- ^ The ID of the allocated Dedicated Host. This is used to launch an instance onto a specific host.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AllocateHostsResponse' value with any optional fields omitted.
mkAllocateHostsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AllocateHostsResponse
mkAllocateHostsResponse responseStatus
  = AllocateHostsResponse'{hostIds = Core.Nothing, responseStatus}

-- | The ID of the allocated Dedicated Host. This is used to launch an instance onto a specific host.
--
-- /Note:/ Consider using 'hostIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahrrsHostIds :: Lens.Lens' AllocateHostsResponse (Core.Maybe [Core.Text])
ahrrsHostIds = Lens.field @"hostIds"
{-# INLINEABLE ahrrsHostIds #-}
{-# DEPRECATED hostIds "Use generic-lens or generic-optics with 'hostIds' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahrrsResponseStatus :: Lens.Lens' AllocateHostsResponse Core.Int
ahrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ahrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
