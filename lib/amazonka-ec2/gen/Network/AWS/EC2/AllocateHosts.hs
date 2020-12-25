{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AllocateHosts (..),
    mkAllocateHosts,

    -- ** Request lenses
    ahAvailabilityZone,
    ahQuantity,
    ahAutoPlacement,
    ahClientToken,
    ahHostRecovery,
    ahInstanceFamily,
    ahInstanceType,
    ahTagSpecifications,

    -- * Destructuring the response
    AllocateHostsResponse (..),
    mkAllocateHostsResponse,

    -- ** Response lenses
    ahrrsHostIds,
    ahrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAllocateHosts' smart constructor.
data AllocateHosts = AllocateHosts'
  { -- | The Availability Zone in which to allocate the Dedicated Host.
    availabilityZone :: Types.String,
    -- | The number of Dedicated Hosts to allocate to your account with these parameters.
    quantity :: Core.Int,
    -- | Indicates whether the host accepts any untargeted instance launches that match its instance type configuration, or if it only accepts Host tenancy instance launches that specify its unique host ID. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding Instance Placement and Host Affinity> in the /Amazon EC2 User Guide for Linux Instances/ .
    --
    -- Default: @on@
    autoPlacement :: Core.Maybe Types.AutoPlacement,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Core.Maybe Types.ClientToken,
    -- | Indicates whether to enable or disable host recovery for the Dedicated Host. Host recovery is disabled by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
    --
    -- Default: @off@
    hostRecovery :: Core.Maybe Types.HostRecovery,
    -- | Specifies the instance family to be supported by the Dedicated Hosts. If you specify an instance family, the Dedicated Hosts support multiple instance types within that instance family.
    --
    -- If you want the Dedicated Hosts to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
    instanceFamily :: Core.Maybe Types.InstanceFamily,
    -- | Specifies the instance type to be supported by the Dedicated Hosts. If you specify an instance type, the Dedicated Hosts support instances of the specified instance type only.
    --
    -- If you want the Dedicated Hosts to support multiple instance types in a specific instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
    instanceType :: Core.Maybe Types.String,
    -- | The tags to apply to the Dedicated Host during creation.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AllocateHosts' value with any optional fields omitted.
mkAllocateHosts ::
  -- | 'availabilityZone'
  Types.String ->
  -- | 'quantity'
  Core.Int ->
  AllocateHosts
mkAllocateHosts availabilityZone quantity =
  AllocateHosts'
    { availabilityZone,
      quantity,
      autoPlacement = Core.Nothing,
      clientToken = Core.Nothing,
      hostRecovery = Core.Nothing,
      instanceFamily = Core.Nothing,
      instanceType = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | The Availability Zone in which to allocate the Dedicated Host.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahAvailabilityZone :: Lens.Lens' AllocateHosts Types.String
ahAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED ahAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The number of Dedicated Hosts to allocate to your account with these parameters.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahQuantity :: Lens.Lens' AllocateHosts Core.Int
ahQuantity = Lens.field @"quantity"
{-# DEPRECATED ahQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | Indicates whether the host accepts any untargeted instance launches that match its instance type configuration, or if it only accepts Host tenancy instance launches that specify its unique host ID. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/how-dedicated-hosts-work.html#dedicated-hosts-understanding Understanding Instance Placement and Host Affinity> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- Default: @on@
--
-- /Note:/ Consider using 'autoPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahAutoPlacement :: Lens.Lens' AllocateHosts (Core.Maybe Types.AutoPlacement)
ahAutoPlacement = Lens.field @"autoPlacement"
{-# DEPRECATED ahAutoPlacement "Use generic-lens or generic-optics with 'autoPlacement' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahClientToken :: Lens.Lens' AllocateHosts (Core.Maybe Types.ClientToken)
ahClientToken = Lens.field @"clientToken"
{-# DEPRECATED ahClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Indicates whether to enable or disable host recovery for the Dedicated Host. Host recovery is disabled by default. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-recovery.html Host Recovery> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @off@
--
-- /Note:/ Consider using 'hostRecovery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahHostRecovery :: Lens.Lens' AllocateHosts (Core.Maybe Types.HostRecovery)
ahHostRecovery = Lens.field @"hostRecovery"
{-# DEPRECATED ahHostRecovery "Use generic-lens or generic-optics with 'hostRecovery' instead." #-}

-- | Specifies the instance family to be supported by the Dedicated Hosts. If you specify an instance family, the Dedicated Hosts support multiple instance types within that instance family.
--
-- If you want the Dedicated Hosts to support a specific instance type only, omit this parameter and specify __InstanceType__ instead. You cannot specify __InstanceFamily__ and __InstanceType__ in the same request.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahInstanceFamily :: Lens.Lens' AllocateHosts (Core.Maybe Types.InstanceFamily)
ahInstanceFamily = Lens.field @"instanceFamily"
{-# DEPRECATED ahInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | Specifies the instance type to be supported by the Dedicated Hosts. If you specify an instance type, the Dedicated Hosts support instances of the specified instance type only.
--
-- If you want the Dedicated Hosts to support multiple instance types in a specific instance family, omit this parameter and specify __InstanceFamily__ instead. You cannot specify __InstanceType__ and __InstanceFamily__ in the same request.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahInstanceType :: Lens.Lens' AllocateHosts (Core.Maybe Types.String)
ahInstanceType = Lens.field @"instanceType"
{-# DEPRECATED ahInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The tags to apply to the Dedicated Host during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahTagSpecifications :: Lens.Lens' AllocateHosts (Core.Maybe [Types.TagSpecification])
ahTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED ahTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest AllocateHosts where
  type Rs AllocateHosts = AllocateHostsResponse
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
            ( Core.pure ("Action", "AllocateHosts")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "AvailabilityZone" availabilityZone)
                Core.<> (Core.toQueryValue "Quantity" quantity)
                Core.<> (Core.toQueryValue "AutoPlacement" Core.<$> autoPlacement)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "HostRecovery" Core.<$> hostRecovery)
                Core.<> (Core.toQueryValue "InstanceFamily" Core.<$> instanceFamily)
                Core.<> (Core.toQueryValue "InstanceType" Core.<$> instanceType)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          AllocateHostsResponse'
            Core.<$> (x Core..@? "hostIdSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of AllocateHosts.
--
-- /See:/ 'mkAllocateHostsResponse' smart constructor.
data AllocateHostsResponse = AllocateHostsResponse'
  { -- | The ID of the allocated Dedicated Host. This is used to launch an instance onto a specific host.
    hostIds :: Core.Maybe [Types.String],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AllocateHostsResponse' value with any optional fields omitted.
mkAllocateHostsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AllocateHostsResponse
mkAllocateHostsResponse responseStatus =
  AllocateHostsResponse' {hostIds = Core.Nothing, responseStatus}

-- | The ID of the allocated Dedicated Host. This is used to launch an instance onto a specific host.
--
-- /Note:/ Consider using 'hostIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahrrsHostIds :: Lens.Lens' AllocateHostsResponse (Core.Maybe [Types.String])
ahrrsHostIds = Lens.field @"hostIds"
{-# DEPRECATED ahrrsHostIds "Use generic-lens or generic-optics with 'hostIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ahrrsResponseStatus :: Lens.Lens' AllocateHostsResponse Core.Int
ahrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ahrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
