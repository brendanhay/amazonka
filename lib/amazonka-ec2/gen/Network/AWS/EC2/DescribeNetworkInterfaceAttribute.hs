{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a network interface attribute. You can specify only one attribute at a time.
module Network.AWS.EC2.DescribeNetworkInterfaceAttribute
    (
    -- * Creating a request
      DescribeNetworkInterfaceAttribute (..)
    , mkDescribeNetworkInterfaceAttribute
    -- ** Request lenses
    , dniaNetworkInterfaceId
    , dniaAttribute
    , dniaDryRun

    -- * Destructuring the response
    , DescribeNetworkInterfaceAttributeResponse (..)
    , mkDescribeNetworkInterfaceAttributeResponse
    -- ** Response lenses
    , dniarrsAttachment
    , dniarrsDescription
    , dniarrsGroups
    , dniarrsNetworkInterfaceId
    , dniarrsSourceDestCheck
    , dniarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeNetworkInterfaceAttribute.
--
-- /See:/ 'mkDescribeNetworkInterfaceAttribute' smart constructor.
data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute'
  { networkInterfaceId :: Types.NetworkInterfaceId
    -- ^ The ID of the network interface.
  , attribute :: Core.Maybe Types.NetworkInterfaceAttribute
    -- ^ The attribute of the network interface. This parameter is required.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNetworkInterfaceAttribute' value with any optional fields omitted.
mkDescribeNetworkInterfaceAttribute
    :: Types.NetworkInterfaceId -- ^ 'networkInterfaceId'
    -> DescribeNetworkInterfaceAttribute
mkDescribeNetworkInterfaceAttribute networkInterfaceId
  = DescribeNetworkInterfaceAttribute'{networkInterfaceId,
                                       attribute = Core.Nothing, dryRun = Core.Nothing}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniaNetworkInterfaceId :: Lens.Lens' DescribeNetworkInterfaceAttribute Types.NetworkInterfaceId
dniaNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE dniaNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The attribute of the network interface. This parameter is required.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniaAttribute :: Lens.Lens' DescribeNetworkInterfaceAttribute (Core.Maybe Types.NetworkInterfaceAttribute)
dniaAttribute = Lens.field @"attribute"
{-# INLINEABLE dniaAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniaDryRun :: Lens.Lens' DescribeNetworkInterfaceAttribute (Core.Maybe Core.Bool)
dniaDryRun = Lens.field @"dryRun"
{-# INLINEABLE dniaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DescribeNetworkInterfaceAttribute where
        toQuery DescribeNetworkInterfaceAttribute{..}
          = Core.toQueryPair "Action"
              ("DescribeNetworkInterfaceAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "NetworkInterfaceId" networkInterfaceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Attribute") attribute
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DescribeNetworkInterfaceAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeNetworkInterfaceAttribute where
        type Rs DescribeNetworkInterfaceAttribute =
             DescribeNetworkInterfaceAttributeResponse
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
                 DescribeNetworkInterfaceAttributeResponse' Core.<$>
                   (x Core..@? "attachment") Core.<*> x Core..@? "description"
                     Core.<*> x Core..@? "groupSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> x Core..@? "networkInterfaceId"
                     Core.<*> x Core..@? "sourceDestCheck"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of DescribeNetworkInterfaceAttribute.
--
-- /See:/ 'mkDescribeNetworkInterfaceAttributeResponse' smart constructor.
data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse'
  { attachment :: Core.Maybe Types.NetworkInterfaceAttachment
    -- ^ The attachment (if any) of the network interface.
  , description :: Core.Maybe Types.AttributeValue
    -- ^ The description of the network interface.
  , groups :: Core.Maybe [Types.GroupIdentifier]
    -- ^ The security groups associated with the network interface.
  , networkInterfaceId :: Core.Maybe Core.Text
    -- ^ The ID of the network interface.
  , sourceDestCheck :: Core.Maybe Types.AttributeBooleanValue
    -- ^ Indicates whether source/destination checking is enabled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeNetworkInterfaceAttributeResponse' value with any optional fields omitted.
mkDescribeNetworkInterfaceAttributeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeNetworkInterfaceAttributeResponse
mkDescribeNetworkInterfaceAttributeResponse responseStatus
  = DescribeNetworkInterfaceAttributeResponse'{attachment =
                                                 Core.Nothing,
                                               description = Core.Nothing, groups = Core.Nothing,
                                               networkInterfaceId = Core.Nothing,
                                               sourceDestCheck = Core.Nothing, responseStatus}

-- | The attachment (if any) of the network interface.
--
-- /Note:/ Consider using 'attachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniarrsAttachment :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Core.Maybe Types.NetworkInterfaceAttachment)
dniarrsAttachment = Lens.field @"attachment"
{-# INLINEABLE dniarrsAttachment #-}
{-# DEPRECATED attachment "Use generic-lens or generic-optics with 'attachment' instead"  #-}

-- | The description of the network interface.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniarrsDescription :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Core.Maybe Types.AttributeValue)
dniarrsDescription = Lens.field @"description"
{-# INLINEABLE dniarrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The security groups associated with the network interface.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniarrsGroups :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Core.Maybe [Types.GroupIdentifier])
dniarrsGroups = Lens.field @"groups"
{-# INLINEABLE dniarrsGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniarrsNetworkInterfaceId :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Core.Maybe Core.Text)
dniarrsNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE dniarrsNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | Indicates whether source/destination checking is enabled.
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniarrsSourceDestCheck :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse (Core.Maybe Types.AttributeBooleanValue)
dniarrsSourceDestCheck = Lens.field @"sourceDestCheck"
{-# INLINEABLE dniarrsSourceDestCheck #-}
{-# DEPRECATED sourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniarrsResponseStatus :: Lens.Lens' DescribeNetworkInterfaceAttributeResponse Core.Int
dniarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dniarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
