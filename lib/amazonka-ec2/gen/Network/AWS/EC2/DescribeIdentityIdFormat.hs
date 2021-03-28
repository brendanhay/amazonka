{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeIdentityIdFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ID format settings for resources for the specified IAM user, IAM role, or root user. For example, you can view the resource types that are enabled for longer IDs. This request only returns information about resource types whose ID formats can be modified; it does not return information about other resource types. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/resource-ids.html Resource IDs> in the /Amazon Elastic Compute Cloud User Guide/ . 
--
-- The following resource types support longer IDs: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ . 
-- These settings apply to the principal specified in the request. They do not apply to the principal that makes the request.
module Network.AWS.EC2.DescribeIdentityIdFormat
    (
    -- * Creating a request
      DescribeIdentityIdFormat (..)
    , mkDescribeIdentityIdFormat
    -- ** Request lenses
    , diifPrincipalArn
    , diifResource

    -- * Destructuring the response
    , DescribeIdentityIdFormatResponse (..)
    , mkDescribeIdentityIdFormatResponse
    -- ** Response lenses
    , diifrrsStatuses
    , diifrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeIdentityIdFormat' smart constructor.
data DescribeIdentityIdFormat = DescribeIdentityIdFormat'
  { principalArn :: Core.Text
    -- ^ The ARN of the principal, which can be an IAM role, IAM user, or the root user.
  , resource :: Core.Maybe Core.Text
    -- ^ The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeIdentityIdFormat' value with any optional fields omitted.
mkDescribeIdentityIdFormat
    :: Core.Text -- ^ 'principalArn'
    -> DescribeIdentityIdFormat
mkDescribeIdentityIdFormat principalArn
  = DescribeIdentityIdFormat'{principalArn, resource = Core.Nothing}

-- | The ARN of the principal, which can be an IAM role, IAM user, or the root user.
--
-- /Note:/ Consider using 'principalArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diifPrincipalArn :: Lens.Lens' DescribeIdentityIdFormat Core.Text
diifPrincipalArn = Lens.field @"principalArn"
{-# INLINEABLE diifPrincipalArn #-}
{-# DEPRECATED principalArn "Use generic-lens or generic-optics with 'principalArn' instead"  #-}

-- | The type of resource: @bundle@ | @conversion-task@ | @customer-gateway@ | @dhcp-options@ | @elastic-ip-allocation@ | @elastic-ip-association@ | @export-task@ | @flow-log@ | @image@ | @import-task@ | @instance@ | @internet-gateway@ | @network-acl@ | @network-acl-association@ | @network-interface@ | @network-interface-attachment@ | @prefix-list@ | @reservation@ | @route-table@ | @route-table-association@ | @security-group@ | @snapshot@ | @subnet@ | @subnet-cidr-block-association@ | @volume@ | @vpc@ | @vpc-cidr-block-association@ | @vpc-endpoint@ | @vpc-peering-connection@ | @vpn-connection@ | @vpn-gateway@ 
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diifResource :: Lens.Lens' DescribeIdentityIdFormat (Core.Maybe Core.Text)
diifResource = Lens.field @"resource"
{-# INLINEABLE diifResource #-}
{-# DEPRECATED resource "Use generic-lens or generic-optics with 'resource' instead"  #-}

instance Core.ToQuery DescribeIdentityIdFormat where
        toQuery DescribeIdentityIdFormat{..}
          = Core.toQueryPair "Action"
              ("DescribeIdentityIdFormat" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "PrincipalArn" principalArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Resource") resource

instance Core.ToHeaders DescribeIdentityIdFormat where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeIdentityIdFormat where
        type Rs DescribeIdentityIdFormat = DescribeIdentityIdFormatResponse
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
                 DescribeIdentityIdFormatResponse' Core.<$>
                   (x Core..@? "statusSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeIdentityIdFormatResponse' smart constructor.
data DescribeIdentityIdFormatResponse = DescribeIdentityIdFormatResponse'
  { statuses :: Core.Maybe [Types.IdFormat]
    -- ^ Information about the ID format for the resources.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeIdentityIdFormatResponse' value with any optional fields omitted.
mkDescribeIdentityIdFormatResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeIdentityIdFormatResponse
mkDescribeIdentityIdFormatResponse responseStatus
  = DescribeIdentityIdFormatResponse'{statuses = Core.Nothing,
                                      responseStatus}

-- | Information about the ID format for the resources.
--
-- /Note:/ Consider using 'statuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diifrrsStatuses :: Lens.Lens' DescribeIdentityIdFormatResponse (Core.Maybe [Types.IdFormat])
diifrrsStatuses = Lens.field @"statuses"
{-# INLINEABLE diifrrsStatuses #-}
{-# DEPRECATED statuses "Use generic-lens or generic-optics with 'statuses' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diifrrsResponseStatus :: Lens.Lens' DescribeIdentityIdFormatResponse Core.Int
diifrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diifrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
