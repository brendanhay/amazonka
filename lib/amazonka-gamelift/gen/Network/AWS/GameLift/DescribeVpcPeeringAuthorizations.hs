{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeVpcPeeringAuthorizations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves valid VPC peering authorizations that are pending for the AWS account. This operation returns all VPC peering authorizations and requests for peering. This includes those initiated and received by this account. 
--
--
--     * 'CreateVpcPeeringAuthorization' 
--
--
--     * 'DescribeVpcPeeringAuthorizations' 
--
--
--     * 'DeleteVpcPeeringAuthorization' 
--
--
--     * 'CreateVpcPeeringConnection' 
--
--
--     * 'DescribeVpcPeeringConnections' 
--
--
--     * 'DeleteVpcPeeringConnection' 
--
--
module Network.AWS.GameLift.DescribeVpcPeeringAuthorizations
    (
    -- * Creating a request
      DescribeVpcPeeringAuthorizations (..)
    , mkDescribeVpcPeeringAuthorizations

    -- * Destructuring the response
    , DescribeVpcPeeringAuthorizationsResponse (..)
    , mkDescribeVpcPeeringAuthorizationsResponse
    -- ** Response lenses
    , dvparrsVpcPeeringAuthorizations
    , dvparrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVpcPeeringAuthorizations' smart constructor.
data DescribeVpcPeeringAuthorizations = DescribeVpcPeeringAuthorizations'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcPeeringAuthorizations' value with any optional fields omitted.
mkDescribeVpcPeeringAuthorizations
    :: DescribeVpcPeeringAuthorizations
mkDescribeVpcPeeringAuthorizations
  = DescribeVpcPeeringAuthorizations'

instance Core.ToQuery DescribeVpcPeeringAuthorizations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeVpcPeeringAuthorizations where
        toHeaders DescribeVpcPeeringAuthorizations{..}
          = Core.pure
              ("X-Amz-Target", "GameLift.DescribeVpcPeeringAuthorizations")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeVpcPeeringAuthorizations where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeVpcPeeringAuthorizations where
        type Rs DescribeVpcPeeringAuthorizations =
             DescribeVpcPeeringAuthorizationsResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeVpcPeeringAuthorizationsResponse' Core.<$>
                   (x Core..:? "VpcPeeringAuthorizations") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeVpcPeeringAuthorizationsResponse' smart constructor.
data DescribeVpcPeeringAuthorizationsResponse = DescribeVpcPeeringAuthorizationsResponse'
  { vpcPeeringAuthorizations :: Core.Maybe [Types.VpcPeeringAuthorization]
    -- ^ A collection of objects that describe all valid VPC peering operations for the current AWS account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeVpcPeeringAuthorizationsResponse' value with any optional fields omitted.
mkDescribeVpcPeeringAuthorizationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpcPeeringAuthorizationsResponse
mkDescribeVpcPeeringAuthorizationsResponse responseStatus
  = DescribeVpcPeeringAuthorizationsResponse'{vpcPeeringAuthorizations
                                                = Core.Nothing,
                                              responseStatus}

-- | A collection of objects that describe all valid VPC peering operations for the current AWS account.
--
-- /Note:/ Consider using 'vpcPeeringAuthorizations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvparrsVpcPeeringAuthorizations :: Lens.Lens' DescribeVpcPeeringAuthorizationsResponse (Core.Maybe [Types.VpcPeeringAuthorization])
dvparrsVpcPeeringAuthorizations = Lens.field @"vpcPeeringAuthorizations"
{-# INLINEABLE dvparrsVpcPeeringAuthorizations #-}
{-# DEPRECATED vpcPeeringAuthorizations "Use generic-lens or generic-optics with 'vpcPeeringAuthorizations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvparrsResponseStatus :: Lens.Lens' DescribeVpcPeeringAuthorizationsResponse Core.Int
dvparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
