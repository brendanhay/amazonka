{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVpc
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified VPC. You must detach or delete all gateways and resources that are associated with the VPC before you can delete it. For example, you must terminate all instances running in the VPC, delete all security groups associated with the VPC (except the default one), delete all route tables associated with the VPC (except the default one), and so on.
module Network.AWS.EC2.DeleteVpc
    (
    -- * Creating a request
      DeleteVpc (..)
    , mkDeleteVpc
    -- ** Request lenses
    , dvhVpcId
    , dvhDryRun

    -- * Destructuring the response
    , DeleteVpcResponse (..)
    , mkDeleteVpcResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteVpc' smart constructor.
data DeleteVpc = DeleteVpc'
  { vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpc' value with any optional fields omitted.
mkDeleteVpc
    :: Types.VpcId -- ^ 'vpcId'
    -> DeleteVpc
mkDeleteVpc vpcId = DeleteVpc'{vpcId, dryRun = Core.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvhVpcId :: Lens.Lens' DeleteVpc Types.VpcId
dvhVpcId = Lens.field @"vpcId"
{-# INLINEABLE dvhVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvhDryRun :: Lens.Lens' DeleteVpc (Core.Maybe Core.Bool)
dvhDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvhDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteVpc where
        toQuery DeleteVpc{..}
          = Core.toQueryPair "Action" ("DeleteVpc" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteVpc where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteVpc where
        type Rs DeleteVpc = DeleteVpcResponse
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
        parseResponse = Response.receiveNull DeleteVpcResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVpcResponse' smart constructor.
data DeleteVpcResponse = DeleteVpcResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcResponse' value with any optional fields omitted.
mkDeleteVpcResponse
    :: DeleteVpcResponse
mkDeleteVpcResponse = DeleteVpcResponse'
