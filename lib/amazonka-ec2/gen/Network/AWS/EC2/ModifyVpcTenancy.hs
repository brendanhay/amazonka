{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVpcTenancy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the instance tenancy attribute of the specified VPC. You can change the instance tenancy attribute of a VPC to @default@ only. You cannot change the instance tenancy attribute to @dedicated@ .
--
-- After you modify the tenancy of the VPC, any new instances that you launch into the VPC have a tenancy of @default@ , unless you specify otherwise during launch. The tenancy of any existing instances in the VPC is not affected.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-instance.html Dedicated Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ModifyVpcTenancy
    (
    -- * Creating a request
      ModifyVpcTenancy (..)
    , mkModifyVpcTenancy
    -- ** Request lenses
    , mvtVpcId
    , mvtInstanceTenancy
    , mvtDryRun

    -- * Destructuring the response
    , ModifyVpcTenancyResponse (..)
    , mkModifyVpcTenancyResponse
    -- ** Response lenses
    , mvtrrsReturnValue
    , mvtrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyVpcTenancy' smart constructor.
data ModifyVpcTenancy = ModifyVpcTenancy'
  { vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , instanceTenancy :: Types.VpcTenancy
    -- ^ The instance tenancy attribute for the VPC. 
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcTenancy' value with any optional fields omitted.
mkModifyVpcTenancy
    :: Types.VpcId -- ^ 'vpcId'
    -> Types.VpcTenancy -- ^ 'instanceTenancy'
    -> ModifyVpcTenancy
mkModifyVpcTenancy vpcId instanceTenancy
  = ModifyVpcTenancy'{vpcId, instanceTenancy, dryRun = Core.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtVpcId :: Lens.Lens' ModifyVpcTenancy Types.VpcId
mvtVpcId = Lens.field @"vpcId"
{-# INLINEABLE mvtVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The instance tenancy attribute for the VPC. 
--
-- /Note:/ Consider using 'instanceTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtInstanceTenancy :: Lens.Lens' ModifyVpcTenancy Types.VpcTenancy
mvtInstanceTenancy = Lens.field @"instanceTenancy"
{-# INLINEABLE mvtInstanceTenancy #-}
{-# DEPRECATED instanceTenancy "Use generic-lens or generic-optics with 'instanceTenancy' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtDryRun :: Lens.Lens' ModifyVpcTenancy (Core.Maybe Core.Bool)
mvtDryRun = Lens.field @"dryRun"
{-# INLINEABLE mvtDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ModifyVpcTenancy where
        toQuery ModifyVpcTenancy{..}
          = Core.toQueryPair "Action" ("ModifyVpcTenancy" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.toQueryPair "InstanceTenancy" instanceTenancy
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ModifyVpcTenancy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyVpcTenancy where
        type Rs ModifyVpcTenancy = ModifyVpcTenancyResponse
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
                 ModifyVpcTenancyResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyVpcTenancyResponse' smart constructor.
data ModifyVpcTenancyResponse = ModifyVpcTenancyResponse'
  { returnValue :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVpcTenancyResponse' value with any optional fields omitted.
mkModifyVpcTenancyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyVpcTenancyResponse
mkModifyVpcTenancyResponse responseStatus
  = ModifyVpcTenancyResponse'{returnValue = Core.Nothing,
                              responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- /Note:/ Consider using 'returnValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtrrsReturnValue :: Lens.Lens' ModifyVpcTenancyResponse (Core.Maybe Core.Bool)
mvtrrsReturnValue = Lens.field @"returnValue"
{-# INLINEABLE mvtrrsReturnValue #-}
{-# DEPRECATED returnValue "Use generic-lens or generic-optics with 'returnValue' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtrrsResponseStatus :: Lens.Lens' ModifyVpcTenancyResponse Core.Int
mvtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mvtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
