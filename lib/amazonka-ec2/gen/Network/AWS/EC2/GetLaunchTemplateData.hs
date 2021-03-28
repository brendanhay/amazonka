{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetLaunchTemplateData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration data of the specified instance. You can use this data to create a launch template. 
--
-- This action calls on other describe actions to get instance information. Depending on your instance configuration, you may need to allow the following actions in your IAM policy: DescribeSpotInstanceRequests, DescribeInstanceCreditSpecifications, DescribeVolumes, DescribeInstanceAttribute, and DescribeElasticGpus. Or, you can allow @describe*@ depending on your instance requirements.
module Network.AWS.EC2.GetLaunchTemplateData
    (
    -- * Creating a request
      GetLaunchTemplateData (..)
    , mkGetLaunchTemplateData
    -- ** Request lenses
    , gltdInstanceId
    , gltdDryRun

    -- * Destructuring the response
    , GetLaunchTemplateDataResponse (..)
    , mkGetLaunchTemplateDataResponse
    -- ** Response lenses
    , gltdrrsLaunchTemplateData
    , gltdrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLaunchTemplateData' smart constructor.
data GetLaunchTemplateData = GetLaunchTemplateData'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLaunchTemplateData' value with any optional fields omitted.
mkGetLaunchTemplateData
    :: Types.InstanceId -- ^ 'instanceId'
    -> GetLaunchTemplateData
mkGetLaunchTemplateData instanceId
  = GetLaunchTemplateData'{instanceId, dryRun = Core.Nothing}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gltdInstanceId :: Lens.Lens' GetLaunchTemplateData Types.InstanceId
gltdInstanceId = Lens.field @"instanceId"
{-# INLINEABLE gltdInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gltdDryRun :: Lens.Lens' GetLaunchTemplateData (Core.Maybe Core.Bool)
gltdDryRun = Lens.field @"dryRun"
{-# INLINEABLE gltdDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery GetLaunchTemplateData where
        toQuery GetLaunchTemplateData{..}
          = Core.toQueryPair "Action" ("GetLaunchTemplateData" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders GetLaunchTemplateData where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetLaunchTemplateData where
        type Rs GetLaunchTemplateData = GetLaunchTemplateDataResponse
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
                 GetLaunchTemplateDataResponse' Core.<$>
                   (x Core..@? "launchTemplateData") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetLaunchTemplateDataResponse' smart constructor.
data GetLaunchTemplateDataResponse = GetLaunchTemplateDataResponse'
  { launchTemplateData :: Core.Maybe Types.ResponseLaunchTemplateData
    -- ^ The instance data.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetLaunchTemplateDataResponse' value with any optional fields omitted.
mkGetLaunchTemplateDataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetLaunchTemplateDataResponse
mkGetLaunchTemplateDataResponse responseStatus
  = GetLaunchTemplateDataResponse'{launchTemplateData = Core.Nothing,
                                   responseStatus}

-- | The instance data.
--
-- /Note:/ Consider using 'launchTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gltdrrsLaunchTemplateData :: Lens.Lens' GetLaunchTemplateDataResponse (Core.Maybe Types.ResponseLaunchTemplateData)
gltdrrsLaunchTemplateData = Lens.field @"launchTemplateData"
{-# INLINEABLE gltdrrsLaunchTemplateData #-}
{-# DEPRECATED launchTemplateData "Use generic-lens or generic-optics with 'launchTemplateData' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gltdrrsResponseStatus :: Lens.Lens' GetLaunchTemplateDataResponse Core.Int
gltdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gltdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
