{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetLaunchTemplateData (..),
    mkGetLaunchTemplateData,

    -- ** Request lenses
    gltdInstanceId,
    gltdDryRun,

    -- * Destructuring the response
    GetLaunchTemplateDataResponse (..),
    mkGetLaunchTemplateDataResponse,

    -- ** Response lenses
    gltdrrsLaunchTemplateData,
    gltdrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLaunchTemplateData' smart constructor.
data GetLaunchTemplateData = GetLaunchTemplateData'
  { -- | The ID of the instance.
    instanceId :: Types.InstanceId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLaunchTemplateData' value with any optional fields omitted.
mkGetLaunchTemplateData ::
  -- | 'instanceId'
  Types.InstanceId ->
  GetLaunchTemplateData
mkGetLaunchTemplateData instanceId =
  GetLaunchTemplateData' {instanceId, dryRun = Core.Nothing}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gltdInstanceId :: Lens.Lens' GetLaunchTemplateData Types.InstanceId
gltdInstanceId = Lens.field @"instanceId"
{-# DEPRECATED gltdInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gltdDryRun :: Lens.Lens' GetLaunchTemplateData (Core.Maybe Core.Bool)
gltdDryRun = Lens.field @"dryRun"
{-# DEPRECATED gltdDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest GetLaunchTemplateData where
  type Rs GetLaunchTemplateData = GetLaunchTemplateDataResponse
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
            ( Core.pure ("Action", "GetLaunchTemplateData")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InstanceId" instanceId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetLaunchTemplateDataResponse'
            Core.<$> (x Core..@? "launchTemplateData")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetLaunchTemplateDataResponse' smart constructor.
data GetLaunchTemplateDataResponse = GetLaunchTemplateDataResponse'
  { -- | The instance data.
    launchTemplateData :: Core.Maybe Types.ResponseLaunchTemplateData,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetLaunchTemplateDataResponse' value with any optional fields omitted.
mkGetLaunchTemplateDataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetLaunchTemplateDataResponse
mkGetLaunchTemplateDataResponse responseStatus =
  GetLaunchTemplateDataResponse'
    { launchTemplateData = Core.Nothing,
      responseStatus
    }

-- | The instance data.
--
-- /Note:/ Consider using 'launchTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gltdrrsLaunchTemplateData :: Lens.Lens' GetLaunchTemplateDataResponse (Core.Maybe Types.ResponseLaunchTemplateData)
gltdrrsLaunchTemplateData = Lens.field @"launchTemplateData"
{-# DEPRECATED gltdrrsLaunchTemplateData "Use generic-lens or generic-optics with 'launchTemplateData' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gltdrrsResponseStatus :: Lens.Lens' GetLaunchTemplateDataResponse Core.Int
gltdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gltdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
