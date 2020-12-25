{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns AWS resources for this environment.
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
  ( -- * Creating a request
    DescribeEnvironmentResources (..),
    mkDescribeEnvironmentResources,

    -- ** Request lenses
    derEnvironmentId,
    derEnvironmentName,

    -- * Destructuring the response
    DescribeEnvironmentResourcesResponse (..),
    mkDescribeEnvironmentResourcesResponse,

    -- ** Response lenses
    derrrsEnvironmentResources,
    derrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe the resources in an environment.
--
-- /See:/ 'mkDescribeEnvironmentResources' smart constructor.
data DescribeEnvironmentResources = DescribeEnvironmentResources'
  { -- | The ID of the environment to retrieve AWS resource usage data.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentId :: Core.Maybe Types.EnvironmentId,
    -- | The name of the environment to retrieve AWS resource usage data.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentName :: Core.Maybe Types.EnvironmentName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEnvironmentResources' value with any optional fields omitted.
mkDescribeEnvironmentResources ::
  DescribeEnvironmentResources
mkDescribeEnvironmentResources =
  DescribeEnvironmentResources'
    { environmentId = Core.Nothing,
      environmentName = Core.Nothing
    }

-- | The ID of the environment to retrieve AWS resource usage data.
--
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derEnvironmentId :: Lens.Lens' DescribeEnvironmentResources (Core.Maybe Types.EnvironmentId)
derEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED derEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The name of the environment to retrieve AWS resource usage data.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derEnvironmentName :: Lens.Lens' DescribeEnvironmentResources (Core.Maybe Types.EnvironmentName)
derEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED derEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

instance Core.AWSRequest DescribeEnvironmentResources where
  type
    Rs DescribeEnvironmentResources =
      DescribeEnvironmentResourcesResponse
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
            ( Core.pure ("Action", "DescribeEnvironmentResources")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "EnvironmentId" Core.<$> environmentId)
                Core.<> (Core.toQueryValue "EnvironmentName" Core.<$> environmentName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeEnvironmentResourcesResult"
      ( \s h x ->
          DescribeEnvironmentResourcesResponse'
            Core.<$> (x Core..@? "EnvironmentResources")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Result message containing a list of environment resource descriptions.
--
-- /See:/ 'mkDescribeEnvironmentResourcesResponse' smart constructor.
data DescribeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse'
  { -- | A list of 'EnvironmentResourceDescription' .
    environmentResources :: Core.Maybe Types.EnvironmentResourceDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEnvironmentResourcesResponse' value with any optional fields omitted.
mkDescribeEnvironmentResourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEnvironmentResourcesResponse
mkDescribeEnvironmentResourcesResponse responseStatus =
  DescribeEnvironmentResourcesResponse'
    { environmentResources =
        Core.Nothing,
      responseStatus
    }

-- | A list of 'EnvironmentResourceDescription' .
--
-- /Note:/ Consider using 'environmentResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrrsEnvironmentResources :: Lens.Lens' DescribeEnvironmentResourcesResponse (Core.Maybe Types.EnvironmentResourceDescription)
derrrsEnvironmentResources = Lens.field @"environmentResources"
{-# DEPRECATED derrrsEnvironmentResources "Use generic-lens or generic-optics with 'environmentResources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrrsResponseStatus :: Lens.Lens' DescribeEnvironmentResourcesResponse Core.Int
derrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
