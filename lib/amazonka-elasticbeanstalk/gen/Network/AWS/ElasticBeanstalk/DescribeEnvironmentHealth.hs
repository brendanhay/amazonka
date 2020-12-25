{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the overall health of the specified environment. The __DescribeEnvironmentHealth__ operation is only available with AWS Elastic Beanstalk Enhanced Health.
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentHealth
  ( -- * Creating a request
    DescribeEnvironmentHealth (..),
    mkDescribeEnvironmentHealth,

    -- ** Request lenses
    dehAttributeNames,
    dehEnvironmentId,
    dehEnvironmentName,

    -- * Destructuring the response
    DescribeEnvironmentHealthResponse (..),
    mkDescribeEnvironmentHealthResponse,

    -- ** Response lenses
    dehrrsApplicationMetrics,
    dehrrsCauses,
    dehrrsColor,
    dehrrsEnvironmentName,
    dehrrsHealthStatus,
    dehrrsInstancesHealth,
    dehrrsRefreshedAt,
    dehrrsStatus,
    dehrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | See the example below to learn how to create a request body.
--
-- /See:/ 'mkDescribeEnvironmentHealth' smart constructor.
data DescribeEnvironmentHealth = DescribeEnvironmentHealth'
  { -- | Specify the response elements to return. To retrieve all attributes, set to @All@ . If no attribute names are specified, returns the name of the environment.
    attributeNames :: Core.Maybe [Types.EnvironmentHealthAttribute],
    -- | Specify the environment by ID.
    --
    -- You must specify either this or an EnvironmentName, or both.
    environmentId :: Core.Maybe Types.EnvironmentId,
    -- | Specify the environment by name.
    --
    -- You must specify either this or an EnvironmentName, or both.
    environmentName :: Core.Maybe Types.EnvironmentName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEnvironmentHealth' value with any optional fields omitted.
mkDescribeEnvironmentHealth ::
  DescribeEnvironmentHealth
mkDescribeEnvironmentHealth =
  DescribeEnvironmentHealth'
    { attributeNames = Core.Nothing,
      environmentId = Core.Nothing,
      environmentName = Core.Nothing
    }

-- | Specify the response elements to return. To retrieve all attributes, set to @All@ . If no attribute names are specified, returns the name of the environment.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehAttributeNames :: Lens.Lens' DescribeEnvironmentHealth (Core.Maybe [Types.EnvironmentHealthAttribute])
dehAttributeNames = Lens.field @"attributeNames"
{-# DEPRECATED dehAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

-- | Specify the environment by ID.
--
-- You must specify either this or an EnvironmentName, or both.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehEnvironmentId :: Lens.Lens' DescribeEnvironmentHealth (Core.Maybe Types.EnvironmentId)
dehEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED dehEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | Specify the environment by name.
--
-- You must specify either this or an EnvironmentName, or both.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehEnvironmentName :: Lens.Lens' DescribeEnvironmentHealth (Core.Maybe Types.EnvironmentName)
dehEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED dehEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

instance Core.AWSRequest DescribeEnvironmentHealth where
  type
    Rs DescribeEnvironmentHealth =
      DescribeEnvironmentHealthResponse
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
            ( Core.pure ("Action", "DescribeEnvironmentHealth")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> ( Core.toQueryValue
                            "AttributeNames"
                            (Core.toQueryList "member" Core.<$> attributeNames)
                        )
                Core.<> (Core.toQueryValue "EnvironmentId" Core.<$> environmentId)
                Core.<> (Core.toQueryValue "EnvironmentName" Core.<$> environmentName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeEnvironmentHealthResult"
      ( \s h x ->
          DescribeEnvironmentHealthResponse'
            Core.<$> (x Core..@? "ApplicationMetrics")
            Core.<*> (x Core..@? "Causes" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "Color")
            Core.<*> (x Core..@? "EnvironmentName")
            Core.<*> (x Core..@? "HealthStatus")
            Core.<*> (x Core..@? "InstancesHealth")
            Core.<*> (x Core..@? "RefreshedAt")
            Core.<*> (x Core..@? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Health details for an AWS Elastic Beanstalk environment.
--
-- /See:/ 'mkDescribeEnvironmentHealthResponse' smart constructor.
data DescribeEnvironmentHealthResponse = DescribeEnvironmentHealthResponse'
  { -- | Application request metrics for the environment.
    applicationMetrics :: Core.Maybe Types.ApplicationMetrics,
    -- | Descriptions of the data that contributed to the environment's current health status.
    causes :: Core.Maybe [Types.Cause],
    -- | The <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health color> of the environment.
    color :: Core.Maybe Types.String,
    -- | The environment's name.
    environmentName :: Core.Maybe Types.EnvironmentName,
    -- | The <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health status> of the environment. For example, @Ok@ .
    healthStatus :: Core.Maybe Types.String,
    -- | Summary health information for the instances in the environment.
    instancesHealth :: Core.Maybe Types.InstanceHealthSummary,
    -- | The date and time that the health information was retrieved.
    refreshedAt :: Core.Maybe Core.UTCTime,
    -- | The environment's operational status. @Ready@ , @Launching@ , @Updating@ , @Terminating@ , or @Terminated@ .
    status :: Core.Maybe Types.EnvironmentHealth,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeEnvironmentHealthResponse' value with any optional fields omitted.
mkDescribeEnvironmentHealthResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEnvironmentHealthResponse
mkDescribeEnvironmentHealthResponse responseStatus =
  DescribeEnvironmentHealthResponse'
    { applicationMetrics =
        Core.Nothing,
      causes = Core.Nothing,
      color = Core.Nothing,
      environmentName = Core.Nothing,
      healthStatus = Core.Nothing,
      instancesHealth = Core.Nothing,
      refreshedAt = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | Application request metrics for the environment.
--
-- /Note:/ Consider using 'applicationMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrrsApplicationMetrics :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe Types.ApplicationMetrics)
dehrrsApplicationMetrics = Lens.field @"applicationMetrics"
{-# DEPRECATED dehrrsApplicationMetrics "Use generic-lens or generic-optics with 'applicationMetrics' instead." #-}

-- | Descriptions of the data that contributed to the environment's current health status.
--
-- /Note:/ Consider using 'causes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrrsCauses :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe [Types.Cause])
dehrrsCauses = Lens.field @"causes"
{-# DEPRECATED dehrrsCauses "Use generic-lens or generic-optics with 'causes' instead." #-}

-- | The <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health color> of the environment.
--
-- /Note:/ Consider using 'color' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrrsColor :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe Types.String)
dehrrsColor = Lens.field @"color"
{-# DEPRECATED dehrrsColor "Use generic-lens or generic-optics with 'color' instead." #-}

-- | The environment's name.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrrsEnvironmentName :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe Types.EnvironmentName)
dehrrsEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED dehrrsEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health status> of the environment. For example, @Ok@ .
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrrsHealthStatus :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe Types.String)
dehrrsHealthStatus = Lens.field @"healthStatus"
{-# DEPRECATED dehrrsHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

-- | Summary health information for the instances in the environment.
--
-- /Note:/ Consider using 'instancesHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrrsInstancesHealth :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe Types.InstanceHealthSummary)
dehrrsInstancesHealth = Lens.field @"instancesHealth"
{-# DEPRECATED dehrrsInstancesHealth "Use generic-lens or generic-optics with 'instancesHealth' instead." #-}

-- | The date and time that the health information was retrieved.
--
-- /Note:/ Consider using 'refreshedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrrsRefreshedAt :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe Core.UTCTime)
dehrrsRefreshedAt = Lens.field @"refreshedAt"
{-# DEPRECATED dehrrsRefreshedAt "Use generic-lens or generic-optics with 'refreshedAt' instead." #-}

-- | The environment's operational status. @Ready@ , @Launching@ , @Updating@ , @Terminating@ , or @Terminated@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrrsStatus :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe Types.EnvironmentHealth)
dehrrsStatus = Lens.field @"status"
{-# DEPRECATED dehrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrrsResponseStatus :: Lens.Lens' DescribeEnvironmentHealthResponse Core.Int
dehrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dehrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
