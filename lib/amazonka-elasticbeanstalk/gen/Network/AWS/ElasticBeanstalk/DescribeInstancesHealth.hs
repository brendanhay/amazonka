{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeInstancesHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves detailed information about the health of instances in your AWS Elastic Beanstalk. This operation requires <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced.html enhanced health reporting> .
module Network.AWS.ElasticBeanstalk.DescribeInstancesHealth
  ( -- * Creating a request
    DescribeInstancesHealth (..),
    mkDescribeInstancesHealth,

    -- ** Request lenses
    dihAttributeNames,
    dihEnvironmentId,
    dihEnvironmentName,
    dihNextToken,

    -- * Destructuring the response
    DescribeInstancesHealthResponse (..),
    mkDescribeInstancesHealthResponse,

    -- ** Response lenses
    dihrrsInstanceHealthList,
    dihrrsNextToken,
    dihrrsRefreshedAt,
    dihrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Parameters for a call to @DescribeInstancesHealth@ .
--
-- /See:/ 'mkDescribeInstancesHealth' smart constructor.
data DescribeInstancesHealth = DescribeInstancesHealth'
  { -- | Specifies the response elements you wish to receive. To retrieve all attributes, set to @All@ . If no attribute names are specified, returns a list of instances.
    attributeNames :: Core.Maybe [Types.InstancesHealthAttribute],
    -- | Specify the AWS Elastic Beanstalk environment by ID.
    environmentId :: Core.Maybe Types.EnvironmentId,
    -- | Specify the AWS Elastic Beanstalk environment by name.
    environmentName :: Core.Maybe Types.EnvironmentName,
    -- | Specify the pagination token returned by a previous call.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstancesHealth' value with any optional fields omitted.
mkDescribeInstancesHealth ::
  DescribeInstancesHealth
mkDescribeInstancesHealth =
  DescribeInstancesHealth'
    { attributeNames = Core.Nothing,
      environmentId = Core.Nothing,
      environmentName = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Specifies the response elements you wish to receive. To retrieve all attributes, set to @All@ . If no attribute names are specified, returns a list of instances.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihAttributeNames :: Lens.Lens' DescribeInstancesHealth (Core.Maybe [Types.InstancesHealthAttribute])
dihAttributeNames = Lens.field @"attributeNames"
{-# DEPRECATED dihAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

-- | Specify the AWS Elastic Beanstalk environment by ID.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihEnvironmentId :: Lens.Lens' DescribeInstancesHealth (Core.Maybe Types.EnvironmentId)
dihEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED dihEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | Specify the AWS Elastic Beanstalk environment by name.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihEnvironmentName :: Lens.Lens' DescribeInstancesHealth (Core.Maybe Types.EnvironmentName)
dihEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED dihEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | Specify the pagination token returned by a previous call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihNextToken :: Lens.Lens' DescribeInstancesHealth (Core.Maybe Types.NextToken)
dihNextToken = Lens.field @"nextToken"
{-# DEPRECATED dihNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeInstancesHealth where
  type Rs DescribeInstancesHealth = DescribeInstancesHealthResponse
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
            ( Core.pure ("Action", "DescribeInstancesHealth")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> ( Core.toQueryValue
                            "AttributeNames"
                            (Core.toQueryList "member" Core.<$> attributeNames)
                        )
                Core.<> (Core.toQueryValue "EnvironmentId" Core.<$> environmentId)
                Core.<> (Core.toQueryValue "EnvironmentName" Core.<$> environmentName)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeInstancesHealthResult"
      ( \s h x ->
          DescribeInstancesHealthResponse'
            Core.<$> ( x Core..@? "InstanceHealthList"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (x Core..@? "RefreshedAt")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Detailed health information about the Amazon EC2 instances in an AWS Elastic Beanstalk environment.
--
-- /See:/ 'mkDescribeInstancesHealthResponse' smart constructor.
data DescribeInstancesHealthResponse = DescribeInstancesHealthResponse'
  { -- | Detailed health information about each instance.
    --
    -- The output differs slightly between Linux and Windows environments. There is a difference in the members that are supported under the @<CPUUtilization>@ type.
    instanceHealthList :: Core.Maybe [Types.SingleInstanceHealth],
    -- | Pagination token for the next page of results, if available.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The date and time that the health information was retrieved.
    refreshedAt :: Core.Maybe Core.UTCTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeInstancesHealthResponse' value with any optional fields omitted.
mkDescribeInstancesHealthResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInstancesHealthResponse
mkDescribeInstancesHealthResponse responseStatus =
  DescribeInstancesHealthResponse'
    { instanceHealthList =
        Core.Nothing,
      nextToken = Core.Nothing,
      refreshedAt = Core.Nothing,
      responseStatus
    }

-- | Detailed health information about each instance.
--
-- The output differs slightly between Linux and Windows environments. There is a difference in the members that are supported under the @<CPUUtilization>@ type.
--
-- /Note:/ Consider using 'instanceHealthList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrrsInstanceHealthList :: Lens.Lens' DescribeInstancesHealthResponse (Core.Maybe [Types.SingleInstanceHealth])
dihrrsInstanceHealthList = Lens.field @"instanceHealthList"
{-# DEPRECATED dihrrsInstanceHealthList "Use generic-lens or generic-optics with 'instanceHealthList' instead." #-}

-- | Pagination token for the next page of results, if available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrrsNextToken :: Lens.Lens' DescribeInstancesHealthResponse (Core.Maybe Types.NextToken)
dihrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dihrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The date and time that the health information was retrieved.
--
-- /Note:/ Consider using 'refreshedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrrsRefreshedAt :: Lens.Lens' DescribeInstancesHealthResponse (Core.Maybe Core.UTCTime)
dihrrsRefreshedAt = Lens.field @"refreshedAt"
{-# DEPRECATED dihrrsRefreshedAt "Use generic-lens or generic-optics with 'refreshedAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihrrsResponseStatus :: Lens.Lens' DescribeInstancesHealthResponse Core.Int
dihrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dihrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
