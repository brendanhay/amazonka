{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dehEnvironmentName,
    dehAttributeNames,
    dehEnvironmentId,

    -- * Destructuring the response
    DescribeEnvironmentHealthResponse (..),
    mkDescribeEnvironmentHealthResponse,

    -- ** Response lenses
    dehrsStatus,
    dehrsCauses,
    dehrsApplicationMetrics,
    dehrsColor,
    dehrsEnvironmentName,
    dehrsHealthStatus,
    dehrsInstancesHealth,
    dehrsRefreshedAt,
    dehrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | See the example below to learn how to create a request body.
--
-- /See:/ 'mkDescribeEnvironmentHealth' smart constructor.
data DescribeEnvironmentHealth = DescribeEnvironmentHealth'
  { environmentName ::
      Lude.Maybe Lude.Text,
    attributeNames ::
      Lude.Maybe [EnvironmentHealthAttribute],
    environmentId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironmentHealth' with the minimum fields required to make a request.
--
-- * 'attributeNames' - Specify the response elements to return. To retrieve all attributes, set to @All@ . If no attribute names are specified, returns the name of the environment.
-- * 'environmentId' - Specify the environment by ID.
--
-- You must specify either this or an EnvironmentName, or both.
-- * 'environmentName' - Specify the environment by name.
--
-- You must specify either this or an EnvironmentName, or both.
mkDescribeEnvironmentHealth ::
  DescribeEnvironmentHealth
mkDescribeEnvironmentHealth =
  DescribeEnvironmentHealth'
    { environmentName = Lude.Nothing,
      attributeNames = Lude.Nothing,
      environmentId = Lude.Nothing
    }

-- | Specify the environment by name.
--
-- You must specify either this or an EnvironmentName, or both.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehEnvironmentName :: Lens.Lens' DescribeEnvironmentHealth (Lude.Maybe Lude.Text)
dehEnvironmentName = Lens.lens (environmentName :: DescribeEnvironmentHealth -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: DescribeEnvironmentHealth)
{-# DEPRECATED dehEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | Specify the response elements to return. To retrieve all attributes, set to @All@ . If no attribute names are specified, returns the name of the environment.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehAttributeNames :: Lens.Lens' DescribeEnvironmentHealth (Lude.Maybe [EnvironmentHealthAttribute])
dehAttributeNames = Lens.lens (attributeNames :: DescribeEnvironmentHealth -> Lude.Maybe [EnvironmentHealthAttribute]) (\s a -> s {attributeNames = a} :: DescribeEnvironmentHealth)
{-# DEPRECATED dehAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

-- | Specify the environment by ID.
--
-- You must specify either this or an EnvironmentName, or both.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehEnvironmentId :: Lens.Lens' DescribeEnvironmentHealth (Lude.Maybe Lude.Text)
dehEnvironmentId = Lens.lens (environmentId :: DescribeEnvironmentHealth -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: DescribeEnvironmentHealth)
{-# DEPRECATED dehEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Lude.AWSRequest DescribeEnvironmentHealth where
  type
    Rs DescribeEnvironmentHealth =
      DescribeEnvironmentHealthResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DescribeEnvironmentHealthResult"
      ( \s h x ->
          DescribeEnvironmentHealthResponse'
            Lude.<$> (x Lude..@? "Status")
            Lude.<*> ( x Lude..@? "Causes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "ApplicationMetrics")
            Lude.<*> (x Lude..@? "Color")
            Lude.<*> (x Lude..@? "EnvironmentName")
            Lude.<*> (x Lude..@? "HealthStatus")
            Lude.<*> (x Lude..@? "InstancesHealth")
            Lude.<*> (x Lude..@? "RefreshedAt")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEnvironmentHealth where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEnvironmentHealth where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEnvironmentHealth where
  toQuery DescribeEnvironmentHealth' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeEnvironmentHealth" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EnvironmentName" Lude.=: environmentName,
        "AttributeNames"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> attributeNames),
        "EnvironmentId" Lude.=: environmentId
      ]

-- | Health details for an AWS Elastic Beanstalk environment.
--
-- /See:/ 'mkDescribeEnvironmentHealthResponse' smart constructor.
data DescribeEnvironmentHealthResponse = DescribeEnvironmentHealthResponse'
  { status ::
      Lude.Maybe
        EnvironmentHealth,
    causes ::
      Lude.Maybe [Lude.Text],
    applicationMetrics ::
      Lude.Maybe
        ApplicationMetrics,
    color ::
      Lude.Maybe Lude.Text,
    environmentName ::
      Lude.Maybe Lude.Text,
    healthStatus ::
      Lude.Maybe Lude.Text,
    instancesHealth ::
      Lude.Maybe
        InstanceHealthSummary,
    refreshedAt ::
      Lude.Maybe
        Lude.DateTime,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEnvironmentHealthResponse' with the minimum fields required to make a request.
--
-- * 'applicationMetrics' - Application request metrics for the environment.
-- * 'causes' - Descriptions of the data that contributed to the environment's current health status.
-- * 'color' - The <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health color> of the environment.
-- * 'environmentName' - The environment's name.
-- * 'healthStatus' - The <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health status> of the environment. For example, @Ok@ .
-- * 'instancesHealth' - Summary health information for the instances in the environment.
-- * 'refreshedAt' - The date and time that the health information was retrieved.
-- * 'responseStatus' - The response status code.
-- * 'status' - The environment's operational status. @Ready@ , @Launching@ , @Updating@ , @Terminating@ , or @Terminated@ .
mkDescribeEnvironmentHealthResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEnvironmentHealthResponse
mkDescribeEnvironmentHealthResponse pResponseStatus_ =
  DescribeEnvironmentHealthResponse'
    { status = Lude.Nothing,
      causes = Lude.Nothing,
      applicationMetrics = Lude.Nothing,
      color = Lude.Nothing,
      environmentName = Lude.Nothing,
      healthStatus = Lude.Nothing,
      instancesHealth = Lude.Nothing,
      refreshedAt = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The environment's operational status. @Ready@ , @Launching@ , @Updating@ , @Terminating@ , or @Terminated@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrsStatus :: Lens.Lens' DescribeEnvironmentHealthResponse (Lude.Maybe EnvironmentHealth)
dehrsStatus = Lens.lens (status :: DescribeEnvironmentHealthResponse -> Lude.Maybe EnvironmentHealth) (\s a -> s {status = a} :: DescribeEnvironmentHealthResponse)
{-# DEPRECATED dehrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Descriptions of the data that contributed to the environment's current health status.
--
-- /Note:/ Consider using 'causes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrsCauses :: Lens.Lens' DescribeEnvironmentHealthResponse (Lude.Maybe [Lude.Text])
dehrsCauses = Lens.lens (causes :: DescribeEnvironmentHealthResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {causes = a} :: DescribeEnvironmentHealthResponse)
{-# DEPRECATED dehrsCauses "Use generic-lens or generic-optics with 'causes' instead." #-}

-- | Application request metrics for the environment.
--
-- /Note:/ Consider using 'applicationMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrsApplicationMetrics :: Lens.Lens' DescribeEnvironmentHealthResponse (Lude.Maybe ApplicationMetrics)
dehrsApplicationMetrics = Lens.lens (applicationMetrics :: DescribeEnvironmentHealthResponse -> Lude.Maybe ApplicationMetrics) (\s a -> s {applicationMetrics = a} :: DescribeEnvironmentHealthResponse)
{-# DEPRECATED dehrsApplicationMetrics "Use generic-lens or generic-optics with 'applicationMetrics' instead." #-}

-- | The <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health color> of the environment.
--
-- /Note:/ Consider using 'color' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrsColor :: Lens.Lens' DescribeEnvironmentHealthResponse (Lude.Maybe Lude.Text)
dehrsColor = Lens.lens (color :: DescribeEnvironmentHealthResponse -> Lude.Maybe Lude.Text) (\s a -> s {color = a} :: DescribeEnvironmentHealthResponse)
{-# DEPRECATED dehrsColor "Use generic-lens or generic-optics with 'color' instead." #-}

-- | The environment's name.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrsEnvironmentName :: Lens.Lens' DescribeEnvironmentHealthResponse (Lude.Maybe Lude.Text)
dehrsEnvironmentName = Lens.lens (environmentName :: DescribeEnvironmentHealthResponse -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: DescribeEnvironmentHealthResponse)
{-# DEPRECATED dehrsEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health status> of the environment. For example, @Ok@ .
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrsHealthStatus :: Lens.Lens' DescribeEnvironmentHealthResponse (Lude.Maybe Lude.Text)
dehrsHealthStatus = Lens.lens (healthStatus :: DescribeEnvironmentHealthResponse -> Lude.Maybe Lude.Text) (\s a -> s {healthStatus = a} :: DescribeEnvironmentHealthResponse)
{-# DEPRECATED dehrsHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

-- | Summary health information for the instances in the environment.
--
-- /Note:/ Consider using 'instancesHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrsInstancesHealth :: Lens.Lens' DescribeEnvironmentHealthResponse (Lude.Maybe InstanceHealthSummary)
dehrsInstancesHealth = Lens.lens (instancesHealth :: DescribeEnvironmentHealthResponse -> Lude.Maybe InstanceHealthSummary) (\s a -> s {instancesHealth = a} :: DescribeEnvironmentHealthResponse)
{-# DEPRECATED dehrsInstancesHealth "Use generic-lens or generic-optics with 'instancesHealth' instead." #-}

-- | The date and time that the health information was retrieved.
--
-- /Note:/ Consider using 'refreshedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrsRefreshedAt :: Lens.Lens' DescribeEnvironmentHealthResponse (Lude.Maybe Lude.DateTime)
dehrsRefreshedAt = Lens.lens (refreshedAt :: DescribeEnvironmentHealthResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {refreshedAt = a} :: DescribeEnvironmentHealthResponse)
{-# DEPRECATED dehrsRefreshedAt "Use generic-lens or generic-optics with 'refreshedAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dehrsResponseStatus :: Lens.Lens' DescribeEnvironmentHealthResponse Lude.Int
dehrsResponseStatus = Lens.lens (responseStatus :: DescribeEnvironmentHealthResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEnvironmentHealthResponse)
{-# DEPRECATED dehrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
