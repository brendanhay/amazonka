{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SingleInstanceHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SingleInstanceHealth
  ( SingleInstanceHealth (..),

    -- * Smart constructor
    mkSingleInstanceHealth,

    -- * Lenses
    sihInstanceId,
    sihCauses,
    sihSystem,
    sihApplicationMetrics,
    sihColor,
    sihInstanceType,
    sihAvailabilityZone,
    sihHealthStatus,
    sihDeployment,
    sihLaunchedAt,
  )
where

import Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics
import Network.AWS.ElasticBeanstalk.Types.Deployment
import Network.AWS.ElasticBeanstalk.Types.SystemStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Detailed health information about an Amazon EC2 instance in your Elastic Beanstalk environment.
--
-- /See:/ 'mkSingleInstanceHealth' smart constructor.
data SingleInstanceHealth = SingleInstanceHealth'
  { instanceId ::
      Lude.Maybe Lude.Text,
    causes :: Lude.Maybe [Lude.Text],
    system :: Lude.Maybe SystemStatus,
    applicationMetrics ::
      Lude.Maybe ApplicationMetrics,
    color :: Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    healthStatus :: Lude.Maybe Lude.Text,
    deployment :: Lude.Maybe Deployment,
    launchedAt :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SingleInstanceHealth' with the minimum fields required to make a request.
--
-- * 'applicationMetrics' - Request metrics from your application.
-- * 'availabilityZone' - The availability zone in which the instance runs.
-- * 'causes' - Represents the causes, which provide more information about the current health status.
-- * 'color' - Represents the color indicator that gives you information about the health of the EC2 instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
-- * 'deployment' - Information about the most recent deployment to an instance.
-- * 'healthStatus' - Returns the health status of the specified instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
-- * 'instanceId' - The ID of the Amazon EC2 instance.
-- * 'instanceType' - The instance's type.
-- * 'launchedAt' - The time at which the EC2 instance was launched.
-- * 'system' - Operating system metrics from the instance.
mkSingleInstanceHealth ::
  SingleInstanceHealth
mkSingleInstanceHealth =
  SingleInstanceHealth'
    { instanceId = Lude.Nothing,
      causes = Lude.Nothing,
      system = Lude.Nothing,
      applicationMetrics = Lude.Nothing,
      color = Lude.Nothing,
      instanceType = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      healthStatus = Lude.Nothing,
      deployment = Lude.Nothing,
      launchedAt = Lude.Nothing
    }

-- | The ID of the Amazon EC2 instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihInstanceId :: Lens.Lens' SingleInstanceHealth (Lude.Maybe Lude.Text)
sihInstanceId = Lens.lens (instanceId :: SingleInstanceHealth -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: SingleInstanceHealth)
{-# DEPRECATED sihInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Represents the causes, which provide more information about the current health status.
--
-- /Note:/ Consider using 'causes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihCauses :: Lens.Lens' SingleInstanceHealth (Lude.Maybe [Lude.Text])
sihCauses = Lens.lens (causes :: SingleInstanceHealth -> Lude.Maybe [Lude.Text]) (\s a -> s {causes = a} :: SingleInstanceHealth)
{-# DEPRECATED sihCauses "Use generic-lens or generic-optics with 'causes' instead." #-}

-- | Operating system metrics from the instance.
--
-- /Note:/ Consider using 'system' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihSystem :: Lens.Lens' SingleInstanceHealth (Lude.Maybe SystemStatus)
sihSystem = Lens.lens (system :: SingleInstanceHealth -> Lude.Maybe SystemStatus) (\s a -> s {system = a} :: SingleInstanceHealth)
{-# DEPRECATED sihSystem "Use generic-lens or generic-optics with 'system' instead." #-}

-- | Request metrics from your application.
--
-- /Note:/ Consider using 'applicationMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihApplicationMetrics :: Lens.Lens' SingleInstanceHealth (Lude.Maybe ApplicationMetrics)
sihApplicationMetrics = Lens.lens (applicationMetrics :: SingleInstanceHealth -> Lude.Maybe ApplicationMetrics) (\s a -> s {applicationMetrics = a} :: SingleInstanceHealth)
{-# DEPRECATED sihApplicationMetrics "Use generic-lens or generic-optics with 'applicationMetrics' instead." #-}

-- | Represents the color indicator that gives you information about the health of the EC2 instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- /Note:/ Consider using 'color' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihColor :: Lens.Lens' SingleInstanceHealth (Lude.Maybe Lude.Text)
sihColor = Lens.lens (color :: SingleInstanceHealth -> Lude.Maybe Lude.Text) (\s a -> s {color = a} :: SingleInstanceHealth)
{-# DEPRECATED sihColor "Use generic-lens or generic-optics with 'color' instead." #-}

-- | The instance's type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihInstanceType :: Lens.Lens' SingleInstanceHealth (Lude.Maybe Lude.Text)
sihInstanceType = Lens.lens (instanceType :: SingleInstanceHealth -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: SingleInstanceHealth)
{-# DEPRECATED sihInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The availability zone in which the instance runs.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihAvailabilityZone :: Lens.Lens' SingleInstanceHealth (Lude.Maybe Lude.Text)
sihAvailabilityZone = Lens.lens (availabilityZone :: SingleInstanceHealth -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: SingleInstanceHealth)
{-# DEPRECATED sihAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Returns the health status of the specified instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihHealthStatus :: Lens.Lens' SingleInstanceHealth (Lude.Maybe Lude.Text)
sihHealthStatus = Lens.lens (healthStatus :: SingleInstanceHealth -> Lude.Maybe Lude.Text) (\s a -> s {healthStatus = a} :: SingleInstanceHealth)
{-# DEPRECATED sihHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

-- | Information about the most recent deployment to an instance.
--
-- /Note:/ Consider using 'deployment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihDeployment :: Lens.Lens' SingleInstanceHealth (Lude.Maybe Deployment)
sihDeployment = Lens.lens (deployment :: SingleInstanceHealth -> Lude.Maybe Deployment) (\s a -> s {deployment = a} :: SingleInstanceHealth)
{-# DEPRECATED sihDeployment "Use generic-lens or generic-optics with 'deployment' instead." #-}

-- | The time at which the EC2 instance was launched.
--
-- /Note:/ Consider using 'launchedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihLaunchedAt :: Lens.Lens' SingleInstanceHealth (Lude.Maybe Lude.DateTime)
sihLaunchedAt = Lens.lens (launchedAt :: SingleInstanceHealth -> Lude.Maybe Lude.DateTime) (\s a -> s {launchedAt = a} :: SingleInstanceHealth)
{-# DEPRECATED sihLaunchedAt "Use generic-lens or generic-optics with 'launchedAt' instead." #-}

instance Lude.FromXML SingleInstanceHealth where
  parseXML x =
    SingleInstanceHealth'
      Lude.<$> (x Lude..@? "InstanceId")
      Lude.<*> ( x Lude..@? "Causes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "System")
      Lude.<*> (x Lude..@? "ApplicationMetrics")
      Lude.<*> (x Lude..@? "Color")
      Lude.<*> (x Lude..@? "InstanceType")
      Lude.<*> (x Lude..@? "AvailabilityZone")
      Lude.<*> (x Lude..@? "HealthStatus")
      Lude.<*> (x Lude..@? "Deployment")
      Lude.<*> (x Lude..@? "LaunchedAt")
