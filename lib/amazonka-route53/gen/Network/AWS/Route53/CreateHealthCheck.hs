{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateHealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new health check.
--
-- For information about adding health checks to resource record sets, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ResourceRecordSet.html#Route53-Type-ResourceRecordSet-HealthCheckId HealthCheckId> in <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html ChangeResourceRecordSets> .
-- __ELB Load Balancers__
-- If you're registering EC2 instances with an Elastic Load Balancing (ELB) load balancer, do not create Amazon Route 53 health checks for the EC2 instances. When you register an EC2 instance with a load balancer, you configure settings for an ELB health check, which performs a similar function to a Route 53 health check.
-- __Private Hosted Zones__
-- You can associate health checks with failover resource record sets in a private hosted zone. Note the following:
--
--     * Route 53 health checkers are outside the VPC. To check the health of an endpoint within a VPC by IP address, you must assign a public IP address to the instance in the VPC.
--
--
--     * You can configure a health checker to check the health of an external resource that the instance relies on, such as a database server.
--
--
--     * You can create a CloudWatch metric, associate an alarm with the metric, and then create a health check that is based on the state of the alarm. For example, you might create a CloudWatch metric that checks the status of the Amazon EC2 @StatusCheckFailed@ metric, add an alarm to the metric, and then create a health check that is based on the state of the alarm. For information about creating CloudWatch metrics and alarms by using the CloudWatch console, see the <https://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/WhatIsCloudWatch.html Amazon CloudWatch User Guide> .
module Network.AWS.Route53.CreateHealthCheck
  ( -- * Creating a request
    CreateHealthCheck (..),
    mkCreateHealthCheck,

    -- ** Request lenses
    chcCallerReference,
    chcHealthCheckConfig,

    -- * Destructuring the response
    CreateHealthCheckResponse (..),
    mkCreateHealthCheckResponse,

    -- ** Response lenses
    chcrsResponseStatus,
    chcrsHealthCheck,
    chcrsLocation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains the health check request information.
--
-- /See:/ 'mkCreateHealthCheck' smart constructor.
data CreateHealthCheck = CreateHealthCheck'
  { callerReference ::
      Lude.Text,
    healthCheckConfig :: HealthCheckConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHealthCheck' with the minimum fields required to make a request.
--
-- * 'callerReference' - A unique string that identifies the request and that allows you to retry a failed @CreateHealthCheck@ request without the risk of creating two identical health checks:
--
--
--     * If you send a @CreateHealthCheck@ request with the same @CallerReference@ and settings as a previous request, and if the health check doesn't exist, Amazon Route 53 creates the health check. If the health check does exist, Route 53 returns the settings for the existing health check.
--
--
--     * If you send a @CreateHealthCheck@ request with the same @CallerReference@ as a deleted health check, regardless of the settings, Route 53 returns a @HealthCheckAlreadyExists@ error.
--
--
--     * If you send a @CreateHealthCheck@ request with the same @CallerReference@ as an existing health check but with different settings, Route 53 returns a @HealthCheckAlreadyExists@ error.
--
--
--     * If you send a @CreateHealthCheck@ request with a unique @CallerReference@ but settings identical to an existing health check, Route 53 creates the health check.
--
--
-- * 'healthCheckConfig' - A complex type that contains settings for a new health check.
mkCreateHealthCheck ::
  -- | 'callerReference'
  Lude.Text ->
  -- | 'healthCheckConfig'
  HealthCheckConfig ->
  CreateHealthCheck
mkCreateHealthCheck pCallerReference_ pHealthCheckConfig_ =
  CreateHealthCheck'
    { callerReference = pCallerReference_,
      healthCheckConfig = pHealthCheckConfig_
    }

-- | A unique string that identifies the request and that allows you to retry a failed @CreateHealthCheck@ request without the risk of creating two identical health checks:
--
--
--     * If you send a @CreateHealthCheck@ request with the same @CallerReference@ and settings as a previous request, and if the health check doesn't exist, Amazon Route 53 creates the health check. If the health check does exist, Route 53 returns the settings for the existing health check.
--
--
--     * If you send a @CreateHealthCheck@ request with the same @CallerReference@ as a deleted health check, regardless of the settings, Route 53 returns a @HealthCheckAlreadyExists@ error.
--
--
--     * If you send a @CreateHealthCheck@ request with the same @CallerReference@ as an existing health check but with different settings, Route 53 returns a @HealthCheckAlreadyExists@ error.
--
--
--     * If you send a @CreateHealthCheck@ request with a unique @CallerReference@ but settings identical to an existing health check, Route 53 creates the health check.
--
--
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcCallerReference :: Lens.Lens' CreateHealthCheck Lude.Text
chcCallerReference = Lens.lens (callerReference :: CreateHealthCheck -> Lude.Text) (\s a -> s {callerReference = a} :: CreateHealthCheck)
{-# DEPRECATED chcCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | A complex type that contains settings for a new health check.
--
-- /Note:/ Consider using 'healthCheckConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHealthCheckConfig :: Lens.Lens' CreateHealthCheck HealthCheckConfig
chcHealthCheckConfig = Lens.lens (healthCheckConfig :: CreateHealthCheck -> HealthCheckConfig) (\s a -> s {healthCheckConfig = a} :: CreateHealthCheck)
{-# DEPRECATED chcHealthCheckConfig "Use generic-lens or generic-optics with 'healthCheckConfig' instead." #-}

instance Lude.AWSRequest CreateHealthCheck where
  type Rs CreateHealthCheck = CreateHealthCheckResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateHealthCheckResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "HealthCheck")
            Lude.<*> (h Lude..# "Location")
      )

instance Lude.ToElement CreateHealthCheck where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateHealthCheckRequest"

instance Lude.ToHeaders CreateHealthCheck where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateHealthCheck where
  toPath = Lude.const "/2013-04-01/healthcheck"

instance Lude.ToQuery CreateHealthCheck where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML CreateHealthCheck where
  toXML CreateHealthCheck' {..} =
    Lude.mconcat
      [ "CallerReference" Lude.@= callerReference,
        "HealthCheckConfig" Lude.@= healthCheckConfig
      ]

-- | A complex type containing the response information for the new health check.
--
-- /See:/ 'mkCreateHealthCheckResponse' smart constructor.
data CreateHealthCheckResponse = CreateHealthCheckResponse'
  { responseStatus ::
      Lude.Int,
    healthCheck :: HealthCheck,
    location :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHealthCheckResponse' with the minimum fields required to make a request.
--
-- * 'healthCheck' - A complex type that contains identifying information about the health check.
-- * 'location' - The unique URL representing the new health check.
-- * 'responseStatus' - The response status code.
mkCreateHealthCheckResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'healthCheck'
  HealthCheck ->
  -- | 'location'
  Lude.Text ->
  CreateHealthCheckResponse
mkCreateHealthCheckResponse
  pResponseStatus_
  pHealthCheck_
  pLocation_ =
    CreateHealthCheckResponse'
      { responseStatus = pResponseStatus_,
        healthCheck = pHealthCheck_,
        location = pLocation_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrsResponseStatus :: Lens.Lens' CreateHealthCheckResponse Lude.Int
chcrsResponseStatus = Lens.lens (responseStatus :: CreateHealthCheckResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateHealthCheckResponse)
{-# DEPRECATED chcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A complex type that contains identifying information about the health check.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrsHealthCheck :: Lens.Lens' CreateHealthCheckResponse HealthCheck
chcrsHealthCheck = Lens.lens (healthCheck :: CreateHealthCheckResponse -> HealthCheck) (\s a -> s {healthCheck = a} :: CreateHealthCheckResponse)
{-# DEPRECATED chcrsHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}

-- | The unique URL representing the new health check.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrsLocation :: Lens.Lens' CreateHealthCheckResponse Lude.Text
chcrsLocation = Lens.lens (location :: CreateHealthCheckResponse -> Lude.Text) (\s a -> s {location = a} :: CreateHealthCheckResponse)
{-# DEPRECATED chcrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}
