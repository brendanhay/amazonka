{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    chcrrsHealthCheck,
    chcrrsLocation,
    chcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains the health check request information.
--
-- /See:/ 'mkCreateHealthCheck' smart constructor.
data CreateHealthCheck = CreateHealthCheck'
  { -- | A unique string that identifies the request and that allows you to retry a failed @CreateHealthCheck@ request without the risk of creating two identical health checks:
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
    callerReference :: Types.CallerReference,
    -- | A complex type that contains settings for a new health check.
    healthCheckConfig :: Types.HealthCheckConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHealthCheck' value with any optional fields omitted.
mkCreateHealthCheck ::
  -- | 'callerReference'
  Types.CallerReference ->
  -- | 'healthCheckConfig'
  Types.HealthCheckConfig ->
  CreateHealthCheck
mkCreateHealthCheck callerReference healthCheckConfig =
  CreateHealthCheck' {callerReference, healthCheckConfig}

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
chcCallerReference :: Lens.Lens' CreateHealthCheck Types.CallerReference
chcCallerReference = Lens.field @"callerReference"
{-# DEPRECATED chcCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | A complex type that contains settings for a new health check.
--
-- /Note:/ Consider using 'healthCheckConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcHealthCheckConfig :: Lens.Lens' CreateHealthCheck Types.HealthCheckConfig
chcHealthCheckConfig = Lens.field @"healthCheckConfig"
{-# DEPRECATED chcHealthCheckConfig "Use generic-lens or generic-optics with 'healthCheckConfig' instead." #-}

instance Core.ToXML CreateHealthCheck where
  toXML CreateHealthCheck {..} =
    Core.toXMLNode "CallerReference" callerReference
      Core.<> Core.toXMLNode "HealthCheckConfig" healthCheckConfig
  toXMLDocument =
    Core.mkXMLElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateHealthCheckRequest"

instance Core.AWSRequest CreateHealthCheck where
  type Rs CreateHealthCheck = CreateHealthCheckResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2013-04-01/healthcheck",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateHealthCheckResponse'
            Core.<$> (x Core..@ "HealthCheck")
            Core.<*> (Core.parseHeader "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type containing the response information for the new health check.
--
-- /See:/ 'mkCreateHealthCheckResponse' smart constructor.
data CreateHealthCheckResponse = CreateHealthCheckResponse'
  { -- | A complex type that contains identifying information about the health check.
    healthCheck :: Types.HealthCheck,
    -- | The unique URL representing the new health check.
    location :: Types.ResourceURI,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHealthCheckResponse' value with any optional fields omitted.
mkCreateHealthCheckResponse ::
  -- | 'healthCheck'
  Types.HealthCheck ->
  -- | 'location'
  Types.ResourceURI ->
  -- | 'responseStatus'
  Core.Int ->
  CreateHealthCheckResponse
mkCreateHealthCheckResponse healthCheck location responseStatus =
  CreateHealthCheckResponse' {healthCheck, location, responseStatus}

-- | A complex type that contains identifying information about the health check.
--
-- /Note:/ Consider using 'healthCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrrsHealthCheck :: Lens.Lens' CreateHealthCheckResponse Types.HealthCheck
chcrrsHealthCheck = Lens.field @"healthCheck"
{-# DEPRECATED chcrrsHealthCheck "Use generic-lens or generic-optics with 'healthCheck' instead." #-}

-- | The unique URL representing the new health check.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrrsLocation :: Lens.Lens' CreateHealthCheckResponse Types.ResourceURI
chcrrsLocation = Lens.field @"location"
{-# DEPRECATED chcrrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chcrrsResponseStatus :: Lens.Lens' CreateHealthCheckResponse Core.Int
chcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED chcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
