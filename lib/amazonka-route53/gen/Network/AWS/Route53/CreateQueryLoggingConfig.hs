{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateQueryLoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration for DNS query logging. After you create a query logging configuration, Amazon Route 53 begins to publish log data to an Amazon CloudWatch Logs log group.
--
-- DNS query logs contain information about the queries that Route 53 receives for a specified public hosted zone, such as the following:
--
--     * Route 53 edge location that responded to the DNS query
--
--
--     * Domain or subdomain that was requested
--
--
--     * DNS record type, such as A or AAAA
--
--
--     * DNS response code, such as @NoError@ or @ServFail@
--
--
--
--     * Log Group and Resource Policy
--
--     * Before you create a query logging configuration, perform the following operations.
--
--     * Create a CloudWatch Logs log group, and make note of the ARN, which you specify when you create a query logging configuration. Note the following:
--
--     * You must create the log group in the us-east-1 region.
--
--
--     * You must use the same AWS account to create the log group and the hosted zone that you want to configure query logging for.
--
--
--     * When you create log groups for query logging, we recommend that you use a consistent prefix, for example:
-- @/aws/route53//hosted zone name/ @
-- In the next step, you'll create a resource policy, which controls access to one or more log groups and the associated AWS resources, such as Route 53 hosted zones. There's a limit on the number of resource policies that you can create, so we recommend that you use a consistent prefix so you can use the same resource policy for all the log groups that you create for query logging.
--
--
--
--
--     * Create a CloudWatch Logs resource policy, and give it the permissions that Route 53 needs to create log streams and to send query logs to log streams. For the value of @Resource@ , specify the ARN for the log group that you created in the previous step. To use the same resource policy for all the CloudWatch Logs log groups that you created for query logging configurations, replace the hosted zone name with @*@ , for example:
-- @arn:aws:logs:us-east-1:123412341234:log-group:/aws/route53/*@
--
--
--
--
--     * Log Streams and Edge Locations
--
--     * When Route 53 finishes creating the configuration for DNS query logging, it does the following:
--
--     * Creates a log stream for an edge location the first time that the edge location responds to DNS queries for the specified hosted zone. That log stream is used to log all queries that Route 53 responds to for that edge location.
--
--
--     * Begins to send query logs to the applicable log stream.
--
--
-- The name of each log stream is in the following format:
-- @/hosted zone ID/ //edge location code/ @
-- The edge location code is a three-letter code and an arbitrarily assigned number, for example, DFW3. The three-letter code typically corresponds with the International Air Transport Association airport code for an airport near the edge location. (These abbreviations might change in the future.) For a list of edge locations, see "The Route 53 Global Network" on the <http://aws.amazon.com/route53/details/ Route 53 Product Details> page.
--
--
--     * Queries That Are Logged
--
--     * Query logs contain only the queries that DNS resolvers forward to Route 53. If a DNS resolver has already cached the response to a query (such as the IP address for a load balancer for example.com), the resolver will continue to return the cached response. It doesn't forward another query to Route 53 until the TTL for the corresponding resource record set expires. Depending on how many DNS queries are submitted for a resource record set, and depending on the TTL for that resource record set, query logs might contain information about only one query out of every several thousand queries that are submitted to DNS. For more information about how DNS works, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/welcome-dns-service.html Routing Internet Traffic to Your Website or Web Application> in the /Amazon Route 53 Developer Guide/ .
--
--
--     * Log File Format
--
--     * For a list of the values in each query log and the format of each value, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html Logging DNS Queries> in the /Amazon Route 53 Developer Guide/ .
--
--
--     * Pricing
--
--     * For information about charges for query logs, see <http://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing> .
--
--
--     * How to Stop Logging
--
--     * If you want Route 53 to stop sending query logs to CloudWatch Logs, delete the query logging configuration. For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteQueryLoggingConfig.html DeleteQueryLoggingConfig> .
module Network.AWS.Route53.CreateQueryLoggingConfig
  ( -- * Creating a request
    CreateQueryLoggingConfig (..),
    mkCreateQueryLoggingConfig,

    -- ** Request lenses
    cqlcHostedZoneId,
    cqlcCloudWatchLogsLogGroupArn,

    -- * Destructuring the response
    CreateQueryLoggingConfigResponse (..),
    mkCreateQueryLoggingConfigResponse,

    -- ** Response lenses
    cqlcrrsQueryLoggingConfig,
    cqlcrrsLocation,
    cqlcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | /See:/ 'mkCreateQueryLoggingConfig' smart constructor.
data CreateQueryLoggingConfig = CreateQueryLoggingConfig'
  { -- | The ID of the hosted zone that you want to log queries for. You can log queries only for public hosted zones.
    hostedZoneId :: Types.ResourceId,
    -- | The Amazon Resource Name (ARN) for the log group that you want to Amazon Route 53 to send query logs to. This is the format of the ARN:
    --
    -- arn:aws:logs:/region/ :/account-id/ :log-group:/log_group_name/
    -- To get the ARN for a log group, you can use the CloudWatch console, the <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogGroups.html DescribeLogGroups> API action, the <https://docs.aws.amazon.com/cli/latest/reference/logs/describe-log-groups.html describe-log-groups> command, or the applicable command in one of the AWS SDKs.
    cloudWatchLogsLogGroupArn :: Types.CloudWatchLogsLogGroupArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateQueryLoggingConfig' value with any optional fields omitted.
mkCreateQueryLoggingConfig ::
  -- | 'hostedZoneId'
  Types.ResourceId ->
  -- | 'cloudWatchLogsLogGroupArn'
  Types.CloudWatchLogsLogGroupArn ->
  CreateQueryLoggingConfig
mkCreateQueryLoggingConfig hostedZoneId cloudWatchLogsLogGroupArn =
  CreateQueryLoggingConfig'
    { hostedZoneId,
      cloudWatchLogsLogGroupArn
    }

-- | The ID of the hosted zone that you want to log queries for. You can log queries only for public hosted zones.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqlcHostedZoneId :: Lens.Lens' CreateQueryLoggingConfig Types.ResourceId
cqlcHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED cqlcHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The Amazon Resource Name (ARN) for the log group that you want to Amazon Route 53 to send query logs to. This is the format of the ARN:
--
-- arn:aws:logs:/region/ :/account-id/ :log-group:/log_group_name/
-- To get the ARN for a log group, you can use the CloudWatch console, the <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogGroups.html DescribeLogGroups> API action, the <https://docs.aws.amazon.com/cli/latest/reference/logs/describe-log-groups.html describe-log-groups> command, or the applicable command in one of the AWS SDKs.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqlcCloudWatchLogsLogGroupArn :: Lens.Lens' CreateQueryLoggingConfig Types.CloudWatchLogsLogGroupArn
cqlcCloudWatchLogsLogGroupArn = Lens.field @"cloudWatchLogsLogGroupArn"
{-# DEPRECATED cqlcCloudWatchLogsLogGroupArn "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupArn' instead." #-}

instance Core.ToXML CreateQueryLoggingConfig where
  toXML CreateQueryLoggingConfig {..} =
    Core.toXMLNode "HostedZoneId" hostedZoneId
      Core.<> Core.toXMLNode
        "CloudWatchLogsLogGroupArn"
        cloudWatchLogsLogGroupArn
  toXMLDocument =
    Core.mkXMLElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateQueryLoggingConfigRequest"

instance Core.AWSRequest CreateQueryLoggingConfig where
  type Rs CreateQueryLoggingConfig = CreateQueryLoggingConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2013-04-01/queryloggingconfig",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateQueryLoggingConfigResponse'
            Core.<$> (x Core..@ "QueryLoggingConfig")
            Core.<*> (Core.parseHeader "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateQueryLoggingConfigResponse' smart constructor.
data CreateQueryLoggingConfigResponse = CreateQueryLoggingConfigResponse'
  { -- | A complex type that contains the ID for a query logging configuration, the ID of the hosted zone that you want to log queries for, and the ARN for the log group that you want Amazon Route 53 to send query logs to.
    queryLoggingConfig :: Types.QueryLoggingConfig,
    -- | The unique URL representing the new query logging configuration.
    location :: Types.ResourceURI,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateQueryLoggingConfigResponse' value with any optional fields omitted.
mkCreateQueryLoggingConfigResponse ::
  -- | 'queryLoggingConfig'
  Types.QueryLoggingConfig ->
  -- | 'location'
  Types.ResourceURI ->
  -- | 'responseStatus'
  Core.Int ->
  CreateQueryLoggingConfigResponse
mkCreateQueryLoggingConfigResponse
  queryLoggingConfig
  location
  responseStatus =
    CreateQueryLoggingConfigResponse'
      { queryLoggingConfig,
        location,
        responseStatus
      }

-- | A complex type that contains the ID for a query logging configuration, the ID of the hosted zone that you want to log queries for, and the ARN for the log group that you want Amazon Route 53 to send query logs to.
--
-- /Note:/ Consider using 'queryLoggingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqlcrrsQueryLoggingConfig :: Lens.Lens' CreateQueryLoggingConfigResponse Types.QueryLoggingConfig
cqlcrrsQueryLoggingConfig = Lens.field @"queryLoggingConfig"
{-# DEPRECATED cqlcrrsQueryLoggingConfig "Use generic-lens or generic-optics with 'queryLoggingConfig' instead." #-}

-- | The unique URL representing the new query logging configuration.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqlcrrsLocation :: Lens.Lens' CreateQueryLoggingConfigResponse Types.ResourceURI
cqlcrrsLocation = Lens.field @"location"
{-# DEPRECATED cqlcrrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqlcrrsResponseStatus :: Lens.Lens' CreateQueryLoggingConfigResponse Core.Int
cqlcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cqlcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
