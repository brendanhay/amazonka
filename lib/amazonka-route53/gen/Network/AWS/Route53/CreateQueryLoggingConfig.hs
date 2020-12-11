{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cqlcCloudWatchLogsLogGroupARN,

    -- * Destructuring the response
    CreateQueryLoggingConfigResponse (..),
    mkCreateQueryLoggingConfigResponse,

    -- ** Response lenses
    cqlcrsResponseStatus,
    cqlcrsQueryLoggingConfig,
    cqlcrsLocation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | /See:/ 'mkCreateQueryLoggingConfig' smart constructor.
data CreateQueryLoggingConfig = CreateQueryLoggingConfig'
  { hostedZoneId ::
      ResourceId,
    cloudWatchLogsLogGroupARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateQueryLoggingConfig' with the minimum fields required to make a request.
--
-- * 'cloudWatchLogsLogGroupARN' - The Amazon Resource Name (ARN) for the log group that you want to Amazon Route 53 to send query logs to. This is the format of the ARN:
--
-- arn:aws:logs:/region/ :/account-id/ :log-group:/log_group_name/
-- To get the ARN for a log group, you can use the CloudWatch console, the <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogGroups.html DescribeLogGroups> API action, the <https://docs.aws.amazon.com/cli/latest/reference/logs/describe-log-groups.html describe-log-groups> command, or the applicable command in one of the AWS SDKs.
-- * 'hostedZoneId' - The ID of the hosted zone that you want to log queries for. You can log queries only for public hosted zones.
mkCreateQueryLoggingConfig ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'cloudWatchLogsLogGroupARN'
  Lude.Text ->
  CreateQueryLoggingConfig
mkCreateQueryLoggingConfig
  pHostedZoneId_
  pCloudWatchLogsLogGroupARN_ =
    CreateQueryLoggingConfig'
      { hostedZoneId = pHostedZoneId_,
        cloudWatchLogsLogGroupARN = pCloudWatchLogsLogGroupARN_
      }

-- | The ID of the hosted zone that you want to log queries for. You can log queries only for public hosted zones.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqlcHostedZoneId :: Lens.Lens' CreateQueryLoggingConfig ResourceId
cqlcHostedZoneId = Lens.lens (hostedZoneId :: CreateQueryLoggingConfig -> ResourceId) (\s a -> s {hostedZoneId = a} :: CreateQueryLoggingConfig)
{-# DEPRECATED cqlcHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The Amazon Resource Name (ARN) for the log group that you want to Amazon Route 53 to send query logs to. This is the format of the ARN:
--
-- arn:aws:logs:/region/ :/account-id/ :log-group:/log_group_name/
-- To get the ARN for a log group, you can use the CloudWatch console, the <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogGroups.html DescribeLogGroups> API action, the <https://docs.aws.amazon.com/cli/latest/reference/logs/describe-log-groups.html describe-log-groups> command, or the applicable command in one of the AWS SDKs.
--
-- /Note:/ Consider using 'cloudWatchLogsLogGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqlcCloudWatchLogsLogGroupARN :: Lens.Lens' CreateQueryLoggingConfig Lude.Text
cqlcCloudWatchLogsLogGroupARN = Lens.lens (cloudWatchLogsLogGroupARN :: CreateQueryLoggingConfig -> Lude.Text) (\s a -> s {cloudWatchLogsLogGroupARN = a} :: CreateQueryLoggingConfig)
{-# DEPRECATED cqlcCloudWatchLogsLogGroupARN "Use generic-lens or generic-optics with 'cloudWatchLogsLogGroupARN' instead." #-}

instance Lude.AWSRequest CreateQueryLoggingConfig where
  type Rs CreateQueryLoggingConfig = CreateQueryLoggingConfigResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateQueryLoggingConfigResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "QueryLoggingConfig")
            Lude.<*> (h Lude..# "Location")
      )

instance Lude.ToElement CreateQueryLoggingConfig where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateQueryLoggingConfigRequest"

instance Lude.ToHeaders CreateQueryLoggingConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateQueryLoggingConfig where
  toPath = Lude.const "/2013-04-01/queryloggingconfig"

instance Lude.ToQuery CreateQueryLoggingConfig where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML CreateQueryLoggingConfig where
  toXML CreateQueryLoggingConfig' {..} =
    Lude.mconcat
      [ "HostedZoneId" Lude.@= hostedZoneId,
        "CloudWatchLogsLogGroupArn" Lude.@= cloudWatchLogsLogGroupARN
      ]

-- | /See:/ 'mkCreateQueryLoggingConfigResponse' smart constructor.
data CreateQueryLoggingConfigResponse = CreateQueryLoggingConfigResponse'
  { responseStatus ::
      Lude.Int,
    queryLoggingConfig ::
      QueryLoggingConfig,
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

-- | Creates a value of 'CreateQueryLoggingConfigResponse' with the minimum fields required to make a request.
--
-- * 'location' - The unique URL representing the new query logging configuration.
-- * 'queryLoggingConfig' - A complex type that contains the ID for a query logging configuration, the ID of the hosted zone that you want to log queries for, and the ARN for the log group that you want Amazon Route 53 to send query logs to.
-- * 'responseStatus' - The response status code.
mkCreateQueryLoggingConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'queryLoggingConfig'
  QueryLoggingConfig ->
  -- | 'location'
  Lude.Text ->
  CreateQueryLoggingConfigResponse
mkCreateQueryLoggingConfigResponse
  pResponseStatus_
  pQueryLoggingConfig_
  pLocation_ =
    CreateQueryLoggingConfigResponse'
      { responseStatus =
          pResponseStatus_,
        queryLoggingConfig = pQueryLoggingConfig_,
        location = pLocation_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqlcrsResponseStatus :: Lens.Lens' CreateQueryLoggingConfigResponse Lude.Int
cqlcrsResponseStatus = Lens.lens (responseStatus :: CreateQueryLoggingConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateQueryLoggingConfigResponse)
{-# DEPRECATED cqlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A complex type that contains the ID for a query logging configuration, the ID of the hosted zone that you want to log queries for, and the ARN for the log group that you want Amazon Route 53 to send query logs to.
--
-- /Note:/ Consider using 'queryLoggingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqlcrsQueryLoggingConfig :: Lens.Lens' CreateQueryLoggingConfigResponse QueryLoggingConfig
cqlcrsQueryLoggingConfig = Lens.lens (queryLoggingConfig :: CreateQueryLoggingConfigResponse -> QueryLoggingConfig) (\s a -> s {queryLoggingConfig = a} :: CreateQueryLoggingConfigResponse)
{-# DEPRECATED cqlcrsQueryLoggingConfig "Use generic-lens or generic-optics with 'queryLoggingConfig' instead." #-}

-- | The unique URL representing the new query logging configuration.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cqlcrsLocation :: Lens.Lens' CreateQueryLoggingConfigResponse Lude.Text
cqlcrsLocation = Lens.lens (location :: CreateQueryLoggingConfigResponse -> Lude.Text) (\s a -> s {location = a} :: CreateQueryLoggingConfigResponse)
{-# DEPRECATED cqlcrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}
