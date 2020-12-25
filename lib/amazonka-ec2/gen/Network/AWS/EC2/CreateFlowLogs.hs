{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateFlowLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more flow logs to capture information about IP traffic for a specific network interface, subnet, or VPC.
--
-- Flow log data for a monitored network interface is recorded as flow log records, which are log events consisting of fields that describe the traffic flow. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records> in the /Amazon Virtual Private Cloud User Guide/ .
-- When publishing to CloudWatch Logs, flow log records are published to a log group, and each network interface has a unique log stream in the log group. When publishing to Amazon S3, flow log records for all of the monitored network interfaces are published to a single log file object that is stored in the specified bucket.
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html VPC Flow Logs> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateFlowLogs
  ( -- * Creating a request
    CreateFlowLogs (..),
    mkCreateFlowLogs,

    -- ** Request lenses
    cflResourceIds,
    cflResourceType,
    cflTrafficType,
    cflClientToken,
    cflDeliverLogsPermissionArn,
    cflDryRun,
    cflLogDestination,
    cflLogDestinationType,
    cflLogFormat,
    cflLogGroupName,
    cflMaxAggregationInterval,
    cflTagSpecifications,

    -- * Destructuring the response
    CreateFlowLogsResponse (..),
    mkCreateFlowLogsResponse,

    -- ** Response lenses
    cflrrsClientToken,
    cflrrsFlowLogIds,
    cflrrsUnsuccessful,
    cflrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFlowLogs' smart constructor.
data CreateFlowLogs = CreateFlowLogs'
  { -- | The ID of the subnet, network interface, or VPC for which you want to create a flow log.
    --
    -- Constraints: Maximum of 1000 resources
    resourceIds :: [Types.FlowLogResourceId],
    -- | The type of resource for which to create the flow log. For example, if you specified a VPC ID for the @ResourceId@ property, specify @VPC@ for this property.
    resourceType :: Types.FlowLogsResourceType,
    -- | The type of traffic to log. You can log traffic that the resource accepts or rejects, or all traffic.
    trafficType :: Types.TrafficType,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Core.Maybe Types.String,
    -- | The ARN for the IAM role that permits Amazon EC2 to publish flow logs to a CloudWatch Logs log group in your account.
    --
    -- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
    deliverLogsPermissionArn :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Specifies the destination to which the flow log data is to be published. Flow log data can be published to a CloudWatch Logs log group or an Amazon S3 bucket. The value specified for this parameter depends on the value specified for @LogDestinationType@ .
    --
    -- If @LogDestinationType@ is not specified or @cloud-watch-logs@ , specify the Amazon Resource Name (ARN) of the CloudWatch Logs log group. For example, to publish to a log group called @my-logs@ , specify @arn:aws:logs:us-east-1:123456789012:log-group:my-logs@ . Alternatively, use @LogGroupName@ instead.
    -- If LogDestinationType is @s3@ , specify the ARN of the Amazon S3 bucket. You can also specify a subfolder in the bucket. To specify a subfolder in the bucket, use the following ARN format: @bucket_ARN/subfolder_name/@ . For example, to specify a subfolder named @my-logs@ in a bucket named @my-bucket@ , use the following ARN: @arn:aws:s3:::my-bucket/my-logs/@ . You cannot use @AWSLogs@ as a subfolder name. This is a reserved term.
    logDestination :: Core.Maybe Types.String,
    -- | Specifies the type of destination to which the flow log data is to be published. Flow log data can be published to CloudWatch Logs or Amazon S3. To publish flow log data to CloudWatch Logs, specify @cloud-watch-logs@ . To publish flow log data to Amazon S3, specify @s3@ .
    --
    -- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
    -- Default: @cloud-watch-logs@
    logDestinationType :: Core.Maybe Types.LogDestinationType,
    -- | The fields to include in the flow log record, in the order in which they should appear. For a list of available fields, see <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records> . If you omit this parameter, the flow log is created using the default format. If you specify this parameter, you must specify at least one field.
    --
    -- Specify the fields using the @> {field-id}@ format, separated by spaces. For the AWS CLI, use single quotation marks (' ') to surround the parameter value.
    logFormat :: Core.Maybe Types.String,
    -- | The name of a new or existing CloudWatch Logs log group where Amazon EC2 publishes your flow logs.
    --
    -- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
    logGroupName :: Core.Maybe Types.String,
    -- | The maximum interval of time during which a flow of packets is captured and aggregated into a flow log record. You can specify 60 seconds (1 minute) or 600 seconds (10 minutes).
    --
    -- When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds or less, regardless of the value that you specify.
    -- Default: 600
    maxAggregationInterval :: Core.Maybe Core.Int,
    -- | The tags to apply to the flow logs.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFlowLogs' value with any optional fields omitted.
mkCreateFlowLogs ::
  -- | 'resourceType'
  Types.FlowLogsResourceType ->
  -- | 'trafficType'
  Types.TrafficType ->
  CreateFlowLogs
mkCreateFlowLogs resourceType trafficType =
  CreateFlowLogs'
    { resourceIds = Core.mempty,
      resourceType,
      trafficType,
      clientToken = Core.Nothing,
      deliverLogsPermissionArn = Core.Nothing,
      dryRun = Core.Nothing,
      logDestination = Core.Nothing,
      logDestinationType = Core.Nothing,
      logFormat = Core.Nothing,
      logGroupName = Core.Nothing,
      maxAggregationInterval = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | The ID of the subnet, network interface, or VPC for which you want to create a flow log.
--
-- Constraints: Maximum of 1000 resources
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflResourceIds :: Lens.Lens' CreateFlowLogs [Types.FlowLogResourceId]
cflResourceIds = Lens.field @"resourceIds"
{-# DEPRECATED cflResourceIds "Use generic-lens or generic-optics with 'resourceIds' instead." #-}

-- | The type of resource for which to create the flow log. For example, if you specified a VPC ID for the @ResourceId@ property, specify @VPC@ for this property.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflResourceType :: Lens.Lens' CreateFlowLogs Types.FlowLogsResourceType
cflResourceType = Lens.field @"resourceType"
{-# DEPRECATED cflResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The type of traffic to log. You can log traffic that the resource accepts or rejects, or all traffic.
--
-- /Note:/ Consider using 'trafficType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflTrafficType :: Lens.Lens' CreateFlowLogs Types.TrafficType
cflTrafficType = Lens.field @"trafficType"
{-# DEPRECATED cflTrafficType "Use generic-lens or generic-optics with 'trafficType' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflClientToken :: Lens.Lens' CreateFlowLogs (Core.Maybe Types.String)
cflClientToken = Lens.field @"clientToken"
{-# DEPRECATED cflClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ARN for the IAM role that permits Amazon EC2 to publish flow logs to a CloudWatch Logs log group in your account.
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
--
-- /Note:/ Consider using 'deliverLogsPermissionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflDeliverLogsPermissionArn :: Lens.Lens' CreateFlowLogs (Core.Maybe Types.String)
cflDeliverLogsPermissionArn = Lens.field @"deliverLogsPermissionArn"
{-# DEPRECATED cflDeliverLogsPermissionArn "Use generic-lens or generic-optics with 'deliverLogsPermissionArn' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflDryRun :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Bool)
cflDryRun = Lens.field @"dryRun"
{-# DEPRECATED cflDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Specifies the destination to which the flow log data is to be published. Flow log data can be published to a CloudWatch Logs log group or an Amazon S3 bucket. The value specified for this parameter depends on the value specified for @LogDestinationType@ .
--
-- If @LogDestinationType@ is not specified or @cloud-watch-logs@ , specify the Amazon Resource Name (ARN) of the CloudWatch Logs log group. For example, to publish to a log group called @my-logs@ , specify @arn:aws:logs:us-east-1:123456789012:log-group:my-logs@ . Alternatively, use @LogGroupName@ instead.
-- If LogDestinationType is @s3@ , specify the ARN of the Amazon S3 bucket. You can also specify a subfolder in the bucket. To specify a subfolder in the bucket, use the following ARN format: @bucket_ARN/subfolder_name/@ . For example, to specify a subfolder named @my-logs@ in a bucket named @my-bucket@ , use the following ARN: @arn:aws:s3:::my-bucket/my-logs/@ . You cannot use @AWSLogs@ as a subfolder name. This is a reserved term.
--
-- /Note:/ Consider using 'logDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflLogDestination :: Lens.Lens' CreateFlowLogs (Core.Maybe Types.String)
cflLogDestination = Lens.field @"logDestination"
{-# DEPRECATED cflLogDestination "Use generic-lens or generic-optics with 'logDestination' instead." #-}

-- | Specifies the type of destination to which the flow log data is to be published. Flow log data can be published to CloudWatch Logs or Amazon S3. To publish flow log data to CloudWatch Logs, specify @cloud-watch-logs@ . To publish flow log data to Amazon S3, specify @s3@ .
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
-- Default: @cloud-watch-logs@
--
-- /Note:/ Consider using 'logDestinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflLogDestinationType :: Lens.Lens' CreateFlowLogs (Core.Maybe Types.LogDestinationType)
cflLogDestinationType = Lens.field @"logDestinationType"
{-# DEPRECATED cflLogDestinationType "Use generic-lens or generic-optics with 'logDestinationType' instead." #-}

-- | The fields to include in the flow log record, in the order in which they should appear. For a list of available fields, see <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records> . If you omit this parameter, the flow log is created using the default format. If you specify this parameter, you must specify at least one field.
--
-- Specify the fields using the @> {field-id}@ format, separated by spaces. For the AWS CLI, use single quotation marks (' ') to surround the parameter value.
--
-- /Note:/ Consider using 'logFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflLogFormat :: Lens.Lens' CreateFlowLogs (Core.Maybe Types.String)
cflLogFormat = Lens.field @"logFormat"
{-# DEPRECATED cflLogFormat "Use generic-lens or generic-optics with 'logFormat' instead." #-}

-- | The name of a new or existing CloudWatch Logs log group where Amazon EC2 publishes your flow logs.
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflLogGroupName :: Lens.Lens' CreateFlowLogs (Core.Maybe Types.String)
cflLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED cflLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The maximum interval of time during which a flow of packets is captured and aggregated into a flow log record. You can specify 60 seconds (1 minute) or 600 seconds (10 minutes).
--
-- When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds or less, regardless of the value that you specify.
-- Default: 600
--
-- /Note:/ Consider using 'maxAggregationInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflMaxAggregationInterval :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Int)
cflMaxAggregationInterval = Lens.field @"maxAggregationInterval"
{-# DEPRECATED cflMaxAggregationInterval "Use generic-lens or generic-optics with 'maxAggregationInterval' instead." #-}

-- | The tags to apply to the flow logs.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflTagSpecifications :: Lens.Lens' CreateFlowLogs (Core.Maybe [Types.TagSpecification])
cflTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED cflTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateFlowLogs where
  type Rs CreateFlowLogs = CreateFlowLogsResponse
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
            ( Core.pure ("Action", "CreateFlowLogs")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "ResourceId" resourceIds)
                Core.<> (Core.toQueryValue "ResourceType" resourceType)
                Core.<> (Core.toQueryValue "TrafficType" trafficType)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> ( Core.toQueryValue "DeliverLogsPermissionArn"
                            Core.<$> deliverLogsPermissionArn
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "LogDestination" Core.<$> logDestination)
                Core.<> ( Core.toQueryValue "LogDestinationType"
                            Core.<$> logDestinationType
                        )
                Core.<> (Core.toQueryValue "LogFormat" Core.<$> logFormat)
                Core.<> (Core.toQueryValue "LogGroupName" Core.<$> logGroupName)
                Core.<> ( Core.toQueryValue "MaxAggregationInterval"
                            Core.<$> maxAggregationInterval
                        )
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFlowLogsResponse'
            Core.<$> (x Core..@? "clientToken")
            Core.<*> (x Core..@? "flowLogIdSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "unsuccessful" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateFlowLogsResponse' smart constructor.
data CreateFlowLogsResponse = CreateFlowLogsResponse'
  { -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
    clientToken :: Core.Maybe Types.String,
    -- | The IDs of the flow logs.
    flowLogIds :: Core.Maybe [Types.String],
    -- | Information about the flow logs that could not be created successfully.
    unsuccessful :: Core.Maybe [Types.UnsuccessfulItem],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFlowLogsResponse' value with any optional fields omitted.
mkCreateFlowLogsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateFlowLogsResponse
mkCreateFlowLogsResponse responseStatus =
  CreateFlowLogsResponse'
    { clientToken = Core.Nothing,
      flowLogIds = Core.Nothing,
      unsuccessful = Core.Nothing,
      responseStatus
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflrrsClientToken :: Lens.Lens' CreateFlowLogsResponse (Core.Maybe Types.String)
cflrrsClientToken = Lens.field @"clientToken"
{-# DEPRECATED cflrrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The IDs of the flow logs.
--
-- /Note:/ Consider using 'flowLogIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflrrsFlowLogIds :: Lens.Lens' CreateFlowLogsResponse (Core.Maybe [Types.String])
cflrrsFlowLogIds = Lens.field @"flowLogIds"
{-# DEPRECATED cflrrsFlowLogIds "Use generic-lens or generic-optics with 'flowLogIds' instead." #-}

-- | Information about the flow logs that could not be created successfully.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflrrsUnsuccessful :: Lens.Lens' CreateFlowLogsResponse (Core.Maybe [Types.UnsuccessfulItem])
cflrrsUnsuccessful = Lens.field @"unsuccessful"
{-# DEPRECATED cflrrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflrrsResponseStatus :: Lens.Lens' CreateFlowLogsResponse Core.Int
cflrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cflrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
