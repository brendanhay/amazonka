{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateFlowLogs (..)
    , mkCreateFlowLogs
    -- ** Request lenses
    , cflResourceIds
    , cflResourceType
    , cflTrafficType
    , cflClientToken
    , cflDeliverLogsPermissionArn
    , cflDryRun
    , cflLogDestination
    , cflLogDestinationType
    , cflLogFormat
    , cflLogGroupName
    , cflMaxAggregationInterval
    , cflTagSpecifications

    -- * Destructuring the response
    , CreateFlowLogsResponse (..)
    , mkCreateFlowLogsResponse
    -- ** Response lenses
    , cflrrsClientToken
    , cflrrsFlowLogIds
    , cflrrsUnsuccessful
    , cflrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFlowLogs' smart constructor.
data CreateFlowLogs = CreateFlowLogs'
  { resourceIds :: [Types.FlowLogResourceId]
    -- ^ The ID of the subnet, network interface, or VPC for which you want to create a flow log.
--
-- Constraints: Maximum of 1000 resources
  , resourceType :: Types.FlowLogsResourceType
    -- ^ The type of resource for which to create the flow log. For example, if you specified a VPC ID for the @ResourceId@ property, specify @VPC@ for this property.
  , trafficType :: Types.TrafficType
    -- ^ The type of traffic to log. You can log traffic that the resource accepts or rejects, or all traffic.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , deliverLogsPermissionArn :: Core.Maybe Core.Text
    -- ^ The ARN for the IAM role that permits Amazon EC2 to publish flow logs to a CloudWatch Logs log group in your account.
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , logDestination :: Core.Maybe Core.Text
    -- ^ Specifies the destination to which the flow log data is to be published. Flow log data can be published to a CloudWatch Logs log group or an Amazon S3 bucket. The value specified for this parameter depends on the value specified for @LogDestinationType@ .
--
-- If @LogDestinationType@ is not specified or @cloud-watch-logs@ , specify the Amazon Resource Name (ARN) of the CloudWatch Logs log group. For example, to publish to a log group called @my-logs@ , specify @arn:aws:logs:us-east-1:123456789012:log-group:my-logs@ . Alternatively, use @LogGroupName@ instead.
-- If LogDestinationType is @s3@ , specify the ARN of the Amazon S3 bucket. You can also specify a subfolder in the bucket. To specify a subfolder in the bucket, use the following ARN format: @bucket_ARN/subfolder_name/@ . For example, to specify a subfolder named @my-logs@ in a bucket named @my-bucket@ , use the following ARN: @arn:aws:s3:::my-bucket/my-logs/@ . You cannot use @AWSLogs@ as a subfolder name. This is a reserved term.
  , logDestinationType :: Core.Maybe Types.LogDestinationType
    -- ^ Specifies the type of destination to which the flow log data is to be published. Flow log data can be published to CloudWatch Logs or Amazon S3. To publish flow log data to CloudWatch Logs, specify @cloud-watch-logs@ . To publish flow log data to Amazon S3, specify @s3@ .
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
-- Default: @cloud-watch-logs@ 
  , logFormat :: Core.Maybe Core.Text
    -- ^ The fields to include in the flow log record, in the order in which they should appear. For a list of available fields, see <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records> . If you omit this parameter, the flow log is created using the default format. If you specify this parameter, you must specify at least one field.
--
-- Specify the fields using the @> {field-id}@ format, separated by spaces. For the AWS CLI, use single quotation marks (' ') to surround the parameter value.
  , logGroupName :: Core.Maybe Core.Text
    -- ^ The name of a new or existing CloudWatch Logs log group where Amazon EC2 publishes your flow logs.
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
  , maxAggregationInterval :: Core.Maybe Core.Int
    -- ^ The maximum interval of time during which a flow of packets is captured and aggregated into a flow log record. You can specify 60 seconds (1 minute) or 600 seconds (10 minutes).
--
-- When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds or less, regardless of the value that you specify.
-- Default: 600
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the flow logs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFlowLogs' value with any optional fields omitted.
mkCreateFlowLogs
    :: Types.FlowLogsResourceType -- ^ 'resourceType'
    -> Types.TrafficType -- ^ 'trafficType'
    -> CreateFlowLogs
mkCreateFlowLogs resourceType trafficType
  = CreateFlowLogs'{resourceIds = Core.mempty, resourceType,
                    trafficType, clientToken = Core.Nothing,
                    deliverLogsPermissionArn = Core.Nothing, dryRun = Core.Nothing,
                    logDestination = Core.Nothing, logDestinationType = Core.Nothing,
                    logFormat = Core.Nothing, logGroupName = Core.Nothing,
                    maxAggregationInterval = Core.Nothing,
                    tagSpecifications = Core.Nothing}

-- | The ID of the subnet, network interface, or VPC for which you want to create a flow log.
--
-- Constraints: Maximum of 1000 resources
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflResourceIds :: Lens.Lens' CreateFlowLogs [Types.FlowLogResourceId]
cflResourceIds = Lens.field @"resourceIds"
{-# INLINEABLE cflResourceIds #-}
{-# DEPRECATED resourceIds "Use generic-lens or generic-optics with 'resourceIds' instead"  #-}

-- | The type of resource for which to create the flow log. For example, if you specified a VPC ID for the @ResourceId@ property, specify @VPC@ for this property.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflResourceType :: Lens.Lens' CreateFlowLogs Types.FlowLogsResourceType
cflResourceType = Lens.field @"resourceType"
{-# INLINEABLE cflResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The type of traffic to log. You can log traffic that the resource accepts or rejects, or all traffic.
--
-- /Note:/ Consider using 'trafficType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflTrafficType :: Lens.Lens' CreateFlowLogs Types.TrafficType
cflTrafficType = Lens.field @"trafficType"
{-# INLINEABLE cflTrafficType #-}
{-# DEPRECATED trafficType "Use generic-lens or generic-optics with 'trafficType' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflClientToken :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Text)
cflClientToken = Lens.field @"clientToken"
{-# INLINEABLE cflClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The ARN for the IAM role that permits Amazon EC2 to publish flow logs to a CloudWatch Logs log group in your account.
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
--
-- /Note:/ Consider using 'deliverLogsPermissionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflDeliverLogsPermissionArn :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Text)
cflDeliverLogsPermissionArn = Lens.field @"deliverLogsPermissionArn"
{-# INLINEABLE cflDeliverLogsPermissionArn #-}
{-# DEPRECATED deliverLogsPermissionArn "Use generic-lens or generic-optics with 'deliverLogsPermissionArn' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflDryRun :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Bool)
cflDryRun = Lens.field @"dryRun"
{-# INLINEABLE cflDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Specifies the destination to which the flow log data is to be published. Flow log data can be published to a CloudWatch Logs log group or an Amazon S3 bucket. The value specified for this parameter depends on the value specified for @LogDestinationType@ .
--
-- If @LogDestinationType@ is not specified or @cloud-watch-logs@ , specify the Amazon Resource Name (ARN) of the CloudWatch Logs log group. For example, to publish to a log group called @my-logs@ , specify @arn:aws:logs:us-east-1:123456789012:log-group:my-logs@ . Alternatively, use @LogGroupName@ instead.
-- If LogDestinationType is @s3@ , specify the ARN of the Amazon S3 bucket. You can also specify a subfolder in the bucket. To specify a subfolder in the bucket, use the following ARN format: @bucket_ARN/subfolder_name/@ . For example, to specify a subfolder named @my-logs@ in a bucket named @my-bucket@ , use the following ARN: @arn:aws:s3:::my-bucket/my-logs/@ . You cannot use @AWSLogs@ as a subfolder name. This is a reserved term.
--
-- /Note:/ Consider using 'logDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflLogDestination :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Text)
cflLogDestination = Lens.field @"logDestination"
{-# INLINEABLE cflLogDestination #-}
{-# DEPRECATED logDestination "Use generic-lens or generic-optics with 'logDestination' instead"  #-}

-- | Specifies the type of destination to which the flow log data is to be published. Flow log data can be published to CloudWatch Logs or Amazon S3. To publish flow log data to CloudWatch Logs, specify @cloud-watch-logs@ . To publish flow log data to Amazon S3, specify @s3@ .
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
-- Default: @cloud-watch-logs@ 
--
-- /Note:/ Consider using 'logDestinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflLogDestinationType :: Lens.Lens' CreateFlowLogs (Core.Maybe Types.LogDestinationType)
cflLogDestinationType = Lens.field @"logDestinationType"
{-# INLINEABLE cflLogDestinationType #-}
{-# DEPRECATED logDestinationType "Use generic-lens or generic-optics with 'logDestinationType' instead"  #-}

-- | The fields to include in the flow log record, in the order in which they should appear. For a list of available fields, see <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records> . If you omit this parameter, the flow log is created using the default format. If you specify this parameter, you must specify at least one field.
--
-- Specify the fields using the @> {field-id}@ format, separated by spaces. For the AWS CLI, use single quotation marks (' ') to surround the parameter value.
--
-- /Note:/ Consider using 'logFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflLogFormat :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Text)
cflLogFormat = Lens.field @"logFormat"
{-# INLINEABLE cflLogFormat #-}
{-# DEPRECATED logFormat "Use generic-lens or generic-optics with 'logFormat' instead"  #-}

-- | The name of a new or existing CloudWatch Logs log group where Amazon EC2 publishes your flow logs.
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflLogGroupName :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Text)
cflLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE cflLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | The maximum interval of time during which a flow of packets is captured and aggregated into a flow log record. You can specify 60 seconds (1 minute) or 600 seconds (10 minutes).
--
-- When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds or less, regardless of the value that you specify.
-- Default: 600
--
-- /Note:/ Consider using 'maxAggregationInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflMaxAggregationInterval :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Int)
cflMaxAggregationInterval = Lens.field @"maxAggregationInterval"
{-# INLINEABLE cflMaxAggregationInterval #-}
{-# DEPRECATED maxAggregationInterval "Use generic-lens or generic-optics with 'maxAggregationInterval' instead"  #-}

-- | The tags to apply to the flow logs.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflTagSpecifications :: Lens.Lens' CreateFlowLogs (Core.Maybe [Types.TagSpecification])
cflTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cflTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateFlowLogs where
        toQuery CreateFlowLogs{..}
          = Core.toQueryPair "Action" ("CreateFlowLogs" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryList "ResourceId" resourceIds
              Core.<> Core.toQueryPair "ResourceType" resourceType
              Core.<> Core.toQueryPair "TrafficType" trafficType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DeliverLogsPermissionArn")
                deliverLogsPermissionArn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LogDestination")
                logDestination
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LogDestinationType")
                logDestinationType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LogFormat") logFormat
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "LogGroupName")
                logGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxAggregationInterval")
                maxAggregationInterval
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateFlowLogs where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateFlowLogs where
        type Rs CreateFlowLogs = CreateFlowLogsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateFlowLogsResponse' Core.<$>
                   (x Core..@? "clientToken") Core.<*>
                     x Core..@? "flowLogIdSet" Core..<@> Core.parseXMLList "item"
                     Core.<*>
                     x Core..@? "unsuccessful" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateFlowLogsResponse' smart constructor.
data CreateFlowLogsResponse = CreateFlowLogsResponse'
  { clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
  , flowLogIds :: Core.Maybe [Core.Text]
    -- ^ The IDs of the flow logs.
  , unsuccessful :: Core.Maybe [Types.UnsuccessfulItem]
    -- ^ Information about the flow logs that could not be created successfully.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFlowLogsResponse' value with any optional fields omitted.
mkCreateFlowLogsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateFlowLogsResponse
mkCreateFlowLogsResponse responseStatus
  = CreateFlowLogsResponse'{clientToken = Core.Nothing,
                            flowLogIds = Core.Nothing, unsuccessful = Core.Nothing,
                            responseStatus}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflrrsClientToken :: Lens.Lens' CreateFlowLogsResponse (Core.Maybe Core.Text)
cflrrsClientToken = Lens.field @"clientToken"
{-# INLINEABLE cflrrsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The IDs of the flow logs.
--
-- /Note:/ Consider using 'flowLogIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflrrsFlowLogIds :: Lens.Lens' CreateFlowLogsResponse (Core.Maybe [Core.Text])
cflrrsFlowLogIds = Lens.field @"flowLogIds"
{-# INLINEABLE cflrrsFlowLogIds #-}
{-# DEPRECATED flowLogIds "Use generic-lens or generic-optics with 'flowLogIds' instead"  #-}

-- | Information about the flow logs that could not be created successfully.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflrrsUnsuccessful :: Lens.Lens' CreateFlowLogsResponse (Core.Maybe [Types.UnsuccessfulItem])
cflrrsUnsuccessful = Lens.field @"unsuccessful"
{-# INLINEABLE cflrrsUnsuccessful #-}
{-# DEPRECATED unsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflrrsResponseStatus :: Lens.Lens' CreateFlowLogsResponse Core.Int
cflrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cflrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
