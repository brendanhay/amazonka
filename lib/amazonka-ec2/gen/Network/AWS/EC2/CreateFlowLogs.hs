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
    cflLogFormat,
    cflMaxAggregationInterval,
    cflResourceType,
    cflClientToken,
    cflTrafficType,
    cflLogDestination,
    cflLogGroupName,
    cflTagSpecifications,
    cflDeliverLogsPermissionARN,
    cflLogDestinationType,
    cflDryRun,

    -- * Destructuring the response
    CreateFlowLogsResponse (..),
    mkCreateFlowLogsResponse,

    -- ** Response lenses
    cflrsUnsuccessful,
    cflrsClientToken,
    cflrsFlowLogIds,
    cflrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateFlowLogs' smart constructor.
data CreateFlowLogs = CreateFlowLogs'
  { -- | The ID of the subnet, network interface, or VPC for which you want to create a flow log.
    --
    -- Constraints: Maximum of 1000 resources
    resourceIds :: [Lude.Text],
    -- | The fields to include in the flow log record, in the order in which they should appear. For a list of available fields, see <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records> . If you omit this parameter, the flow log is created using the default format. If you specify this parameter, you must specify at least one field.
    --
    -- Specify the fields using the @> {field-id}@ format, separated by spaces. For the AWS CLI, use single quotation marks (' ') to surround the parameter value.
    logFormat :: Lude.Maybe Lude.Text,
    -- | The maximum interval of time during which a flow of packets is captured and aggregated into a flow log record. You can specify 60 seconds (1 minute) or 600 seconds (10 minutes).
    --
    -- When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds or less, regardless of the value that you specify.
    -- Default: 600
    maxAggregationInterval :: Lude.Maybe Lude.Int,
    -- | The type of resource for which to create the flow log. For example, if you specified a VPC ID for the @ResourceId@ property, specify @VPC@ for this property.
    resourceType :: FlowLogsResourceType,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Lude.Maybe Lude.Text,
    -- | The type of traffic to log. You can log traffic that the resource accepts or rejects, or all traffic.
    trafficType :: TrafficType,
    -- | Specifies the destination to which the flow log data is to be published. Flow log data can be published to a CloudWatch Logs log group or an Amazon S3 bucket. The value specified for this parameter depends on the value specified for @LogDestinationType@ .
    --
    -- If @LogDestinationType@ is not specified or @cloud-watch-logs@ , specify the Amazon Resource Name (ARN) of the CloudWatch Logs log group. For example, to publish to a log group called @my-logs@ , specify @arn:aws:logs:us-east-1:123456789012:log-group:my-logs@ . Alternatively, use @LogGroupName@ instead.
    -- If LogDestinationType is @s3@ , specify the ARN of the Amazon S3 bucket. You can also specify a subfolder in the bucket. To specify a subfolder in the bucket, use the following ARN format: @bucket_ARN/subfolder_name/@ . For example, to specify a subfolder named @my-logs@ in a bucket named @my-bucket@ , use the following ARN: @arn:aws:s3:::my-bucket/my-logs/@ . You cannot use @AWSLogs@ as a subfolder name. This is a reserved term.
    logDestination :: Lude.Maybe Lude.Text,
    -- | The name of a new or existing CloudWatch Logs log group where Amazon EC2 publishes your flow logs.
    --
    -- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
    logGroupName :: Lude.Maybe Lude.Text,
    -- | The tags to apply to the flow logs.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | The ARN for the IAM role that permits Amazon EC2 to publish flow logs to a CloudWatch Logs log group in your account.
    --
    -- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
    deliverLogsPermissionARN :: Lude.Maybe Lude.Text,
    -- | Specifies the type of destination to which the flow log data is to be published. Flow log data can be published to CloudWatch Logs or Amazon S3. To publish flow log data to CloudWatch Logs, specify @cloud-watch-logs@ . To publish flow log data to Amazon S3, specify @s3@ .
    --
    -- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
    -- Default: @cloud-watch-logs@
    logDestinationType :: Lude.Maybe LogDestinationType,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFlowLogs' with the minimum fields required to make a request.
--
-- * 'resourceIds' - The ID of the subnet, network interface, or VPC for which you want to create a flow log.
--
-- Constraints: Maximum of 1000 resources
-- * 'logFormat' - The fields to include in the flow log record, in the order in which they should appear. For a list of available fields, see <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records> . If you omit this parameter, the flow log is created using the default format. If you specify this parameter, you must specify at least one field.
--
-- Specify the fields using the @> {field-id}@ format, separated by spaces. For the AWS CLI, use single quotation marks (' ') to surround the parameter value.
-- * 'maxAggregationInterval' - The maximum interval of time during which a flow of packets is captured and aggregated into a flow log record. You can specify 60 seconds (1 minute) or 600 seconds (10 minutes).
--
-- When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds or less, regardless of the value that you specify.
-- Default: 600
-- * 'resourceType' - The type of resource for which to create the flow log. For example, if you specified a VPC ID for the @ResourceId@ property, specify @VPC@ for this property.
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'trafficType' - The type of traffic to log. You can log traffic that the resource accepts or rejects, or all traffic.
-- * 'logDestination' - Specifies the destination to which the flow log data is to be published. Flow log data can be published to a CloudWatch Logs log group or an Amazon S3 bucket. The value specified for this parameter depends on the value specified for @LogDestinationType@ .
--
-- If @LogDestinationType@ is not specified or @cloud-watch-logs@ , specify the Amazon Resource Name (ARN) of the CloudWatch Logs log group. For example, to publish to a log group called @my-logs@ , specify @arn:aws:logs:us-east-1:123456789012:log-group:my-logs@ . Alternatively, use @LogGroupName@ instead.
-- If LogDestinationType is @s3@ , specify the ARN of the Amazon S3 bucket. You can also specify a subfolder in the bucket. To specify a subfolder in the bucket, use the following ARN format: @bucket_ARN/subfolder_name/@ . For example, to specify a subfolder named @my-logs@ in a bucket named @my-bucket@ , use the following ARN: @arn:aws:s3:::my-bucket/my-logs/@ . You cannot use @AWSLogs@ as a subfolder name. This is a reserved term.
-- * 'logGroupName' - The name of a new or existing CloudWatch Logs log group where Amazon EC2 publishes your flow logs.
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
-- * 'tagSpecifications' - The tags to apply to the flow logs.
-- * 'deliverLogsPermissionARN' - The ARN for the IAM role that permits Amazon EC2 to publish flow logs to a CloudWatch Logs log group in your account.
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
-- * 'logDestinationType' - Specifies the type of destination to which the flow log data is to be published. Flow log data can be published to CloudWatch Logs or Amazon S3. To publish flow log data to CloudWatch Logs, specify @cloud-watch-logs@ . To publish flow log data to Amazon S3, specify @s3@ .
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
-- Default: @cloud-watch-logs@
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateFlowLogs ::
  -- | 'resourceType'
  FlowLogsResourceType ->
  -- | 'trafficType'
  TrafficType ->
  CreateFlowLogs
mkCreateFlowLogs pResourceType_ pTrafficType_ =
  CreateFlowLogs'
    { resourceIds = Lude.mempty,
      logFormat = Lude.Nothing,
      maxAggregationInterval = Lude.Nothing,
      resourceType = pResourceType_,
      clientToken = Lude.Nothing,
      trafficType = pTrafficType_,
      logDestination = Lude.Nothing,
      logGroupName = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      deliverLogsPermissionARN = Lude.Nothing,
      logDestinationType = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ID of the subnet, network interface, or VPC for which you want to create a flow log.
--
-- Constraints: Maximum of 1000 resources
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflResourceIds :: Lens.Lens' CreateFlowLogs [Lude.Text]
cflResourceIds = Lens.lens (resourceIds :: CreateFlowLogs -> [Lude.Text]) (\s a -> s {resourceIds = a} :: CreateFlowLogs)
{-# DEPRECATED cflResourceIds "Use generic-lens or generic-optics with 'resourceIds' instead." #-}

-- | The fields to include in the flow log record, in the order in which they should appear. For a list of available fields, see <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records> . If you omit this parameter, the flow log is created using the default format. If you specify this parameter, you must specify at least one field.
--
-- Specify the fields using the @> {field-id}@ format, separated by spaces. For the AWS CLI, use single quotation marks (' ') to surround the parameter value.
--
-- /Note:/ Consider using 'logFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflLogFormat :: Lens.Lens' CreateFlowLogs (Lude.Maybe Lude.Text)
cflLogFormat = Lens.lens (logFormat :: CreateFlowLogs -> Lude.Maybe Lude.Text) (\s a -> s {logFormat = a} :: CreateFlowLogs)
{-# DEPRECATED cflLogFormat "Use generic-lens or generic-optics with 'logFormat' instead." #-}

-- | The maximum interval of time during which a flow of packets is captured and aggregated into a flow log record. You can specify 60 seconds (1 minute) or 600 seconds (10 minutes).
--
-- When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds or less, regardless of the value that you specify.
-- Default: 600
--
-- /Note:/ Consider using 'maxAggregationInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflMaxAggregationInterval :: Lens.Lens' CreateFlowLogs (Lude.Maybe Lude.Int)
cflMaxAggregationInterval = Lens.lens (maxAggregationInterval :: CreateFlowLogs -> Lude.Maybe Lude.Int) (\s a -> s {maxAggregationInterval = a} :: CreateFlowLogs)
{-# DEPRECATED cflMaxAggregationInterval "Use generic-lens or generic-optics with 'maxAggregationInterval' instead." #-}

-- | The type of resource for which to create the flow log. For example, if you specified a VPC ID for the @ResourceId@ property, specify @VPC@ for this property.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflResourceType :: Lens.Lens' CreateFlowLogs FlowLogsResourceType
cflResourceType = Lens.lens (resourceType :: CreateFlowLogs -> FlowLogsResourceType) (\s a -> s {resourceType = a} :: CreateFlowLogs)
{-# DEPRECATED cflResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflClientToken :: Lens.Lens' CreateFlowLogs (Lude.Maybe Lude.Text)
cflClientToken = Lens.lens (clientToken :: CreateFlowLogs -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateFlowLogs)
{-# DEPRECATED cflClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The type of traffic to log. You can log traffic that the resource accepts or rejects, or all traffic.
--
-- /Note:/ Consider using 'trafficType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflTrafficType :: Lens.Lens' CreateFlowLogs TrafficType
cflTrafficType = Lens.lens (trafficType :: CreateFlowLogs -> TrafficType) (\s a -> s {trafficType = a} :: CreateFlowLogs)
{-# DEPRECATED cflTrafficType "Use generic-lens or generic-optics with 'trafficType' instead." #-}

-- | Specifies the destination to which the flow log data is to be published. Flow log data can be published to a CloudWatch Logs log group or an Amazon S3 bucket. The value specified for this parameter depends on the value specified for @LogDestinationType@ .
--
-- If @LogDestinationType@ is not specified or @cloud-watch-logs@ , specify the Amazon Resource Name (ARN) of the CloudWatch Logs log group. For example, to publish to a log group called @my-logs@ , specify @arn:aws:logs:us-east-1:123456789012:log-group:my-logs@ . Alternatively, use @LogGroupName@ instead.
-- If LogDestinationType is @s3@ , specify the ARN of the Amazon S3 bucket. You can also specify a subfolder in the bucket. To specify a subfolder in the bucket, use the following ARN format: @bucket_ARN/subfolder_name/@ . For example, to specify a subfolder named @my-logs@ in a bucket named @my-bucket@ , use the following ARN: @arn:aws:s3:::my-bucket/my-logs/@ . You cannot use @AWSLogs@ as a subfolder name. This is a reserved term.
--
-- /Note:/ Consider using 'logDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflLogDestination :: Lens.Lens' CreateFlowLogs (Lude.Maybe Lude.Text)
cflLogDestination = Lens.lens (logDestination :: CreateFlowLogs -> Lude.Maybe Lude.Text) (\s a -> s {logDestination = a} :: CreateFlowLogs)
{-# DEPRECATED cflLogDestination "Use generic-lens or generic-optics with 'logDestination' instead." #-}

-- | The name of a new or existing CloudWatch Logs log group where Amazon EC2 publishes your flow logs.
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflLogGroupName :: Lens.Lens' CreateFlowLogs (Lude.Maybe Lude.Text)
cflLogGroupName = Lens.lens (logGroupName :: CreateFlowLogs -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: CreateFlowLogs)
{-# DEPRECATED cflLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The tags to apply to the flow logs.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflTagSpecifications :: Lens.Lens' CreateFlowLogs (Lude.Maybe [TagSpecification])
cflTagSpecifications = Lens.lens (tagSpecifications :: CreateFlowLogs -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateFlowLogs)
{-# DEPRECATED cflTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The ARN for the IAM role that permits Amazon EC2 to publish flow logs to a CloudWatch Logs log group in your account.
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
--
-- /Note:/ Consider using 'deliverLogsPermissionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflDeliverLogsPermissionARN :: Lens.Lens' CreateFlowLogs (Lude.Maybe Lude.Text)
cflDeliverLogsPermissionARN = Lens.lens (deliverLogsPermissionARN :: CreateFlowLogs -> Lude.Maybe Lude.Text) (\s a -> s {deliverLogsPermissionARN = a} :: CreateFlowLogs)
{-# DEPRECATED cflDeliverLogsPermissionARN "Use generic-lens or generic-optics with 'deliverLogsPermissionARN' instead." #-}

-- | Specifies the type of destination to which the flow log data is to be published. Flow log data can be published to CloudWatch Logs or Amazon S3. To publish flow log data to CloudWatch Logs, specify @cloud-watch-logs@ . To publish flow log data to Amazon S3, specify @s3@ .
--
-- If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
-- Default: @cloud-watch-logs@
--
-- /Note:/ Consider using 'logDestinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflLogDestinationType :: Lens.Lens' CreateFlowLogs (Lude.Maybe LogDestinationType)
cflLogDestinationType = Lens.lens (logDestinationType :: CreateFlowLogs -> Lude.Maybe LogDestinationType) (\s a -> s {logDestinationType = a} :: CreateFlowLogs)
{-# DEPRECATED cflLogDestinationType "Use generic-lens or generic-optics with 'logDestinationType' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflDryRun :: Lens.Lens' CreateFlowLogs (Lude.Maybe Lude.Bool)
cflDryRun = Lens.lens (dryRun :: CreateFlowLogs -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateFlowLogs)
{-# DEPRECATED cflDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateFlowLogs where
  type Rs CreateFlowLogs = CreateFlowLogsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateFlowLogsResponse'
            Lude.<$> ( x Lude..@? "unsuccessful" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "clientToken")
            Lude.<*> ( x Lude..@? "flowLogIdSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateFlowLogs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateFlowLogs where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateFlowLogs where
  toQuery CreateFlowLogs' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateFlowLogs" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "ResourceId" resourceIds,
        "LogFormat" Lude.=: logFormat,
        "MaxAggregationInterval" Lude.=: maxAggregationInterval,
        "ResourceType" Lude.=: resourceType,
        "ClientToken" Lude.=: clientToken,
        "TrafficType" Lude.=: trafficType,
        "LogDestination" Lude.=: logDestination,
        "LogGroupName" Lude.=: logGroupName,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DeliverLogsPermissionArn" Lude.=: deliverLogsPermissionARN,
        "LogDestinationType" Lude.=: logDestinationType,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateFlowLogsResponse' smart constructor.
data CreateFlowLogsResponse = CreateFlowLogsResponse'
  { -- | Information about the flow logs that could not be created successfully.
    unsuccessful :: Lude.Maybe [UnsuccessfulItem],
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
    clientToken :: Lude.Maybe Lude.Text,
    -- | The IDs of the flow logs.
    flowLogIds :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFlowLogsResponse' with the minimum fields required to make a request.
--
-- * 'unsuccessful' - Information about the flow logs that could not be created successfully.
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
-- * 'flowLogIds' - The IDs of the flow logs.
-- * 'responseStatus' - The response status code.
mkCreateFlowLogsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateFlowLogsResponse
mkCreateFlowLogsResponse pResponseStatus_ =
  CreateFlowLogsResponse'
    { unsuccessful = Lude.Nothing,
      clientToken = Lude.Nothing,
      flowLogIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the flow logs that could not be created successfully.
--
-- /Note:/ Consider using 'unsuccessful' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflrsUnsuccessful :: Lens.Lens' CreateFlowLogsResponse (Lude.Maybe [UnsuccessfulItem])
cflrsUnsuccessful = Lens.lens (unsuccessful :: CreateFlowLogsResponse -> Lude.Maybe [UnsuccessfulItem]) (\s a -> s {unsuccessful = a} :: CreateFlowLogsResponse)
{-# DEPRECATED cflrsUnsuccessful "Use generic-lens or generic-optics with 'unsuccessful' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflrsClientToken :: Lens.Lens' CreateFlowLogsResponse (Lude.Maybe Lude.Text)
cflrsClientToken = Lens.lens (clientToken :: CreateFlowLogsResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateFlowLogsResponse)
{-# DEPRECATED cflrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The IDs of the flow logs.
--
-- /Note:/ Consider using 'flowLogIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflrsFlowLogIds :: Lens.Lens' CreateFlowLogsResponse (Lude.Maybe [Lude.Text])
cflrsFlowLogIds = Lens.lens (flowLogIds :: CreateFlowLogsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {flowLogIds = a} :: CreateFlowLogsResponse)
{-# DEPRECATED cflrsFlowLogIds "Use generic-lens or generic-optics with 'flowLogIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cflrsResponseStatus :: Lens.Lens' CreateFlowLogsResponse Lude.Int
cflrsResponseStatus = Lens.lens (responseStatus :: CreateFlowLogsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateFlowLogsResponse)
{-# DEPRECATED cflrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
