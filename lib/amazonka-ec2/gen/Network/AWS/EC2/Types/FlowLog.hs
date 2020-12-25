{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FlowLog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FlowLog
  ( FlowLog (..),

    -- * Smart constructor
    mkFlowLog,

    -- * Lenses
    flCreationTime,
    flDeliverLogsErrorMessage,
    flDeliverLogsPermissionArn,
    flDeliverLogsStatus,
    flFlowLogId,
    flFlowLogStatus,
    flLogDestination,
    flLogDestinationType,
    flLogFormat,
    flLogGroupName,
    flMaxAggregationInterval,
    flResourceId,
    flTags,
    flTrafficType,
  )
where

import qualified Network.AWS.EC2.Types.LogDestinationType as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TrafficType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a flow log.
--
-- /See:/ 'mkFlowLog' smart constructor.
data FlowLog = FlowLog'
  { -- | The date and time the flow log was created.
    creationTime :: Core.Maybe Core.UTCTime,
    -- | Information about the error that occurred. @Rate limited@ indicates that CloudWatch Logs throttling has been applied for one or more network interfaces, or that you've reached the limit on the number of log groups that you can create. @Access error@ indicates that the IAM role associated with the flow log does not have sufficient permissions to publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
    deliverLogsErrorMessage :: Core.Maybe Types.String,
    -- | The ARN of the IAM role that posts logs to CloudWatch Logs.
    deliverLogsPermissionArn :: Core.Maybe Types.String,
    -- | The status of the logs delivery (@SUCCESS@ | @FAILED@ ).
    deliverLogsStatus :: Core.Maybe Types.String,
    -- | The flow log ID.
    flowLogId :: Core.Maybe Types.String,
    -- | The status of the flow log (@ACTIVE@ ).
    flowLogStatus :: Core.Maybe Types.String,
    -- | Specifies the destination to which the flow log data is published. Flow log data can be published to an CloudWatch Logs log group or an Amazon S3 bucket. If the flow log publishes to CloudWatch Logs, this element indicates the Amazon Resource Name (ARN) of the CloudWatch Logs log group to which the data is published. If the flow log publishes to Amazon S3, this element indicates the ARN of the Amazon S3 bucket to which the data is published.
    logDestination :: Core.Maybe Types.String,
    -- | Specifies the type of destination to which the flow log data is published. Flow log data can be published to CloudWatch Logs or Amazon S3.
    logDestinationType :: Core.Maybe Types.LogDestinationType,
    -- | The format of the flow log record.
    logFormat :: Core.Maybe Types.String,
    -- | The name of the flow log group.
    logGroupName :: Core.Maybe Types.String,
    -- | The maximum interval of time, in seconds, during which a flow of packets is captured and aggregated into a flow log record.
    --
    -- When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds (1 minute) or less, regardless of the specified value.
    -- Valid Values: @60@ | @600@
    maxAggregationInterval :: Core.Maybe Core.Int,
    -- | The ID of the resource on which the flow log was created.
    resourceId :: Core.Maybe Types.String,
    -- | The tags for the flow log.
    tags :: Core.Maybe [Types.Tag],
    -- | The type of traffic captured for the flow log.
    trafficType :: Core.Maybe Types.TrafficType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FlowLog' value with any optional fields omitted.
mkFlowLog ::
  FlowLog
mkFlowLog =
  FlowLog'
    { creationTime = Core.Nothing,
      deliverLogsErrorMessage = Core.Nothing,
      deliverLogsPermissionArn = Core.Nothing,
      deliverLogsStatus = Core.Nothing,
      flowLogId = Core.Nothing,
      flowLogStatus = Core.Nothing,
      logDestination = Core.Nothing,
      logDestinationType = Core.Nothing,
      logFormat = Core.Nothing,
      logGroupName = Core.Nothing,
      maxAggregationInterval = Core.Nothing,
      resourceId = Core.Nothing,
      tags = Core.Nothing,
      trafficType = Core.Nothing
    }

-- | The date and time the flow log was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flCreationTime :: Lens.Lens' FlowLog (Core.Maybe Core.UTCTime)
flCreationTime = Lens.field @"creationTime"
{-# DEPRECATED flCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Information about the error that occurred. @Rate limited@ indicates that CloudWatch Logs throttling has been applied for one or more network interfaces, or that you've reached the limit on the number of log groups that you can create. @Access error@ indicates that the IAM role associated with the flow log does not have sufficient permissions to publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
--
-- /Note:/ Consider using 'deliverLogsErrorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flDeliverLogsErrorMessage :: Lens.Lens' FlowLog (Core.Maybe Types.String)
flDeliverLogsErrorMessage = Lens.field @"deliverLogsErrorMessage"
{-# DEPRECATED flDeliverLogsErrorMessage "Use generic-lens or generic-optics with 'deliverLogsErrorMessage' instead." #-}

-- | The ARN of the IAM role that posts logs to CloudWatch Logs.
--
-- /Note:/ Consider using 'deliverLogsPermissionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flDeliverLogsPermissionArn :: Lens.Lens' FlowLog (Core.Maybe Types.String)
flDeliverLogsPermissionArn = Lens.field @"deliverLogsPermissionArn"
{-# DEPRECATED flDeliverLogsPermissionArn "Use generic-lens or generic-optics with 'deliverLogsPermissionArn' instead." #-}

-- | The status of the logs delivery (@SUCCESS@ | @FAILED@ ).
--
-- /Note:/ Consider using 'deliverLogsStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flDeliverLogsStatus :: Lens.Lens' FlowLog (Core.Maybe Types.String)
flDeliverLogsStatus = Lens.field @"deliverLogsStatus"
{-# DEPRECATED flDeliverLogsStatus "Use generic-lens or generic-optics with 'deliverLogsStatus' instead." #-}

-- | The flow log ID.
--
-- /Note:/ Consider using 'flowLogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flFlowLogId :: Lens.Lens' FlowLog (Core.Maybe Types.String)
flFlowLogId = Lens.field @"flowLogId"
{-# DEPRECATED flFlowLogId "Use generic-lens or generic-optics with 'flowLogId' instead." #-}

-- | The status of the flow log (@ACTIVE@ ).
--
-- /Note:/ Consider using 'flowLogStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flFlowLogStatus :: Lens.Lens' FlowLog (Core.Maybe Types.String)
flFlowLogStatus = Lens.field @"flowLogStatus"
{-# DEPRECATED flFlowLogStatus "Use generic-lens or generic-optics with 'flowLogStatus' instead." #-}

-- | Specifies the destination to which the flow log data is published. Flow log data can be published to an CloudWatch Logs log group or an Amazon S3 bucket. If the flow log publishes to CloudWatch Logs, this element indicates the Amazon Resource Name (ARN) of the CloudWatch Logs log group to which the data is published. If the flow log publishes to Amazon S3, this element indicates the ARN of the Amazon S3 bucket to which the data is published.
--
-- /Note:/ Consider using 'logDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flLogDestination :: Lens.Lens' FlowLog (Core.Maybe Types.String)
flLogDestination = Lens.field @"logDestination"
{-# DEPRECATED flLogDestination "Use generic-lens or generic-optics with 'logDestination' instead." #-}

-- | Specifies the type of destination to which the flow log data is published. Flow log data can be published to CloudWatch Logs or Amazon S3.
--
-- /Note:/ Consider using 'logDestinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flLogDestinationType :: Lens.Lens' FlowLog (Core.Maybe Types.LogDestinationType)
flLogDestinationType = Lens.field @"logDestinationType"
{-# DEPRECATED flLogDestinationType "Use generic-lens or generic-optics with 'logDestinationType' instead." #-}

-- | The format of the flow log record.
--
-- /Note:/ Consider using 'logFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flLogFormat :: Lens.Lens' FlowLog (Core.Maybe Types.String)
flLogFormat = Lens.field @"logFormat"
{-# DEPRECATED flLogFormat "Use generic-lens or generic-optics with 'logFormat' instead." #-}

-- | The name of the flow log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flLogGroupName :: Lens.Lens' FlowLog (Core.Maybe Types.String)
flLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED flLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The maximum interval of time, in seconds, during which a flow of packets is captured and aggregated into a flow log record.
--
-- When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds (1 minute) or less, regardless of the specified value.
-- Valid Values: @60@ | @600@
--
-- /Note:/ Consider using 'maxAggregationInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flMaxAggregationInterval :: Lens.Lens' FlowLog (Core.Maybe Core.Int)
flMaxAggregationInterval = Lens.field @"maxAggregationInterval"
{-# DEPRECATED flMaxAggregationInterval "Use generic-lens or generic-optics with 'maxAggregationInterval' instead." #-}

-- | The ID of the resource on which the flow log was created.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flResourceId :: Lens.Lens' FlowLog (Core.Maybe Types.String)
flResourceId = Lens.field @"resourceId"
{-# DEPRECATED flResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The tags for the flow log.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flTags :: Lens.Lens' FlowLog (Core.Maybe [Types.Tag])
flTags = Lens.field @"tags"
{-# DEPRECATED flTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The type of traffic captured for the flow log.
--
-- /Note:/ Consider using 'trafficType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flTrafficType :: Lens.Lens' FlowLog (Core.Maybe Types.TrafficType)
flTrafficType = Lens.field @"trafficType"
{-# DEPRECATED flTrafficType "Use generic-lens or generic-optics with 'trafficType' instead." #-}

instance Core.FromXML FlowLog where
  parseXML x =
    FlowLog'
      Core.<$> (x Core..@? "creationTime")
      Core.<*> (x Core..@? "deliverLogsErrorMessage")
      Core.<*> (x Core..@? "deliverLogsPermissionArn")
      Core.<*> (x Core..@? "deliverLogsStatus")
      Core.<*> (x Core..@? "flowLogId")
      Core.<*> (x Core..@? "flowLogStatus")
      Core.<*> (x Core..@? "logDestination")
      Core.<*> (x Core..@? "logDestinationType")
      Core.<*> (x Core..@? "logFormat")
      Core.<*> (x Core..@? "logGroupName")
      Core.<*> (x Core..@? "maxAggregationInterval")
      Core.<*> (x Core..@? "resourceId")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "trafficType")
