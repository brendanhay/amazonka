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
    flLogFormat,
    flMaxAggregationInterval,
    flResourceId,
    flFlowLogStatus,
    flTrafficType,
    flLogDestination,
    flDeliverLogsStatus,
    flDeliverLogsErrorMessage,
    flLogGroupName,
    flDeliverLogsPermissionARN,
    flLogDestinationType,
    flFlowLogId,
    flTags,
  )
where

import Network.AWS.EC2.Types.LogDestinationType
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TrafficType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a flow log.
--
-- /See:/ 'mkFlowLog' smart constructor.
data FlowLog = FlowLog'
  { -- | The date and time the flow log was created.
    creationTime :: Lude.Maybe Lude.DateTime,
    -- | The format of the flow log record.
    logFormat :: Lude.Maybe Lude.Text,
    -- | The maximum interval of time, in seconds, during which a flow of packets is captured and aggregated into a flow log record.
    --
    -- When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds (1 minute) or less, regardless of the specified value.
    -- Valid Values: @60@ | @600@
    maxAggregationInterval :: Lude.Maybe Lude.Int,
    -- | The ID of the resource on which the flow log was created.
    resourceId :: Lude.Maybe Lude.Text,
    -- | The status of the flow log (@ACTIVE@ ).
    flowLogStatus :: Lude.Maybe Lude.Text,
    -- | The type of traffic captured for the flow log.
    trafficType :: Lude.Maybe TrafficType,
    -- | Specifies the destination to which the flow log data is published. Flow log data can be published to an CloudWatch Logs log group or an Amazon S3 bucket. If the flow log publishes to CloudWatch Logs, this element indicates the Amazon Resource Name (ARN) of the CloudWatch Logs log group to which the data is published. If the flow log publishes to Amazon S3, this element indicates the ARN of the Amazon S3 bucket to which the data is published.
    logDestination :: Lude.Maybe Lude.Text,
    -- | The status of the logs delivery (@SUCCESS@ | @FAILED@ ).
    deliverLogsStatus :: Lude.Maybe Lude.Text,
    -- | Information about the error that occurred. @Rate limited@ indicates that CloudWatch Logs throttling has been applied for one or more network interfaces, or that you've reached the limit on the number of log groups that you can create. @Access error@ indicates that the IAM role associated with the flow log does not have sufficient permissions to publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
    deliverLogsErrorMessage :: Lude.Maybe Lude.Text,
    -- | The name of the flow log group.
    logGroupName :: Lude.Maybe Lude.Text,
    -- | The ARN of the IAM role that posts logs to CloudWatch Logs.
    deliverLogsPermissionARN :: Lude.Maybe Lude.Text,
    -- | Specifies the type of destination to which the flow log data is published. Flow log data can be published to CloudWatch Logs or Amazon S3.
    logDestinationType :: Lude.Maybe LogDestinationType,
    -- | The flow log ID.
    flowLogId :: Lude.Maybe Lude.Text,
    -- | The tags for the flow log.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FlowLog' with the minimum fields required to make a request.
--
-- * 'creationTime' - The date and time the flow log was created.
-- * 'logFormat' - The format of the flow log record.
-- * 'maxAggregationInterval' - The maximum interval of time, in seconds, during which a flow of packets is captured and aggregated into a flow log record.
--
-- When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds (1 minute) or less, regardless of the specified value.
-- Valid Values: @60@ | @600@
-- * 'resourceId' - The ID of the resource on which the flow log was created.
-- * 'flowLogStatus' - The status of the flow log (@ACTIVE@ ).
-- * 'trafficType' - The type of traffic captured for the flow log.
-- * 'logDestination' - Specifies the destination to which the flow log data is published. Flow log data can be published to an CloudWatch Logs log group or an Amazon S3 bucket. If the flow log publishes to CloudWatch Logs, this element indicates the Amazon Resource Name (ARN) of the CloudWatch Logs log group to which the data is published. If the flow log publishes to Amazon S3, this element indicates the ARN of the Amazon S3 bucket to which the data is published.
-- * 'deliverLogsStatus' - The status of the logs delivery (@SUCCESS@ | @FAILED@ ).
-- * 'deliverLogsErrorMessage' - Information about the error that occurred. @Rate limited@ indicates that CloudWatch Logs throttling has been applied for one or more network interfaces, or that you've reached the limit on the number of log groups that you can create. @Access error@ indicates that the IAM role associated with the flow log does not have sufficient permissions to publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
-- * 'logGroupName' - The name of the flow log group.
-- * 'deliverLogsPermissionARN' - The ARN of the IAM role that posts logs to CloudWatch Logs.
-- * 'logDestinationType' - Specifies the type of destination to which the flow log data is published. Flow log data can be published to CloudWatch Logs or Amazon S3.
-- * 'flowLogId' - The flow log ID.
-- * 'tags' - The tags for the flow log.
mkFlowLog ::
  FlowLog
mkFlowLog =
  FlowLog'
    { creationTime = Lude.Nothing,
      logFormat = Lude.Nothing,
      maxAggregationInterval = Lude.Nothing,
      resourceId = Lude.Nothing,
      flowLogStatus = Lude.Nothing,
      trafficType = Lude.Nothing,
      logDestination = Lude.Nothing,
      deliverLogsStatus = Lude.Nothing,
      deliverLogsErrorMessage = Lude.Nothing,
      logGroupName = Lude.Nothing,
      deliverLogsPermissionARN = Lude.Nothing,
      logDestinationType = Lude.Nothing,
      flowLogId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The date and time the flow log was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flCreationTime :: Lens.Lens' FlowLog (Lude.Maybe Lude.DateTime)
flCreationTime = Lens.lens (creationTime :: FlowLog -> Lude.Maybe Lude.DateTime) (\s a -> s {creationTime = a} :: FlowLog)
{-# DEPRECATED flCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The format of the flow log record.
--
-- /Note:/ Consider using 'logFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flLogFormat :: Lens.Lens' FlowLog (Lude.Maybe Lude.Text)
flLogFormat = Lens.lens (logFormat :: FlowLog -> Lude.Maybe Lude.Text) (\s a -> s {logFormat = a} :: FlowLog)
{-# DEPRECATED flLogFormat "Use generic-lens or generic-optics with 'logFormat' instead." #-}

-- | The maximum interval of time, in seconds, during which a flow of packets is captured and aggregated into a flow log record.
--
-- When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds (1 minute) or less, regardless of the specified value.
-- Valid Values: @60@ | @600@
--
-- /Note:/ Consider using 'maxAggregationInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flMaxAggregationInterval :: Lens.Lens' FlowLog (Lude.Maybe Lude.Int)
flMaxAggregationInterval = Lens.lens (maxAggregationInterval :: FlowLog -> Lude.Maybe Lude.Int) (\s a -> s {maxAggregationInterval = a} :: FlowLog)
{-# DEPRECATED flMaxAggregationInterval "Use generic-lens or generic-optics with 'maxAggregationInterval' instead." #-}

-- | The ID of the resource on which the flow log was created.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flResourceId :: Lens.Lens' FlowLog (Lude.Maybe Lude.Text)
flResourceId = Lens.lens (resourceId :: FlowLog -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: FlowLog)
{-# DEPRECATED flResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The status of the flow log (@ACTIVE@ ).
--
-- /Note:/ Consider using 'flowLogStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flFlowLogStatus :: Lens.Lens' FlowLog (Lude.Maybe Lude.Text)
flFlowLogStatus = Lens.lens (flowLogStatus :: FlowLog -> Lude.Maybe Lude.Text) (\s a -> s {flowLogStatus = a} :: FlowLog)
{-# DEPRECATED flFlowLogStatus "Use generic-lens or generic-optics with 'flowLogStatus' instead." #-}

-- | The type of traffic captured for the flow log.
--
-- /Note:/ Consider using 'trafficType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flTrafficType :: Lens.Lens' FlowLog (Lude.Maybe TrafficType)
flTrafficType = Lens.lens (trafficType :: FlowLog -> Lude.Maybe TrafficType) (\s a -> s {trafficType = a} :: FlowLog)
{-# DEPRECATED flTrafficType "Use generic-lens or generic-optics with 'trafficType' instead." #-}

-- | Specifies the destination to which the flow log data is published. Flow log data can be published to an CloudWatch Logs log group or an Amazon S3 bucket. If the flow log publishes to CloudWatch Logs, this element indicates the Amazon Resource Name (ARN) of the CloudWatch Logs log group to which the data is published. If the flow log publishes to Amazon S3, this element indicates the ARN of the Amazon S3 bucket to which the data is published.
--
-- /Note:/ Consider using 'logDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flLogDestination :: Lens.Lens' FlowLog (Lude.Maybe Lude.Text)
flLogDestination = Lens.lens (logDestination :: FlowLog -> Lude.Maybe Lude.Text) (\s a -> s {logDestination = a} :: FlowLog)
{-# DEPRECATED flLogDestination "Use generic-lens or generic-optics with 'logDestination' instead." #-}

-- | The status of the logs delivery (@SUCCESS@ | @FAILED@ ).
--
-- /Note:/ Consider using 'deliverLogsStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flDeliverLogsStatus :: Lens.Lens' FlowLog (Lude.Maybe Lude.Text)
flDeliverLogsStatus = Lens.lens (deliverLogsStatus :: FlowLog -> Lude.Maybe Lude.Text) (\s a -> s {deliverLogsStatus = a} :: FlowLog)
{-# DEPRECATED flDeliverLogsStatus "Use generic-lens or generic-optics with 'deliverLogsStatus' instead." #-}

-- | Information about the error that occurred. @Rate limited@ indicates that CloudWatch Logs throttling has been applied for one or more network interfaces, or that you've reached the limit on the number of log groups that you can create. @Access error@ indicates that the IAM role associated with the flow log does not have sufficient permissions to publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
--
-- /Note:/ Consider using 'deliverLogsErrorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flDeliverLogsErrorMessage :: Lens.Lens' FlowLog (Lude.Maybe Lude.Text)
flDeliverLogsErrorMessage = Lens.lens (deliverLogsErrorMessage :: FlowLog -> Lude.Maybe Lude.Text) (\s a -> s {deliverLogsErrorMessage = a} :: FlowLog)
{-# DEPRECATED flDeliverLogsErrorMessage "Use generic-lens or generic-optics with 'deliverLogsErrorMessage' instead." #-}

-- | The name of the flow log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flLogGroupName :: Lens.Lens' FlowLog (Lude.Maybe Lude.Text)
flLogGroupName = Lens.lens (logGroupName :: FlowLog -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: FlowLog)
{-# DEPRECATED flLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The ARN of the IAM role that posts logs to CloudWatch Logs.
--
-- /Note:/ Consider using 'deliverLogsPermissionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flDeliverLogsPermissionARN :: Lens.Lens' FlowLog (Lude.Maybe Lude.Text)
flDeliverLogsPermissionARN = Lens.lens (deliverLogsPermissionARN :: FlowLog -> Lude.Maybe Lude.Text) (\s a -> s {deliverLogsPermissionARN = a} :: FlowLog)
{-# DEPRECATED flDeliverLogsPermissionARN "Use generic-lens or generic-optics with 'deliverLogsPermissionARN' instead." #-}

-- | Specifies the type of destination to which the flow log data is published. Flow log data can be published to CloudWatch Logs or Amazon S3.
--
-- /Note:/ Consider using 'logDestinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flLogDestinationType :: Lens.Lens' FlowLog (Lude.Maybe LogDestinationType)
flLogDestinationType = Lens.lens (logDestinationType :: FlowLog -> Lude.Maybe LogDestinationType) (\s a -> s {logDestinationType = a} :: FlowLog)
{-# DEPRECATED flLogDestinationType "Use generic-lens or generic-optics with 'logDestinationType' instead." #-}

-- | The flow log ID.
--
-- /Note:/ Consider using 'flowLogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flFlowLogId :: Lens.Lens' FlowLog (Lude.Maybe Lude.Text)
flFlowLogId = Lens.lens (flowLogId :: FlowLog -> Lude.Maybe Lude.Text) (\s a -> s {flowLogId = a} :: FlowLog)
{-# DEPRECATED flFlowLogId "Use generic-lens or generic-optics with 'flowLogId' instead." #-}

-- | The tags for the flow log.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flTags :: Lens.Lens' FlowLog (Lude.Maybe [Tag])
flTags = Lens.lens (tags :: FlowLog -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: FlowLog)
{-# DEPRECATED flTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML FlowLog where
  parseXML x =
    FlowLog'
      Lude.<$> (x Lude..@? "creationTime")
      Lude.<*> (x Lude..@? "logFormat")
      Lude.<*> (x Lude..@? "maxAggregationInterval")
      Lude.<*> (x Lude..@? "resourceId")
      Lude.<*> (x Lude..@? "flowLogStatus")
      Lude.<*> (x Lude..@? "trafficType")
      Lude.<*> (x Lude..@? "logDestination")
      Lude.<*> (x Lude..@? "deliverLogsStatus")
      Lude.<*> (x Lude..@? "deliverLogsErrorMessage")
      Lude.<*> (x Lude..@? "logGroupName")
      Lude.<*> (x Lude..@? "deliverLogsPermissionArn")
      Lude.<*> (x Lude..@? "logDestinationType")
      Lude.<*> (x Lude..@? "flowLogId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
