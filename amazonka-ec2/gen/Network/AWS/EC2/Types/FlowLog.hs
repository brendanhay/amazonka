{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FlowLog
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FlowLog where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LogDestinationType
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TrafficType
import qualified Network.AWS.Lens as Lens

-- | Describes a flow log.
--
-- /See:/ 'newFlowLog' smart constructor.
data FlowLog = FlowLog'
  { -- | The ID of the resource on which the flow log was created.
    resourceId :: Core.Maybe Core.Text,
    -- | The maximum interval of time, in seconds, during which a flow of packets
    -- is captured and aggregated into a flow log record.
    --
    -- When a network interface is attached to a
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance>,
    -- the aggregation interval is always 60 seconds (1 minute) or less,
    -- regardless of the specified value.
    --
    -- Valid Values: @60@ | @600@
    maxAggregationInterval :: Core.Maybe Core.Int,
    -- | The date and time the flow log was created.
    creationTime :: Core.Maybe Core.ISO8601,
    -- | The status of the logs delivery (@SUCCESS@ | @FAILED@).
    deliverLogsStatus :: Core.Maybe Core.Text,
    -- | The flow log ID.
    flowLogId :: Core.Maybe Core.Text,
    -- | Specifies the destination to which the flow log data is published. Flow
    -- log data can be published to an CloudWatch Logs log group or an Amazon
    -- S3 bucket. If the flow log publishes to CloudWatch Logs, this element
    -- indicates the Amazon Resource Name (ARN) of the CloudWatch Logs log
    -- group to which the data is published. If the flow log publishes to
    -- Amazon S3, this element indicates the ARN of the Amazon S3 bucket to
    -- which the data is published.
    logDestination :: Core.Maybe Core.Text,
    -- | The type of traffic captured for the flow log.
    trafficType :: Core.Maybe TrafficType,
    -- | The format of the flow log record.
    logFormat :: Core.Maybe Core.Text,
    -- | The name of the flow log group.
    logGroupName :: Core.Maybe Core.Text,
    -- | The ARN of the IAM role that posts logs to CloudWatch Logs.
    deliverLogsPermissionArn :: Core.Maybe Core.Text,
    -- | Information about the error that occurred. @Rate limited@ indicates that
    -- CloudWatch Logs throttling has been applied for one or more network
    -- interfaces, or that you\'ve reached the limit on the number of log
    -- groups that you can create. @Access error@ indicates that the IAM role
    -- associated with the flow log does not have sufficient permissions to
    -- publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
    deliverLogsErrorMessage :: Core.Maybe Core.Text,
    -- | The tags for the flow log.
    tags :: Core.Maybe [Tag],
    -- | Specifies the type of destination to which the flow log data is
    -- published. Flow log data can be published to CloudWatch Logs or Amazon
    -- S3.
    logDestinationType :: Core.Maybe LogDestinationType,
    -- | The status of the flow log (@ACTIVE@).
    flowLogStatus :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FlowLog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'flowLog_resourceId' - The ID of the resource on which the flow log was created.
--
-- 'maxAggregationInterval', 'flowLog_maxAggregationInterval' - The maximum interval of time, in seconds, during which a flow of packets
-- is captured and aggregated into a flow log record.
--
-- When a network interface is attached to a
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance>,
-- the aggregation interval is always 60 seconds (1 minute) or less,
-- regardless of the specified value.
--
-- Valid Values: @60@ | @600@
--
-- 'creationTime', 'flowLog_creationTime' - The date and time the flow log was created.
--
-- 'deliverLogsStatus', 'flowLog_deliverLogsStatus' - The status of the logs delivery (@SUCCESS@ | @FAILED@).
--
-- 'flowLogId', 'flowLog_flowLogId' - The flow log ID.
--
-- 'logDestination', 'flowLog_logDestination' - Specifies the destination to which the flow log data is published. Flow
-- log data can be published to an CloudWatch Logs log group or an Amazon
-- S3 bucket. If the flow log publishes to CloudWatch Logs, this element
-- indicates the Amazon Resource Name (ARN) of the CloudWatch Logs log
-- group to which the data is published. If the flow log publishes to
-- Amazon S3, this element indicates the ARN of the Amazon S3 bucket to
-- which the data is published.
--
-- 'trafficType', 'flowLog_trafficType' - The type of traffic captured for the flow log.
--
-- 'logFormat', 'flowLog_logFormat' - The format of the flow log record.
--
-- 'logGroupName', 'flowLog_logGroupName' - The name of the flow log group.
--
-- 'deliverLogsPermissionArn', 'flowLog_deliverLogsPermissionArn' - The ARN of the IAM role that posts logs to CloudWatch Logs.
--
-- 'deliverLogsErrorMessage', 'flowLog_deliverLogsErrorMessage' - Information about the error that occurred. @Rate limited@ indicates that
-- CloudWatch Logs throttling has been applied for one or more network
-- interfaces, or that you\'ve reached the limit on the number of log
-- groups that you can create. @Access error@ indicates that the IAM role
-- associated with the flow log does not have sufficient permissions to
-- publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
--
-- 'tags', 'flowLog_tags' - The tags for the flow log.
--
-- 'logDestinationType', 'flowLog_logDestinationType' - Specifies the type of destination to which the flow log data is
-- published. Flow log data can be published to CloudWatch Logs or Amazon
-- S3.
--
-- 'flowLogStatus', 'flowLog_flowLogStatus' - The status of the flow log (@ACTIVE@).
newFlowLog ::
  FlowLog
newFlowLog =
  FlowLog'
    { resourceId = Core.Nothing,
      maxAggregationInterval = Core.Nothing,
      creationTime = Core.Nothing,
      deliverLogsStatus = Core.Nothing,
      flowLogId = Core.Nothing,
      logDestination = Core.Nothing,
      trafficType = Core.Nothing,
      logFormat = Core.Nothing,
      logGroupName = Core.Nothing,
      deliverLogsPermissionArn = Core.Nothing,
      deliverLogsErrorMessage = Core.Nothing,
      tags = Core.Nothing,
      logDestinationType = Core.Nothing,
      flowLogStatus = Core.Nothing
    }

-- | The ID of the resource on which the flow log was created.
flowLog_resourceId :: Lens.Lens' FlowLog (Core.Maybe Core.Text)
flowLog_resourceId = Lens.lens (\FlowLog' {resourceId} -> resourceId) (\s@FlowLog' {} a -> s {resourceId = a} :: FlowLog)

-- | The maximum interval of time, in seconds, during which a flow of packets
-- is captured and aggregated into a flow log record.
--
-- When a network interface is attached to a
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance>,
-- the aggregation interval is always 60 seconds (1 minute) or less,
-- regardless of the specified value.
--
-- Valid Values: @60@ | @600@
flowLog_maxAggregationInterval :: Lens.Lens' FlowLog (Core.Maybe Core.Int)
flowLog_maxAggregationInterval = Lens.lens (\FlowLog' {maxAggregationInterval} -> maxAggregationInterval) (\s@FlowLog' {} a -> s {maxAggregationInterval = a} :: FlowLog)

-- | The date and time the flow log was created.
flowLog_creationTime :: Lens.Lens' FlowLog (Core.Maybe Core.UTCTime)
flowLog_creationTime = Lens.lens (\FlowLog' {creationTime} -> creationTime) (\s@FlowLog' {} a -> s {creationTime = a} :: FlowLog) Core.. Lens.mapping Core._Time

-- | The status of the logs delivery (@SUCCESS@ | @FAILED@).
flowLog_deliverLogsStatus :: Lens.Lens' FlowLog (Core.Maybe Core.Text)
flowLog_deliverLogsStatus = Lens.lens (\FlowLog' {deliverLogsStatus} -> deliverLogsStatus) (\s@FlowLog' {} a -> s {deliverLogsStatus = a} :: FlowLog)

-- | The flow log ID.
flowLog_flowLogId :: Lens.Lens' FlowLog (Core.Maybe Core.Text)
flowLog_flowLogId = Lens.lens (\FlowLog' {flowLogId} -> flowLogId) (\s@FlowLog' {} a -> s {flowLogId = a} :: FlowLog)

-- | Specifies the destination to which the flow log data is published. Flow
-- log data can be published to an CloudWatch Logs log group or an Amazon
-- S3 bucket. If the flow log publishes to CloudWatch Logs, this element
-- indicates the Amazon Resource Name (ARN) of the CloudWatch Logs log
-- group to which the data is published. If the flow log publishes to
-- Amazon S3, this element indicates the ARN of the Amazon S3 bucket to
-- which the data is published.
flowLog_logDestination :: Lens.Lens' FlowLog (Core.Maybe Core.Text)
flowLog_logDestination = Lens.lens (\FlowLog' {logDestination} -> logDestination) (\s@FlowLog' {} a -> s {logDestination = a} :: FlowLog)

-- | The type of traffic captured for the flow log.
flowLog_trafficType :: Lens.Lens' FlowLog (Core.Maybe TrafficType)
flowLog_trafficType = Lens.lens (\FlowLog' {trafficType} -> trafficType) (\s@FlowLog' {} a -> s {trafficType = a} :: FlowLog)

-- | The format of the flow log record.
flowLog_logFormat :: Lens.Lens' FlowLog (Core.Maybe Core.Text)
flowLog_logFormat = Lens.lens (\FlowLog' {logFormat} -> logFormat) (\s@FlowLog' {} a -> s {logFormat = a} :: FlowLog)

-- | The name of the flow log group.
flowLog_logGroupName :: Lens.Lens' FlowLog (Core.Maybe Core.Text)
flowLog_logGroupName = Lens.lens (\FlowLog' {logGroupName} -> logGroupName) (\s@FlowLog' {} a -> s {logGroupName = a} :: FlowLog)

-- | The ARN of the IAM role that posts logs to CloudWatch Logs.
flowLog_deliverLogsPermissionArn :: Lens.Lens' FlowLog (Core.Maybe Core.Text)
flowLog_deliverLogsPermissionArn = Lens.lens (\FlowLog' {deliverLogsPermissionArn} -> deliverLogsPermissionArn) (\s@FlowLog' {} a -> s {deliverLogsPermissionArn = a} :: FlowLog)

-- | Information about the error that occurred. @Rate limited@ indicates that
-- CloudWatch Logs throttling has been applied for one or more network
-- interfaces, or that you\'ve reached the limit on the number of log
-- groups that you can create. @Access error@ indicates that the IAM role
-- associated with the flow log does not have sufficient permissions to
-- publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
flowLog_deliverLogsErrorMessage :: Lens.Lens' FlowLog (Core.Maybe Core.Text)
flowLog_deliverLogsErrorMessage = Lens.lens (\FlowLog' {deliverLogsErrorMessage} -> deliverLogsErrorMessage) (\s@FlowLog' {} a -> s {deliverLogsErrorMessage = a} :: FlowLog)

-- | The tags for the flow log.
flowLog_tags :: Lens.Lens' FlowLog (Core.Maybe [Tag])
flowLog_tags = Lens.lens (\FlowLog' {tags} -> tags) (\s@FlowLog' {} a -> s {tags = a} :: FlowLog) Core.. Lens.mapping Lens._Coerce

-- | Specifies the type of destination to which the flow log data is
-- published. Flow log data can be published to CloudWatch Logs or Amazon
-- S3.
flowLog_logDestinationType :: Lens.Lens' FlowLog (Core.Maybe LogDestinationType)
flowLog_logDestinationType = Lens.lens (\FlowLog' {logDestinationType} -> logDestinationType) (\s@FlowLog' {} a -> s {logDestinationType = a} :: FlowLog)

-- | The status of the flow log (@ACTIVE@).
flowLog_flowLogStatus :: Lens.Lens' FlowLog (Core.Maybe Core.Text)
flowLog_flowLogStatus = Lens.lens (\FlowLog' {flowLogStatus} -> flowLogStatus) (\s@FlowLog' {} a -> s {flowLogStatus = a} :: FlowLog)

instance Core.FromXML FlowLog where
  parseXML x =
    FlowLog'
      Core.<$> (x Core..@? "resourceId")
      Core.<*> (x Core..@? "maxAggregationInterval")
      Core.<*> (x Core..@? "creationTime")
      Core.<*> (x Core..@? "deliverLogsStatus")
      Core.<*> (x Core..@? "flowLogId")
      Core.<*> (x Core..@? "logDestination")
      Core.<*> (x Core..@? "trafficType")
      Core.<*> (x Core..@? "logFormat")
      Core.<*> (x Core..@? "logGroupName")
      Core.<*> (x Core..@? "deliverLogsPermissionArn")
      Core.<*> (x Core..@? "deliverLogsErrorMessage")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "logDestinationType")
      Core.<*> (x Core..@? "flowLogStatus")

instance Core.Hashable FlowLog

instance Core.NFData FlowLog
