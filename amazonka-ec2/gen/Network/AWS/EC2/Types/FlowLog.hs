{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LogDestinationType
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TrafficType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a flow log.
--
-- /See:/ 'newFlowLog' smart constructor.
data FlowLog = FlowLog'
  { -- | The ID of the resource on which the flow log was created.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The maximum interval of time, in seconds, during which a flow of packets
    -- is captured and aggregated into a flow log record.
    --
    -- When a network interface is attached to a
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance>,
    -- the aggregation interval is always 60 seconds (1 minute) or less,
    -- regardless of the specified value.
    --
    -- Valid Values: @60@ | @600@
    maxAggregationInterval :: Prelude.Maybe Prelude.Int,
    -- | The date and time the flow log was created.
    creationTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The status of the logs delivery (@SUCCESS@ | @FAILED@).
    deliverLogsStatus :: Prelude.Maybe Prelude.Text,
    -- | The flow log ID.
    flowLogId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the destination to which the flow log data is published. Flow
    -- log data can be published to an CloudWatch Logs log group or an Amazon
    -- S3 bucket. If the flow log publishes to CloudWatch Logs, this element
    -- indicates the Amazon Resource Name (ARN) of the CloudWatch Logs log
    -- group to which the data is published. If the flow log publishes to
    -- Amazon S3, this element indicates the ARN of the Amazon S3 bucket to
    -- which the data is published.
    logDestination :: Prelude.Maybe Prelude.Text,
    -- | The type of traffic captured for the flow log.
    trafficType :: Prelude.Maybe TrafficType,
    -- | The format of the flow log record.
    logFormat :: Prelude.Maybe Prelude.Text,
    -- | The name of the flow log group.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that posts logs to CloudWatch Logs.
    deliverLogsPermissionArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the error that occurred. @Rate limited@ indicates that
    -- CloudWatch Logs throttling has been applied for one or more network
    -- interfaces, or that you\'ve reached the limit on the number of log
    -- groups that you can create. @Access error@ indicates that the IAM role
    -- associated with the flow log does not have sufficient permissions to
    -- publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
    deliverLogsErrorMessage :: Prelude.Maybe Prelude.Text,
    -- | The tags for the flow log.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies the type of destination to which the flow log data is
    -- published. Flow log data can be published to CloudWatch Logs or Amazon
    -- S3.
    logDestinationType :: Prelude.Maybe LogDestinationType,
    -- | The status of the flow log (@ACTIVE@).
    flowLogStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { resourceId = Prelude.Nothing,
      maxAggregationInterval = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      deliverLogsStatus = Prelude.Nothing,
      flowLogId = Prelude.Nothing,
      logDestination = Prelude.Nothing,
      trafficType = Prelude.Nothing,
      logFormat = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      deliverLogsPermissionArn = Prelude.Nothing,
      deliverLogsErrorMessage = Prelude.Nothing,
      tags = Prelude.Nothing,
      logDestinationType = Prelude.Nothing,
      flowLogStatus = Prelude.Nothing
    }

-- | The ID of the resource on which the flow log was created.
flowLog_resourceId :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
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
flowLog_maxAggregationInterval :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Int)
flowLog_maxAggregationInterval = Lens.lens (\FlowLog' {maxAggregationInterval} -> maxAggregationInterval) (\s@FlowLog' {} a -> s {maxAggregationInterval = a} :: FlowLog)

-- | The date and time the flow log was created.
flowLog_creationTime :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.UTCTime)
flowLog_creationTime = Lens.lens (\FlowLog' {creationTime} -> creationTime) (\s@FlowLog' {} a -> s {creationTime = a} :: FlowLog) Prelude.. Lens.mapping Prelude._Time

-- | The status of the logs delivery (@SUCCESS@ | @FAILED@).
flowLog_deliverLogsStatus :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_deliverLogsStatus = Lens.lens (\FlowLog' {deliverLogsStatus} -> deliverLogsStatus) (\s@FlowLog' {} a -> s {deliverLogsStatus = a} :: FlowLog)

-- | The flow log ID.
flowLog_flowLogId :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_flowLogId = Lens.lens (\FlowLog' {flowLogId} -> flowLogId) (\s@FlowLog' {} a -> s {flowLogId = a} :: FlowLog)

-- | Specifies the destination to which the flow log data is published. Flow
-- log data can be published to an CloudWatch Logs log group or an Amazon
-- S3 bucket. If the flow log publishes to CloudWatch Logs, this element
-- indicates the Amazon Resource Name (ARN) of the CloudWatch Logs log
-- group to which the data is published. If the flow log publishes to
-- Amazon S3, this element indicates the ARN of the Amazon S3 bucket to
-- which the data is published.
flowLog_logDestination :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_logDestination = Lens.lens (\FlowLog' {logDestination} -> logDestination) (\s@FlowLog' {} a -> s {logDestination = a} :: FlowLog)

-- | The type of traffic captured for the flow log.
flowLog_trafficType :: Lens.Lens' FlowLog (Prelude.Maybe TrafficType)
flowLog_trafficType = Lens.lens (\FlowLog' {trafficType} -> trafficType) (\s@FlowLog' {} a -> s {trafficType = a} :: FlowLog)

-- | The format of the flow log record.
flowLog_logFormat :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_logFormat = Lens.lens (\FlowLog' {logFormat} -> logFormat) (\s@FlowLog' {} a -> s {logFormat = a} :: FlowLog)

-- | The name of the flow log group.
flowLog_logGroupName :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_logGroupName = Lens.lens (\FlowLog' {logGroupName} -> logGroupName) (\s@FlowLog' {} a -> s {logGroupName = a} :: FlowLog)

-- | The ARN of the IAM role that posts logs to CloudWatch Logs.
flowLog_deliverLogsPermissionArn :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_deliverLogsPermissionArn = Lens.lens (\FlowLog' {deliverLogsPermissionArn} -> deliverLogsPermissionArn) (\s@FlowLog' {} a -> s {deliverLogsPermissionArn = a} :: FlowLog)

-- | Information about the error that occurred. @Rate limited@ indicates that
-- CloudWatch Logs throttling has been applied for one or more network
-- interfaces, or that you\'ve reached the limit on the number of log
-- groups that you can create. @Access error@ indicates that the IAM role
-- associated with the flow log does not have sufficient permissions to
-- publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
flowLog_deliverLogsErrorMessage :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_deliverLogsErrorMessage = Lens.lens (\FlowLog' {deliverLogsErrorMessage} -> deliverLogsErrorMessage) (\s@FlowLog' {} a -> s {deliverLogsErrorMessage = a} :: FlowLog)

-- | The tags for the flow log.
flowLog_tags :: Lens.Lens' FlowLog (Prelude.Maybe [Tag])
flowLog_tags = Lens.lens (\FlowLog' {tags} -> tags) (\s@FlowLog' {} a -> s {tags = a} :: FlowLog) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the type of destination to which the flow log data is
-- published. Flow log data can be published to CloudWatch Logs or Amazon
-- S3.
flowLog_logDestinationType :: Lens.Lens' FlowLog (Prelude.Maybe LogDestinationType)
flowLog_logDestinationType = Lens.lens (\FlowLog' {logDestinationType} -> logDestinationType) (\s@FlowLog' {} a -> s {logDestinationType = a} :: FlowLog)

-- | The status of the flow log (@ACTIVE@).
flowLog_flowLogStatus :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_flowLogStatus = Lens.lens (\FlowLog' {flowLogStatus} -> flowLogStatus) (\s@FlowLog' {} a -> s {flowLogStatus = a} :: FlowLog)

instance Prelude.FromXML FlowLog where
  parseXML x =
    FlowLog'
      Prelude.<$> (x Prelude..@? "resourceId")
      Prelude.<*> (x Prelude..@? "maxAggregationInterval")
      Prelude.<*> (x Prelude..@? "creationTime")
      Prelude.<*> (x Prelude..@? "deliverLogsStatus")
      Prelude.<*> (x Prelude..@? "flowLogId")
      Prelude.<*> (x Prelude..@? "logDestination")
      Prelude.<*> (x Prelude..@? "trafficType")
      Prelude.<*> (x Prelude..@? "logFormat")
      Prelude.<*> (x Prelude..@? "logGroupName")
      Prelude.<*> (x Prelude..@? "deliverLogsPermissionArn")
      Prelude.<*> (x Prelude..@? "deliverLogsErrorMessage")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "logDestinationType")
      Prelude.<*> (x Prelude..@? "flowLogStatus")

instance Prelude.Hashable FlowLog

instance Prelude.NFData FlowLog
