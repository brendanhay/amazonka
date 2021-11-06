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
-- Module      : Amazonka.EC2.Types.FlowLog
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FlowLog where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DestinationOptionsResponse
import Amazonka.EC2.Types.LogDestinationType
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TrafficType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a flow log.
--
-- /See:/ 'newFlowLog' smart constructor.
data FlowLog = FlowLog'
  { -- | The date and time the flow log was created.
    creationTime :: Prelude.Maybe Core.ISO8601,
    -- | The format of the flow log record.
    logFormat :: Prelude.Maybe Prelude.Text,
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
    -- | The ID of the resource on which the flow log was created.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The status of the flow log (@ACTIVE@).
    flowLogStatus :: Prelude.Maybe Prelude.Text,
    -- | The type of traffic captured for the flow log.
    trafficType :: Prelude.Maybe TrafficType,
    -- | The destination to which the flow log data is published. Flow log data
    -- can be published to an CloudWatch Logs log group or an Amazon S3 bucket.
    -- If the flow log publishes to CloudWatch Logs, this element indicates the
    -- Amazon Resource Name (ARN) of the CloudWatch Logs log group to which the
    -- data is published. If the flow log publishes to Amazon S3, this element
    -- indicates the ARN of the Amazon S3 bucket to which the data is
    -- published.
    logDestination :: Prelude.Maybe Prelude.Text,
    -- | The status of the logs delivery (@SUCCESS@ | @FAILED@).
    deliverLogsStatus :: Prelude.Maybe Prelude.Text,
    -- | Information about the error that occurred. @Rate limited@ indicates that
    -- CloudWatch Logs throttling has been applied for one or more network
    -- interfaces, or that you\'ve reached the limit on the number of log
    -- groups that you can create. @Access error@ indicates that the IAM role
    -- associated with the flow log does not have sufficient permissions to
    -- publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
    deliverLogsErrorMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the flow log group.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The destination options.
    destinationOptions :: Prelude.Maybe DestinationOptionsResponse,
    -- | The ARN of the IAM role that posts logs to CloudWatch Logs.
    deliverLogsPermissionArn :: Prelude.Maybe Prelude.Text,
    -- | The type of destination to which the flow log data is published. Flow
    -- log data can be published to CloudWatch Logs or Amazon S3.
    logDestinationType :: Prelude.Maybe LogDestinationType,
    -- | The flow log ID.
    flowLogId :: Prelude.Maybe Prelude.Text,
    -- | The tags for the flow log.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlowLog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'flowLog_creationTime' - The date and time the flow log was created.
--
-- 'logFormat', 'flowLog_logFormat' - The format of the flow log record.
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
-- 'resourceId', 'flowLog_resourceId' - The ID of the resource on which the flow log was created.
--
-- 'flowLogStatus', 'flowLog_flowLogStatus' - The status of the flow log (@ACTIVE@).
--
-- 'trafficType', 'flowLog_trafficType' - The type of traffic captured for the flow log.
--
-- 'logDestination', 'flowLog_logDestination' - The destination to which the flow log data is published. Flow log data
-- can be published to an CloudWatch Logs log group or an Amazon S3 bucket.
-- If the flow log publishes to CloudWatch Logs, this element indicates the
-- Amazon Resource Name (ARN) of the CloudWatch Logs log group to which the
-- data is published. If the flow log publishes to Amazon S3, this element
-- indicates the ARN of the Amazon S3 bucket to which the data is
-- published.
--
-- 'deliverLogsStatus', 'flowLog_deliverLogsStatus' - The status of the logs delivery (@SUCCESS@ | @FAILED@).
--
-- 'deliverLogsErrorMessage', 'flowLog_deliverLogsErrorMessage' - Information about the error that occurred. @Rate limited@ indicates that
-- CloudWatch Logs throttling has been applied for one or more network
-- interfaces, or that you\'ve reached the limit on the number of log
-- groups that you can create. @Access error@ indicates that the IAM role
-- associated with the flow log does not have sufficient permissions to
-- publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
--
-- 'logGroupName', 'flowLog_logGroupName' - The name of the flow log group.
--
-- 'destinationOptions', 'flowLog_destinationOptions' - The destination options.
--
-- 'deliverLogsPermissionArn', 'flowLog_deliverLogsPermissionArn' - The ARN of the IAM role that posts logs to CloudWatch Logs.
--
-- 'logDestinationType', 'flowLog_logDestinationType' - The type of destination to which the flow log data is published. Flow
-- log data can be published to CloudWatch Logs or Amazon S3.
--
-- 'flowLogId', 'flowLog_flowLogId' - The flow log ID.
--
-- 'tags', 'flowLog_tags' - The tags for the flow log.
newFlowLog ::
  FlowLog
newFlowLog =
  FlowLog'
    { creationTime = Prelude.Nothing,
      logFormat = Prelude.Nothing,
      maxAggregationInterval = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      flowLogStatus = Prelude.Nothing,
      trafficType = Prelude.Nothing,
      logDestination = Prelude.Nothing,
      deliverLogsStatus = Prelude.Nothing,
      deliverLogsErrorMessage = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      destinationOptions = Prelude.Nothing,
      deliverLogsPermissionArn = Prelude.Nothing,
      logDestinationType = Prelude.Nothing,
      flowLogId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The date and time the flow log was created.
flowLog_creationTime :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.UTCTime)
flowLog_creationTime = Lens.lens (\FlowLog' {creationTime} -> creationTime) (\s@FlowLog' {} a -> s {creationTime = a} :: FlowLog) Prelude.. Lens.mapping Core._Time

-- | The format of the flow log record.
flowLog_logFormat :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_logFormat = Lens.lens (\FlowLog' {logFormat} -> logFormat) (\s@FlowLog' {} a -> s {logFormat = a} :: FlowLog)

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

-- | The ID of the resource on which the flow log was created.
flowLog_resourceId :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_resourceId = Lens.lens (\FlowLog' {resourceId} -> resourceId) (\s@FlowLog' {} a -> s {resourceId = a} :: FlowLog)

-- | The status of the flow log (@ACTIVE@).
flowLog_flowLogStatus :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_flowLogStatus = Lens.lens (\FlowLog' {flowLogStatus} -> flowLogStatus) (\s@FlowLog' {} a -> s {flowLogStatus = a} :: FlowLog)

-- | The type of traffic captured for the flow log.
flowLog_trafficType :: Lens.Lens' FlowLog (Prelude.Maybe TrafficType)
flowLog_trafficType = Lens.lens (\FlowLog' {trafficType} -> trafficType) (\s@FlowLog' {} a -> s {trafficType = a} :: FlowLog)

-- | The destination to which the flow log data is published. Flow log data
-- can be published to an CloudWatch Logs log group or an Amazon S3 bucket.
-- If the flow log publishes to CloudWatch Logs, this element indicates the
-- Amazon Resource Name (ARN) of the CloudWatch Logs log group to which the
-- data is published. If the flow log publishes to Amazon S3, this element
-- indicates the ARN of the Amazon S3 bucket to which the data is
-- published.
flowLog_logDestination :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_logDestination = Lens.lens (\FlowLog' {logDestination} -> logDestination) (\s@FlowLog' {} a -> s {logDestination = a} :: FlowLog)

-- | The status of the logs delivery (@SUCCESS@ | @FAILED@).
flowLog_deliverLogsStatus :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_deliverLogsStatus = Lens.lens (\FlowLog' {deliverLogsStatus} -> deliverLogsStatus) (\s@FlowLog' {} a -> s {deliverLogsStatus = a} :: FlowLog)

-- | Information about the error that occurred. @Rate limited@ indicates that
-- CloudWatch Logs throttling has been applied for one or more network
-- interfaces, or that you\'ve reached the limit on the number of log
-- groups that you can create. @Access error@ indicates that the IAM role
-- associated with the flow log does not have sufficient permissions to
-- publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
flowLog_deliverLogsErrorMessage :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_deliverLogsErrorMessage = Lens.lens (\FlowLog' {deliverLogsErrorMessage} -> deliverLogsErrorMessage) (\s@FlowLog' {} a -> s {deliverLogsErrorMessage = a} :: FlowLog)

-- | The name of the flow log group.
flowLog_logGroupName :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_logGroupName = Lens.lens (\FlowLog' {logGroupName} -> logGroupName) (\s@FlowLog' {} a -> s {logGroupName = a} :: FlowLog)

-- | The destination options.
flowLog_destinationOptions :: Lens.Lens' FlowLog (Prelude.Maybe DestinationOptionsResponse)
flowLog_destinationOptions = Lens.lens (\FlowLog' {destinationOptions} -> destinationOptions) (\s@FlowLog' {} a -> s {destinationOptions = a} :: FlowLog)

-- | The ARN of the IAM role that posts logs to CloudWatch Logs.
flowLog_deliverLogsPermissionArn :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_deliverLogsPermissionArn = Lens.lens (\FlowLog' {deliverLogsPermissionArn} -> deliverLogsPermissionArn) (\s@FlowLog' {} a -> s {deliverLogsPermissionArn = a} :: FlowLog)

-- | The type of destination to which the flow log data is published. Flow
-- log data can be published to CloudWatch Logs or Amazon S3.
flowLog_logDestinationType :: Lens.Lens' FlowLog (Prelude.Maybe LogDestinationType)
flowLog_logDestinationType = Lens.lens (\FlowLog' {logDestinationType} -> logDestinationType) (\s@FlowLog' {} a -> s {logDestinationType = a} :: FlowLog)

-- | The flow log ID.
flowLog_flowLogId :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_flowLogId = Lens.lens (\FlowLog' {flowLogId} -> flowLogId) (\s@FlowLog' {} a -> s {flowLogId = a} :: FlowLog)

-- | The tags for the flow log.
flowLog_tags :: Lens.Lens' FlowLog (Prelude.Maybe [Tag])
flowLog_tags = Lens.lens (\FlowLog' {tags} -> tags) (\s@FlowLog' {} a -> s {tags = a} :: FlowLog) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML FlowLog where
  parseXML x =
    FlowLog'
      Prelude.<$> (x Core..@? "creationTime")
      Prelude.<*> (x Core..@? "logFormat")
      Prelude.<*> (x Core..@? "maxAggregationInterval")
      Prelude.<*> (x Core..@? "resourceId")
      Prelude.<*> (x Core..@? "flowLogStatus")
      Prelude.<*> (x Core..@? "trafficType")
      Prelude.<*> (x Core..@? "logDestination")
      Prelude.<*> (x Core..@? "deliverLogsStatus")
      Prelude.<*> (x Core..@? "deliverLogsErrorMessage")
      Prelude.<*> (x Core..@? "logGroupName")
      Prelude.<*> (x Core..@? "destinationOptions")
      Prelude.<*> (x Core..@? "deliverLogsPermissionArn")
      Prelude.<*> (x Core..@? "logDestinationType")
      Prelude.<*> (x Core..@? "flowLogId")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable FlowLog

instance Prelude.NFData FlowLog
