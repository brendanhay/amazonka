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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FlowLog where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DestinationOptionsResponse
import Amazonka.EC2.Types.LogDestinationType
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TrafficType
import qualified Amazonka.Prelude as Prelude

-- | Describes a flow log.
--
-- /See:/ 'newFlowLog' smart constructor.
data FlowLog = FlowLog'
  { -- | The date and time the flow log was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The ARN of the IAM role that allows the service to publish flow logs
    -- across accounts.
    deliverCrossAccountRole :: Prelude.Maybe Prelude.Text,
    -- | Information about the error that occurred. @Rate limited@ indicates that
    -- CloudWatch Logs throttling has been applied for one or more network
    -- interfaces, or that you\'ve reached the limit on the number of log
    -- groups that you can create. @Access error@ indicates that the IAM role
    -- associated with the flow log does not have sufficient permissions to
    -- publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
    deliverLogsErrorMessage :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role allows the service to publish logs to CloudWatch
    -- Logs.
    deliverLogsPermissionArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the logs delivery (@SUCCESS@ | @FAILED@).
    deliverLogsStatus :: Prelude.Maybe Prelude.Text,
    -- | The destination options.
    destinationOptions :: Prelude.Maybe DestinationOptionsResponse,
    -- | The ID of the flow log.
    flowLogId :: Prelude.Maybe Prelude.Text,
    -- | The status of the flow log (@ACTIVE@).
    flowLogStatus :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the destination for the flow log data.
    logDestination :: Prelude.Maybe Prelude.Text,
    -- | The type of destination for the flow log data.
    logDestinationType :: Prelude.Maybe LogDestinationType,
    -- | The format of the flow log record.
    logFormat :: Prelude.Maybe Prelude.Text,
    -- | The name of the flow log group.
    logGroupName :: Prelude.Maybe Prelude.Text,
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
    -- | The ID of the resource being monitored.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The tags for the flow log.
    tags :: Prelude.Maybe [Tag],
    -- | The type of traffic captured for the flow log.
    trafficType :: Prelude.Maybe TrafficType
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
-- 'deliverCrossAccountRole', 'flowLog_deliverCrossAccountRole' - The ARN of the IAM role that allows the service to publish flow logs
-- across accounts.
--
-- 'deliverLogsErrorMessage', 'flowLog_deliverLogsErrorMessage' - Information about the error that occurred. @Rate limited@ indicates that
-- CloudWatch Logs throttling has been applied for one or more network
-- interfaces, or that you\'ve reached the limit on the number of log
-- groups that you can create. @Access error@ indicates that the IAM role
-- associated with the flow log does not have sufficient permissions to
-- publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
--
-- 'deliverLogsPermissionArn', 'flowLog_deliverLogsPermissionArn' - The ARN of the IAM role allows the service to publish logs to CloudWatch
-- Logs.
--
-- 'deliverLogsStatus', 'flowLog_deliverLogsStatus' - The status of the logs delivery (@SUCCESS@ | @FAILED@).
--
-- 'destinationOptions', 'flowLog_destinationOptions' - The destination options.
--
-- 'flowLogId', 'flowLog_flowLogId' - The ID of the flow log.
--
-- 'flowLogStatus', 'flowLog_flowLogStatus' - The status of the flow log (@ACTIVE@).
--
-- 'logDestination', 'flowLog_logDestination' - The Amazon Resource Name (ARN) of the destination for the flow log data.
--
-- 'logDestinationType', 'flowLog_logDestinationType' - The type of destination for the flow log data.
--
-- 'logFormat', 'flowLog_logFormat' - The format of the flow log record.
--
-- 'logGroupName', 'flowLog_logGroupName' - The name of the flow log group.
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
-- 'resourceId', 'flowLog_resourceId' - The ID of the resource being monitored.
--
-- 'tags', 'flowLog_tags' - The tags for the flow log.
--
-- 'trafficType', 'flowLog_trafficType' - The type of traffic captured for the flow log.
newFlowLog ::
  FlowLog
newFlowLog =
  FlowLog'
    { creationTime = Prelude.Nothing,
      deliverCrossAccountRole = Prelude.Nothing,
      deliverLogsErrorMessage = Prelude.Nothing,
      deliverLogsPermissionArn = Prelude.Nothing,
      deliverLogsStatus = Prelude.Nothing,
      destinationOptions = Prelude.Nothing,
      flowLogId = Prelude.Nothing,
      flowLogStatus = Prelude.Nothing,
      logDestination = Prelude.Nothing,
      logDestinationType = Prelude.Nothing,
      logFormat = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      maxAggregationInterval = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      tags = Prelude.Nothing,
      trafficType = Prelude.Nothing
    }

-- | The date and time the flow log was created.
flowLog_creationTime :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.UTCTime)
flowLog_creationTime = Lens.lens (\FlowLog' {creationTime} -> creationTime) (\s@FlowLog' {} a -> s {creationTime = a} :: FlowLog) Prelude.. Lens.mapping Data._Time

-- | The ARN of the IAM role that allows the service to publish flow logs
-- across accounts.
flowLog_deliverCrossAccountRole :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_deliverCrossAccountRole = Lens.lens (\FlowLog' {deliverCrossAccountRole} -> deliverCrossAccountRole) (\s@FlowLog' {} a -> s {deliverCrossAccountRole = a} :: FlowLog)

-- | Information about the error that occurred. @Rate limited@ indicates that
-- CloudWatch Logs throttling has been applied for one or more network
-- interfaces, or that you\'ve reached the limit on the number of log
-- groups that you can create. @Access error@ indicates that the IAM role
-- associated with the flow log does not have sufficient permissions to
-- publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
flowLog_deliverLogsErrorMessage :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_deliverLogsErrorMessage = Lens.lens (\FlowLog' {deliverLogsErrorMessage} -> deliverLogsErrorMessage) (\s@FlowLog' {} a -> s {deliverLogsErrorMessage = a} :: FlowLog)

-- | The ARN of the IAM role allows the service to publish logs to CloudWatch
-- Logs.
flowLog_deliverLogsPermissionArn :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_deliverLogsPermissionArn = Lens.lens (\FlowLog' {deliverLogsPermissionArn} -> deliverLogsPermissionArn) (\s@FlowLog' {} a -> s {deliverLogsPermissionArn = a} :: FlowLog)

-- | The status of the logs delivery (@SUCCESS@ | @FAILED@).
flowLog_deliverLogsStatus :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_deliverLogsStatus = Lens.lens (\FlowLog' {deliverLogsStatus} -> deliverLogsStatus) (\s@FlowLog' {} a -> s {deliverLogsStatus = a} :: FlowLog)

-- | The destination options.
flowLog_destinationOptions :: Lens.Lens' FlowLog (Prelude.Maybe DestinationOptionsResponse)
flowLog_destinationOptions = Lens.lens (\FlowLog' {destinationOptions} -> destinationOptions) (\s@FlowLog' {} a -> s {destinationOptions = a} :: FlowLog)

-- | The ID of the flow log.
flowLog_flowLogId :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_flowLogId = Lens.lens (\FlowLog' {flowLogId} -> flowLogId) (\s@FlowLog' {} a -> s {flowLogId = a} :: FlowLog)

-- | The status of the flow log (@ACTIVE@).
flowLog_flowLogStatus :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_flowLogStatus = Lens.lens (\FlowLog' {flowLogStatus} -> flowLogStatus) (\s@FlowLog' {} a -> s {flowLogStatus = a} :: FlowLog)

-- | The Amazon Resource Name (ARN) of the destination for the flow log data.
flowLog_logDestination :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_logDestination = Lens.lens (\FlowLog' {logDestination} -> logDestination) (\s@FlowLog' {} a -> s {logDestination = a} :: FlowLog)

-- | The type of destination for the flow log data.
flowLog_logDestinationType :: Lens.Lens' FlowLog (Prelude.Maybe LogDestinationType)
flowLog_logDestinationType = Lens.lens (\FlowLog' {logDestinationType} -> logDestinationType) (\s@FlowLog' {} a -> s {logDestinationType = a} :: FlowLog)

-- | The format of the flow log record.
flowLog_logFormat :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_logFormat = Lens.lens (\FlowLog' {logFormat} -> logFormat) (\s@FlowLog' {} a -> s {logFormat = a} :: FlowLog)

-- | The name of the flow log group.
flowLog_logGroupName :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_logGroupName = Lens.lens (\FlowLog' {logGroupName} -> logGroupName) (\s@FlowLog' {} a -> s {logGroupName = a} :: FlowLog)

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

-- | The ID of the resource being monitored.
flowLog_resourceId :: Lens.Lens' FlowLog (Prelude.Maybe Prelude.Text)
flowLog_resourceId = Lens.lens (\FlowLog' {resourceId} -> resourceId) (\s@FlowLog' {} a -> s {resourceId = a} :: FlowLog)

-- | The tags for the flow log.
flowLog_tags :: Lens.Lens' FlowLog (Prelude.Maybe [Tag])
flowLog_tags = Lens.lens (\FlowLog' {tags} -> tags) (\s@FlowLog' {} a -> s {tags = a} :: FlowLog) Prelude.. Lens.mapping Lens.coerced

-- | The type of traffic captured for the flow log.
flowLog_trafficType :: Lens.Lens' FlowLog (Prelude.Maybe TrafficType)
flowLog_trafficType = Lens.lens (\FlowLog' {trafficType} -> trafficType) (\s@FlowLog' {} a -> s {trafficType = a} :: FlowLog)

instance Data.FromXML FlowLog where
  parseXML x =
    FlowLog'
      Prelude.<$> (x Data..@? "creationTime")
      Prelude.<*> (x Data..@? "deliverCrossAccountRole")
      Prelude.<*> (x Data..@? "deliverLogsErrorMessage")
      Prelude.<*> (x Data..@? "deliverLogsPermissionArn")
      Prelude.<*> (x Data..@? "deliverLogsStatus")
      Prelude.<*> (x Data..@? "destinationOptions")
      Prelude.<*> (x Data..@? "flowLogId")
      Prelude.<*> (x Data..@? "flowLogStatus")
      Prelude.<*> (x Data..@? "logDestination")
      Prelude.<*> (x Data..@? "logDestinationType")
      Prelude.<*> (x Data..@? "logFormat")
      Prelude.<*> (x Data..@? "logGroupName")
      Prelude.<*> (x Data..@? "maxAggregationInterval")
      Prelude.<*> (x Data..@? "resourceId")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "trafficType")

instance Prelude.Hashable FlowLog where
  hashWithSalt _salt FlowLog' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` deliverCrossAccountRole
      `Prelude.hashWithSalt` deliverLogsErrorMessage
      `Prelude.hashWithSalt` deliverLogsPermissionArn
      `Prelude.hashWithSalt` deliverLogsStatus
      `Prelude.hashWithSalt` destinationOptions
      `Prelude.hashWithSalt` flowLogId
      `Prelude.hashWithSalt` flowLogStatus
      `Prelude.hashWithSalt` logDestination
      `Prelude.hashWithSalt` logDestinationType
      `Prelude.hashWithSalt` logFormat
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` maxAggregationInterval
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` trafficType

instance Prelude.NFData FlowLog where
  rnf FlowLog' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf deliverCrossAccountRole
      `Prelude.seq` Prelude.rnf deliverLogsErrorMessage
      `Prelude.seq` Prelude.rnf deliverLogsPermissionArn
      `Prelude.seq` Prelude.rnf deliverLogsStatus
      `Prelude.seq` Prelude.rnf destinationOptions
      `Prelude.seq` Prelude.rnf flowLogId
      `Prelude.seq` Prelude.rnf flowLogStatus
      `Prelude.seq` Prelude.rnf logDestination
      `Prelude.seq` Prelude.rnf logDestinationType
      `Prelude.seq` Prelude.rnf logFormat
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf maxAggregationInterval
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf trafficType
