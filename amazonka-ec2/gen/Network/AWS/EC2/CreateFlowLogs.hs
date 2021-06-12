{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateFlowLogs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more flow logs to capture information about IP traffic
-- for a specific network interface, subnet, or VPC.
--
-- Flow log data for a monitored network interface is recorded as flow log
-- records, which are log events consisting of fields that describe the
-- traffic flow. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- When publishing to CloudWatch Logs, flow log records are published to a
-- log group, and each network interface has a unique log stream in the log
-- group. When publishing to Amazon S3, flow log records for all of the
-- monitored network interfaces are published to a single log file object
-- that is stored in the specified bucket.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html VPC Flow Logs>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.CreateFlowLogs
  ( -- * Creating a Request
    CreateFlowLogs (..),
    newCreateFlowLogs,

    -- * Request Lenses
    createFlowLogs_maxAggregationInterval,
    createFlowLogs_tagSpecifications,
    createFlowLogs_dryRun,
    createFlowLogs_logDestination,
    createFlowLogs_logFormat,
    createFlowLogs_logGroupName,
    createFlowLogs_deliverLogsPermissionArn,
    createFlowLogs_logDestinationType,
    createFlowLogs_clientToken,
    createFlowLogs_resourceIds,
    createFlowLogs_resourceType,
    createFlowLogs_trafficType,

    -- * Destructuring the Response
    CreateFlowLogsResponse (..),
    newCreateFlowLogsResponse,

    -- * Response Lenses
    createFlowLogsResponse_unsuccessful,
    createFlowLogsResponse_flowLogIds,
    createFlowLogsResponse_clientToken,
    createFlowLogsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFlowLogs' smart constructor.
data CreateFlowLogs = CreateFlowLogs'
  { -- | The maximum interval of time during which a flow of packets is captured
    -- and aggregated into a flow log record. You can specify 60 seconds (1
    -- minute) or 600 seconds (10 minutes).
    --
    -- When a network interface is attached to a
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance>,
    -- the aggregation interval is always 60 seconds or less, regardless of the
    -- value that you specify.
    --
    -- Default: 600
    maxAggregationInterval :: Core.Maybe Core.Int,
    -- | The tags to apply to the flow logs.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | Specifies the destination to which the flow log data is to be published.
    -- Flow log data can be published to a CloudWatch Logs log group or an
    -- Amazon S3 bucket. The value specified for this parameter depends on the
    -- value specified for @LogDestinationType@.
    --
    -- If @LogDestinationType@ is not specified or @cloud-watch-logs@, specify
    -- the Amazon Resource Name (ARN) of the CloudWatch Logs log group. For
    -- example, to publish to a log group called @my-logs@, specify
    -- @arn:aws:logs:us-east-1:123456789012:log-group:my-logs@. Alternatively,
    -- use @LogGroupName@ instead.
    --
    -- If LogDestinationType is @s3@, specify the ARN of the Amazon S3 bucket.
    -- You can also specify a subfolder in the bucket. To specify a subfolder
    -- in the bucket, use the following ARN format:
    -- @bucket_ARN\/subfolder_name\/@. For example, to specify a subfolder
    -- named @my-logs@ in a bucket named @my-bucket@, use the following ARN:
    -- @arn:aws:s3:::my-bucket\/my-logs\/@. You cannot use @AWSLogs@ as a
    -- subfolder name. This is a reserved term.
    logDestination :: Core.Maybe Core.Text,
    -- | The fields to include in the flow log record, in the order in which they
    -- should appear. For a list of available fields, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records>.
    -- If you omit this parameter, the flow log is created using the default
    -- format. If you specify this parameter, you must specify at least one
    -- field.
    --
    -- Specify the fields using the @${field-id}@ format, separated by spaces.
    -- For the AWS CLI, use single quotation marks (\' \') to surround the
    -- parameter value.
    logFormat :: Core.Maybe Core.Text,
    -- | The name of a new or existing CloudWatch Logs log group where Amazon EC2
    -- publishes your flow logs.
    --
    -- If you specify @LogDestinationType@ as @s3@, do not specify
    -- @DeliverLogsPermissionArn@ or @LogGroupName@.
    logGroupName :: Core.Maybe Core.Text,
    -- | The ARN for the IAM role that permits Amazon EC2 to publish flow logs to
    -- a CloudWatch Logs log group in your account.
    --
    -- If you specify @LogDestinationType@ as @s3@, do not specify
    -- @DeliverLogsPermissionArn@ or @LogGroupName@.
    deliverLogsPermissionArn :: Core.Maybe Core.Text,
    -- | Specifies the type of destination to which the flow log data is to be
    -- published. Flow log data can be published to CloudWatch Logs or Amazon
    -- S3. To publish flow log data to CloudWatch Logs, specify
    -- @cloud-watch-logs@. To publish flow log data to Amazon S3, specify @s3@.
    --
    -- If you specify @LogDestinationType@ as @s3@, do not specify
    -- @DeliverLogsPermissionArn@ or @LogGroupName@.
    --
    -- Default: @cloud-watch-logs@
    logDestinationType :: Core.Maybe LogDestinationType,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | The ID of the subnet, network interface, or VPC for which you want to
    -- create a flow log.
    --
    -- Constraints: Maximum of 1000 resources
    resourceIds :: [Core.Text],
    -- | The type of resource for which to create the flow log. For example, if
    -- you specified a VPC ID for the @ResourceId@ property, specify @VPC@ for
    -- this property.
    resourceType :: FlowLogsResourceType,
    -- | The type of traffic to log. You can log traffic that the resource
    -- accepts or rejects, or all traffic.
    trafficType :: TrafficType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateFlowLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxAggregationInterval', 'createFlowLogs_maxAggregationInterval' - The maximum interval of time during which a flow of packets is captured
-- and aggregated into a flow log record. You can specify 60 seconds (1
-- minute) or 600 seconds (10 minutes).
--
-- When a network interface is attached to a
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance>,
-- the aggregation interval is always 60 seconds or less, regardless of the
-- value that you specify.
--
-- Default: 600
--
-- 'tagSpecifications', 'createFlowLogs_tagSpecifications' - The tags to apply to the flow logs.
--
-- 'dryRun', 'createFlowLogs_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'logDestination', 'createFlowLogs_logDestination' - Specifies the destination to which the flow log data is to be published.
-- Flow log data can be published to a CloudWatch Logs log group or an
-- Amazon S3 bucket. The value specified for this parameter depends on the
-- value specified for @LogDestinationType@.
--
-- If @LogDestinationType@ is not specified or @cloud-watch-logs@, specify
-- the Amazon Resource Name (ARN) of the CloudWatch Logs log group. For
-- example, to publish to a log group called @my-logs@, specify
-- @arn:aws:logs:us-east-1:123456789012:log-group:my-logs@. Alternatively,
-- use @LogGroupName@ instead.
--
-- If LogDestinationType is @s3@, specify the ARN of the Amazon S3 bucket.
-- You can also specify a subfolder in the bucket. To specify a subfolder
-- in the bucket, use the following ARN format:
-- @bucket_ARN\/subfolder_name\/@. For example, to specify a subfolder
-- named @my-logs@ in a bucket named @my-bucket@, use the following ARN:
-- @arn:aws:s3:::my-bucket\/my-logs\/@. You cannot use @AWSLogs@ as a
-- subfolder name. This is a reserved term.
--
-- 'logFormat', 'createFlowLogs_logFormat' - The fields to include in the flow log record, in the order in which they
-- should appear. For a list of available fields, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records>.
-- If you omit this parameter, the flow log is created using the default
-- format. If you specify this parameter, you must specify at least one
-- field.
--
-- Specify the fields using the @${field-id}@ format, separated by spaces.
-- For the AWS CLI, use single quotation marks (\' \') to surround the
-- parameter value.
--
-- 'logGroupName', 'createFlowLogs_logGroupName' - The name of a new or existing CloudWatch Logs log group where Amazon EC2
-- publishes your flow logs.
--
-- If you specify @LogDestinationType@ as @s3@, do not specify
-- @DeliverLogsPermissionArn@ or @LogGroupName@.
--
-- 'deliverLogsPermissionArn', 'createFlowLogs_deliverLogsPermissionArn' - The ARN for the IAM role that permits Amazon EC2 to publish flow logs to
-- a CloudWatch Logs log group in your account.
--
-- If you specify @LogDestinationType@ as @s3@, do not specify
-- @DeliverLogsPermissionArn@ or @LogGroupName@.
--
-- 'logDestinationType', 'createFlowLogs_logDestinationType' - Specifies the type of destination to which the flow log data is to be
-- published. Flow log data can be published to CloudWatch Logs or Amazon
-- S3. To publish flow log data to CloudWatch Logs, specify
-- @cloud-watch-logs@. To publish flow log data to Amazon S3, specify @s3@.
--
-- If you specify @LogDestinationType@ as @s3@, do not specify
-- @DeliverLogsPermissionArn@ or @LogGroupName@.
--
-- Default: @cloud-watch-logs@
--
-- 'clientToken', 'createFlowLogs_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'resourceIds', 'createFlowLogs_resourceIds' - The ID of the subnet, network interface, or VPC for which you want to
-- create a flow log.
--
-- Constraints: Maximum of 1000 resources
--
-- 'resourceType', 'createFlowLogs_resourceType' - The type of resource for which to create the flow log. For example, if
-- you specified a VPC ID for the @ResourceId@ property, specify @VPC@ for
-- this property.
--
-- 'trafficType', 'createFlowLogs_trafficType' - The type of traffic to log. You can log traffic that the resource
-- accepts or rejects, or all traffic.
newCreateFlowLogs ::
  -- | 'resourceType'
  FlowLogsResourceType ->
  -- | 'trafficType'
  TrafficType ->
  CreateFlowLogs
newCreateFlowLogs pResourceType_ pTrafficType_ =
  CreateFlowLogs'
    { maxAggregationInterval =
        Core.Nothing,
      tagSpecifications = Core.Nothing,
      dryRun = Core.Nothing,
      logDestination = Core.Nothing,
      logFormat = Core.Nothing,
      logGroupName = Core.Nothing,
      deliverLogsPermissionArn = Core.Nothing,
      logDestinationType = Core.Nothing,
      clientToken = Core.Nothing,
      resourceIds = Core.mempty,
      resourceType = pResourceType_,
      trafficType = pTrafficType_
    }

-- | The maximum interval of time during which a flow of packets is captured
-- and aggregated into a flow log record. You can specify 60 seconds (1
-- minute) or 600 seconds (10 minutes).
--
-- When a network interface is attached to a
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance>,
-- the aggregation interval is always 60 seconds or less, regardless of the
-- value that you specify.
--
-- Default: 600
createFlowLogs_maxAggregationInterval :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Int)
createFlowLogs_maxAggregationInterval = Lens.lens (\CreateFlowLogs' {maxAggregationInterval} -> maxAggregationInterval) (\s@CreateFlowLogs' {} a -> s {maxAggregationInterval = a} :: CreateFlowLogs)

-- | The tags to apply to the flow logs.
createFlowLogs_tagSpecifications :: Lens.Lens' CreateFlowLogs (Core.Maybe [TagSpecification])
createFlowLogs_tagSpecifications = Lens.lens (\CreateFlowLogs' {tagSpecifications} -> tagSpecifications) (\s@CreateFlowLogs' {} a -> s {tagSpecifications = a} :: CreateFlowLogs) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createFlowLogs_dryRun :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Bool)
createFlowLogs_dryRun = Lens.lens (\CreateFlowLogs' {dryRun} -> dryRun) (\s@CreateFlowLogs' {} a -> s {dryRun = a} :: CreateFlowLogs)

-- | Specifies the destination to which the flow log data is to be published.
-- Flow log data can be published to a CloudWatch Logs log group or an
-- Amazon S3 bucket. The value specified for this parameter depends on the
-- value specified for @LogDestinationType@.
--
-- If @LogDestinationType@ is not specified or @cloud-watch-logs@, specify
-- the Amazon Resource Name (ARN) of the CloudWatch Logs log group. For
-- example, to publish to a log group called @my-logs@, specify
-- @arn:aws:logs:us-east-1:123456789012:log-group:my-logs@. Alternatively,
-- use @LogGroupName@ instead.
--
-- If LogDestinationType is @s3@, specify the ARN of the Amazon S3 bucket.
-- You can also specify a subfolder in the bucket. To specify a subfolder
-- in the bucket, use the following ARN format:
-- @bucket_ARN\/subfolder_name\/@. For example, to specify a subfolder
-- named @my-logs@ in a bucket named @my-bucket@, use the following ARN:
-- @arn:aws:s3:::my-bucket\/my-logs\/@. You cannot use @AWSLogs@ as a
-- subfolder name. This is a reserved term.
createFlowLogs_logDestination :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Text)
createFlowLogs_logDestination = Lens.lens (\CreateFlowLogs' {logDestination} -> logDestination) (\s@CreateFlowLogs' {} a -> s {logDestination = a} :: CreateFlowLogs)

-- | The fields to include in the flow log record, in the order in which they
-- should appear. For a list of available fields, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records>.
-- If you omit this parameter, the flow log is created using the default
-- format. If you specify this parameter, you must specify at least one
-- field.
--
-- Specify the fields using the @${field-id}@ format, separated by spaces.
-- For the AWS CLI, use single quotation marks (\' \') to surround the
-- parameter value.
createFlowLogs_logFormat :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Text)
createFlowLogs_logFormat = Lens.lens (\CreateFlowLogs' {logFormat} -> logFormat) (\s@CreateFlowLogs' {} a -> s {logFormat = a} :: CreateFlowLogs)

-- | The name of a new or existing CloudWatch Logs log group where Amazon EC2
-- publishes your flow logs.
--
-- If you specify @LogDestinationType@ as @s3@, do not specify
-- @DeliverLogsPermissionArn@ or @LogGroupName@.
createFlowLogs_logGroupName :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Text)
createFlowLogs_logGroupName = Lens.lens (\CreateFlowLogs' {logGroupName} -> logGroupName) (\s@CreateFlowLogs' {} a -> s {logGroupName = a} :: CreateFlowLogs)

-- | The ARN for the IAM role that permits Amazon EC2 to publish flow logs to
-- a CloudWatch Logs log group in your account.
--
-- If you specify @LogDestinationType@ as @s3@, do not specify
-- @DeliverLogsPermissionArn@ or @LogGroupName@.
createFlowLogs_deliverLogsPermissionArn :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Text)
createFlowLogs_deliverLogsPermissionArn = Lens.lens (\CreateFlowLogs' {deliverLogsPermissionArn} -> deliverLogsPermissionArn) (\s@CreateFlowLogs' {} a -> s {deliverLogsPermissionArn = a} :: CreateFlowLogs)

-- | Specifies the type of destination to which the flow log data is to be
-- published. Flow log data can be published to CloudWatch Logs or Amazon
-- S3. To publish flow log data to CloudWatch Logs, specify
-- @cloud-watch-logs@. To publish flow log data to Amazon S3, specify @s3@.
--
-- If you specify @LogDestinationType@ as @s3@, do not specify
-- @DeliverLogsPermissionArn@ or @LogGroupName@.
--
-- Default: @cloud-watch-logs@
createFlowLogs_logDestinationType :: Lens.Lens' CreateFlowLogs (Core.Maybe LogDestinationType)
createFlowLogs_logDestinationType = Lens.lens (\CreateFlowLogs' {logDestinationType} -> logDestinationType) (\s@CreateFlowLogs' {} a -> s {logDestinationType = a} :: CreateFlowLogs)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createFlowLogs_clientToken :: Lens.Lens' CreateFlowLogs (Core.Maybe Core.Text)
createFlowLogs_clientToken = Lens.lens (\CreateFlowLogs' {clientToken} -> clientToken) (\s@CreateFlowLogs' {} a -> s {clientToken = a} :: CreateFlowLogs)

-- | The ID of the subnet, network interface, or VPC for which you want to
-- create a flow log.
--
-- Constraints: Maximum of 1000 resources
createFlowLogs_resourceIds :: Lens.Lens' CreateFlowLogs [Core.Text]
createFlowLogs_resourceIds = Lens.lens (\CreateFlowLogs' {resourceIds} -> resourceIds) (\s@CreateFlowLogs' {} a -> s {resourceIds = a} :: CreateFlowLogs) Core.. Lens._Coerce

-- | The type of resource for which to create the flow log. For example, if
-- you specified a VPC ID for the @ResourceId@ property, specify @VPC@ for
-- this property.
createFlowLogs_resourceType :: Lens.Lens' CreateFlowLogs FlowLogsResourceType
createFlowLogs_resourceType = Lens.lens (\CreateFlowLogs' {resourceType} -> resourceType) (\s@CreateFlowLogs' {} a -> s {resourceType = a} :: CreateFlowLogs)

-- | The type of traffic to log. You can log traffic that the resource
-- accepts or rejects, or all traffic.
createFlowLogs_trafficType :: Lens.Lens' CreateFlowLogs TrafficType
createFlowLogs_trafficType = Lens.lens (\CreateFlowLogs' {trafficType} -> trafficType) (\s@CreateFlowLogs' {} a -> s {trafficType = a} :: CreateFlowLogs)

instance Core.AWSRequest CreateFlowLogs where
  type
    AWSResponse CreateFlowLogs =
      CreateFlowLogsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFlowLogsResponse'
            Core.<$> ( x Core..@? "unsuccessful" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> ( x Core..@? "flowLogIdSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "clientToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateFlowLogs

instance Core.NFData CreateFlowLogs

instance Core.ToHeaders CreateFlowLogs where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateFlowLogs where
  toPath = Core.const "/"

instance Core.ToQuery CreateFlowLogs where
  toQuery CreateFlowLogs' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateFlowLogs" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "MaxAggregationInterval"
          Core.=: maxAggregationInterval,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "LogDestination" Core.=: logDestination,
        "LogFormat" Core.=: logFormat,
        "LogGroupName" Core.=: logGroupName,
        "DeliverLogsPermissionArn"
          Core.=: deliverLogsPermissionArn,
        "LogDestinationType" Core.=: logDestinationType,
        "ClientToken" Core.=: clientToken,
        Core.toQueryList "ResourceId" resourceIds,
        "ResourceType" Core.=: resourceType,
        "TrafficType" Core.=: trafficType
      ]

-- | /See:/ 'newCreateFlowLogsResponse' smart constructor.
data CreateFlowLogsResponse = CreateFlowLogsResponse'
  { -- | Information about the flow logs that could not be created successfully.
    unsuccessful :: Core.Maybe [UnsuccessfulItem],
    -- | The IDs of the flow logs.
    flowLogIds :: Core.Maybe [Core.Text],
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateFlowLogsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessful', 'createFlowLogsResponse_unsuccessful' - Information about the flow logs that could not be created successfully.
--
-- 'flowLogIds', 'createFlowLogsResponse_flowLogIds' - The IDs of the flow logs.
--
-- 'clientToken', 'createFlowLogsResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'httpStatus', 'createFlowLogsResponse_httpStatus' - The response's http status code.
newCreateFlowLogsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateFlowLogsResponse
newCreateFlowLogsResponse pHttpStatus_ =
  CreateFlowLogsResponse'
    { unsuccessful =
        Core.Nothing,
      flowLogIds = Core.Nothing,
      clientToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the flow logs that could not be created successfully.
createFlowLogsResponse_unsuccessful :: Lens.Lens' CreateFlowLogsResponse (Core.Maybe [UnsuccessfulItem])
createFlowLogsResponse_unsuccessful = Lens.lens (\CreateFlowLogsResponse' {unsuccessful} -> unsuccessful) (\s@CreateFlowLogsResponse' {} a -> s {unsuccessful = a} :: CreateFlowLogsResponse) Core.. Lens.mapping Lens._Coerce

-- | The IDs of the flow logs.
createFlowLogsResponse_flowLogIds :: Lens.Lens' CreateFlowLogsResponse (Core.Maybe [Core.Text])
createFlowLogsResponse_flowLogIds = Lens.lens (\CreateFlowLogsResponse' {flowLogIds} -> flowLogIds) (\s@CreateFlowLogsResponse' {} a -> s {flowLogIds = a} :: CreateFlowLogsResponse) Core.. Lens.mapping Lens._Coerce

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createFlowLogsResponse_clientToken :: Lens.Lens' CreateFlowLogsResponse (Core.Maybe Core.Text)
createFlowLogsResponse_clientToken = Lens.lens (\CreateFlowLogsResponse' {clientToken} -> clientToken) (\s@CreateFlowLogsResponse' {} a -> s {clientToken = a} :: CreateFlowLogsResponse)

-- | The response's http status code.
createFlowLogsResponse_httpStatus :: Lens.Lens' CreateFlowLogsResponse Core.Int
createFlowLogsResponse_httpStatus = Lens.lens (\CreateFlowLogsResponse' {httpStatus} -> httpStatus) (\s@CreateFlowLogsResponse' {} a -> s {httpStatus = a} :: CreateFlowLogsResponse)

instance Core.NFData CreateFlowLogsResponse
