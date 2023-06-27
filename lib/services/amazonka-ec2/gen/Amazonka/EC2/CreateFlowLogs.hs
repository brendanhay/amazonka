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
-- Module      : Amazonka.EC2.CreateFlowLogs
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow log records>
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
module Amazonka.EC2.CreateFlowLogs
  ( -- * Creating a Request
    CreateFlowLogs (..),
    newCreateFlowLogs,

    -- * Request Lenses
    createFlowLogs_clientToken,
    createFlowLogs_deliverCrossAccountRole,
    createFlowLogs_deliverLogsPermissionArn,
    createFlowLogs_destinationOptions,
    createFlowLogs_dryRun,
    createFlowLogs_logDestination,
    createFlowLogs_logDestinationType,
    createFlowLogs_logFormat,
    createFlowLogs_logGroupName,
    createFlowLogs_maxAggregationInterval,
    createFlowLogs_tagSpecifications,
    createFlowLogs_trafficType,
    createFlowLogs_resourceIds,
    createFlowLogs_resourceType,

    -- * Destructuring the Response
    CreateFlowLogsResponse (..),
    newCreateFlowLogsResponse,

    -- * Response Lenses
    createFlowLogsResponse_clientToken,
    createFlowLogsResponse_flowLogIds,
    createFlowLogsResponse_unsuccessful,
    createFlowLogsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFlowLogs' smart constructor.
data CreateFlowLogs = CreateFlowLogs'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that allows Amazon EC2 to publish flow logs
    -- across accounts.
    deliverCrossAccountRole :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that allows Amazon EC2 to publish flow logs to a
    -- CloudWatch Logs log group in your account.
    --
    -- This parameter is required if the destination type is @cloud-watch-logs@
    -- and unsupported otherwise.
    deliverLogsPermissionArn :: Prelude.Maybe Prelude.Text,
    -- | The destination options.
    destinationOptions :: Prelude.Maybe DestinationOptionsRequest,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The destination for the flow log data. The meaning of this parameter
    -- depends on the destination type.
    --
    -- -   If the destination type is @cloud-watch-logs@, specify the ARN of a
    --     CloudWatch Logs log group. For example:
    --
    --     arn:aws:logs:/region/:/account_id/:log-group:/my_group/
    --
    --     Alternatively, use the @LogGroupName@ parameter.
    --
    -- -   If the destination type is @s3@, specify the ARN of an S3 bucket.
    --     For example:
    --
    --     arn:aws:s3:::/my_bucket/\//my_subfolder/\/
    --
    --     The subfolder is optional. Note that you can\'t use @AWSLogs@ as a
    --     subfolder name.
    --
    -- -   If the destination type is @kinesis-data-firehose@, specify the ARN
    --     of a Kinesis Data Firehose delivery stream. For example:
    --
    --     arn:aws:firehose:/region/:/account_id/:deliverystream:/my_stream/
    logDestination :: Prelude.Maybe Prelude.Text,
    -- | The type of destination for the flow log data.
    --
    -- Default: @cloud-watch-logs@
    logDestinationType :: Prelude.Maybe LogDestinationType,
    -- | The fields to include in the flow log record. List the fields in the
    -- order in which they should appear. If you omit this parameter, the flow
    -- log is created using the default format. If you specify this parameter,
    -- you must include at least one field. For more information about the
    -- available fields, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow log records>
    -- in the /Amazon VPC User Guide/ or
    -- <https://docs.aws.amazon.com/vpc/latest/tgw/tgw-flow-logs.html#flow-log-records Transit Gateway Flow Log records>
    -- in the /Amazon Web Services Transit Gateway Guide/.
    --
    -- Specify the fields using the @${field-id}@ format, separated by spaces.
    -- For the CLI, surround this parameter value with single quotes on Linux
    -- or double quotes on Windows.
    logFormat :: Prelude.Maybe Prelude.Text,
    -- | The name of a new or existing CloudWatch Logs log group where Amazon EC2
    -- publishes your flow logs.
    --
    -- This parameter is valid only if the destination type is
    -- @cloud-watch-logs@.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The maximum interval of time during which a flow of packets is captured
    -- and aggregated into a flow log record. The possible values are 60
    -- seconds (1 minute) or 600 seconds (10 minutes). This parameter must be
    -- 60 seconds for transit gateway resource types.
    --
    -- When a network interface is attached to a
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance>,
    -- the aggregation interval is always 60 seconds or less, regardless of the
    -- value that you specify.
    --
    -- Default: 600
    maxAggregationInterval :: Prelude.Maybe Prelude.Int,
    -- | The tags to apply to the flow logs.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The type of traffic to monitor (accepted traffic, rejected traffic, or
    -- all traffic). This parameter is not supported for transit gateway
    -- resource types. It is required for the other resource types.
    trafficType :: Prelude.Maybe TrafficType,
    -- | The IDs of the resources to monitor. For example, if the resource type
    -- is @VPC@, specify the IDs of the VPCs.
    --
    -- Constraints: Maximum of 25 for transit gateway resource types. Maximum
    -- of 1000 for the other resource types.
    resourceIds :: [Prelude.Text],
    -- | The type of resource to monitor.
    resourceType :: FlowLogsResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFlowLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createFlowLogs_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'deliverCrossAccountRole', 'createFlowLogs_deliverCrossAccountRole' - The ARN of the IAM role that allows Amazon EC2 to publish flow logs
-- across accounts.
--
-- 'deliverLogsPermissionArn', 'createFlowLogs_deliverLogsPermissionArn' - The ARN of the IAM role that allows Amazon EC2 to publish flow logs to a
-- CloudWatch Logs log group in your account.
--
-- This parameter is required if the destination type is @cloud-watch-logs@
-- and unsupported otherwise.
--
-- 'destinationOptions', 'createFlowLogs_destinationOptions' - The destination options.
--
-- 'dryRun', 'createFlowLogs_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'logDestination', 'createFlowLogs_logDestination' - The destination for the flow log data. The meaning of this parameter
-- depends on the destination type.
--
-- -   If the destination type is @cloud-watch-logs@, specify the ARN of a
--     CloudWatch Logs log group. For example:
--
--     arn:aws:logs:/region/:/account_id/:log-group:/my_group/
--
--     Alternatively, use the @LogGroupName@ parameter.
--
-- -   If the destination type is @s3@, specify the ARN of an S3 bucket.
--     For example:
--
--     arn:aws:s3:::/my_bucket/\//my_subfolder/\/
--
--     The subfolder is optional. Note that you can\'t use @AWSLogs@ as a
--     subfolder name.
--
-- -   If the destination type is @kinesis-data-firehose@, specify the ARN
--     of a Kinesis Data Firehose delivery stream. For example:
--
--     arn:aws:firehose:/region/:/account_id/:deliverystream:/my_stream/
--
-- 'logDestinationType', 'createFlowLogs_logDestinationType' - The type of destination for the flow log data.
--
-- Default: @cloud-watch-logs@
--
-- 'logFormat', 'createFlowLogs_logFormat' - The fields to include in the flow log record. List the fields in the
-- order in which they should appear. If you omit this parameter, the flow
-- log is created using the default format. If you specify this parameter,
-- you must include at least one field. For more information about the
-- available fields, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow log records>
-- in the /Amazon VPC User Guide/ or
-- <https://docs.aws.amazon.com/vpc/latest/tgw/tgw-flow-logs.html#flow-log-records Transit Gateway Flow Log records>
-- in the /Amazon Web Services Transit Gateway Guide/.
--
-- Specify the fields using the @${field-id}@ format, separated by spaces.
-- For the CLI, surround this parameter value with single quotes on Linux
-- or double quotes on Windows.
--
-- 'logGroupName', 'createFlowLogs_logGroupName' - The name of a new or existing CloudWatch Logs log group where Amazon EC2
-- publishes your flow logs.
--
-- This parameter is valid only if the destination type is
-- @cloud-watch-logs@.
--
-- 'maxAggregationInterval', 'createFlowLogs_maxAggregationInterval' - The maximum interval of time during which a flow of packets is captured
-- and aggregated into a flow log record. The possible values are 60
-- seconds (1 minute) or 600 seconds (10 minutes). This parameter must be
-- 60 seconds for transit gateway resource types.
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
-- 'trafficType', 'createFlowLogs_trafficType' - The type of traffic to monitor (accepted traffic, rejected traffic, or
-- all traffic). This parameter is not supported for transit gateway
-- resource types. It is required for the other resource types.
--
-- 'resourceIds', 'createFlowLogs_resourceIds' - The IDs of the resources to monitor. For example, if the resource type
-- is @VPC@, specify the IDs of the VPCs.
--
-- Constraints: Maximum of 25 for transit gateway resource types. Maximum
-- of 1000 for the other resource types.
--
-- 'resourceType', 'createFlowLogs_resourceType' - The type of resource to monitor.
newCreateFlowLogs ::
  -- | 'resourceType'
  FlowLogsResourceType ->
  CreateFlowLogs
newCreateFlowLogs pResourceType_ =
  CreateFlowLogs'
    { clientToken = Prelude.Nothing,
      deliverCrossAccountRole = Prelude.Nothing,
      deliverLogsPermissionArn = Prelude.Nothing,
      destinationOptions = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      logDestination = Prelude.Nothing,
      logDestinationType = Prelude.Nothing,
      logFormat = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      maxAggregationInterval = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      trafficType = Prelude.Nothing,
      resourceIds = Prelude.mempty,
      resourceType = pResourceType_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
createFlowLogs_clientToken :: Lens.Lens' CreateFlowLogs (Prelude.Maybe Prelude.Text)
createFlowLogs_clientToken = Lens.lens (\CreateFlowLogs' {clientToken} -> clientToken) (\s@CreateFlowLogs' {} a -> s {clientToken = a} :: CreateFlowLogs)

-- | The ARN of the IAM role that allows Amazon EC2 to publish flow logs
-- across accounts.
createFlowLogs_deliverCrossAccountRole :: Lens.Lens' CreateFlowLogs (Prelude.Maybe Prelude.Text)
createFlowLogs_deliverCrossAccountRole = Lens.lens (\CreateFlowLogs' {deliverCrossAccountRole} -> deliverCrossAccountRole) (\s@CreateFlowLogs' {} a -> s {deliverCrossAccountRole = a} :: CreateFlowLogs)

-- | The ARN of the IAM role that allows Amazon EC2 to publish flow logs to a
-- CloudWatch Logs log group in your account.
--
-- This parameter is required if the destination type is @cloud-watch-logs@
-- and unsupported otherwise.
createFlowLogs_deliverLogsPermissionArn :: Lens.Lens' CreateFlowLogs (Prelude.Maybe Prelude.Text)
createFlowLogs_deliverLogsPermissionArn = Lens.lens (\CreateFlowLogs' {deliverLogsPermissionArn} -> deliverLogsPermissionArn) (\s@CreateFlowLogs' {} a -> s {deliverLogsPermissionArn = a} :: CreateFlowLogs)

-- | The destination options.
createFlowLogs_destinationOptions :: Lens.Lens' CreateFlowLogs (Prelude.Maybe DestinationOptionsRequest)
createFlowLogs_destinationOptions = Lens.lens (\CreateFlowLogs' {destinationOptions} -> destinationOptions) (\s@CreateFlowLogs' {} a -> s {destinationOptions = a} :: CreateFlowLogs)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createFlowLogs_dryRun :: Lens.Lens' CreateFlowLogs (Prelude.Maybe Prelude.Bool)
createFlowLogs_dryRun = Lens.lens (\CreateFlowLogs' {dryRun} -> dryRun) (\s@CreateFlowLogs' {} a -> s {dryRun = a} :: CreateFlowLogs)

-- | The destination for the flow log data. The meaning of this parameter
-- depends on the destination type.
--
-- -   If the destination type is @cloud-watch-logs@, specify the ARN of a
--     CloudWatch Logs log group. For example:
--
--     arn:aws:logs:/region/:/account_id/:log-group:/my_group/
--
--     Alternatively, use the @LogGroupName@ parameter.
--
-- -   If the destination type is @s3@, specify the ARN of an S3 bucket.
--     For example:
--
--     arn:aws:s3:::/my_bucket/\//my_subfolder/\/
--
--     The subfolder is optional. Note that you can\'t use @AWSLogs@ as a
--     subfolder name.
--
-- -   If the destination type is @kinesis-data-firehose@, specify the ARN
--     of a Kinesis Data Firehose delivery stream. For example:
--
--     arn:aws:firehose:/region/:/account_id/:deliverystream:/my_stream/
createFlowLogs_logDestination :: Lens.Lens' CreateFlowLogs (Prelude.Maybe Prelude.Text)
createFlowLogs_logDestination = Lens.lens (\CreateFlowLogs' {logDestination} -> logDestination) (\s@CreateFlowLogs' {} a -> s {logDestination = a} :: CreateFlowLogs)

-- | The type of destination for the flow log data.
--
-- Default: @cloud-watch-logs@
createFlowLogs_logDestinationType :: Lens.Lens' CreateFlowLogs (Prelude.Maybe LogDestinationType)
createFlowLogs_logDestinationType = Lens.lens (\CreateFlowLogs' {logDestinationType} -> logDestinationType) (\s@CreateFlowLogs' {} a -> s {logDestinationType = a} :: CreateFlowLogs)

-- | The fields to include in the flow log record. List the fields in the
-- order in which they should appear. If you omit this parameter, the flow
-- log is created using the default format. If you specify this parameter,
-- you must include at least one field. For more information about the
-- available fields, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow log records>
-- in the /Amazon VPC User Guide/ or
-- <https://docs.aws.amazon.com/vpc/latest/tgw/tgw-flow-logs.html#flow-log-records Transit Gateway Flow Log records>
-- in the /Amazon Web Services Transit Gateway Guide/.
--
-- Specify the fields using the @${field-id}@ format, separated by spaces.
-- For the CLI, surround this parameter value with single quotes on Linux
-- or double quotes on Windows.
createFlowLogs_logFormat :: Lens.Lens' CreateFlowLogs (Prelude.Maybe Prelude.Text)
createFlowLogs_logFormat = Lens.lens (\CreateFlowLogs' {logFormat} -> logFormat) (\s@CreateFlowLogs' {} a -> s {logFormat = a} :: CreateFlowLogs)

-- | The name of a new or existing CloudWatch Logs log group where Amazon EC2
-- publishes your flow logs.
--
-- This parameter is valid only if the destination type is
-- @cloud-watch-logs@.
createFlowLogs_logGroupName :: Lens.Lens' CreateFlowLogs (Prelude.Maybe Prelude.Text)
createFlowLogs_logGroupName = Lens.lens (\CreateFlowLogs' {logGroupName} -> logGroupName) (\s@CreateFlowLogs' {} a -> s {logGroupName = a} :: CreateFlowLogs)

-- | The maximum interval of time during which a flow of packets is captured
-- and aggregated into a flow log record. The possible values are 60
-- seconds (1 minute) or 600 seconds (10 minutes). This parameter must be
-- 60 seconds for transit gateway resource types.
--
-- When a network interface is attached to a
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance>,
-- the aggregation interval is always 60 seconds or less, regardless of the
-- value that you specify.
--
-- Default: 600
createFlowLogs_maxAggregationInterval :: Lens.Lens' CreateFlowLogs (Prelude.Maybe Prelude.Int)
createFlowLogs_maxAggregationInterval = Lens.lens (\CreateFlowLogs' {maxAggregationInterval} -> maxAggregationInterval) (\s@CreateFlowLogs' {} a -> s {maxAggregationInterval = a} :: CreateFlowLogs)

-- | The tags to apply to the flow logs.
createFlowLogs_tagSpecifications :: Lens.Lens' CreateFlowLogs (Prelude.Maybe [TagSpecification])
createFlowLogs_tagSpecifications = Lens.lens (\CreateFlowLogs' {tagSpecifications} -> tagSpecifications) (\s@CreateFlowLogs' {} a -> s {tagSpecifications = a} :: CreateFlowLogs) Prelude.. Lens.mapping Lens.coerced

-- | The type of traffic to monitor (accepted traffic, rejected traffic, or
-- all traffic). This parameter is not supported for transit gateway
-- resource types. It is required for the other resource types.
createFlowLogs_trafficType :: Lens.Lens' CreateFlowLogs (Prelude.Maybe TrafficType)
createFlowLogs_trafficType = Lens.lens (\CreateFlowLogs' {trafficType} -> trafficType) (\s@CreateFlowLogs' {} a -> s {trafficType = a} :: CreateFlowLogs)

-- | The IDs of the resources to monitor. For example, if the resource type
-- is @VPC@, specify the IDs of the VPCs.
--
-- Constraints: Maximum of 25 for transit gateway resource types. Maximum
-- of 1000 for the other resource types.
createFlowLogs_resourceIds :: Lens.Lens' CreateFlowLogs [Prelude.Text]
createFlowLogs_resourceIds = Lens.lens (\CreateFlowLogs' {resourceIds} -> resourceIds) (\s@CreateFlowLogs' {} a -> s {resourceIds = a} :: CreateFlowLogs) Prelude.. Lens.coerced

-- | The type of resource to monitor.
createFlowLogs_resourceType :: Lens.Lens' CreateFlowLogs FlowLogsResourceType
createFlowLogs_resourceType = Lens.lens (\CreateFlowLogs' {resourceType} -> resourceType) (\s@CreateFlowLogs' {} a -> s {resourceType = a} :: CreateFlowLogs)

instance Core.AWSRequest CreateFlowLogs where
  type
    AWSResponse CreateFlowLogs =
      CreateFlowLogsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFlowLogsResponse'
            Prelude.<$> (x Data..@? "clientToken")
            Prelude.<*> ( x
                            Data..@? "flowLogIdSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> ( x
                            Data..@? "unsuccessful"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFlowLogs where
  hashWithSalt _salt CreateFlowLogs' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` deliverCrossAccountRole
      `Prelude.hashWithSalt` deliverLogsPermissionArn
      `Prelude.hashWithSalt` destinationOptions
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` logDestination
      `Prelude.hashWithSalt` logDestinationType
      `Prelude.hashWithSalt` logFormat
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` maxAggregationInterval
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` trafficType
      `Prelude.hashWithSalt` resourceIds
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData CreateFlowLogs where
  rnf CreateFlowLogs' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf deliverCrossAccountRole
      `Prelude.seq` Prelude.rnf deliverLogsPermissionArn
      `Prelude.seq` Prelude.rnf destinationOptions
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf logDestination
      `Prelude.seq` Prelude.rnf logDestinationType
      `Prelude.seq` Prelude.rnf logFormat
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf maxAggregationInterval
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf trafficType
      `Prelude.seq` Prelude.rnf resourceIds
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders CreateFlowLogs where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateFlowLogs where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFlowLogs where
  toQuery CreateFlowLogs' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateFlowLogs" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DeliverCrossAccountRole"
          Data.=: deliverCrossAccountRole,
        "DeliverLogsPermissionArn"
          Data.=: deliverLogsPermissionArn,
        "DestinationOptions" Data.=: destinationOptions,
        "DryRun" Data.=: dryRun,
        "LogDestination" Data.=: logDestination,
        "LogDestinationType" Data.=: logDestinationType,
        "LogFormat" Data.=: logFormat,
        "LogGroupName" Data.=: logGroupName,
        "MaxAggregationInterval"
          Data.=: maxAggregationInterval,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "TrafficType" Data.=: trafficType,
        Data.toQueryList "ResourceId" resourceIds,
        "ResourceType" Data.=: resourceType
      ]

-- | /See:/ 'newCreateFlowLogsResponse' smart constructor.
data CreateFlowLogsResponse = CreateFlowLogsResponse'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the flow logs.
    flowLogIds :: Prelude.Maybe [Prelude.Text],
    -- | Information about the flow logs that could not be created successfully.
    unsuccessful :: Prelude.Maybe [UnsuccessfulItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFlowLogsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createFlowLogsResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'flowLogIds', 'createFlowLogsResponse_flowLogIds' - The IDs of the flow logs.
--
-- 'unsuccessful', 'createFlowLogsResponse_unsuccessful' - Information about the flow logs that could not be created successfully.
--
-- 'httpStatus', 'createFlowLogsResponse_httpStatus' - The response's http status code.
newCreateFlowLogsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFlowLogsResponse
newCreateFlowLogsResponse pHttpStatus_ =
  CreateFlowLogsResponse'
    { clientToken =
        Prelude.Nothing,
      flowLogIds = Prelude.Nothing,
      unsuccessful = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createFlowLogsResponse_clientToken :: Lens.Lens' CreateFlowLogsResponse (Prelude.Maybe Prelude.Text)
createFlowLogsResponse_clientToken = Lens.lens (\CreateFlowLogsResponse' {clientToken} -> clientToken) (\s@CreateFlowLogsResponse' {} a -> s {clientToken = a} :: CreateFlowLogsResponse)

-- | The IDs of the flow logs.
createFlowLogsResponse_flowLogIds :: Lens.Lens' CreateFlowLogsResponse (Prelude.Maybe [Prelude.Text])
createFlowLogsResponse_flowLogIds = Lens.lens (\CreateFlowLogsResponse' {flowLogIds} -> flowLogIds) (\s@CreateFlowLogsResponse' {} a -> s {flowLogIds = a} :: CreateFlowLogsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the flow logs that could not be created successfully.
createFlowLogsResponse_unsuccessful :: Lens.Lens' CreateFlowLogsResponse (Prelude.Maybe [UnsuccessfulItem])
createFlowLogsResponse_unsuccessful = Lens.lens (\CreateFlowLogsResponse' {unsuccessful} -> unsuccessful) (\s@CreateFlowLogsResponse' {} a -> s {unsuccessful = a} :: CreateFlowLogsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createFlowLogsResponse_httpStatus :: Lens.Lens' CreateFlowLogsResponse Prelude.Int
createFlowLogsResponse_httpStatus = Lens.lens (\CreateFlowLogsResponse' {httpStatus} -> httpStatus) (\s@CreateFlowLogsResponse' {} a -> s {httpStatus = a} :: CreateFlowLogsResponse)

instance Prelude.NFData CreateFlowLogsResponse where
  rnf CreateFlowLogsResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf flowLogIds
      `Prelude.seq` Prelude.rnf unsuccessful
      `Prelude.seq` Prelude.rnf httpStatus
