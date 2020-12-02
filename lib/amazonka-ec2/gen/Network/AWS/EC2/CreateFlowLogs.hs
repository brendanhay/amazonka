{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- Flow log data for a monitored network interface is recorded as flow log records, which are log events consisting of fields that describe the traffic flow. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- When publishing to CloudWatch Logs, flow log records are published to a log group, and each network interface has a unique log stream in the log group. When publishing to Amazon S3, flow log records for all of the monitored network interfaces are published to a single log file object that is stored in the specified bucket.
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html VPC Flow Logs> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateFlowLogs
  ( -- * Creating a Request
    createFlowLogs,
    CreateFlowLogs,

    -- * Request Lenses
    cflLogFormat,
    cflMaxAggregationInterval,
    cflClientToken,
    cflLogDestination,
    cflLogGroupName,
    cflTagSpecifications,
    cflDeliverLogsPermissionARN,
    cflLogDestinationType,
    cflDryRun,
    cflResourceIds,
    cflResourceType,
    cflTrafficType,

    -- * Destructuring the Response
    createFlowLogsResponse,
    CreateFlowLogsResponse,

    -- * Response Lenses
    cflrsUnsuccessful,
    cflrsClientToken,
    cflrsFlowLogIds,
    cflrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFlowLogs' smart constructor.
data CreateFlowLogs = CreateFlowLogs'
  { _cflLogFormat ::
      !(Maybe Text),
    _cflMaxAggregationInterval :: !(Maybe Int),
    _cflClientToken :: !(Maybe Text),
    _cflLogDestination :: !(Maybe Text),
    _cflLogGroupName :: !(Maybe Text),
    _cflTagSpecifications :: !(Maybe [TagSpecification]),
    _cflDeliverLogsPermissionARN :: !(Maybe Text),
    _cflLogDestinationType :: !(Maybe LogDestinationType),
    _cflDryRun :: !(Maybe Bool),
    _cflResourceIds :: ![Text],
    _cflResourceType :: !FlowLogsResourceType,
    _cflTrafficType :: !TrafficType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateFlowLogs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cflLogFormat' - The fields to include in the flow log record, in the order in which they should appear. For a list of available fields, see <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records> . If you omit this parameter, the flow log is created using the default format. If you specify this parameter, you must specify at least one field. Specify the fields using the @> {field-id}@ format, separated by spaces. For the AWS CLI, use single quotation marks (' ') to surround the parameter value.
--
-- * 'cflMaxAggregationInterval' - The maximum interval of time during which a flow of packets is captured and aggregated into a flow log record. You can specify 60 seconds (1 minute) or 600 seconds (10 minutes). When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds or less, regardless of the value that you specify. Default: 600
--
-- * 'cflClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'cflLogDestination' - Specifies the destination to which the flow log data is to be published. Flow log data can be published to a CloudWatch Logs log group or an Amazon S3 bucket. The value specified for this parameter depends on the value specified for @LogDestinationType@ . If @LogDestinationType@ is not specified or @cloud-watch-logs@ , specify the Amazon Resource Name (ARN) of the CloudWatch Logs log group. For example, to publish to a log group called @my-logs@ , specify @arn:aws:logs:us-east-1:123456789012:log-group:my-logs@ . Alternatively, use @LogGroupName@ instead. If LogDestinationType is @s3@ , specify the ARN of the Amazon S3 bucket. You can also specify a subfolder in the bucket. To specify a subfolder in the bucket, use the following ARN format: @bucket_ARN/subfolder_name/@ . For example, to specify a subfolder named @my-logs@ in a bucket named @my-bucket@ , use the following ARN: @arn:aws:s3:::my-bucket/my-logs/@ . You cannot use @AWSLogs@ as a subfolder name. This is a reserved term.
--
-- * 'cflLogGroupName' - The name of a new or existing CloudWatch Logs log group where Amazon EC2 publishes your flow logs. If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
--
-- * 'cflTagSpecifications' - The tags to apply to the flow logs.
--
-- * 'cflDeliverLogsPermissionARN' - The ARN for the IAM role that permits Amazon EC2 to publish flow logs to a CloudWatch Logs log group in your account. If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
--
-- * 'cflLogDestinationType' - Specifies the type of destination to which the flow log data is to be published. Flow log data can be published to CloudWatch Logs or Amazon S3. To publish flow log data to CloudWatch Logs, specify @cloud-watch-logs@ . To publish flow log data to Amazon S3, specify @s3@ . If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ . Default: @cloud-watch-logs@
--
-- * 'cflDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cflResourceIds' - The ID of the subnet, network interface, or VPC for which you want to create a flow log. Constraints: Maximum of 1000 resources
--
-- * 'cflResourceType' - The type of resource for which to create the flow log. For example, if you specified a VPC ID for the @ResourceId@ property, specify @VPC@ for this property.
--
-- * 'cflTrafficType' - The type of traffic to log. You can log traffic that the resource accepts or rejects, or all traffic.
createFlowLogs ::
  -- | 'cflResourceType'
  FlowLogsResourceType ->
  -- | 'cflTrafficType'
  TrafficType ->
  CreateFlowLogs
createFlowLogs pResourceType_ pTrafficType_ =
  CreateFlowLogs'
    { _cflLogFormat = Nothing,
      _cflMaxAggregationInterval = Nothing,
      _cflClientToken = Nothing,
      _cflLogDestination = Nothing,
      _cflLogGroupName = Nothing,
      _cflTagSpecifications = Nothing,
      _cflDeliverLogsPermissionARN = Nothing,
      _cflLogDestinationType = Nothing,
      _cflDryRun = Nothing,
      _cflResourceIds = mempty,
      _cflResourceType = pResourceType_,
      _cflTrafficType = pTrafficType_
    }

-- | The fields to include in the flow log record, in the order in which they should appear. For a list of available fields, see <https://docs.aws.amazon.com/vpc/latest/userguide/flow-logs.html#flow-log-records Flow Log Records> . If you omit this parameter, the flow log is created using the default format. If you specify this parameter, you must specify at least one field. Specify the fields using the @> {field-id}@ format, separated by spaces. For the AWS CLI, use single quotation marks (' ') to surround the parameter value.
cflLogFormat :: Lens' CreateFlowLogs (Maybe Text)
cflLogFormat = lens _cflLogFormat (\s a -> s {_cflLogFormat = a})

-- | The maximum interval of time during which a flow of packets is captured and aggregated into a flow log record. You can specify 60 seconds (1 minute) or 600 seconds (10 minutes). When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds or less, regardless of the value that you specify. Default: 600
cflMaxAggregationInterval :: Lens' CreateFlowLogs (Maybe Int)
cflMaxAggregationInterval = lens _cflMaxAggregationInterval (\s a -> s {_cflMaxAggregationInterval = a})

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
cflClientToken :: Lens' CreateFlowLogs (Maybe Text)
cflClientToken = lens _cflClientToken (\s a -> s {_cflClientToken = a})

-- | Specifies the destination to which the flow log data is to be published. Flow log data can be published to a CloudWatch Logs log group or an Amazon S3 bucket. The value specified for this parameter depends on the value specified for @LogDestinationType@ . If @LogDestinationType@ is not specified or @cloud-watch-logs@ , specify the Amazon Resource Name (ARN) of the CloudWatch Logs log group. For example, to publish to a log group called @my-logs@ , specify @arn:aws:logs:us-east-1:123456789012:log-group:my-logs@ . Alternatively, use @LogGroupName@ instead. If LogDestinationType is @s3@ , specify the ARN of the Amazon S3 bucket. You can also specify a subfolder in the bucket. To specify a subfolder in the bucket, use the following ARN format: @bucket_ARN/subfolder_name/@ . For example, to specify a subfolder named @my-logs@ in a bucket named @my-bucket@ , use the following ARN: @arn:aws:s3:::my-bucket/my-logs/@ . You cannot use @AWSLogs@ as a subfolder name. This is a reserved term.
cflLogDestination :: Lens' CreateFlowLogs (Maybe Text)
cflLogDestination = lens _cflLogDestination (\s a -> s {_cflLogDestination = a})

-- | The name of a new or existing CloudWatch Logs log group where Amazon EC2 publishes your flow logs. If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
cflLogGroupName :: Lens' CreateFlowLogs (Maybe Text)
cflLogGroupName = lens _cflLogGroupName (\s a -> s {_cflLogGroupName = a})

-- | The tags to apply to the flow logs.
cflTagSpecifications :: Lens' CreateFlowLogs [TagSpecification]
cflTagSpecifications = lens _cflTagSpecifications (\s a -> s {_cflTagSpecifications = a}) . _Default . _Coerce

-- | The ARN for the IAM role that permits Amazon EC2 to publish flow logs to a CloudWatch Logs log group in your account. If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ .
cflDeliverLogsPermissionARN :: Lens' CreateFlowLogs (Maybe Text)
cflDeliverLogsPermissionARN = lens _cflDeliverLogsPermissionARN (\s a -> s {_cflDeliverLogsPermissionARN = a})

-- | Specifies the type of destination to which the flow log data is to be published. Flow log data can be published to CloudWatch Logs or Amazon S3. To publish flow log data to CloudWatch Logs, specify @cloud-watch-logs@ . To publish flow log data to Amazon S3, specify @s3@ . If you specify @LogDestinationType@ as @s3@ , do not specify @DeliverLogsPermissionArn@ or @LogGroupName@ . Default: @cloud-watch-logs@
cflLogDestinationType :: Lens' CreateFlowLogs (Maybe LogDestinationType)
cflLogDestinationType = lens _cflLogDestinationType (\s a -> s {_cflLogDestinationType = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cflDryRun :: Lens' CreateFlowLogs (Maybe Bool)
cflDryRun = lens _cflDryRun (\s a -> s {_cflDryRun = a})

-- | The ID of the subnet, network interface, or VPC for which you want to create a flow log. Constraints: Maximum of 1000 resources
cflResourceIds :: Lens' CreateFlowLogs [Text]
cflResourceIds = lens _cflResourceIds (\s a -> s {_cflResourceIds = a}) . _Coerce

-- | The type of resource for which to create the flow log. For example, if you specified a VPC ID for the @ResourceId@ property, specify @VPC@ for this property.
cflResourceType :: Lens' CreateFlowLogs FlowLogsResourceType
cflResourceType = lens _cflResourceType (\s a -> s {_cflResourceType = a})

-- | The type of traffic to log. You can log traffic that the resource accepts or rejects, or all traffic.
cflTrafficType :: Lens' CreateFlowLogs TrafficType
cflTrafficType = lens _cflTrafficType (\s a -> s {_cflTrafficType = a})

instance AWSRequest CreateFlowLogs where
  type Rs CreateFlowLogs = CreateFlowLogsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateFlowLogsResponse'
            <$> (x .@? "unsuccessful" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "clientToken")
            <*> (x .@? "flowLogIdSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (pure (fromEnum s))
      )

instance Hashable CreateFlowLogs

instance NFData CreateFlowLogs

instance ToHeaders CreateFlowLogs where
  toHeaders = const mempty

instance ToPath CreateFlowLogs where
  toPath = const "/"

instance ToQuery CreateFlowLogs where
  toQuery CreateFlowLogs' {..} =
    mconcat
      [ "Action" =: ("CreateFlowLogs" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "LogFormat" =: _cflLogFormat,
        "MaxAggregationInterval" =: _cflMaxAggregationInterval,
        "ClientToken" =: _cflClientToken,
        "LogDestination" =: _cflLogDestination,
        "LogGroupName" =: _cflLogGroupName,
        toQuery (toQueryList "TagSpecification" <$> _cflTagSpecifications),
        "DeliverLogsPermissionArn" =: _cflDeliverLogsPermissionARN,
        "LogDestinationType" =: _cflLogDestinationType,
        "DryRun" =: _cflDryRun,
        toQueryList "ResourceId" _cflResourceIds,
        "ResourceType" =: _cflResourceType,
        "TrafficType" =: _cflTrafficType
      ]

-- | /See:/ 'createFlowLogsResponse' smart constructor.
data CreateFlowLogsResponse = CreateFlowLogsResponse'
  { _cflrsUnsuccessful ::
      !(Maybe [UnsuccessfulItem]),
    _cflrsClientToken :: !(Maybe Text),
    _cflrsFlowLogIds :: !(Maybe [Text]),
    _cflrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateFlowLogsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cflrsUnsuccessful' - Information about the flow logs that could not be created successfully.
--
-- * 'cflrsClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- * 'cflrsFlowLogIds' - The IDs of the flow logs.
--
-- * 'cflrsResponseStatus' - -- | The response status code.
createFlowLogsResponse ::
  -- | 'cflrsResponseStatus'
  Int ->
  CreateFlowLogsResponse
createFlowLogsResponse pResponseStatus_ =
  CreateFlowLogsResponse'
    { _cflrsUnsuccessful = Nothing,
      _cflrsClientToken = Nothing,
      _cflrsFlowLogIds = Nothing,
      _cflrsResponseStatus = pResponseStatus_
    }

-- | Information about the flow logs that could not be created successfully.
cflrsUnsuccessful :: Lens' CreateFlowLogsResponse [UnsuccessfulItem]
cflrsUnsuccessful = lens _cflrsUnsuccessful (\s a -> s {_cflrsUnsuccessful = a}) . _Default . _Coerce

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
cflrsClientToken :: Lens' CreateFlowLogsResponse (Maybe Text)
cflrsClientToken = lens _cflrsClientToken (\s a -> s {_cflrsClientToken = a})

-- | The IDs of the flow logs.
cflrsFlowLogIds :: Lens' CreateFlowLogsResponse [Text]
cflrsFlowLogIds = lens _cflrsFlowLogIds (\s a -> s {_cflrsFlowLogIds = a}) . _Default . _Coerce

-- | -- | The response status code.
cflrsResponseStatus :: Lens' CreateFlowLogsResponse Int
cflrsResponseStatus = lens _cflrsResponseStatus (\s a -> s {_cflrsResponseStatus = a})

instance NFData CreateFlowLogsResponse
