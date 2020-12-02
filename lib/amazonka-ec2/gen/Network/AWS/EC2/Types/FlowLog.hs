{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FlowLog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FlowLog where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LogDestinationType
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TrafficType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a flow log.
--
--
--
-- /See:/ 'flowLog' smart constructor.
data FlowLog = FlowLog'
  { _flCreationTime :: !(Maybe ISO8601),
    _flLogFormat :: !(Maybe Text),
    _flMaxAggregationInterval :: !(Maybe Int),
    _flResourceId :: !(Maybe Text),
    _flFlowLogStatus :: !(Maybe Text),
    _flTrafficType :: !(Maybe TrafficType),
    _flLogDestination :: !(Maybe Text),
    _flDeliverLogsStatus :: !(Maybe Text),
    _flDeliverLogsErrorMessage :: !(Maybe Text),
    _flLogGroupName :: !(Maybe Text),
    _flDeliverLogsPermissionARN :: !(Maybe Text),
    _flLogDestinationType :: !(Maybe LogDestinationType),
    _flFlowLogId :: !(Maybe Text),
    _flTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FlowLog' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flCreationTime' - The date and time the flow log was created.
--
-- * 'flLogFormat' - The format of the flow log record.
--
-- * 'flMaxAggregationInterval' - The maximum interval of time, in seconds, during which a flow of packets is captured and aggregated into a flow log record. When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds (1 minute) or less, regardless of the specified value. Valid Values: @60@ | @600@
--
-- * 'flResourceId' - The ID of the resource on which the flow log was created.
--
-- * 'flFlowLogStatus' - The status of the flow log (@ACTIVE@ ).
--
-- * 'flTrafficType' - The type of traffic captured for the flow log.
--
-- * 'flLogDestination' - Specifies the destination to which the flow log data is published. Flow log data can be published to an CloudWatch Logs log group or an Amazon S3 bucket. If the flow log publishes to CloudWatch Logs, this element indicates the Amazon Resource Name (ARN) of the CloudWatch Logs log group to which the data is published. If the flow log publishes to Amazon S3, this element indicates the ARN of the Amazon S3 bucket to which the data is published.
--
-- * 'flDeliverLogsStatus' - The status of the logs delivery (@SUCCESS@ | @FAILED@ ).
--
-- * 'flDeliverLogsErrorMessage' - Information about the error that occurred. @Rate limited@ indicates that CloudWatch Logs throttling has been applied for one or more network interfaces, or that you've reached the limit on the number of log groups that you can create. @Access error@ indicates that the IAM role associated with the flow log does not have sufficient permissions to publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
--
-- * 'flLogGroupName' - The name of the flow log group.
--
-- * 'flDeliverLogsPermissionARN' - The ARN of the IAM role that posts logs to CloudWatch Logs.
--
-- * 'flLogDestinationType' - Specifies the type of destination to which the flow log data is published. Flow log data can be published to CloudWatch Logs or Amazon S3.
--
-- * 'flFlowLogId' - The flow log ID.
--
-- * 'flTags' - The tags for the flow log.
flowLog ::
  FlowLog
flowLog =
  FlowLog'
    { _flCreationTime = Nothing,
      _flLogFormat = Nothing,
      _flMaxAggregationInterval = Nothing,
      _flResourceId = Nothing,
      _flFlowLogStatus = Nothing,
      _flTrafficType = Nothing,
      _flLogDestination = Nothing,
      _flDeliverLogsStatus = Nothing,
      _flDeliverLogsErrorMessage = Nothing,
      _flLogGroupName = Nothing,
      _flDeliverLogsPermissionARN = Nothing,
      _flLogDestinationType = Nothing,
      _flFlowLogId = Nothing,
      _flTags = Nothing
    }

-- | The date and time the flow log was created.
flCreationTime :: Lens' FlowLog (Maybe UTCTime)
flCreationTime = lens _flCreationTime (\s a -> s {_flCreationTime = a}) . mapping _Time

-- | The format of the flow log record.
flLogFormat :: Lens' FlowLog (Maybe Text)
flLogFormat = lens _flLogFormat (\s a -> s {_flLogFormat = a})

-- | The maximum interval of time, in seconds, during which a flow of packets is captured and aggregated into a flow log record. When a network interface is attached to a <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instance> , the aggregation interval is always 60 seconds (1 minute) or less, regardless of the specified value. Valid Values: @60@ | @600@
flMaxAggregationInterval :: Lens' FlowLog (Maybe Int)
flMaxAggregationInterval = lens _flMaxAggregationInterval (\s a -> s {_flMaxAggregationInterval = a})

-- | The ID of the resource on which the flow log was created.
flResourceId :: Lens' FlowLog (Maybe Text)
flResourceId = lens _flResourceId (\s a -> s {_flResourceId = a})

-- | The status of the flow log (@ACTIVE@ ).
flFlowLogStatus :: Lens' FlowLog (Maybe Text)
flFlowLogStatus = lens _flFlowLogStatus (\s a -> s {_flFlowLogStatus = a})

-- | The type of traffic captured for the flow log.
flTrafficType :: Lens' FlowLog (Maybe TrafficType)
flTrafficType = lens _flTrafficType (\s a -> s {_flTrafficType = a})

-- | Specifies the destination to which the flow log data is published. Flow log data can be published to an CloudWatch Logs log group or an Amazon S3 bucket. If the flow log publishes to CloudWatch Logs, this element indicates the Amazon Resource Name (ARN) of the CloudWatch Logs log group to which the data is published. If the flow log publishes to Amazon S3, this element indicates the ARN of the Amazon S3 bucket to which the data is published.
flLogDestination :: Lens' FlowLog (Maybe Text)
flLogDestination = lens _flLogDestination (\s a -> s {_flLogDestination = a})

-- | The status of the logs delivery (@SUCCESS@ | @FAILED@ ).
flDeliverLogsStatus :: Lens' FlowLog (Maybe Text)
flDeliverLogsStatus = lens _flDeliverLogsStatus (\s a -> s {_flDeliverLogsStatus = a})

-- | Information about the error that occurred. @Rate limited@ indicates that CloudWatch Logs throttling has been applied for one or more network interfaces, or that you've reached the limit on the number of log groups that you can create. @Access error@ indicates that the IAM role associated with the flow log does not have sufficient permissions to publish to CloudWatch Logs. @Unknown error@ indicates an internal error.
flDeliverLogsErrorMessage :: Lens' FlowLog (Maybe Text)
flDeliverLogsErrorMessage = lens _flDeliverLogsErrorMessage (\s a -> s {_flDeliverLogsErrorMessage = a})

-- | The name of the flow log group.
flLogGroupName :: Lens' FlowLog (Maybe Text)
flLogGroupName = lens _flLogGroupName (\s a -> s {_flLogGroupName = a})

-- | The ARN of the IAM role that posts logs to CloudWatch Logs.
flDeliverLogsPermissionARN :: Lens' FlowLog (Maybe Text)
flDeliverLogsPermissionARN = lens _flDeliverLogsPermissionARN (\s a -> s {_flDeliverLogsPermissionARN = a})

-- | Specifies the type of destination to which the flow log data is published. Flow log data can be published to CloudWatch Logs or Amazon S3.
flLogDestinationType :: Lens' FlowLog (Maybe LogDestinationType)
flLogDestinationType = lens _flLogDestinationType (\s a -> s {_flLogDestinationType = a})

-- | The flow log ID.
flFlowLogId :: Lens' FlowLog (Maybe Text)
flFlowLogId = lens _flFlowLogId (\s a -> s {_flFlowLogId = a})

-- | The tags for the flow log.
flTags :: Lens' FlowLog [Tag]
flTags = lens _flTags (\s a -> s {_flTags = a}) . _Default . _Coerce

instance FromXML FlowLog where
  parseXML x =
    FlowLog'
      <$> (x .@? "creationTime")
      <*> (x .@? "logFormat")
      <*> (x .@? "maxAggregationInterval")
      <*> (x .@? "resourceId")
      <*> (x .@? "flowLogStatus")
      <*> (x .@? "trafficType")
      <*> (x .@? "logDestination")
      <*> (x .@? "deliverLogsStatus")
      <*> (x .@? "deliverLogsErrorMessage")
      <*> (x .@? "logGroupName")
      <*> (x .@? "deliverLogsPermissionArn")
      <*> (x .@? "logDestinationType")
      <*> (x .@? "flowLogId")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable FlowLog

instance NFData FlowLog
