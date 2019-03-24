{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateFlowLogs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more flow logs to capture information about IP traffic for a specific network interface, subnet, or VPC.
--
--
-- Flow log data for a monitored network interface is recorded as flow log records, which are log events consisting of fields that describe the traffic flow. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/flow-logs.html#flow-log-records Flow Log Records> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- When publishing to CloudWatch Logs, flow log records are published to a log group, and each network interface has a unique log stream in the log group. When publishing to Amazon S3, flow log records for all of the monitored network interfaces are published to a single log file object that is stored in the specified bucket.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/flow-logs.html VPC Flow Logs> in the /Amazon Virtual Private Cloud User Guide/ .
--
module Network.AWS.EC2.CreateFlowLogs
    (
    -- * Creating a Request
      createFlowLogs
    , CreateFlowLogs
    -- * Request Lenses
    , cflClientToken
    , cflLogDestination
    , cflLogGroupName
    , cflDeliverLogsPermissionARN
    , cflLogDestinationType
    , cflDryRun
    , cflResourceIds
    , cflResourceType
    , cflTrafficType

    -- * Destructuring the Response
    , createFlowLogsResponse
    , CreateFlowLogsResponse
    -- * Response Lenses
    , cflrsUnsuccessful
    , cflrsClientToken
    , cflrsFlowLogIds
    , cflrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFlowLogs' smart constructor.
data CreateFlowLogs = CreateFlowLogs'
  { _cflClientToken              :: !(Maybe Text)
  , _cflLogDestination           :: !(Maybe Text)
  , _cflLogGroupName             :: !(Maybe Text)
  , _cflDeliverLogsPermissionARN :: !(Maybe Text)
  , _cflLogDestinationType       :: !(Maybe LogDestinationType)
  , _cflDryRun                   :: !(Maybe Bool)
  , _cflResourceIds              :: ![Text]
  , _cflResourceType             :: !FlowLogsResourceType
  , _cflTrafficType              :: !TrafficType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFlowLogs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cflClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'cflLogDestination' - Specifies the destination to which the flow log data is to be published. Flow log data can be published to an CloudWatch Logs log group or an Amazon S3 bucket. The value specified for this parameter depends on the value specified for LogDestinationType. If LogDestinationType is not specified or @cloud-watch-logs@ , specify the Amazon Resource Name (ARN) of the CloudWatch Logs log group. If LogDestinationType is @s3@ , specify the ARN of the Amazon S3 bucket. You can also specify a subfolder in the bucket. To specify a subfolder in the bucket, use the following ARN format: @bucket_ARN/subfolder_name/@ . For example, to specify a subfolder named @my-logs@ in a bucket named @my-bucket@ , use the following ARN: @arn:aws:s3:::my-bucket/my-logs/@ . You cannot use @AWSLogs@ as a subfolder name. This is a reserved term.
--
-- * 'cflLogGroupName' - The name of the log group.
--
-- * 'cflDeliverLogsPermissionARN' - The ARN for the IAM role that's used to post flow logs to a log group.
--
-- * 'cflLogDestinationType' - Specifies the type of destination to which the flow log data is to be published. Flow log data can be published to CloudWatch Logs or Amazon S3. To publish flow log data to CloudWatch Logs, specify @cloud-watch-logs@ . To publish flow log data to Amazon S3, specify @s3@ . Default: @cloud-watch-logs@
--
-- * 'cflDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cflResourceIds' - One or more subnet, network interface, or VPC IDs. Constraints: Maximum of 1000 resources
--
-- * 'cflResourceType' - The type of resource on which to create the flow log.
--
-- * 'cflTrafficType' - The type of traffic to log.
createFlowLogs
    :: FlowLogsResourceType -- ^ 'cflResourceType'
    -> TrafficType -- ^ 'cflTrafficType'
    -> CreateFlowLogs
createFlowLogs pResourceType_ pTrafficType_ =
  CreateFlowLogs'
    { _cflClientToken = Nothing
    , _cflLogDestination = Nothing
    , _cflLogGroupName = Nothing
    , _cflDeliverLogsPermissionARN = Nothing
    , _cflLogDestinationType = Nothing
    , _cflDryRun = Nothing
    , _cflResourceIds = mempty
    , _cflResourceType = pResourceType_
    , _cflTrafficType = pTrafficType_
    }


-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
cflClientToken :: Lens' CreateFlowLogs (Maybe Text)
cflClientToken = lens _cflClientToken (\ s a -> s{_cflClientToken = a})

-- | Specifies the destination to which the flow log data is to be published. Flow log data can be published to an CloudWatch Logs log group or an Amazon S3 bucket. The value specified for this parameter depends on the value specified for LogDestinationType. If LogDestinationType is not specified or @cloud-watch-logs@ , specify the Amazon Resource Name (ARN) of the CloudWatch Logs log group. If LogDestinationType is @s3@ , specify the ARN of the Amazon S3 bucket. You can also specify a subfolder in the bucket. To specify a subfolder in the bucket, use the following ARN format: @bucket_ARN/subfolder_name/@ . For example, to specify a subfolder named @my-logs@ in a bucket named @my-bucket@ , use the following ARN: @arn:aws:s3:::my-bucket/my-logs/@ . You cannot use @AWSLogs@ as a subfolder name. This is a reserved term.
cflLogDestination :: Lens' CreateFlowLogs (Maybe Text)
cflLogDestination = lens _cflLogDestination (\ s a -> s{_cflLogDestination = a})

-- | The name of the log group.
cflLogGroupName :: Lens' CreateFlowLogs (Maybe Text)
cflLogGroupName = lens _cflLogGroupName (\ s a -> s{_cflLogGroupName = a})

-- | The ARN for the IAM role that's used to post flow logs to a log group.
cflDeliverLogsPermissionARN :: Lens' CreateFlowLogs (Maybe Text)
cflDeliverLogsPermissionARN = lens _cflDeliverLogsPermissionARN (\ s a -> s{_cflDeliverLogsPermissionARN = a})

-- | Specifies the type of destination to which the flow log data is to be published. Flow log data can be published to CloudWatch Logs or Amazon S3. To publish flow log data to CloudWatch Logs, specify @cloud-watch-logs@ . To publish flow log data to Amazon S3, specify @s3@ . Default: @cloud-watch-logs@
cflLogDestinationType :: Lens' CreateFlowLogs (Maybe LogDestinationType)
cflLogDestinationType = lens _cflLogDestinationType (\ s a -> s{_cflLogDestinationType = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cflDryRun :: Lens' CreateFlowLogs (Maybe Bool)
cflDryRun = lens _cflDryRun (\ s a -> s{_cflDryRun = a})

-- | One or more subnet, network interface, or VPC IDs. Constraints: Maximum of 1000 resources
cflResourceIds :: Lens' CreateFlowLogs [Text]
cflResourceIds = lens _cflResourceIds (\ s a -> s{_cflResourceIds = a}) . _Coerce

-- | The type of resource on which to create the flow log.
cflResourceType :: Lens' CreateFlowLogs FlowLogsResourceType
cflResourceType = lens _cflResourceType (\ s a -> s{_cflResourceType = a})

-- | The type of traffic to log.
cflTrafficType :: Lens' CreateFlowLogs TrafficType
cflTrafficType = lens _cflTrafficType (\ s a -> s{_cflTrafficType = a})

instance AWSRequest CreateFlowLogs where
        type Rs CreateFlowLogs = CreateFlowLogsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateFlowLogsResponse' <$>
                   (x .@? "unsuccessful" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "clientToken")
                     <*>
                     (x .@? "flowLogIdSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable CreateFlowLogs where

instance NFData CreateFlowLogs where

instance ToHeaders CreateFlowLogs where
        toHeaders = const mempty

instance ToPath CreateFlowLogs where
        toPath = const "/"

instance ToQuery CreateFlowLogs where
        toQuery CreateFlowLogs'{..}
          = mconcat
              ["Action" =: ("CreateFlowLogs" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _cflClientToken,
               "LogDestination" =: _cflLogDestination,
               "LogGroupName" =: _cflLogGroupName,
               "DeliverLogsPermissionArn" =:
                 _cflDeliverLogsPermissionARN,
               "LogDestinationType" =: _cflLogDestinationType,
               "DryRun" =: _cflDryRun,
               toQueryList "ResourceId" _cflResourceIds,
               "ResourceType" =: _cflResourceType,
               "TrafficType" =: _cflTrafficType]

-- | /See:/ 'createFlowLogsResponse' smart constructor.
data CreateFlowLogsResponse = CreateFlowLogsResponse'
  { _cflrsUnsuccessful   :: !(Maybe [UnsuccessfulItem])
  , _cflrsClientToken    :: !(Maybe Text)
  , _cflrsFlowLogIds     :: !(Maybe [Text])
  , _cflrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
createFlowLogsResponse
    :: Int -- ^ 'cflrsResponseStatus'
    -> CreateFlowLogsResponse
createFlowLogsResponse pResponseStatus_ =
  CreateFlowLogsResponse'
    { _cflrsUnsuccessful = Nothing
    , _cflrsClientToken = Nothing
    , _cflrsFlowLogIds = Nothing
    , _cflrsResponseStatus = pResponseStatus_
    }


-- | Information about the flow logs that could not be created successfully.
cflrsUnsuccessful :: Lens' CreateFlowLogsResponse [UnsuccessfulItem]
cflrsUnsuccessful = lens _cflrsUnsuccessful (\ s a -> s{_cflrsUnsuccessful = a}) . _Default . _Coerce

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
cflrsClientToken :: Lens' CreateFlowLogsResponse (Maybe Text)
cflrsClientToken = lens _cflrsClientToken (\ s a -> s{_cflrsClientToken = a})

-- | The IDs of the flow logs.
cflrsFlowLogIds :: Lens' CreateFlowLogsResponse [Text]
cflrsFlowLogIds = lens _cflrsFlowLogIds (\ s a -> s{_cflrsFlowLogIds = a}) . _Default . _Coerce

-- | -- | The response status code.
cflrsResponseStatus :: Lens' CreateFlowLogsResponse Int
cflrsResponseStatus = lens _cflrsResponseStatus (\ s a -> s{_cflrsResponseStatus = a})

instance NFData CreateFlowLogsResponse where
