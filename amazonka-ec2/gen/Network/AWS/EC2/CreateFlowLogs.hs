{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.CreateFlowLogs
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates one or more flow logs to capture IP traffic for a specific
-- network interface, subnet, or VPC. Flow logs are delivered to a
-- specified log group in Amazon CloudWatch Logs. If you specify a VPC or
-- subnet in the request, a log stream is created in CloudWatch Logs for
-- each network interface in the subnet or VPC. Log streams can include
-- information about accepted and rejected traffic to a network interface.
-- You can view the data in your log streams using Amazon CloudWatch Logs.
--
-- In your request, you must also specify an IAM role that has permission
-- to publish logs to CloudWatch Logs.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateFlowLogs.html>
module Network.AWS.EC2.CreateFlowLogs
    (
    -- * Request
      CreateFlowLogs
    -- ** Request constructor
    , createFlowLogs
    -- ** Request lenses
    , cflClientToken
    , cflResourceIds
    , cflResourceType
    , cflTrafficType
    , cflLogGroupName
    , cflDeliverLogsPermissionARN

    -- * Response
    , CreateFlowLogsResponse
    -- ** Response constructor
    , createFlowLogsResponse
    -- ** Response lenses
    , cflrUnsuccessful
    , cflrClientToken
    , cflrFlowLogIds
    , cflrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createFlowLogs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cflClientToken'
--
-- * 'cflResourceIds'
--
-- * 'cflResourceType'
--
-- * 'cflTrafficType'
--
-- * 'cflLogGroupName'
--
-- * 'cflDeliverLogsPermissionARN'
data CreateFlowLogs = CreateFlowLogs'
    { _cflClientToken              :: !(Maybe Text)
    , _cflResourceIds              :: ![Text]
    , _cflResourceType             :: !FlowLogsResourceType
    , _cflTrafficType              :: !TrafficType
    , _cflLogGroupName             :: !Text
    , _cflDeliverLogsPermissionARN :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreateFlowLogs' smart constructor.
createFlowLogs :: FlowLogsResourceType -> TrafficType -> Text -> Text -> CreateFlowLogs
createFlowLogs pResourceType pTrafficType pLogGroupName pDeliverLogsPermissionARN =
    CreateFlowLogs'
    { _cflClientToken = Nothing
    , _cflResourceIds = mempty
    , _cflResourceType = pResourceType
    , _cflTrafficType = pTrafficType
    , _cflLogGroupName = pLogGroupName
    , _cflDeliverLogsPermissionARN = pDeliverLogsPermissionARN
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
cflClientToken :: Lens' CreateFlowLogs (Maybe Text)
cflClientToken = lens _cflClientToken (\ s a -> s{_cflClientToken = a});

-- | One or more subnet, network interface, or VPC IDs.
cflResourceIds :: Lens' CreateFlowLogs [Text]
cflResourceIds = lens _cflResourceIds (\ s a -> s{_cflResourceIds = a});

-- | The type of resource on which to create the flow log.
cflResourceType :: Lens' CreateFlowLogs FlowLogsResourceType
cflResourceType = lens _cflResourceType (\ s a -> s{_cflResourceType = a});

-- | The type of traffic to log.
cflTrafficType :: Lens' CreateFlowLogs TrafficType
cflTrafficType = lens _cflTrafficType (\ s a -> s{_cflTrafficType = a});

-- | The name of the CloudWatch log group.
cflLogGroupName :: Lens' CreateFlowLogs Text
cflLogGroupName = lens _cflLogGroupName (\ s a -> s{_cflLogGroupName = a});

-- | The ARN for the IAM role that\'s used to post flow logs to a CloudWatch
-- Logs log group.
cflDeliverLogsPermissionARN :: Lens' CreateFlowLogs Text
cflDeliverLogsPermissionARN = lens _cflDeliverLogsPermissionARN (\ s a -> s{_cflDeliverLogsPermissionARN = a});

instance AWSRequest CreateFlowLogs where
        type Sv CreateFlowLogs = EC2
        type Rs CreateFlowLogs = CreateFlowLogsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateFlowLogsResponse' <$>
                   (may (parseXMLList "item") x) <*>
                     (x .@? "clientToken")
                     <*> (may (parseXMLList "item") x)
                     <*> (pure s))

instance ToHeaders CreateFlowLogs where
        toHeaders = const mempty

instance ToPath CreateFlowLogs where
        toPath = const "/"

instance ToQuery CreateFlowLogs where
        toQuery CreateFlowLogs'{..}
          = mconcat
              ["Action" =: ("CreateFlowLogs" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "ClientToken" =: _cflClientToken,
               toQueryList "item" _cflResourceIds,
               "ResourceType" =: _cflResourceType,
               "TrafficType" =: _cflTrafficType,
               "LogGroupName" =: _cflLogGroupName,
               "DeliverLogsPermissionArn" =:
                 _cflDeliverLogsPermissionARN]

-- | /See:/ 'createFlowLogsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cflrUnsuccessful'
--
-- * 'cflrClientToken'
--
-- * 'cflrFlowLogIds'
--
-- * 'cflrStatus'
data CreateFlowLogsResponse = CreateFlowLogsResponse'
    { _cflrUnsuccessful :: !(Maybe [UnsuccessfulItem])
    , _cflrClientToken  :: !(Maybe Text)
    , _cflrFlowLogIds   :: !(Maybe [Text])
    , _cflrStatus       :: !Status
    } deriving (Eq,Show)

-- | 'CreateFlowLogsResponse' smart constructor.
createFlowLogsResponse :: Status -> CreateFlowLogsResponse
createFlowLogsResponse pStatus =
    CreateFlowLogsResponse'
    { _cflrUnsuccessful = Nothing
    , _cflrClientToken = Nothing
    , _cflrFlowLogIds = Nothing
    , _cflrStatus = pStatus
    }

-- | Information about the flow logs that could not be created successfully.
cflrUnsuccessful :: Lens' CreateFlowLogsResponse [UnsuccessfulItem]
cflrUnsuccessful = lens _cflrUnsuccessful (\ s a -> s{_cflrUnsuccessful = a}) . _Default;

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request.
cflrClientToken :: Lens' CreateFlowLogsResponse (Maybe Text)
cflrClientToken = lens _cflrClientToken (\ s a -> s{_cflrClientToken = a});

-- | The IDs of the flow logs.
cflrFlowLogIds :: Lens' CreateFlowLogsResponse [Text]
cflrFlowLogIds = lens _cflrFlowLogIds (\ s a -> s{_cflrFlowLogIds = a}) . _Default;

-- | FIXME: Undocumented member.
cflrStatus :: Lens' CreateFlowLogsResponse Status
cflrStatus = lens _cflrStatus (\ s a -> s{_cflrStatus = a});
