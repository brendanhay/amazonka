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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more flow logs to capture IP traffic for a specific
-- network interface, subnet, or VPC. Flow logs are delivered to a
-- specified log group in Amazon CloudWatch Logs. If you specify a VPC or
-- subnet in the request, a log stream is created in CloudWatch Logs for
-- each network interface in the subnet or VPC. Log streams can include
-- information about accepted and rejected traffic to a network interface.
-- You can view the data in your log streams using Amazon CloudWatch Logs.
--
-- In your request, you must also specify an IAM role that has permission
-- to publish logs to CloudWatch Logs.
module Network.AWS.EC2.CreateFlowLogs
    (
    -- * Creating a Request
      createFlowLogs
    , CreateFlowLogs
    -- * Request Lenses
    , cflClientToken
    , cflResourceIds
    , cflResourceType
    , cflTrafficType
    , cflLogGroupName
    , cflDeliverLogsPermissionARN

    -- * Destructuring the Response
    , createFlowLogsResponse
    , CreateFlowLogsResponse
    -- * Response Lenses
    , cflrsUnsuccessful
    , cflrsClientToken
    , cflrsFlowLogIds
    , cflrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for CreateFlowLogs.
--
-- /See:/ 'createFlowLogs' smart constructor.
data CreateFlowLogs = CreateFlowLogs'
    { _cflClientToken              :: !(Maybe Text)
    , _cflResourceIds              :: ![Text]
    , _cflResourceType             :: !FlowLogsResourceType
    , _cflTrafficType              :: !TrafficType
    , _cflLogGroupName             :: !Text
    , _cflDeliverLogsPermissionARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateFlowLogs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
createFlowLogs
    :: FlowLogsResourceType -- ^ 'cflResourceType'
    -> TrafficType -- ^ 'cflTrafficType'
    -> Text -- ^ 'cflLogGroupName'
    -> Text -- ^ 'cflDeliverLogsPermissionARN'
    -> CreateFlowLogs
createFlowLogs pResourceType_ pTrafficType_ pLogGroupName_ pDeliverLogsPermissionARN_ =
    CreateFlowLogs'
    { _cflClientToken = Nothing
    , _cflResourceIds = mempty
    , _cflResourceType = pResourceType_
    , _cflTrafficType = pTrafficType_
    , _cflLogGroupName = pLogGroupName_
    , _cflDeliverLogsPermissionARN = pDeliverLogsPermissionARN_
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
cflClientToken :: Lens' CreateFlowLogs (Maybe Text)
cflClientToken = lens _cflClientToken (\ s a -> s{_cflClientToken = a});

-- | One or more subnet, network interface, or VPC IDs.
cflResourceIds :: Lens' CreateFlowLogs [Text]
cflResourceIds = lens _cflResourceIds (\ s a -> s{_cflResourceIds = a}) . _Coerce;

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

instance Hashable CreateFlowLogs

instance NFData CreateFlowLogs

instance ToHeaders CreateFlowLogs where
        toHeaders = const mempty

instance ToPath CreateFlowLogs where
        toPath = const "/"

instance ToQuery CreateFlowLogs where
        toQuery CreateFlowLogs'{..}
          = mconcat
              ["Action" =: ("CreateFlowLogs" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "ClientToken" =: _cflClientToken,
               toQueryList "ResourceId" _cflResourceIds,
               "ResourceType" =: _cflResourceType,
               "TrafficType" =: _cflTrafficType,
               "LogGroupName" =: _cflLogGroupName,
               "DeliverLogsPermissionArn" =:
                 _cflDeliverLogsPermissionARN]

-- | Contains the output of CreateFlowLogs.
--
-- /See:/ 'createFlowLogsResponse' smart constructor.
data CreateFlowLogsResponse = CreateFlowLogsResponse'
    { _cflrsUnsuccessful   :: !(Maybe [UnsuccessfulItem])
    , _cflrsClientToken    :: !(Maybe Text)
    , _cflrsFlowLogIds     :: !(Maybe [Text])
    , _cflrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateFlowLogsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cflrsUnsuccessful'
--
-- * 'cflrsClientToken'
--
-- * 'cflrsFlowLogIds'
--
-- * 'cflrsResponseStatus'
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
cflrsUnsuccessful = lens _cflrsUnsuccessful (\ s a -> s{_cflrsUnsuccessful = a}) . _Default . _Coerce;

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request.
cflrsClientToken :: Lens' CreateFlowLogsResponse (Maybe Text)
cflrsClientToken = lens _cflrsClientToken (\ s a -> s{_cflrsClientToken = a});

-- | The IDs of the flow logs.
cflrsFlowLogIds :: Lens' CreateFlowLogsResponse [Text]
cflrsFlowLogIds = lens _cflrsFlowLogIds (\ s a -> s{_cflrsFlowLogIds = a}) . _Default . _Coerce;

-- | The response status code.
cflrsResponseStatus :: Lens' CreateFlowLogsResponse Int
cflrsResponseStatus = lens _cflrsResponseStatus (\ s a -> s{_cflrsResponseStatus = a});

instance NFData CreateFlowLogsResponse
