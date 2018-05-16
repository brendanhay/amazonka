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
-- Module      : Network.AWS.ELBv2.CreateTargetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a target group.
--
--
-- To register targets with the target group, use 'RegisterTargets' . To update the health check settings for the target group, use 'ModifyTargetGroup' . To monitor the health of targets in the target group, use 'DescribeTargetHealth' .
--
-- To route traffic to the targets in a target group, specify the target group in an action using 'CreateListener' or 'CreateRule' .
--
-- To delete a target group, use 'DeleteTargetGroup' .
--
-- This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple target groups with the same settings, each call succeeds.
--
-- For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-target-groups.html Target Groups for Your Application Load Balancers> in the /Application Load Balancers Guide/ or <http://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-target-groups.html Target Groups for Your Network Load Balancers> in the /Network Load Balancers Guide/ .
--
module Network.AWS.ELBv2.CreateTargetGroup
    (
    -- * Creating a Request
      createTargetGroup
    , CreateTargetGroup
    -- * Request Lenses
    , ctgMatcher
    , ctgHealthCheckPath
    , ctgUnhealthyThresholdCount
    , ctgHealthCheckIntervalSeconds
    , ctgTargetType
    , ctgHealthyThresholdCount
    , ctgHealthCheckProtocol
    , ctgHealthCheckTimeoutSeconds
    , ctgHealthCheckPort
    , ctgName
    , ctgProtocol
    , ctgPort
    , ctgVPCId

    -- * Destructuring the Response
    , createTargetGroupResponse
    , CreateTargetGroupResponse
    -- * Response Lenses
    , ctgrsTargetGroups
    , ctgrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTargetGroup' smart constructor.
data CreateTargetGroup = CreateTargetGroup'
  { _ctgMatcher                    :: !(Maybe Matcher)
  , _ctgHealthCheckPath            :: !(Maybe Text)
  , _ctgUnhealthyThresholdCount    :: !(Maybe Nat)
  , _ctgHealthCheckIntervalSeconds :: !(Maybe Nat)
  , _ctgTargetType                 :: !(Maybe TargetTypeEnum)
  , _ctgHealthyThresholdCount      :: !(Maybe Nat)
  , _ctgHealthCheckProtocol        :: !(Maybe ProtocolEnum)
  , _ctgHealthCheckTimeoutSeconds  :: !(Maybe Nat)
  , _ctgHealthCheckPort            :: !(Maybe Text)
  , _ctgName                       :: !Text
  , _ctgProtocol                   :: !ProtocolEnum
  , _ctgPort                       :: !Nat
  , _ctgVPCId                      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTargetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgMatcher' - [HTTP/HTTPS health checks] The HTTP codes to use when checking for a successful response from a target.
--
-- * 'ctgHealthCheckPath' - [HTTP/HTTPS health checks] The ping path that is the destination on the targets for health checks. The default is /.
--
-- * 'ctgUnhealthyThresholdCount' - The number of consecutive health check failures required before considering a target unhealthy. For Application Load Balancers, the default is 2. For Network Load Balancers, this value must be the same as the healthy threshold count.
--
-- * 'ctgHealthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an individual target. For Application Load Balancers, the range is 5 to 300 seconds. For Network Load Balancers, the supported values are 10 or 30 seconds. The default is 30 seconds.
--
-- * 'ctgTargetType' - The type of target that you must specify when registering targets with this target group. The possible values are @instance@ (targets are specified by instance ID) or @ip@ (targets are specified by IP address). The default is @instance@ . Note that you can't specify targets for a target group using both instance IDs and IP addresses. If the target type is @ip@ , specify IP addresses from the subnets of the virtual private cloud (VPC) for the target group, the RFC 1918 range (10.0.0.0/8, 172.16.0.0/12, and 192.168.0.0/16), and the RFC 6598 range (100.64.0.0/10). You can't specify publicly routable IP addresses.
--
-- * 'ctgHealthyThresholdCount' - The number of consecutive health checks successes required before considering an unhealthy target healthy. For Application Load Balancers, the default is 5. For Network Load Balancers, the default is 3.
--
-- * 'ctgHealthCheckProtocol' - The protocol the load balancer uses when performing health checks on targets. The TCP protocol is supported only if the protocol of the target group is TCP. For Application Load Balancers, the default is HTTP. For Network Load Balancers, the default is TCP.
--
-- * 'ctgHealthCheckTimeoutSeconds' - The amount of time, in seconds, during which no response from a target means a failed health check. For Application Load Balancers, the range is 2 to 60 seconds and the default is 5 seconds. For Network Load Balancers, this is 10 seconds for TCP and HTTPS health checks and 6 seconds for HTTP health checks.
--
-- * 'ctgHealthCheckPort' - The port the load balancer uses when performing health checks on targets. The default is @traffic-port@ , which is the port on which each target receives traffic from the load balancer.
--
-- * 'ctgName' - The name of the target group. This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, and must not begin or end with a hyphen.
--
-- * 'ctgProtocol' - The protocol to use for routing traffic to the targets. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocol is TCP.
--
-- * 'ctgPort' - The port on which the targets receive traffic. This port is used unless you specify a port override when registering the target.
--
-- * 'ctgVPCId' - The identifier of the virtual private cloud (VPC).
createTargetGroup
    :: Text -- ^ 'ctgName'
    -> ProtocolEnum -- ^ 'ctgProtocol'
    -> Natural -- ^ 'ctgPort'
    -> Text -- ^ 'ctgVPCId'
    -> CreateTargetGroup
createTargetGroup pName_ pProtocol_ pPort_ pVPCId_ =
  CreateTargetGroup'
    { _ctgMatcher = Nothing
    , _ctgHealthCheckPath = Nothing
    , _ctgUnhealthyThresholdCount = Nothing
    , _ctgHealthCheckIntervalSeconds = Nothing
    , _ctgTargetType = Nothing
    , _ctgHealthyThresholdCount = Nothing
    , _ctgHealthCheckProtocol = Nothing
    , _ctgHealthCheckTimeoutSeconds = Nothing
    , _ctgHealthCheckPort = Nothing
    , _ctgName = pName_
    , _ctgProtocol = pProtocol_
    , _ctgPort = _Nat # pPort_
    , _ctgVPCId = pVPCId_
    }


-- | [HTTP/HTTPS health checks] The HTTP codes to use when checking for a successful response from a target.
ctgMatcher :: Lens' CreateTargetGroup (Maybe Matcher)
ctgMatcher = lens _ctgMatcher (\ s a -> s{_ctgMatcher = a})

-- | [HTTP/HTTPS health checks] The ping path that is the destination on the targets for health checks. The default is /.
ctgHealthCheckPath :: Lens' CreateTargetGroup (Maybe Text)
ctgHealthCheckPath = lens _ctgHealthCheckPath (\ s a -> s{_ctgHealthCheckPath = a})

-- | The number of consecutive health check failures required before considering a target unhealthy. For Application Load Balancers, the default is 2. For Network Load Balancers, this value must be the same as the healthy threshold count.
ctgUnhealthyThresholdCount :: Lens' CreateTargetGroup (Maybe Natural)
ctgUnhealthyThresholdCount = lens _ctgUnhealthyThresholdCount (\ s a -> s{_ctgUnhealthyThresholdCount = a}) . mapping _Nat

-- | The approximate amount of time, in seconds, between health checks of an individual target. For Application Load Balancers, the range is 5 to 300 seconds. For Network Load Balancers, the supported values are 10 or 30 seconds. The default is 30 seconds.
ctgHealthCheckIntervalSeconds :: Lens' CreateTargetGroup (Maybe Natural)
ctgHealthCheckIntervalSeconds = lens _ctgHealthCheckIntervalSeconds (\ s a -> s{_ctgHealthCheckIntervalSeconds = a}) . mapping _Nat

-- | The type of target that you must specify when registering targets with this target group. The possible values are @instance@ (targets are specified by instance ID) or @ip@ (targets are specified by IP address). The default is @instance@ . Note that you can't specify targets for a target group using both instance IDs and IP addresses. If the target type is @ip@ , specify IP addresses from the subnets of the virtual private cloud (VPC) for the target group, the RFC 1918 range (10.0.0.0/8, 172.16.0.0/12, and 192.168.0.0/16), and the RFC 6598 range (100.64.0.0/10). You can't specify publicly routable IP addresses.
ctgTargetType :: Lens' CreateTargetGroup (Maybe TargetTypeEnum)
ctgTargetType = lens _ctgTargetType (\ s a -> s{_ctgTargetType = a})

-- | The number of consecutive health checks successes required before considering an unhealthy target healthy. For Application Load Balancers, the default is 5. For Network Load Balancers, the default is 3.
ctgHealthyThresholdCount :: Lens' CreateTargetGroup (Maybe Natural)
ctgHealthyThresholdCount = lens _ctgHealthyThresholdCount (\ s a -> s{_ctgHealthyThresholdCount = a}) . mapping _Nat

-- | The protocol the load balancer uses when performing health checks on targets. The TCP protocol is supported only if the protocol of the target group is TCP. For Application Load Balancers, the default is HTTP. For Network Load Balancers, the default is TCP.
ctgHealthCheckProtocol :: Lens' CreateTargetGroup (Maybe ProtocolEnum)
ctgHealthCheckProtocol = lens _ctgHealthCheckProtocol (\ s a -> s{_ctgHealthCheckProtocol = a})

-- | The amount of time, in seconds, during which no response from a target means a failed health check. For Application Load Balancers, the range is 2 to 60 seconds and the default is 5 seconds. For Network Load Balancers, this is 10 seconds for TCP and HTTPS health checks and 6 seconds for HTTP health checks.
ctgHealthCheckTimeoutSeconds :: Lens' CreateTargetGroup (Maybe Natural)
ctgHealthCheckTimeoutSeconds = lens _ctgHealthCheckTimeoutSeconds (\ s a -> s{_ctgHealthCheckTimeoutSeconds = a}) . mapping _Nat

-- | The port the load balancer uses when performing health checks on targets. The default is @traffic-port@ , which is the port on which each target receives traffic from the load balancer.
ctgHealthCheckPort :: Lens' CreateTargetGroup (Maybe Text)
ctgHealthCheckPort = lens _ctgHealthCheckPort (\ s a -> s{_ctgHealthCheckPort = a})

-- | The name of the target group. This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, and must not begin or end with a hyphen.
ctgName :: Lens' CreateTargetGroup Text
ctgName = lens _ctgName (\ s a -> s{_ctgName = a})

-- | The protocol to use for routing traffic to the targets. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocol is TCP.
ctgProtocol :: Lens' CreateTargetGroup ProtocolEnum
ctgProtocol = lens _ctgProtocol (\ s a -> s{_ctgProtocol = a})

-- | The port on which the targets receive traffic. This port is used unless you specify a port override when registering the target.
ctgPort :: Lens' CreateTargetGroup Natural
ctgPort = lens _ctgPort (\ s a -> s{_ctgPort = a}) . _Nat

-- | The identifier of the virtual private cloud (VPC).
ctgVPCId :: Lens' CreateTargetGroup Text
ctgVPCId = lens _ctgVPCId (\ s a -> s{_ctgVPCId = a})

instance AWSRequest CreateTargetGroup where
        type Rs CreateTargetGroup = CreateTargetGroupResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "CreateTargetGroupResult"
              (\ s h x ->
                 CreateTargetGroupResponse' <$>
                   (x .@? "TargetGroups" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable CreateTargetGroup where

instance NFData CreateTargetGroup where

instance ToHeaders CreateTargetGroup where
        toHeaders = const mempty

instance ToPath CreateTargetGroup where
        toPath = const "/"

instance ToQuery CreateTargetGroup where
        toQuery CreateTargetGroup'{..}
          = mconcat
              ["Action" =: ("CreateTargetGroup" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "Matcher" =: _ctgMatcher,
               "HealthCheckPath" =: _ctgHealthCheckPath,
               "UnhealthyThresholdCount" =:
                 _ctgUnhealthyThresholdCount,
               "HealthCheckIntervalSeconds" =:
                 _ctgHealthCheckIntervalSeconds,
               "TargetType" =: _ctgTargetType,
               "HealthyThresholdCount" =: _ctgHealthyThresholdCount,
               "HealthCheckProtocol" =: _ctgHealthCheckProtocol,
               "HealthCheckTimeoutSeconds" =:
                 _ctgHealthCheckTimeoutSeconds,
               "HealthCheckPort" =: _ctgHealthCheckPort,
               "Name" =: _ctgName, "Protocol" =: _ctgProtocol,
               "Port" =: _ctgPort, "VpcId" =: _ctgVPCId]

-- | /See:/ 'createTargetGroupResponse' smart constructor.
data CreateTargetGroupResponse = CreateTargetGroupResponse'
  { _ctgrsTargetGroups   :: !(Maybe [TargetGroup])
  , _ctgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTargetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgrsTargetGroups' - Information about the target group.
--
-- * 'ctgrsResponseStatus' - -- | The response status code.
createTargetGroupResponse
    :: Int -- ^ 'ctgrsResponseStatus'
    -> CreateTargetGroupResponse
createTargetGroupResponse pResponseStatus_ =
  CreateTargetGroupResponse'
    {_ctgrsTargetGroups = Nothing, _ctgrsResponseStatus = pResponseStatus_}


-- | Information about the target group.
ctgrsTargetGroups :: Lens' CreateTargetGroupResponse [TargetGroup]
ctgrsTargetGroups = lens _ctgrsTargetGroups (\ s a -> s{_ctgrsTargetGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
ctgrsResponseStatus :: Lens' CreateTargetGroupResponse Int
ctgrsResponseStatus = lens _ctgrsResponseStatus (\ s a -> s{_ctgrsResponseStatus = a})

instance NFData CreateTargetGroupResponse where
