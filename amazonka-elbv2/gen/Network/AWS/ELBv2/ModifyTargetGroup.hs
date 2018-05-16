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
-- Module      : Network.AWS.ELBv2.ModifyTargetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the health checks used when evaluating the health state of the targets in the specified target group.
--
--
-- To monitor the health of the targets, use 'DescribeTargetHealth' .
--
module Network.AWS.ELBv2.ModifyTargetGroup
    (
    -- * Creating a Request
      modifyTargetGroup
    , ModifyTargetGroup
    -- * Request Lenses
    , mtgMatcher
    , mtgHealthCheckPath
    , mtgUnhealthyThresholdCount
    , mtgHealthCheckIntervalSeconds
    , mtgHealthyThresholdCount
    , mtgHealthCheckProtocol
    , mtgHealthCheckTimeoutSeconds
    , mtgHealthCheckPort
    , mtgTargetGroupARN

    -- * Destructuring the Response
    , modifyTargetGroupResponse
    , ModifyTargetGroupResponse
    -- * Response Lenses
    , mtgrsTargetGroups
    , mtgrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyTargetGroup' smart constructor.
data ModifyTargetGroup = ModifyTargetGroup'
  { _mtgMatcher                    :: !(Maybe Matcher)
  , _mtgHealthCheckPath            :: !(Maybe Text)
  , _mtgUnhealthyThresholdCount    :: !(Maybe Nat)
  , _mtgHealthCheckIntervalSeconds :: !(Maybe Nat)
  , _mtgHealthyThresholdCount      :: !(Maybe Nat)
  , _mtgHealthCheckProtocol        :: !(Maybe ProtocolEnum)
  , _mtgHealthCheckTimeoutSeconds  :: !(Maybe Nat)
  , _mtgHealthCheckPort            :: !(Maybe Text)
  , _mtgTargetGroupARN             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyTargetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtgMatcher' - [HTTP/HTTPS health checks] The HTTP codes to use when checking for a successful response from a target.
--
-- * 'mtgHealthCheckPath' - [HTTP/HTTPS health checks] The ping path that is the destination for the health check request.
--
-- * 'mtgUnhealthyThresholdCount' - The number of consecutive health check failures required before considering the target unhealthy. For Network Load Balancers, this value must be the same as the healthy threshold count.
--
-- * 'mtgHealthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an individual target. For Application Load Balancers, the range is 5 to 300 seconds. For Network Load Balancers, the supported values are 10 or 30 seconds.
--
-- * 'mtgHealthyThresholdCount' - The number of consecutive health checks successes required before considering an unhealthy target healthy.
--
-- * 'mtgHealthCheckProtocol' - The protocol the load balancer uses when performing health checks on targets. The TCP protocol is supported only if the protocol of the target group is TCP.
--
-- * 'mtgHealthCheckTimeoutSeconds' - [HTTP/HTTPS health checks] The amount of time, in seconds, during which no response means a failed health check.
--
-- * 'mtgHealthCheckPort' - The port the load balancer uses when performing health checks on targets.
--
-- * 'mtgTargetGroupARN' - The Amazon Resource Name (ARN) of the target group.
modifyTargetGroup
    :: Text -- ^ 'mtgTargetGroupARN'
    -> ModifyTargetGroup
modifyTargetGroup pTargetGroupARN_ =
  ModifyTargetGroup'
    { _mtgMatcher = Nothing
    , _mtgHealthCheckPath = Nothing
    , _mtgUnhealthyThresholdCount = Nothing
    , _mtgHealthCheckIntervalSeconds = Nothing
    , _mtgHealthyThresholdCount = Nothing
    , _mtgHealthCheckProtocol = Nothing
    , _mtgHealthCheckTimeoutSeconds = Nothing
    , _mtgHealthCheckPort = Nothing
    , _mtgTargetGroupARN = pTargetGroupARN_
    }


-- | [HTTP/HTTPS health checks] The HTTP codes to use when checking for a successful response from a target.
mtgMatcher :: Lens' ModifyTargetGroup (Maybe Matcher)
mtgMatcher = lens _mtgMatcher (\ s a -> s{_mtgMatcher = a})

-- | [HTTP/HTTPS health checks] The ping path that is the destination for the health check request.
mtgHealthCheckPath :: Lens' ModifyTargetGroup (Maybe Text)
mtgHealthCheckPath = lens _mtgHealthCheckPath (\ s a -> s{_mtgHealthCheckPath = a})

-- | The number of consecutive health check failures required before considering the target unhealthy. For Network Load Balancers, this value must be the same as the healthy threshold count.
mtgUnhealthyThresholdCount :: Lens' ModifyTargetGroup (Maybe Natural)
mtgUnhealthyThresholdCount = lens _mtgUnhealthyThresholdCount (\ s a -> s{_mtgUnhealthyThresholdCount = a}) . mapping _Nat

-- | The approximate amount of time, in seconds, between health checks of an individual target. For Application Load Balancers, the range is 5 to 300 seconds. For Network Load Balancers, the supported values are 10 or 30 seconds.
mtgHealthCheckIntervalSeconds :: Lens' ModifyTargetGroup (Maybe Natural)
mtgHealthCheckIntervalSeconds = lens _mtgHealthCheckIntervalSeconds (\ s a -> s{_mtgHealthCheckIntervalSeconds = a}) . mapping _Nat

-- | The number of consecutive health checks successes required before considering an unhealthy target healthy.
mtgHealthyThresholdCount :: Lens' ModifyTargetGroup (Maybe Natural)
mtgHealthyThresholdCount = lens _mtgHealthyThresholdCount (\ s a -> s{_mtgHealthyThresholdCount = a}) . mapping _Nat

-- | The protocol the load balancer uses when performing health checks on targets. The TCP protocol is supported only if the protocol of the target group is TCP.
mtgHealthCheckProtocol :: Lens' ModifyTargetGroup (Maybe ProtocolEnum)
mtgHealthCheckProtocol = lens _mtgHealthCheckProtocol (\ s a -> s{_mtgHealthCheckProtocol = a})

-- | [HTTP/HTTPS health checks] The amount of time, in seconds, during which no response means a failed health check.
mtgHealthCheckTimeoutSeconds :: Lens' ModifyTargetGroup (Maybe Natural)
mtgHealthCheckTimeoutSeconds = lens _mtgHealthCheckTimeoutSeconds (\ s a -> s{_mtgHealthCheckTimeoutSeconds = a}) . mapping _Nat

-- | The port the load balancer uses when performing health checks on targets.
mtgHealthCheckPort :: Lens' ModifyTargetGroup (Maybe Text)
mtgHealthCheckPort = lens _mtgHealthCheckPort (\ s a -> s{_mtgHealthCheckPort = a})

-- | The Amazon Resource Name (ARN) of the target group.
mtgTargetGroupARN :: Lens' ModifyTargetGroup Text
mtgTargetGroupARN = lens _mtgTargetGroupARN (\ s a -> s{_mtgTargetGroupARN = a})

instance AWSRequest ModifyTargetGroup where
        type Rs ModifyTargetGroup = ModifyTargetGroupResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "ModifyTargetGroupResult"
              (\ s h x ->
                 ModifyTargetGroupResponse' <$>
                   (x .@? "TargetGroups" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ModifyTargetGroup where

instance NFData ModifyTargetGroup where

instance ToHeaders ModifyTargetGroup where
        toHeaders = const mempty

instance ToPath ModifyTargetGroup where
        toPath = const "/"

instance ToQuery ModifyTargetGroup where
        toQuery ModifyTargetGroup'{..}
          = mconcat
              ["Action" =: ("ModifyTargetGroup" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "Matcher" =: _mtgMatcher,
               "HealthCheckPath" =: _mtgHealthCheckPath,
               "UnhealthyThresholdCount" =:
                 _mtgUnhealthyThresholdCount,
               "HealthCheckIntervalSeconds" =:
                 _mtgHealthCheckIntervalSeconds,
               "HealthyThresholdCount" =: _mtgHealthyThresholdCount,
               "HealthCheckProtocol" =: _mtgHealthCheckProtocol,
               "HealthCheckTimeoutSeconds" =:
                 _mtgHealthCheckTimeoutSeconds,
               "HealthCheckPort" =: _mtgHealthCheckPort,
               "TargetGroupArn" =: _mtgTargetGroupARN]

-- | /See:/ 'modifyTargetGroupResponse' smart constructor.
data ModifyTargetGroupResponse = ModifyTargetGroupResponse'
  { _mtgrsTargetGroups   :: !(Maybe [TargetGroup])
  , _mtgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyTargetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtgrsTargetGroups' - Information about the target group.
--
-- * 'mtgrsResponseStatus' - -- | The response status code.
modifyTargetGroupResponse
    :: Int -- ^ 'mtgrsResponseStatus'
    -> ModifyTargetGroupResponse
modifyTargetGroupResponse pResponseStatus_ =
  ModifyTargetGroupResponse'
    {_mtgrsTargetGroups = Nothing, _mtgrsResponseStatus = pResponseStatus_}


-- | Information about the target group.
mtgrsTargetGroups :: Lens' ModifyTargetGroupResponse [TargetGroup]
mtgrsTargetGroups = lens _mtgrsTargetGroups (\ s a -> s{_mtgrsTargetGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
mtgrsResponseStatus :: Lens' ModifyTargetGroupResponse Int
mtgrsResponseStatus = lens _mtgrsResponseStatus (\ s a -> s{_mtgrsResponseStatus = a})

instance NFData ModifyTargetGroupResponse where
