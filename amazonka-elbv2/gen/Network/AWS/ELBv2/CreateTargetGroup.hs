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
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-target-groups.html Target Groups for Your Application Load Balancers> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-target-groups.html Target Groups for Your Network Load Balancers> in the /Network Load Balancers Guide/ .
--
module Network.AWS.ELBv2.CreateTargetGroup
    (
    -- * Creating a Request
      createTargetGroup
    , CreateTargetGroup
    -- * Request Lenses
    , ctgMatcher
    , ctgHealthCheckPath
    , ctgHealthCheckEnabled
    , ctgUnhealthyThresholdCount
    , ctgVPCId
    , ctgProtocol
    , ctgHealthCheckIntervalSeconds
    , ctgTargetType
    , ctgHealthyThresholdCount
    , ctgHealthCheckProtocol
    , ctgHealthCheckTimeoutSeconds
    , ctgHealthCheckPort
    , ctgPort
    , ctgName

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
  , _ctgHealthCheckEnabled         :: !(Maybe Bool)
  , _ctgUnhealthyThresholdCount    :: !(Maybe Nat)
  , _ctgVPCId                      :: !(Maybe Text)
  , _ctgProtocol                   :: !(Maybe ProtocolEnum)
  , _ctgHealthCheckIntervalSeconds :: !(Maybe Nat)
  , _ctgTargetType                 :: !(Maybe TargetTypeEnum)
  , _ctgHealthyThresholdCount      :: !(Maybe Nat)
  , _ctgHealthCheckProtocol        :: !(Maybe ProtocolEnum)
  , _ctgHealthCheckTimeoutSeconds  :: !(Maybe Nat)
  , _ctgHealthCheckPort            :: !(Maybe Text)
  , _ctgPort                       :: !(Maybe Nat)
  , _ctgName                       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTargetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgMatcher' - [HTTP/HTTPS health checks] The HTTP codes to use when checking for a successful response from a target.
--
-- * 'ctgHealthCheckPath' - [HTTP/HTTPS health checks] The ping path that is the destination on the targets for health checks. The default is /.
--
-- * 'ctgHealthCheckEnabled' - Indicates whether health checks are enabled. If the target type is @instance@ or @ip@ , the default is @true@ . If the target type is @lambda@ , the default is @false@ .
--
-- * 'ctgUnhealthyThresholdCount' - The number of consecutive health check failures required before considering a target unhealthy. For Application Load Balancers, the default is 2. For Network Load Balancers, this value must be the same as the healthy threshold count.
--
-- * 'ctgVPCId' - The identifier of the virtual private cloud (VPC). If the target is a Lambda function, this parameter does not apply.
--
-- * 'ctgProtocol' - The protocol to use for routing traffic to the targets. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocols are TCP and TLS. If the target is a Lambda function, this parameter does not apply.
--
-- * 'ctgHealthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an individual target. For Application Load Balancers, the range is 5
