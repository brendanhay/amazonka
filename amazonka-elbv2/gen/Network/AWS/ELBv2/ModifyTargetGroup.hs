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
    , mtgHealthCheckEnabled
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
  , _mtgHealthCheckEnabled         :: !(Maybe Bool)
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
-- * 'mtgMatcher' - [HTTP/HTTPS health checks] The HTTP codes to use when checking for a successful response from a target. If the protocol of the target group is TCP, you can't modify this setting.
--
-- * 'mtgHealthCheckPath' - [HTTP/HTTPS health checks] The ping path that is the destination for the health check request.
--
-- * 'mtgHealthCheckEnabled' - Indicates whether health checks are enabled.
--
-- * 'mtgUnhealthyThresholdCount' - The number of consecutive health check failures required before considering the target unhealthy. For Network Load Balancers, this value must be the same as the healthy threshold count.
--
-- * 'mtgHealthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an individual target. For Application Load Balancers, the range is 5
