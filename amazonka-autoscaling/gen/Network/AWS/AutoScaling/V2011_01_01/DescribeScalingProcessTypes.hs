{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeScalingProcessTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns scaling process types for use in the ResumeProcesses and
-- SuspendProcesses actions.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01
-- &Action=DescribeScalingProcessTypes &AUTHPARAMS AZRebalance
-- AddToLoadBalancer AlarmNotification HealthCheck Launch ReplaceUnhealthy
-- ScheduledActions Terminate 27f2eacc-b73f-11e2-ad99-c7aba3a9c963.
module Network.AWS.AutoScaling.V2011_01_01.DescribeScalingProcessTypes
    (
    -- * Request
      DescribeScalingProcessTypes
    -- ** Request constructor
    , mkDescribeScalingProcessTypes
    -- * Response
    , DescribeScalingProcessTypesResponse
    -- ** Response lenses
    , dsptrProcesses
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

data DescribeScalingProcessTypes = DescribeScalingProcessTypes
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeScalingProcessTypes' request.
mkDescribeScalingProcessTypes :: DescribeScalingProcessTypes
mkDescribeScalingProcessTypes = DescribeScalingProcessTypes

instance ToQuery DescribeScalingProcessTypes where
    toQuery = genericQuery def

-- | The output of the DescribeScalingProcessTypes action.
newtype DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse
    { _dsptrProcesses :: [ProcessType]
    } deriving (Show, Generic)

-- | A list of ProcessType names.
dsptrProcesses :: Lens' DescribeScalingProcessTypesResponse [ProcessType]
dsptrProcesses = lens _dsptrProcesses (\s a -> s { _dsptrProcesses = a })

instance FromXML DescribeScalingProcessTypesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeScalingProcessTypes where
    type Sv DescribeScalingProcessTypes = AutoScaling
    type Rs DescribeScalingProcessTypes = DescribeScalingProcessTypesResponse

    request = post "DescribeScalingProcessTypes"
    response _ = xmlResponse
