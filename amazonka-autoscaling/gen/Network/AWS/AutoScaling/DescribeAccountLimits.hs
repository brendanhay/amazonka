{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeAccountLimits
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the limits for the Auto Scaling resources currently allowed for
-- your AWS account. Your AWS account comes with default limits on resources
-- for Auto Scaling. There is a default limit of 20 Auto Scaling groups and
-- 100 launch configurations per region. If you reach the limits for the
-- number of Auto Scaling groups or the launch configurations, you can go to
-- the Support Center and place a request to raise the limits.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAccountLimits.html>
module Network.AWS.AutoScaling.DescribeAccountLimits
    (
    -- * Request
      DescribeAccountLimits
    -- ** Request constructor
    , describeAccountLimits

    -- * Response
    , DescribeAccountLimitsResponse
    -- ** Response constructor
    , describeAccountLimitsResponse
    -- ** Response lenses
    , dalrMaxNumberOfAutoScalingGroups
    , dalrMaxNumberOfLaunchConfigurations
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeAccountLimits = DescribeAccountLimits
    deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAccountLimits' constructor.
describeAccountLimits :: DescribeAccountLimits
describeAccountLimits = DescribeAccountLimits

data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse
    { _dalrMaxNumberOfAutoScalingGroups    :: Maybe Int
    , _dalrMaxNumberOfLaunchConfigurations :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAccountLimitsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dalrMaxNumberOfAutoScalingGroups' @::@ 'Maybe' 'Int'
--
-- * 'dalrMaxNumberOfLaunchConfigurations' @::@ 'Maybe' 'Int'
--
describeAccountLimitsResponse :: DescribeAccountLimitsResponse
describeAccountLimitsResponse = DescribeAccountLimitsResponse
    { _dalrMaxNumberOfAutoScalingGroups    = Nothing
    , _dalrMaxNumberOfLaunchConfigurations = Nothing
    }

-- | The maximum number of Auto Scaling groups allowed for your AWS account.
dalrMaxNumberOfAutoScalingGroups :: Lens' DescribeAccountLimitsResponse (Maybe Int)
dalrMaxNumberOfAutoScalingGroups =
    lens _dalrMaxNumberOfAutoScalingGroups
        (\s a -> s { _dalrMaxNumberOfAutoScalingGroups = a })

-- | The maximum number of launch configurations allowed for your AWS account.
dalrMaxNumberOfLaunchConfigurations :: Lens' DescribeAccountLimitsResponse (Maybe Int)
dalrMaxNumberOfLaunchConfigurations =
    lens _dalrMaxNumberOfLaunchConfigurations
        (\s a -> s { _dalrMaxNumberOfLaunchConfigurations = a })

instance ToPath DescribeAccountLimits where
    toPath = const "/"

instance ToQuery DescribeAccountLimits where
    toQuery = const mempty

instance ToHeaders DescribeAccountLimits

instance AWSRequest DescribeAccountLimits where
    type Sv DescribeAccountLimits = AutoScaling
    type Rs DescribeAccountLimits = DescribeAccountLimitsResponse

    request  = post "DescribeAccountLimits"
    response = xmlResponse

instance FromXML DescribeAccountLimitsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeAccountLimitsResponse"
