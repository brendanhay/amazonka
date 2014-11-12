{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.AutoScaling.DescribeAccountLimits
    (
    -- * Request
      DescribeAccountLimits
    -- ** Request constructor
    , describeAccountLimits

    -- * Response
    , DescribeAccountLimitsAnswer
    -- ** Response constructor
    , describeAccountLimitsResponse
    -- ** Response lenses
    , dalaMaxNumberOfAutoScalingGroups
    , dalaMaxNumberOfLaunchConfigurations
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DescribeAccountLimits = DescribeAccountLimits
    deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAccountLimits' constructor.
describeAccountLimits :: DescribeAccountLimits
describeAccountLimits = DescribeAccountLimits

instance ToQuery DescribeAccountLimits

instance ToPath DescribeAccountLimits where
    toPath = const "/"

data DescribeAccountLimitsAnswer = DescribeAccountLimitsAnswer
    { _dalaMaxNumberOfAutoScalingGroups    :: Maybe Int
    , _dalaMaxNumberOfLaunchConfigurations :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAccountLimitsAnswer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dalaMaxNumberOfAutoScalingGroups' @::@ 'Maybe' 'Int'
--
-- * 'dalaMaxNumberOfLaunchConfigurations' @::@ 'Maybe' 'Int'
--
describeAccountLimitsResponse :: DescribeAccountLimitsAnswer
describeAccountLimitsResponse = DescribeAccountLimitsAnswer
    { _dalaMaxNumberOfAutoScalingGroups    = Nothing
    , _dalaMaxNumberOfLaunchConfigurations = Nothing
    }

-- | The maximum number of Auto Scaling groups allowed for your AWS account.
dalaMaxNumberOfAutoScalingGroups :: Lens' DescribeAccountLimitsAnswer (Maybe Int)
dalaMaxNumberOfAutoScalingGroups =
    lens _dalaMaxNumberOfAutoScalingGroups
        (\s a -> s { _dalaMaxNumberOfAutoScalingGroups = a })

-- | The maximum number of launch configurations allowed for your AWS account.
dalaMaxNumberOfLaunchConfigurations :: Lens' DescribeAccountLimitsAnswer (Maybe Int)
dalaMaxNumberOfLaunchConfigurations =
    lens _dalaMaxNumberOfLaunchConfigurations
        (\s a -> s { _dalaMaxNumberOfLaunchConfigurations = a })

instance FromXML DescribeAccountLimitsAnswer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeAccountLimitsAnswer"

instance AWSRequest DescribeAccountLimits where
    type Sv DescribeAccountLimits = AutoScaling
    type Rs DescribeAccountLimits = DescribeAccountLimitsAnswer

    request  = post "DescribeAccountLimits"
    response = xmlResponse $ \h x -> DescribeAccountLimitsAnswer
        <$> x %| "MaxNumberOfAutoScalingGroups"
        <*> x %| "MaxNumberOfLaunchConfigurations"
