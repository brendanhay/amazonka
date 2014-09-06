{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeAccountLimits
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
-- https://autoscaling.amazonaws.com/?Version=2011-01-01
-- &Action=DescribeAccountLimits &AUTHPARAMS 100 20
-- a32bd184-519d-11e3-a8a4-c1c467cbcc3b.
module Network.AWS.AutoScaling.V2011_01_01.DescribeAccountLimits
    (
    -- * Request
      DescribeAccountLimits
    -- ** Request constructor
    , mkDescribeAccountLimits
    -- * Response
    , DescribeAccountLimitsResponse
    -- ** Response lenses
    , dalrsMaxNumberOfAutoScalingGroups
    , dalrsMaxNumberOfLaunchConfigurations
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

data DescribeAccountLimits = DescribeAccountLimits
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAccountLimits' request.
mkDescribeAccountLimits :: DescribeAccountLimits
mkDescribeAccountLimits = DescribeAccountLimits
{-# INLINE mkDescribeAccountLimits #-}

instance ToQuery DescribeAccountLimits where
    toQuery = genericQuery def

-- | The output of the DescribeAccountLimitsResult action.
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse
    { _dalrsMaxNumberOfAutoScalingGroups :: Maybe Integer
    , _dalrsMaxNumberOfLaunchConfigurations :: Maybe Integer
    } deriving (Show, Generic)

-- | The maximum number of Auto Scaling groups allowed for your AWS account.
dalrsMaxNumberOfAutoScalingGroups :: Lens' DescribeAccountLimitsResponse (Maybe Integer)
dalrsMaxNumberOfAutoScalingGroups =
    lens _dalrsMaxNumberOfAutoScalingGroups
         (\s a -> s { _dalrsMaxNumberOfAutoScalingGroups = a })
{-# INLINE dalrsMaxNumberOfAutoScalingGroups #-}

-- | The maximum number of launch configurations allowed for your AWS account.
dalrsMaxNumberOfLaunchConfigurations :: Lens' DescribeAccountLimitsResponse (Maybe Integer)
dalrsMaxNumberOfLaunchConfigurations =
    lens _dalrsMaxNumberOfLaunchConfigurations
         (\s a -> s { _dalrsMaxNumberOfLaunchConfigurations = a })
{-# INLINE dalrsMaxNumberOfLaunchConfigurations #-}

instance FromXML DescribeAccountLimitsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAccountLimits where
    type Sv DescribeAccountLimits = AutoScaling
    type Rs DescribeAccountLimits = DescribeAccountLimitsResponse

    request = post "DescribeAccountLimits"
    response _ = xmlResponse
