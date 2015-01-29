{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the current Auto Scaling resource limits for your AWS account.
--
-- For information about requesting an increase in these limits, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html AWSService Limits>.
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
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DescribeAccountLimits' constructor.
describeAccountLimits :: DescribeAccountLimits
describeAccountLimits = DescribeAccountLimits

data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse
    { _dalrMaxNumberOfAutoScalingGroups    :: Maybe Int
    , _dalrMaxNumberOfLaunchConfigurations :: Maybe Int
    } deriving (Eq, Ord, Read, Show)

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

-- | The maximum number of groups allowed for your AWS account. The default limit
-- is 20 per region.
dalrMaxNumberOfAutoScalingGroups :: Lens' DescribeAccountLimitsResponse (Maybe Int)
dalrMaxNumberOfAutoScalingGroups =
    lens _dalrMaxNumberOfAutoScalingGroups
        (\s a -> s { _dalrMaxNumberOfAutoScalingGroups = a })

-- | The maximum number of launch configurations allowed for your AWS account. The
-- default limit is 100 per region.
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
    parseXML = withElement "DescribeAccountLimitsResult" $ \x -> DescribeAccountLimitsResponse
        <$> x .@? "MaxNumberOfAutoScalingGroups"
        <*> x .@? "MaxNumberOfLaunchConfigurations"
