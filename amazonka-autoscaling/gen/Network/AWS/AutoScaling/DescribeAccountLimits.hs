{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DescribeAccountLimits
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- For information about requesting an increase in these limits, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html AWS Service Limits>.
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
    , dalrMaxNumberOfLaunchConfigurations
    , dalrMaxNumberOfAutoScalingGroups
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.AutoScaling.Types

-- | /See:/ 'describeAccountLimits' smart constructor.
data DescribeAccountLimits = DescribeAccountLimits' deriving (Eq, Read, Show)

-- | 'DescribeAccountLimits' smart constructor.
describeAccountLimits :: DescribeAccountLimits
describeAccountLimits = DescribeAccountLimits';

instance AWSRequest DescribeAccountLimits where
        type Sv DescribeAccountLimits = AutoScaling
        type Rs DescribeAccountLimits =
             DescribeAccountLimitsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeAccountLimitsResult"
              (\ s h x ->
                 DescribeAccountLimitsResponse' <$>
                   x .@? "MaxNumberOfLaunchConfigurations" <*>
                     x .@? "MaxNumberOfAutoScalingGroups")

instance ToHeaders DescribeAccountLimits where
        toHeaders = const mempty

instance ToPath DescribeAccountLimits where
        toPath = const "/"

instance ToQuery DescribeAccountLimits where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("DescribeAccountLimits" :: ByteString),
                  "Version" =: ("2011-01-01" :: ByteString)])

-- | /See:/ 'describeAccountLimitsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dalrMaxNumberOfLaunchConfigurations'
--
-- * 'dalrMaxNumberOfAutoScalingGroups'
data DescribeAccountLimitsResponse = DescribeAccountLimitsResponse'{_dalrMaxNumberOfLaunchConfigurations :: Maybe Int, _dalrMaxNumberOfAutoScalingGroups :: Maybe Int} deriving (Eq, Read, Show)

-- | 'DescribeAccountLimitsResponse' smart constructor.
describeAccountLimitsResponse :: DescribeAccountLimitsResponse
describeAccountLimitsResponse = DescribeAccountLimitsResponse'{_dalrMaxNumberOfLaunchConfigurations = Nothing, _dalrMaxNumberOfAutoScalingGroups = Nothing};

-- | The maximum number of launch configurations allowed for your AWS
-- account. The default limit is 100 per region.
dalrMaxNumberOfLaunchConfigurations :: Lens' DescribeAccountLimitsResponse (Maybe Int)
dalrMaxNumberOfLaunchConfigurations = lens _dalrMaxNumberOfLaunchConfigurations (\ s a -> s{_dalrMaxNumberOfLaunchConfigurations = a});

-- | The maximum number of groups allowed for your AWS account. The default
-- limit is 20 per region.
dalrMaxNumberOfAutoScalingGroups :: Lens' DescribeAccountLimitsResponse (Maybe Int)
dalrMaxNumberOfAutoScalingGroups = lens _dalrMaxNumberOfAutoScalingGroups (\ s a -> s{_dalrMaxNumberOfAutoScalingGroups = a});
