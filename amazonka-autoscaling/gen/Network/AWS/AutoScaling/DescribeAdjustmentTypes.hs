{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DescribeAdjustmentTypes
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

-- | Describes the policy adjustment types for use with PutScalingPolicy.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAdjustmentTypes.html>
module Network.AWS.AutoScaling.DescribeAdjustmentTypes
    (
    -- * Request
      DescribeAdjustmentTypes
    -- ** Request constructor
    , describeAdjustmentTypes

    -- * Response
    , DescribeAdjustmentTypesResponse
    -- ** Response constructor
    , describeAdjustmentTypesResponse
    -- ** Response lenses
    , datrAdjustmentTypes
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAdjustmentTypes' smart constructor.
data DescribeAdjustmentTypes = DescribeAdjustmentTypes' deriving (Eq, Read, Show)

-- | 'DescribeAdjustmentTypes' smart constructor.
describeAdjustmentTypes :: DescribeAdjustmentTypes
describeAdjustmentTypes = DescribeAdjustmentTypes';

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeAdjustmentTypes where
        type Sv DescribeAdjustmentTypes = AutoScaling
        type Rs DescribeAdjustmentTypes =
             DescribeAdjustmentTypesResponse
        request = post
        response
          = receiveXMLWrapper "DescribeAdjustmentTypesResult"
              (\ s h x ->
                 DescribeAdjustmentTypesResponse' <$>
                   (x .@? "AdjustmentTypes" .!@ mempty >>=
                      may (parseXMLList "member")))

instance ToHeaders DescribeAdjustmentTypes where
        toHeaders = const mempty

instance ToPath DescribeAdjustmentTypes where
        toPath = const "/"

instance ToQuery DescribeAdjustmentTypes where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("DescribeAdjustmentTypes" :: ByteString),
                  "Version" =: ("2011-01-01" :: ByteString)])

-- | /See:/ 'describeAdjustmentTypesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'datrAdjustmentTypes'
newtype DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse'{_datrAdjustmentTypes :: Maybe [AdjustmentType]} deriving (Eq, Read, Show)

-- | 'DescribeAdjustmentTypesResponse' smart constructor.
describeAdjustmentTypesResponse :: DescribeAdjustmentTypesResponse
describeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse'{_datrAdjustmentTypes = Nothing};

-- | The policy adjustment types.
datrAdjustmentTypes :: Lens' DescribeAdjustmentTypesResponse [AdjustmentType]
datrAdjustmentTypes = lens _datrAdjustmentTypes (\ s a -> s{_datrAdjustmentTypes = a}) . _Default;
