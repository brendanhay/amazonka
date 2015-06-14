{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHookTypes
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

-- | Describes the available types of lifecycle hooks.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLifecycleHookTypes.html>
module Network.AWS.AutoScaling.DescribeLifecycleHookTypes
    (
    -- * Request
      DescribeLifecycleHookTypes
    -- ** Request constructor
    , describeLifecycleHookTypes

    -- * Response
    , DescribeLifecycleHookTypesResponse
    -- ** Response constructor
    , describeLifecycleHookTypesResponse
    -- ** Response lenses
    , dlhtrLifecycleHookTypes
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.AutoScaling.Types

-- | /See:/ 'describeLifecycleHookTypes' smart constructor.
data DescribeLifecycleHookTypes = DescribeLifecycleHookTypes' deriving (Eq, Read, Show)

-- | 'DescribeLifecycleHookTypes' smart constructor.
describeLifecycleHookTypes :: DescribeLifecycleHookTypes
describeLifecycleHookTypes = DescribeLifecycleHookTypes';

instance AWSRequest DescribeLifecycleHookTypes where
        type Sv DescribeLifecycleHookTypes = AutoScaling
        type Rs DescribeLifecycleHookTypes =
             DescribeLifecycleHookTypesResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeLifecycleHookTypesResult"
              (\ s h x ->
                 DescribeLifecycleHookTypesResponse' <$>
                   (x .@? "LifecycleHookTypes" .!@ mempty >>=
                      parseXMLList "member"))

instance ToHeaders DescribeLifecycleHookTypes where
        toHeaders = const mempty

instance ToPath DescribeLifecycleHookTypes where
        toPath = const "/"

instance ToQuery DescribeLifecycleHookTypes where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("DescribeLifecycleHookTypes" :: ByteString),
                  "Version" =: ("2011-01-01" :: ByteString)])

-- | /See:/ 'describeLifecycleHookTypesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlhtrLifecycleHookTypes'
newtype DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse'{_dlhtrLifecycleHookTypes :: [Text]} deriving (Eq, Read, Show)

-- | 'DescribeLifecycleHookTypesResponse' smart constructor.
describeLifecycleHookTypesResponse :: DescribeLifecycleHookTypesResponse
describeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse'{_dlhtrLifecycleHookTypes = mempty};

-- | One or more of the following notification types:
--
-- -   @autoscaling:EC2_INSTANCE_LAUNCHING@
--
-- -   @autoscaling:EC2_INSTANCE_TERMINATING@
--
dlhtrLifecycleHookTypes :: Lens' DescribeLifecycleHookTypesResponse [Text]
dlhtrLifecycleHookTypes = lens _dlhtrLifecycleHookTypes (\ s a -> s{_dlhtrLifecycleHookTypes = a});
