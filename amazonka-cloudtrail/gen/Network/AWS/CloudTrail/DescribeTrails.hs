{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudTrail.DescribeTrails
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

-- | Retrieves settings for the trail associated with the current region for
-- your account.
--
-- <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_DescribeTrails.html>
module Network.AWS.CloudTrail.DescribeTrails
    (
    -- * Request
      DescribeTrails
    -- ** Request constructor
    , describeTrails
    -- ** Request lenses
    , dtTrailNameList

    -- * Response
    , DescribeTrailsResponse
    -- ** Response constructor
    , describeTrailsResponse
    -- ** Response lenses
    , dtrTrailList
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudTrail.Types

-- | /See:/ 'describeTrails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtTrailNameList'
newtype DescribeTrails = DescribeTrails'{_dtTrailNameList :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeTrails' smart constructor.
describeTrails :: DescribeTrails
describeTrails = DescribeTrails'{_dtTrailNameList = Nothing};

-- | The trail returned.
dtTrailNameList :: Lens' DescribeTrails (Maybe [Text])
dtTrailNameList = lens _dtTrailNameList (\ s a -> s{_dtTrailNameList = a});

instance AWSRequest DescribeTrails where
        type Sv DescribeTrails = CloudTrail
        type Rs DescribeTrails = DescribeTrailsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTrailsResponse' <$>
                   x .?> "trailList" .!@ mempty)

instance ToHeaders DescribeTrails where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.DescribeTrails"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTrails where
        toJSON DescribeTrails'{..}
          = object ["trailNameList" .= _dtTrailNameList]

instance ToPath DescribeTrails where
        toPath = const "/"

instance ToQuery DescribeTrails where
        toQuery = const mempty

-- | /See:/ 'describeTrailsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrTrailList'
newtype DescribeTrailsResponse = DescribeTrailsResponse'{_dtrTrailList :: Maybe [Trail]} deriving (Eq, Read, Show)

-- | 'DescribeTrailsResponse' smart constructor.
describeTrailsResponse :: DescribeTrailsResponse
describeTrailsResponse = DescribeTrailsResponse'{_dtrTrailList = Nothing};

-- | The list of trails.
dtrTrailList :: Lens' DescribeTrailsResponse (Maybe [Trail])
dtrTrailList = lens _dtrTrailList (\ s a -> s{_dtrTrailList = a});
