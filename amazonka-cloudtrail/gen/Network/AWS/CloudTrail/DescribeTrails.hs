{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
    , dtrStatus
    ) where

import           Network.AWS.CloudTrail.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Returns information about the trail.
--
-- /See:/ 'describeTrails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtTrailNameList'
newtype DescribeTrails = DescribeTrails'
    { _dtTrailNameList :: Maybe [Text]
    } deriving (Eq,Read,Show)

-- | 'DescribeTrails' smart constructor.
describeTrails :: DescribeTrails
describeTrails =
    DescribeTrails'
    { _dtTrailNameList = Nothing
    }

-- | The trail returned.
dtTrailNameList :: Lens' DescribeTrails [Text]
dtTrailNameList = lens _dtTrailNameList (\ s a -> s{_dtTrailNameList = a}) . _Default;

instance AWSRequest DescribeTrails where
        type Sv DescribeTrails = CloudTrail
        type Rs DescribeTrails = DescribeTrailsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTrailsResponse' <$>
                   (x .?> "trailList" .!@ mempty) <*>
                     (pure (fromEnum s)))

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

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'describeTrailsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrTrailList'
--
-- * 'dtrStatus'
data DescribeTrailsResponse = DescribeTrailsResponse'
    { _dtrTrailList :: !(Maybe [Trail])
    , _dtrStatus    :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeTrailsResponse' smart constructor.
describeTrailsResponse :: Int -> DescribeTrailsResponse
describeTrailsResponse pStatus =
    DescribeTrailsResponse'
    { _dtrTrailList = Nothing
    , _dtrStatus = pStatus
    }

-- | The list of trails.
dtrTrailList :: Lens' DescribeTrailsResponse [Trail]
dtrTrailList = lens _dtrTrailList (\ s a -> s{_dtrTrailList = a}) . _Default;

-- | FIXME: Undocumented member.
dtrStatus :: Lens' DescribeTrailsResponse Int
dtrStatus = lens _dtrStatus (\ s a -> s{_dtrStatus = a});
