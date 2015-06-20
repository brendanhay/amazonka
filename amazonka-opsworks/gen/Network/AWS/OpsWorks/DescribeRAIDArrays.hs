{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.DescribeRAIDArrays
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

-- | Describe an instance\'s RAID arrays.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeRAIDArrays.html>
module Network.AWS.OpsWorks.DescribeRAIDArrays
    (
    -- * Request
      DescribeRAIDArrays
    -- ** Request constructor
    , describeRAIDArrays
    -- ** Request lenses
    , draInstanceId
    , draRAIDArrayIds
    , draStackId

    -- * Response
    , DescribeRAIDArraysResponse
    -- ** Response constructor
    , describeRAIDArraysResponse
    -- ** Response lenses
    , drarRAIDArrays
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRAIDArrays' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'draInstanceId'
--
-- * 'draRAIDArrayIds'
--
-- * 'draStackId'
data DescribeRAIDArrays = DescribeRAIDArrays'{_draInstanceId :: Maybe Text, _draRAIDArrayIds :: Maybe [Text], _draStackId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeRAIDArrays' smart constructor.
describeRAIDArrays :: DescribeRAIDArrays
describeRAIDArrays = DescribeRAIDArrays'{_draInstanceId = Nothing, _draRAIDArrayIds = Nothing, _draStackId = Nothing};

-- | The instance ID. If you use this parameter, @DescribeRaidArrays@ returns
-- descriptions of the RAID arrays associated with the specified instance.
draInstanceId :: Lens' DescribeRAIDArrays (Maybe Text)
draInstanceId = lens _draInstanceId (\ s a -> s{_draInstanceId = a});

-- | An array of RAID array IDs. If you use this parameter,
-- @DescribeRaidArrays@ returns descriptions of the specified arrays.
-- Otherwise, it returns a description of every array.
draRAIDArrayIds :: Lens' DescribeRAIDArrays [Text]
draRAIDArrayIds = lens _draRAIDArrayIds (\ s a -> s{_draRAIDArrayIds = a}) . _Default;

-- | The stack ID.
draStackId :: Lens' DescribeRAIDArrays (Maybe Text)
draStackId = lens _draStackId (\ s a -> s{_draStackId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeRAIDArrays where
        type Sv DescribeRAIDArrays = OpsWorks
        type Rs DescribeRAIDArrays =
             DescribeRAIDArraysResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRAIDArraysResponse' <$>
                   (x .?> "RaidArrays" .!@ mempty))

instance ToHeaders DescribeRAIDArrays where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeRAIDArrays" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRAIDArrays where
        toJSON DescribeRAIDArrays'{..}
          = object
              ["InstanceId" .= _draInstanceId,
               "RaidArrayIds" .= _draRAIDArrayIds,
               "StackId" .= _draStackId]

instance ToPath DescribeRAIDArrays where
        toPath = const "/"

instance ToQuery DescribeRAIDArrays where
        toQuery = const mempty

-- | /See:/ 'describeRAIDArraysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drarRAIDArrays'
newtype DescribeRAIDArraysResponse = DescribeRAIDArraysResponse'{_drarRAIDArrays :: Maybe [RAIDArray]} deriving (Eq, Read, Show)

-- | 'DescribeRAIDArraysResponse' smart constructor.
describeRAIDArraysResponse :: DescribeRAIDArraysResponse
describeRAIDArraysResponse = DescribeRAIDArraysResponse'{_drarRAIDArrays = Nothing};

-- | A @RaidArrays@ object that describes the specified RAID arrays.
drarRAIDArrays :: Lens' DescribeRAIDArraysResponse [RAIDArray]
drarRAIDArrays = lens _drarRAIDArrays (\ s a -> s{_drarRAIDArrays = a}) . _Default;
