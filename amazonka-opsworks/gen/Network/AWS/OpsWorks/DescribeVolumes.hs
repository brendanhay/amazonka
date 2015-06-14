{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.DescribeVolumes
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

-- | Describes an instance\'s Amazon EBS volumes.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeVolumes.html>
module Network.AWS.OpsWorks.DescribeVolumes
    (
    -- * Request
      DescribeVolumes
    -- ** Request constructor
    , describeVolumes
    -- ** Request lenses
    , dvInstanceId
    , dvVolumeIds
    , dvRAIDArrayId
    , dvStackId

    -- * Response
    , DescribeVolumesResponse
    -- ** Response constructor
    , describeVolumesResponse
    -- ** Response lenses
    , dvrVolumes
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.OpsWorks.Types

-- | /See:/ 'describeVolumes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvInstanceId'
--
-- * 'dvVolumeIds'
--
-- * 'dvRAIDArrayId'
--
-- * 'dvStackId'
data DescribeVolumes = DescribeVolumes'{_dvInstanceId :: Maybe Text, _dvVolumeIds :: Maybe [Text], _dvRAIDArrayId :: Maybe Text, _dvStackId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeVolumes' smart constructor.
describeVolumes :: DescribeVolumes
describeVolumes = DescribeVolumes'{_dvInstanceId = Nothing, _dvVolumeIds = Nothing, _dvRAIDArrayId = Nothing, _dvStackId = Nothing};

-- | The instance ID. If you use this parameter, @DescribeVolumes@ returns
-- descriptions of the volumes associated with the specified instance.
dvInstanceId :: Lens' DescribeVolumes (Maybe Text)
dvInstanceId = lens _dvInstanceId (\ s a -> s{_dvInstanceId = a});

-- | Am array of volume IDs. If you use this parameter, @DescribeVolumes@
-- returns descriptions of the specified volumes. Otherwise, it returns a
-- description of every volume.
dvVolumeIds :: Lens' DescribeVolumes (Maybe [Text])
dvVolumeIds = lens _dvVolumeIds (\ s a -> s{_dvVolumeIds = a});

-- | The RAID array ID. If you use this parameter, @DescribeVolumes@ returns
-- descriptions of the volumes associated with the specified RAID array.
dvRAIDArrayId :: Lens' DescribeVolumes (Maybe Text)
dvRAIDArrayId = lens _dvRAIDArrayId (\ s a -> s{_dvRAIDArrayId = a});

-- | A stack ID. The action describes the stack\'s registered Amazon EBS
-- volumes.
dvStackId :: Lens' DescribeVolumes (Maybe Text)
dvStackId = lens _dvStackId (\ s a -> s{_dvStackId = a});

instance AWSRequest DescribeVolumes where
        type Sv DescribeVolumes = OpsWorks
        type Rs DescribeVolumes = DescribeVolumesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVolumesResponse' <$>
                   x .?> "Volumes" .!@ mempty)

instance ToHeaders DescribeVolumes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeVolumes" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeVolumes where
        toJSON DescribeVolumes'{..}
          = object
              ["InstanceId" .= _dvInstanceId,
               "VolumeIds" .= _dvVolumeIds,
               "RaidArrayId" .= _dvRAIDArrayId,
               "StackId" .= _dvStackId]

instance ToPath DescribeVolumes where
        toPath = const "/"

instance ToQuery DescribeVolumes where
        toQuery = const mempty

-- | /See:/ 'describeVolumesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrVolumes'
newtype DescribeVolumesResponse = DescribeVolumesResponse'{_dvrVolumes :: Maybe [Volume]} deriving (Eq, Read, Show)

-- | 'DescribeVolumesResponse' smart constructor.
describeVolumesResponse :: DescribeVolumesResponse
describeVolumesResponse = DescribeVolumesResponse'{_dvrVolumes = Nothing};

-- | An array of volume IDs.
dvrVolumes :: Lens' DescribeVolumesResponse (Maybe [Volume])
dvrVolumes = lens _dvrVolumes (\ s a -> s{_dvrVolumes = a});
