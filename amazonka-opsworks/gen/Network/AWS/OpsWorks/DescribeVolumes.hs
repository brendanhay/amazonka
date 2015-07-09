{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeVolumes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
    , dvRAIdArrayId
    , dvStackId

    -- * Response
    , DescribeVolumesResponse
    -- ** Response constructor
    , describeVolumesResponse
    -- ** Response lenses
    , dvrVolumes
    , dvrStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVolumes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvInstanceId'
--
-- * 'dvVolumeIds'
--
-- * 'dvRAIdArrayId'
--
-- * 'dvStackId'
data DescribeVolumes = DescribeVolumes'
    { _dvInstanceId  :: !(Maybe Text)
    , _dvVolumeIds   :: !(Maybe [Text])
    , _dvRAIdArrayId :: !(Maybe Text)
    , _dvStackId     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVolumes' smart constructor.
describeVolumes :: DescribeVolumes
describeVolumes =
    DescribeVolumes'
    { _dvInstanceId = Nothing
    , _dvVolumeIds = Nothing
    , _dvRAIdArrayId = Nothing
    , _dvStackId = Nothing
    }

-- | The instance ID. If you use this parameter, @DescribeVolumes@ returns
-- descriptions of the volumes associated with the specified instance.
dvInstanceId :: Lens' DescribeVolumes (Maybe Text)
dvInstanceId = lens _dvInstanceId (\ s a -> s{_dvInstanceId = a});

-- | Am array of volume IDs. If you use this parameter, @DescribeVolumes@
-- returns descriptions of the specified volumes. Otherwise, it returns a
-- description of every volume.
dvVolumeIds :: Lens' DescribeVolumes [Text]
dvVolumeIds = lens _dvVolumeIds (\ s a -> s{_dvVolumeIds = a}) . _Default;

-- | The RAID array ID. If you use this parameter, @DescribeVolumes@ returns
-- descriptions of the volumes associated with the specified RAID array.
dvRAIdArrayId :: Lens' DescribeVolumes (Maybe Text)
dvRAIdArrayId = lens _dvRAIdArrayId (\ s a -> s{_dvRAIdArrayId = a});

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
                   (x .?> "Volumes" .!@ mempty) <*> (pure (fromEnum s)))

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
               "RaidArrayId" .= _dvRAIdArrayId,
               "StackId" .= _dvStackId]

instance ToPath DescribeVolumes where
        toPath = const "/"

instance ToQuery DescribeVolumes where
        toQuery = const mempty

-- | Contains the response to a @DescribeVolumes@ request.
--
-- /See:/ 'describeVolumesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrVolumes'
--
-- * 'dvrStatus'
data DescribeVolumesResponse = DescribeVolumesResponse'
    { _dvrVolumes :: !(Maybe [Volume])
    , _dvrStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVolumesResponse' smart constructor.
describeVolumesResponse :: Int -> DescribeVolumesResponse
describeVolumesResponse pStatus =
    DescribeVolumesResponse'
    { _dvrVolumes = Nothing
    , _dvrStatus = pStatus
    }

-- | An array of volume IDs.
dvrVolumes :: Lens' DescribeVolumesResponse [Volume]
dvrVolumes = lens _dvrVolumes (\ s a -> s{_dvrVolumes = a}) . _Default;

-- | FIXME: Undocumented member.
dvrStatus :: Lens' DescribeVolumesResponse Int
dvrStatus = lens _dvrStatus (\ s a -> s{_dvrStatus = a});
