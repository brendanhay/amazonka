{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeVolumes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes an instance\'s Amazon EBS volumes.
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
    , dvrqInstanceId
    , dvrqVolumeIds
    , dvrqRAIdArrayId
    , dvrqStackId

    -- * Response
    , DescribeVolumesResponse
    -- ** Response constructor
    , describeVolumesResponse
    -- ** Response lenses
    , dvrsVolumes
    , dvrsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVolumes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrqInstanceId'
--
-- * 'dvrqVolumeIds'
--
-- * 'dvrqRAIdArrayId'
--
-- * 'dvrqStackId'
data DescribeVolumes = DescribeVolumes'
    { _dvrqInstanceId  :: !(Maybe Text)
    , _dvrqVolumeIds   :: !(Maybe [Text])
    , _dvrqRAIdArrayId :: !(Maybe Text)
    , _dvrqStackId     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVolumes' smart constructor.
describeVolumes :: DescribeVolumes
describeVolumes =
    DescribeVolumes'
    { _dvrqInstanceId = Nothing
    , _dvrqVolumeIds = Nothing
    , _dvrqRAIdArrayId = Nothing
    , _dvrqStackId = Nothing
    }

-- | The instance ID. If you use this parameter, @DescribeVolumes@ returns
-- descriptions of the volumes associated with the specified instance.
dvrqInstanceId :: Lens' DescribeVolumes (Maybe Text)
dvrqInstanceId = lens _dvrqInstanceId (\ s a -> s{_dvrqInstanceId = a});

-- | Am array of volume IDs. If you use this parameter, @DescribeVolumes@
-- returns descriptions of the specified volumes. Otherwise, it returns a
-- description of every volume.
dvrqVolumeIds :: Lens' DescribeVolumes [Text]
dvrqVolumeIds = lens _dvrqVolumeIds (\ s a -> s{_dvrqVolumeIds = a}) . _Default;

-- | The RAID array ID. If you use this parameter, @DescribeVolumes@ returns
-- descriptions of the volumes associated with the specified RAID array.
dvrqRAIdArrayId :: Lens' DescribeVolumes (Maybe Text)
dvrqRAIdArrayId = lens _dvrqRAIdArrayId (\ s a -> s{_dvrqRAIdArrayId = a});

-- | A stack ID. The action describes the stack\'s registered Amazon EBS
-- volumes.
dvrqStackId :: Lens' DescribeVolumes (Maybe Text)
dvrqStackId = lens _dvrqStackId (\ s a -> s{_dvrqStackId = a});

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
              ["InstanceId" .= _dvrqInstanceId,
               "VolumeIds" .= _dvrqVolumeIds,
               "RaidArrayId" .= _dvrqRAIdArrayId,
               "StackId" .= _dvrqStackId]

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
-- * 'dvrsVolumes'
--
-- * 'dvrsStatus'
data DescribeVolumesResponse = DescribeVolumesResponse'
    { _dvrsVolumes :: !(Maybe [Volume])
    , _dvrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVolumesResponse' smart constructor.
describeVolumesResponse :: Int -> DescribeVolumesResponse
describeVolumesResponse pStatus_ =
    DescribeVolumesResponse'
    { _dvrsVolumes = Nothing
    , _dvrsStatus = pStatus_
    }

-- | An array of volume IDs.
dvrsVolumes :: Lens' DescribeVolumesResponse [Volume]
dvrsVolumes = lens _dvrsVolumes (\ s a -> s{_dvrsVolumes = a}) . _Default;

-- | FIXME: Undocumented member.
dvrsStatus :: Lens' DescribeVolumesResponse Int
dvrsStatus = lens _dvrsStatus (\ s a -> s{_dvrsStatus = a});
