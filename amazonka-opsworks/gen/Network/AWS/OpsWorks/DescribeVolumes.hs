{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeVolumes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an instance's Amazon EBS volumes.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DescribeVolumes
    (
    -- * Creating a Request
      describeVolumes
    , DescribeVolumes
    -- * Request Lenses
    , dvInstanceId
    , dvVolumeIds
    , dvRAIdArrayId
    , dvStackId

    -- * Destructuring the Response
    , describeVolumesResponse
    , DescribeVolumesResponse
    -- * Response Lenses
    , dvrsVolumes
    , dvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVolumes' smart constructor.
data DescribeVolumes = DescribeVolumes'
  { _dvInstanceId  :: !(Maybe Text)
  , _dvVolumeIds   :: !(Maybe [Text])
  , _dvRAIdArrayId :: !(Maybe Text)
  , _dvStackId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVolumes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvInstanceId' - The instance ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified instance.
--
-- * 'dvVolumeIds' - Am array of volume IDs. If you use this parameter, @DescribeVolumes@ returns descriptions of the specified volumes. Otherwise, it returns a description of every volume.
--
-- * 'dvRAIdArrayId' - The RAID array ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified RAID array.
--
-- * 'dvStackId' - A stack ID. The action describes the stack's registered Amazon EBS volumes.
describeVolumes
    :: DescribeVolumes
describeVolumes =
  DescribeVolumes'
    { _dvInstanceId = Nothing
    , _dvVolumeIds = Nothing
    , _dvRAIdArrayId = Nothing
    , _dvStackId = Nothing
    }


-- | The instance ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified instance.
dvInstanceId :: Lens' DescribeVolumes (Maybe Text)
dvInstanceId = lens _dvInstanceId (\ s a -> s{_dvInstanceId = a})

-- | Am array of volume IDs. If you use this parameter, @DescribeVolumes@ returns descriptions of the specified volumes. Otherwise, it returns a description of every volume.
dvVolumeIds :: Lens' DescribeVolumes [Text]
dvVolumeIds = lens _dvVolumeIds (\ s a -> s{_dvVolumeIds = a}) . _Default . _Coerce

-- | The RAID array ID. If you use this parameter, @DescribeVolumes@ returns descriptions of the volumes associated with the specified RAID array.
dvRAIdArrayId :: Lens' DescribeVolumes (Maybe Text)
dvRAIdArrayId = lens _dvRAIdArrayId (\ s a -> s{_dvRAIdArrayId = a})

-- | A stack ID. The action describes the stack's registered Amazon EBS volumes.
dvStackId :: Lens' DescribeVolumes (Maybe Text)
dvStackId = lens _dvStackId (\ s a -> s{_dvStackId = a})

instance AWSRequest DescribeVolumes where
        type Rs DescribeVolumes = DescribeVolumesResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVolumesResponse' <$>
                   (x .?> "Volumes" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable DescribeVolumes where

instance NFData DescribeVolumes where

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
              (catMaybes
                 [("InstanceId" .=) <$> _dvInstanceId,
                  ("VolumeIds" .=) <$> _dvVolumeIds,
                  ("RaidArrayId" .=) <$> _dvRAIdArrayId,
                  ("StackId" .=) <$> _dvStackId])

instance ToPath DescribeVolumes where
        toPath = const "/"

instance ToQuery DescribeVolumes where
        toQuery = const mempty

-- | Contains the response to a @DescribeVolumes@ request.
--
--
--
-- /See:/ 'describeVolumesResponse' smart constructor.
data DescribeVolumesResponse = DescribeVolumesResponse'
  { _dvrsVolumes        :: !(Maybe [Volume])
  , _dvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVolumesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvrsVolumes' - An array of volume IDs.
--
-- * 'dvrsResponseStatus' - -- | The response status code.
describeVolumesResponse
    :: Int -- ^ 'dvrsResponseStatus'
    -> DescribeVolumesResponse
describeVolumesResponse pResponseStatus_ =
  DescribeVolumesResponse'
    {_dvrsVolumes = Nothing, _dvrsResponseStatus = pResponseStatus_}


-- | An array of volume IDs.
dvrsVolumes :: Lens' DescribeVolumesResponse [Volume]
dvrsVolumes = lens _dvrsVolumes (\ s a -> s{_dvrsVolumes = a}) . _Default . _Coerce

-- | -- | The response status code.
dvrsResponseStatus :: Lens' DescribeVolumesResponse Int
dvrsResponseStatus = lens _dvrsResponseStatus (\ s a -> s{_dvrsResponseStatus = a})

instance NFData DescribeVolumesResponse where
