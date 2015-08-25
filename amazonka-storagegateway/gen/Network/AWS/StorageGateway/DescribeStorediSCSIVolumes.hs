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
-- Module      : Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the description of the gateway volumes specified
-- in the request. The list of gateway volumes in the request must be from
-- one gateway. In the response Amazon Storage Gateway returns volume
-- information sorted by volume ARNs.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeStorediSCSIVolumes.html AWS API Reference> for DescribeStorediSCSIVolumes.
module Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
    (
    -- * Creating a Request
      describeStorediSCSIVolumes
    , DescribeStorediSCSIVolumes
    -- * Request Lenses
    , dsscsivVolumeARNs

    -- * Destructuring the Response
    , describeStorediSCSIVolumesResponse
    , DescribeStorediSCSIVolumesResponse
    -- * Response Lenses
    , dsscsivrsStorediSCSIVolumes
    , dsscsivrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | A JSON Object containing a list of
-- DescribeStorediSCSIVolumesInput$VolumeARNs.
--
-- /See:/ 'describeStorediSCSIVolumes' smart constructor.
newtype DescribeStorediSCSIVolumes = DescribeStorediSCSIVolumes'
    { _dsscsivVolumeARNs :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeStorediSCSIVolumes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsscsivVolumeARNs'
describeStorediSCSIVolumes
    :: DescribeStorediSCSIVolumes
describeStorediSCSIVolumes =
    DescribeStorediSCSIVolumes'
    { _dsscsivVolumeARNs = mempty
    }

-- | An array of strings where each string represents the Amazon Resource
-- Name (ARN) of a stored volume. All of the specified stored volumes must
-- from the same gateway. Use ListVolumes to get volume ARNs for a gateway.
dsscsivVolumeARNs :: Lens' DescribeStorediSCSIVolumes [Text]
dsscsivVolumeARNs = lens _dsscsivVolumeARNs (\ s a -> s{_dsscsivVolumeARNs = a}) . _Coerce;

instance AWSRequest DescribeStorediSCSIVolumes where
        type Rs DescribeStorediSCSIVolumes =
             DescribeStorediSCSIVolumesResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DescribeStorediSCSIVolumesResponse' <$>
                   (x .?> "StorediSCSIVolumes" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeStorediSCSIVolumes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeStorediSCSIVolumes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeStorediSCSIVolumes where
        toJSON DescribeStorediSCSIVolumes'{..}
          = object
              (catMaybes
                 [Just ("VolumeARNs" .= _dsscsivVolumeARNs)])

instance ToPath DescribeStorediSCSIVolumes where
        toPath = const "/"

instance ToQuery DescribeStorediSCSIVolumes where
        toQuery = const mempty

-- | /See:/ 'describeStorediSCSIVolumesResponse' smart constructor.
data DescribeStorediSCSIVolumesResponse = DescribeStorediSCSIVolumesResponse'
    { _dsscsivrsStorediSCSIVolumes :: !(Maybe [StorediSCSIVolume])
    , _dsscsivrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeStorediSCSIVolumesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsscsivrsStorediSCSIVolumes'
--
-- * 'dsscsivrsStatus'
describeStorediSCSIVolumesResponse
    :: Int -- ^ 'dsscsivrsStatus'
    -> DescribeStorediSCSIVolumesResponse
describeStorediSCSIVolumesResponse pStatus_ =
    DescribeStorediSCSIVolumesResponse'
    { _dsscsivrsStorediSCSIVolumes = Nothing
    , _dsscsivrsStatus = pStatus_
    }

-- | Undocumented member.
dsscsivrsStorediSCSIVolumes :: Lens' DescribeStorediSCSIVolumesResponse [StorediSCSIVolume]
dsscsivrsStorediSCSIVolumes = lens _dsscsivrsStorediSCSIVolumes (\ s a -> s{_dsscsivrsStorediSCSIVolumes = a}) . _Default . _Coerce;

-- | The response status code.
dsscsivrsStatus :: Lens' DescribeStorediSCSIVolumesResponse Int
dsscsivrsStatus = lens _dsscsivrsStatus (\ s a -> s{_dsscsivrsStatus = a});
