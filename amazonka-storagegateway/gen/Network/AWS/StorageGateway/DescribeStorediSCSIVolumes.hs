{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns the description of the gateway volumes specified
-- in the request. The list of gateway volumes in the request must be from
-- one gateway. In the response Amazon Storage Gateway returns volume
-- information sorted by volume ARNs.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeStorediSCSIVolumes.html>
module Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
    (
    -- * Request
      DescribeStorediSCSIVolumes
    -- ** Request constructor
    , describeStorediSCSIVolumes
    -- ** Request lenses
    , dsscsivVolumeARNs

    -- * Response
    , DescribeStorediSCSIVolumesResponse
    -- ** Response constructor
    , describeStorediSCSIVolumesResponse
    -- ** Response lenses
    , dsscsivrStorediSCSIVolumes
    , dsscsivrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON Object containing a list of
-- DescribeStorediSCSIVolumesInput$VolumeARNs.
--
-- /See:/ 'describeStorediSCSIVolumes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsscsivVolumeARNs'
newtype DescribeStorediSCSIVolumes = DescribeStorediSCSIVolumes'
    { _dsscsivVolumeARNs :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStorediSCSIVolumes' smart constructor.
describeStorediSCSIVolumes :: DescribeStorediSCSIVolumes
describeStorediSCSIVolumes =
    DescribeStorediSCSIVolumes'
    { _dsscsivVolumeARNs = mempty
    }

-- | An array of strings where each string represents the Amazon Resource
-- Name (ARN) of a stored volume. All of the specified stored volumes must
-- from the same gateway. Use ListVolumes to get volume ARNs for a gateway.
dsscsivVolumeARNs :: Lens' DescribeStorediSCSIVolumes [Text]
dsscsivVolumeARNs = lens _dsscsivVolumeARNs (\ s a -> s{_dsscsivVolumeARNs = a});

instance AWSRequest DescribeStorediSCSIVolumes where
        type Sv DescribeStorediSCSIVolumes = StorageGateway
        type Rs DescribeStorediSCSIVolumes =
             DescribeStorediSCSIVolumesResponse
        request = postJSON
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
          = object ["VolumeARNs" .= _dsscsivVolumeARNs]

instance ToPath DescribeStorediSCSIVolumes where
        toPath = const "/"

instance ToQuery DescribeStorediSCSIVolumes where
        toQuery = const mempty

-- | /See:/ 'describeStorediSCSIVolumesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsscsivrStorediSCSIVolumes'
--
-- * 'dsscsivrStatus'
data DescribeStorediSCSIVolumesResponse = DescribeStorediSCSIVolumesResponse'
    { _dsscsivrStorediSCSIVolumes :: !(Maybe [StorediSCSIVolume])
    , _dsscsivrStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeStorediSCSIVolumesResponse' smart constructor.
describeStorediSCSIVolumesResponse :: Int -> DescribeStorediSCSIVolumesResponse
describeStorediSCSIVolumesResponse pStatus =
    DescribeStorediSCSIVolumesResponse'
    { _dsscsivrStorediSCSIVolumes = Nothing
    , _dsscsivrStatus = pStatus
    }

-- | FIXME: Undocumented member.
dsscsivrStorediSCSIVolumes :: Lens' DescribeStorediSCSIVolumesResponse [StorediSCSIVolume]
dsscsivrStorediSCSIVolumes = lens _dsscsivrStorediSCSIVolumes (\ s a -> s{_dsscsivrStorediSCSIVolumes = a}) . _Default;

-- | FIXME: Undocumented member.
dsscsivrStatus :: Lens' DescribeStorediSCSIVolumesResponse Int
dsscsivrStatus = lens _dsscsivrStatus (\ s a -> s{_dsscsivrStatus = a});
