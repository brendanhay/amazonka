{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
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

-- | This operation returns a description of the gateway volumes specified in
-- the request. This operation is supported only for the gateway-cached
-- volume architecture.
--
-- The list of gateway volumes in the request must be from one gateway. In
-- the response Amazon Storage Gateway returns volume information sorted by
-- volume Amazon Resource Name (ARN).
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeCachediSCSIVolumes.html>
module Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
    (
    -- * Request
      DescribeCachediSCSIVolumes
    -- ** Request constructor
    , describeCachediSCSIVolumes
    -- ** Request lenses
    , dcscsivVolumeARNs

    -- * Response
    , DescribeCachediSCSIVolumesResponse
    -- ** Response constructor
    , describeCachediSCSIVolumesResponse
    -- ** Response lenses
    , dcscsivrCachediSCSIVolumes
    , dcscsivrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | /See:/ 'describeCachediSCSIVolumes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcscsivVolumeARNs'
newtype DescribeCachediSCSIVolumes = DescribeCachediSCSIVolumes'
    { _dcscsivVolumeARNs :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCachediSCSIVolumes' smart constructor.
describeCachediSCSIVolumes :: DescribeCachediSCSIVolumes
describeCachediSCSIVolumes =
    DescribeCachediSCSIVolumes'
    { _dcscsivVolumeARNs = mempty
    }

-- | FIXME: Undocumented member.
dcscsivVolumeARNs :: Lens' DescribeCachediSCSIVolumes [Text]
dcscsivVolumeARNs = lens _dcscsivVolumeARNs (\ s a -> s{_dcscsivVolumeARNs = a});

instance AWSRequest DescribeCachediSCSIVolumes where
        type Sv DescribeCachediSCSIVolumes = StorageGateway
        type Rs DescribeCachediSCSIVolumes =
             DescribeCachediSCSIVolumesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCachediSCSIVolumesResponse' <$>
                   (x .?> "CachediSCSIVolumes" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeCachediSCSIVolumes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeCachediSCSIVolumes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCachediSCSIVolumes where
        toJSON DescribeCachediSCSIVolumes'{..}
          = object ["VolumeARNs" .= _dcscsivVolumeARNs]

instance ToPath DescribeCachediSCSIVolumes where
        toPath = const "/"

instance ToQuery DescribeCachediSCSIVolumes where
        toQuery = const mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'describeCachediSCSIVolumesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcscsivrCachediSCSIVolumes'
--
-- * 'dcscsivrStatus'
data DescribeCachediSCSIVolumesResponse = DescribeCachediSCSIVolumesResponse'
    { _dcscsivrCachediSCSIVolumes :: !(Maybe [CachediSCSIVolume])
    , _dcscsivrStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCachediSCSIVolumesResponse' smart constructor.
describeCachediSCSIVolumesResponse :: Int -> DescribeCachediSCSIVolumesResponse
describeCachediSCSIVolumesResponse pStatus =
    DescribeCachediSCSIVolumesResponse'
    { _dcscsivrCachediSCSIVolumes = Nothing
    , _dcscsivrStatus = pStatus
    }

-- | An array of objects where each object contains metadata about one cached
-- volume.
dcscsivrCachediSCSIVolumes :: Lens' DescribeCachediSCSIVolumesResponse [CachediSCSIVolume]
dcscsivrCachediSCSIVolumes = lens _dcscsivrCachediSCSIVolumes (\ s a -> s{_dcscsivrCachediSCSIVolumes = a}) . _Default;

-- | FIXME: Undocumented member.
dcscsivrStatus :: Lens' DescribeCachediSCSIVolumesResponse Int
dcscsivrStatus = lens _dcscsivrStatus (\ s a -> s{_dcscsivrStatus = a});
