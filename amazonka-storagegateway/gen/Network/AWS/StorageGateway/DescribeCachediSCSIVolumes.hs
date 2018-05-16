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
-- Module      : Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the gateway volumes specified in the request. This operation is only supported in the cached volume gateway types.
--
--
-- The list of gateway volumes in the request must be from one gateway. In the response Amazon Storage Gateway returns volume information sorted by volume Amazon Resource Name (ARN).
--
module Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
    (
    -- * Creating a Request
      describeCachediSCSIVolumes
    , DescribeCachediSCSIVolumes
    -- * Request Lenses
    , dcscsivVolumeARNs

    -- * Destructuring the Response
    , describeCachediSCSIVolumesResponse
    , DescribeCachediSCSIVolumesResponse
    -- * Response Lenses
    , dcscsivrsCachediSCSIVolumes
    , dcscsivrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'describeCachediSCSIVolumes' smart constructor.
newtype DescribeCachediSCSIVolumes = DescribeCachediSCSIVolumes'
  { _dcscsivVolumeARNs :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCachediSCSIVolumes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcscsivVolumeARNs' - Undocumented member.
describeCachediSCSIVolumes
    :: DescribeCachediSCSIVolumes
describeCachediSCSIVolumes =
  DescribeCachediSCSIVolumes' {_dcscsivVolumeARNs = mempty}


-- | Undocumented member.
dcscsivVolumeARNs :: Lens' DescribeCachediSCSIVolumes [Text]
dcscsivVolumeARNs = lens _dcscsivVolumeARNs (\ s a -> s{_dcscsivVolumeARNs = a}) . _Coerce

instance AWSRequest DescribeCachediSCSIVolumes where
        type Rs DescribeCachediSCSIVolumes =
             DescribeCachediSCSIVolumesResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCachediSCSIVolumesResponse' <$>
                   (x .?> "CachediSCSIVolumes" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeCachediSCSIVolumes where

instance NFData DescribeCachediSCSIVolumes where

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
          = object
              (catMaybes
                 [Just ("VolumeARNs" .= _dcscsivVolumeARNs)])

instance ToPath DescribeCachediSCSIVolumes where
        toPath = const "/"

instance ToQuery DescribeCachediSCSIVolumes where
        toQuery = const mempty

-- | A JSON object containing the following fields:
--
--
--
-- /See:/ 'describeCachediSCSIVolumesResponse' smart constructor.
data DescribeCachediSCSIVolumesResponse = DescribeCachediSCSIVolumesResponse'
  { _dcscsivrsCachediSCSIVolumes :: !(Maybe [CachediSCSIVolume])
  , _dcscsivrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCachediSCSIVolumesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcscsivrsCachediSCSIVolumes' - An array of objects where each object contains metadata about one cached volume.
--
-- * 'dcscsivrsResponseStatus' - -- | The response status code.
describeCachediSCSIVolumesResponse
    :: Int -- ^ 'dcscsivrsResponseStatus'
    -> DescribeCachediSCSIVolumesResponse
describeCachediSCSIVolumesResponse pResponseStatus_ =
  DescribeCachediSCSIVolumesResponse'
    { _dcscsivrsCachediSCSIVolumes = Nothing
    , _dcscsivrsResponseStatus = pResponseStatus_
    }


-- | An array of objects where each object contains metadata about one cached volume.
dcscsivrsCachediSCSIVolumes :: Lens' DescribeCachediSCSIVolumesResponse [CachediSCSIVolume]
dcscsivrsCachediSCSIVolumes = lens _dcscsivrsCachediSCSIVolumes (\ s a -> s{_dcscsivrsCachediSCSIVolumes = a}) . _Default . _Coerce

-- | -- | The response status code.
dcscsivrsResponseStatus :: Lens' DescribeCachediSCSIVolumesResponse Int
dcscsivrsResponseStatus = lens _dcscsivrsResponseStatus (\ s a -> s{_dcscsivrsResponseStatus = a})

instance NFData DescribeCachediSCSIVolumesResponse
         where
