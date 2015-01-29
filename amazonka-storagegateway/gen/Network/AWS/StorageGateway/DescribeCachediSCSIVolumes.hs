{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns a description of the gateway volumes specified in the
-- request. This operation is supported only for the gateway-cached volume
-- architecture.
--
-- The list of gateway volumes in the request must be from one gateway. In the
-- response Amazon Storage Gateway returns volume information sorted by volume
-- Amazon Resource Name (ARN).
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
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype DescribeCachediSCSIVolumes = DescribeCachediSCSIVolumes
    { _dcscsivVolumeARNs :: List "VolumeARNs" Text
    } deriving (Eq, Ord, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeCachediSCSIVolumes where
    type Item DescribeCachediSCSIVolumes = Text

    fromList = DescribeCachediSCSIVolumes . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dcscsivVolumeARNs

-- | 'DescribeCachediSCSIVolumes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcscsivVolumeARNs' @::@ ['Text']
--
describeCachediSCSIVolumes :: DescribeCachediSCSIVolumes
describeCachediSCSIVolumes = DescribeCachediSCSIVolumes
    { _dcscsivVolumeARNs = mempty
    }

dcscsivVolumeARNs :: Lens' DescribeCachediSCSIVolumes [Text]
dcscsivVolumeARNs =
    lens _dcscsivVolumeARNs (\s a -> s { _dcscsivVolumeARNs = a })
        . _List

newtype DescribeCachediSCSIVolumesResponse = DescribeCachediSCSIVolumesResponse
    { _dcscsivrCachediSCSIVolumes :: List "CachediSCSIVolumes" CachediSCSIVolume
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeCachediSCSIVolumesResponse where
    type Item DescribeCachediSCSIVolumesResponse = CachediSCSIVolume

    fromList = DescribeCachediSCSIVolumesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dcscsivrCachediSCSIVolumes

-- | 'DescribeCachediSCSIVolumesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcscsivrCachediSCSIVolumes' @::@ ['CachediSCSIVolume']
--
describeCachediSCSIVolumesResponse :: DescribeCachediSCSIVolumesResponse
describeCachediSCSIVolumesResponse = DescribeCachediSCSIVolumesResponse
    { _dcscsivrCachediSCSIVolumes = mempty
    }

-- | An array of objects where each object contains metadata about one cached
-- volume.
dcscsivrCachediSCSIVolumes :: Lens' DescribeCachediSCSIVolumesResponse [CachediSCSIVolume]
dcscsivrCachediSCSIVolumes =
    lens _dcscsivrCachediSCSIVolumes
        (\s a -> s { _dcscsivrCachediSCSIVolumes = a })
            . _List

instance ToPath DescribeCachediSCSIVolumes where
    toPath = const "/"

instance ToQuery DescribeCachediSCSIVolumes where
    toQuery = const mempty

instance ToHeaders DescribeCachediSCSIVolumes

instance ToJSON DescribeCachediSCSIVolumes where
    toJSON DescribeCachediSCSIVolumes{..} = object
        [ "VolumeARNs" .= _dcscsivVolumeARNs
        ]

instance AWSRequest DescribeCachediSCSIVolumes where
    type Sv DescribeCachediSCSIVolumes = StorageGateway
    type Rs DescribeCachediSCSIVolumes = DescribeCachediSCSIVolumesResponse

    request  = post "DescribeCachediSCSIVolumes"
    response = jsonResponse

instance FromJSON DescribeCachediSCSIVolumesResponse where
    parseJSON = withObject "DescribeCachediSCSIVolumesResponse" $ \o -> DescribeCachediSCSIVolumesResponse
        <$> o .:? "CachediSCSIVolumes" .!= mempty
