{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns description of the gateway volumes specified in the
-- request. The list of gateway volumes in the request must be from one
-- gateway. In the response Amazon Storage Gateway returns volume information
-- sorted by volume ARNs.
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
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype DescribeStorediSCSIVolumes = DescribeStorediSCSIVolumes
    { _dsscsivVolumeARNs :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeStorediSCSIVolumes where
    type Item DescribeStorediSCSIVolumes = Text

    fromList = DescribeStorediSCSIVolumes . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dsscsivVolumeARNs

-- | 'DescribeStorediSCSIVolumes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsscsivVolumeARNs' @::@ ['Text']
--
describeStorediSCSIVolumes :: DescribeStorediSCSIVolumes
describeStorediSCSIVolumes = DescribeStorediSCSIVolumes
    { _dsscsivVolumeARNs = mempty
    }

-- | An array of strings where each string represents the Amazon Resource Name
-- (ARN) of a stored volume. All of the specified stored volumes must from
-- the same gateway. Use ListVolumes to get volume ARNs for a gateway.
dsscsivVolumeARNs :: Lens' DescribeStorediSCSIVolumes [Text]
dsscsivVolumeARNs =
    lens _dsscsivVolumeARNs (\s a -> s { _dsscsivVolumeARNs = a })

newtype DescribeStorediSCSIVolumesResponse = DescribeStorediSCSIVolumesResponse
    { _dsscsivrStorediSCSIVolumes :: [StorediSCSIVolume]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeStorediSCSIVolumesResponse where
    type Item DescribeStorediSCSIVolumesResponse = StorediSCSIVolume

    fromList = DescribeStorediSCSIVolumesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dsscsivrStorediSCSIVolumes

-- | 'DescribeStorediSCSIVolumesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsscsivrStorediSCSIVolumes' @::@ ['StorediSCSIVolume']
--
describeStorediSCSIVolumesResponse :: DescribeStorediSCSIVolumesResponse
describeStorediSCSIVolumesResponse = DescribeStorediSCSIVolumesResponse
    { _dsscsivrStorediSCSIVolumes = mempty
    }

dsscsivrStorediSCSIVolumes :: Lens' DescribeStorediSCSIVolumesResponse [StorediSCSIVolume]
dsscsivrStorediSCSIVolumes =
    lens _dsscsivrStorediSCSIVolumes
        (\s a -> s { _dsscsivrStorediSCSIVolumes = a })

instance AWSRequest DescribeStorediSCSIVolumes where
    type Sv DescribeStorediSCSIVolumes = StorageGateway
    type Rs DescribeStorediSCSIVolumes = DescribeStorediSCSIVolumesResponse

    request  = post
    response = jsonResponse

instance FromJSON DescribeStorediSCSIVolumesResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath DescribeStorediSCSIVolumes where
    toPath = const "/"

instance ToHeaders DescribeStorediSCSIVolumes

instance ToQuery DescribeStorediSCSIVolumes where
    toQuery = const mempty

instance ToJSON DescribeStorediSCSIVolumes where
    toJSON = genericToJSON jsonOptions
