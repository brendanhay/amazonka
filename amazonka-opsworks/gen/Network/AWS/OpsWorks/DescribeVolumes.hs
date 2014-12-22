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

-- Module      : Network.AWS.OpsWorks.DescribeVolumes
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

-- | Describes an instance's Amazon EBS volumes.
--
-- You must specify at least one of the parameters.
--
-- Required Permissions: To use this action, an IAM user must have a Show,
-- Deploy, or Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
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
    , dvRaidArrayId
    , dvStackId
    , dvVolumeIds

    -- * Response
    , DescribeVolumesResponse
    -- ** Response constructor
    , describeVolumesResponse
    -- ** Response lenses
    , dvrVolumes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data DescribeVolumes = DescribeVolumes
    { _dvInstanceId  :: Maybe Text
    , _dvRaidArrayId :: Maybe Text
    , _dvStackId     :: Maybe Text
    , _dvVolumeIds   :: List "VolumeIds" Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeVolumes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'dvRaidArrayId' @::@ 'Maybe' 'Text'
--
-- * 'dvStackId' @::@ 'Maybe' 'Text'
--
-- * 'dvVolumeIds' @::@ ['Text']
--
describeVolumes :: DescribeVolumes
describeVolumes = DescribeVolumes
    { _dvInstanceId  = Nothing
    , _dvStackId     = Nothing
    , _dvRaidArrayId = Nothing
    , _dvVolumeIds   = mempty
    }

-- | The instance ID. If you use this parameter, 'DescribeVolumes' returns
-- descriptions of the volumes associated with the specified instance.
dvInstanceId :: Lens' DescribeVolumes (Maybe Text)
dvInstanceId = lens _dvInstanceId (\s a -> s { _dvInstanceId = a })

-- | The RAID array ID. If you use this parameter, 'DescribeVolumes' returns
-- descriptions of the volumes associated with the specified RAID array.
dvRaidArrayId :: Lens' DescribeVolumes (Maybe Text)
dvRaidArrayId = lens _dvRaidArrayId (\s a -> s { _dvRaidArrayId = a })

-- | A stack ID. The action describes the stack's registered Amazon EBS volumes.
dvStackId :: Lens' DescribeVolumes (Maybe Text)
dvStackId = lens _dvStackId (\s a -> s { _dvStackId = a })

-- | Am array of volume IDs. If you use this parameter, 'DescribeVolumes' returns
-- descriptions of the specified volumes. Otherwise, it returns a description of
-- every volume.
dvVolumeIds :: Lens' DescribeVolumes [Text]
dvVolumeIds = lens _dvVolumeIds (\s a -> s { _dvVolumeIds = a }) . _List

newtype DescribeVolumesResponse = DescribeVolumesResponse
    { _dvrVolumes :: List "Volumes" Volume
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeVolumesResponse where
    type Item DescribeVolumesResponse = Volume

    fromList = DescribeVolumesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dvrVolumes

-- | 'DescribeVolumesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrVolumes' @::@ ['Volume']
--
describeVolumesResponse :: DescribeVolumesResponse
describeVolumesResponse = DescribeVolumesResponse
    { _dvrVolumes = mempty
    }

-- | An array of volume IDs.
dvrVolumes :: Lens' DescribeVolumesResponse [Volume]
dvrVolumes = lens _dvrVolumes (\s a -> s { _dvrVolumes = a }) . _List

instance ToPath DescribeVolumes where
    toPath = const "/"

instance ToQuery DescribeVolumes where
    toQuery = const mempty

instance ToHeaders DescribeVolumes

instance ToJSON DescribeVolumes where
    toJSON DescribeVolumes{..} = object
        [ "InstanceId"  .= _dvInstanceId
        , "StackId"     .= _dvStackId
        , "RaidArrayId" .= _dvRaidArrayId
        , "VolumeIds"   .= _dvVolumeIds
        ]

instance AWSRequest DescribeVolumes where
    type Sv DescribeVolumes = OpsWorks
    type Rs DescribeVolumes = DescribeVolumesResponse

    request  = post "DescribeVolumes"
    response = jsonResponse

instance FromJSON DescribeVolumesResponse where
    parseJSON = withObject "DescribeVolumesResponse" $ \o -> DescribeVolumesResponse
        <$> o .:? "Volumes" .!= mempty
