{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeVolumes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes an instance's Amazon EBS volumes. You must specify at least one
-- of the parameters. Required Permissions: To use this action, an IAM user
-- must have a Show, Deploy, or Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribeVolumes
    (
    -- * Request
      DescribeVolumes
    -- ** Request constructor
    , mkDescribeVolumes
    -- ** Request lenses
    , dv1InstanceId
    , dv1StackId
    , dv1RaidArrayId
    , dv1VolumeIds

    -- * Response
    , DescribeVolumesResponse
    -- ** Response lenses
    , dvrsVolumes
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data DescribeVolumes = DescribeVolumes
    { _dv1InstanceId :: Maybe Text
    , _dv1StackId :: Maybe Text
    , _dv1RaidArrayId :: Maybe Text
    , _dv1VolumeIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeVolumes' request.
mkDescribeVolumes :: DescribeVolumes
mkDescribeVolumes = DescribeVolumes
    { _dv1InstanceId = Nothing
    , _dv1StackId = Nothing
    , _dv1RaidArrayId = Nothing
    , _dv1VolumeIds = mempty
    }

-- | The instance ID. If you use this parameter, DescribeVolumes returns
-- descriptions of the volumes associated with the specified instance.
dv1InstanceId :: Lens' DescribeVolumes (Maybe Text)
dv1InstanceId = lens _dv1InstanceId (\s a -> s { _dv1InstanceId = a })

-- | A stack ID. The action describes the stack's registered Amazon EBS volumes.
dv1StackId :: Lens' DescribeVolumes (Maybe Text)
dv1StackId = lens _dv1StackId (\s a -> s { _dv1StackId = a })

-- | The RAID array ID. If you use this parameter, DescribeVolumes returns
-- descriptions of the volumes associated with the specified RAID array.
dv1RaidArrayId :: Lens' DescribeVolumes (Maybe Text)
dv1RaidArrayId = lens _dv1RaidArrayId (\s a -> s { _dv1RaidArrayId = a })

-- | Am array of volume IDs. If you use this parameter, DescribeVolumes returns
-- descriptions of the specified volumes. Otherwise, it returns a description
-- of every volume.
dv1VolumeIds :: Lens' DescribeVolumes [Text]
dv1VolumeIds = lens _dv1VolumeIds (\s a -> s { _dv1VolumeIds = a })

instance ToPath DescribeVolumes

instance ToQuery DescribeVolumes

instance ToHeaders DescribeVolumes

instance ToJSON DescribeVolumes

-- | Contains the response to a DescribeVolumes request.
newtype DescribeVolumesResponse = DescribeVolumesResponse
    { _dvrsVolumes :: [Volume]
    } deriving (Show, Generic)

-- | An array of volume IDs.
dvrsVolumes :: Lens' DescribeVolumesResponse [Volume]
dvrsVolumes = lens _dvrsVolumes (\s a -> s { _dvrsVolumes = a })

instance FromJSON DescribeVolumesResponse

instance AWSRequest DescribeVolumes where
    type Sv DescribeVolumes = OpsWorks
    type Rs DescribeVolumes = DescribeVolumesResponse

    request = get
    response _ = jsonResponse
