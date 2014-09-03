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
    , describeVolumes
    -- ** Request lenses
    , dvsInstanceId
    , dvsStackId
    , dvsRaidArrayId
    , dvsVolumeIds

    -- * Response
    , DescribeVolumesResponse
    -- ** Response lenses
    , dvtVolumes
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeVolumes' request.
describeVolumes :: DescribeVolumes
describeVolumes = DescribeVolumes
    { _dvsInstanceId = Nothing
    , _dvsStackId = Nothing
    , _dvsRaidArrayId = Nothing
    , _dvsVolumeIds = mempty
    }

data DescribeVolumes = DescribeVolumes
    { _dvsInstanceId :: Maybe Text
      -- ^ The instance ID. If you use this parameter, DescribeVolumes
      -- returns descriptions of the volumes associated with the specified
      -- instance.
    , _dvsStackId :: Maybe Text
      -- ^ A stack ID. The action describes the stack's registered Amazon
      -- EBS volumes.
    , _dvsRaidArrayId :: Maybe Text
      -- ^ The RAID array ID. If you use this parameter, DescribeVolumes
      -- returns descriptions of the volumes associated with the specified
      -- RAID array.
    , _dvsVolumeIds :: [Text]
      -- ^ Am array of volume IDs. If you use this parameter,
      -- DescribeVolumes returns descriptions of the specified volumes.
      -- Otherwise, it returns a description of every volume.
    } deriving (Show, Generic)

-- | The instance ID. If you use this parameter, DescribeVolumes returns
-- descriptions of the volumes associated with the specified instance.
dvsInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeVolumes
    -> f DescribeVolumes
dvsInstanceId f x =
    (\y -> x { _dvsInstanceId = y })
       <$> f (_dvsInstanceId x)
{-# INLINE dvsInstanceId #-}

-- | A stack ID. The action describes the stack's registered Amazon EBS volumes.
dvsStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeVolumes
    -> f DescribeVolumes
dvsStackId f x =
    (\y -> x { _dvsStackId = y })
       <$> f (_dvsStackId x)
{-# INLINE dvsStackId #-}

-- | The RAID array ID. If you use this parameter, DescribeVolumes returns
-- descriptions of the volumes associated with the specified RAID array.
dvsRaidArrayId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeVolumes
    -> f DescribeVolumes
dvsRaidArrayId f x =
    (\y -> x { _dvsRaidArrayId = y })
       <$> f (_dvsRaidArrayId x)
{-# INLINE dvsRaidArrayId #-}

-- | Am array of volume IDs. If you use this parameter, DescribeVolumes returns
-- descriptions of the specified volumes. Otherwise, it returns a description
-- of every volume.
dvsVolumeIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeVolumes
    -> f DescribeVolumes
dvsVolumeIds f x =
    (\y -> x { _dvsVolumeIds = y })
       <$> f (_dvsVolumeIds x)
{-# INLINE dvsVolumeIds #-}

instance ToPath DescribeVolumes

instance ToQuery DescribeVolumes

instance ToHeaders DescribeVolumes

instance ToJSON DescribeVolumes

data DescribeVolumesResponse = DescribeVolumesResponse
    { _dvtVolumes :: [Volume]
      -- ^ An array of volume IDs.
    } deriving (Show, Generic)

-- | An array of volume IDs.
dvtVolumes
    :: Functor f
    => ([Volume]
    -> f ([Volume]))
    -> DescribeVolumesResponse
    -> f DescribeVolumesResponse
dvtVolumes f x =
    (\y -> x { _dvtVolumes = y })
       <$> f (_dvtVolumes x)
{-# INLINE dvtVolumes #-}

instance FromJSON DescribeVolumesResponse

instance AWSRequest DescribeVolumes where
    type Sv DescribeVolumes = OpsWorks
    type Rs DescribeVolumes = DescribeVolumesResponse

    request = get
    response _ = jsonResponse
