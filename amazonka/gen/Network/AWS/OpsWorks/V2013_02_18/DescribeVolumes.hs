{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.OpsWorks.V2013_02_18.DescribeVolumes where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeVolumes' request.
describeVolumes :: DescribeVolumes
describeVolumes = DescribeVolumes
    { _dvsInstanceId = Nothing
    , _dvsRaidArrayId = Nothing
    , _dvsStackId = Nothing
    , _dvsVolumeIds = mempty
    }

data DescribeVolumes = DescribeVolumes
    { _dvsInstanceId :: Maybe Text
      -- ^ The instance ID. If you use this parameter, DescribeVolumes
      -- returns descriptions of the volumes associated with the specified
      -- instance.
    , _dvsRaidArrayId :: Maybe Text
      -- ^ The RAID array ID. If you use this parameter, DescribeVolumes
      -- returns descriptions of the volumes associated with the specified
      -- RAID array.
    , _dvsStackId :: Maybe Text
      -- ^ A stack ID. The action describes the stack's registered Amazon
      -- EBS volumes.
    , _dvsVolumeIds :: [Text]
      -- ^ Am array of volume IDs. If you use this parameter,
      -- DescribeVolumes returns descriptions of the specified volumes.
      -- Otherwise, it returns a description of every volume.
    } deriving (Generic)

makeLenses ''DescribeVolumes

instance ToPath DescribeVolumes

instance ToQuery DescribeVolumes

instance ToHeaders DescribeVolumes

instance ToJSON DescribeVolumes

data DescribeVolumesResponse = DescribeVolumesResponse
    { _dvtVolumes :: [Volume]
      -- ^ An array of volume IDs.
    } deriving (Generic)

makeLenses ''DescribeVolumesResponse

instance FromJSON DescribeVolumesResponse

instance AWSRequest DescribeVolumes where
    type Sv DescribeVolumes = OpsWorks
    type Rs DescribeVolumes = DescribeVolumesResponse

    request = get
    response _ = jsonResponse
