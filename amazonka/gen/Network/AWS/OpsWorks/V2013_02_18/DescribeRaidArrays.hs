{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeRaidArrays
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describe an instance's RAID arrays. You must specify at least one of the
-- parameters. Required Permissions: To use this action, an IAM user must have
-- a Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribeRaidArrays where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeRaidArrays' request.
describeRaidArrays :: DescribeRaidArrays
describeRaidArrays = DescribeRaidArrays
    { _drarInstanceId = Nothing
    , _drarRaidArrayIds = mempty
    }

data DescribeRaidArrays = DescribeRaidArrays
    { _drarInstanceId :: Maybe Text
      -- ^ The instance ID. If you use this parameter, DescribeRaidArrays
      -- returns descriptions of the RAID arrays associated with the
      -- specified instance.
    , _drarRaidArrayIds :: [Text]
      -- ^ An array of RAID array IDs. If you use this parameter,
      -- DescribeRaidArrays returns descriptions of the specified arrays.
      -- Otherwise, it returns a description of every array.
    } deriving (Show, Generic)

makeLenses ''DescribeRaidArrays

instance ToPath DescribeRaidArrays

instance ToQuery DescribeRaidArrays

instance ToHeaders DescribeRaidArrays

instance ToJSON DescribeRaidArrays

data DescribeRaidArraysResponse = DescribeRaidArraysResponse
    { _drasRaidArrays :: [RaidArray]
      -- ^ A RaidArrays object that describes the specified RAID arrays.
    } deriving (Show, Generic)

makeLenses ''DescribeRaidArraysResponse

instance FromJSON DescribeRaidArraysResponse

instance AWSRequest DescribeRaidArrays where
    type Sv DescribeRaidArrays = OpsWorks
    type Rs DescribeRaidArrays = DescribeRaidArraysResponse

    request = get
    response _ = undefined
