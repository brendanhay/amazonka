{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of a set of instances. You must specify at least one
-- of the parameters. Required Permissions: To use this action, an IAM user
-- must have a Show, Deploy, or Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribeInstances where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeInstances' request.
describeInstances :: DescribeInstances
describeInstances = DescribeInstances
    { _disStackId = Nothing
    , _disLayerId = Nothing
    , _disInstanceIds = mempty
    }

data DescribeInstances = DescribeInstances
    { _disStackId :: Maybe Text
      -- ^ A stack ID. If you use this parameter, DescribeInstances returns
      -- descriptions of the instances associated with the specified
      -- stack.
    , _disLayerId :: Maybe Text
      -- ^ A layer ID. If you use this parameter, DescribeInstances returns
      -- descriptions of the instances associated with the specified
      -- layer.
    , _disInstanceIds :: [Text]
      -- ^ An array of instance IDs to be described. If you use this
      -- parameter, DescribeInstances returns a description of the
      -- specified instances. Otherwise, it returns a description of every
      -- instance.
    } deriving (Show, Generic)

makeLenses ''DescribeInstances

instance ToPath DescribeInstances

instance ToQuery DescribeInstances

instance ToHeaders DescribeInstances

instance ToJSON DescribeInstances

data DescribeInstancesResponse = DescribeInstancesResponse
    { _ditInstances :: [Instance]
      -- ^ An array of Instance objects that describe the instances.
    } deriving (Show, Generic)

makeLenses ''DescribeInstancesResponse

instance FromJSON DescribeInstancesResponse

instance AWSRequest DescribeInstances where
    type Sv DescribeInstances = OpsWorks
    type Rs DescribeInstances = DescribeInstancesResponse

    request = get
    response _ = jsonResponse
