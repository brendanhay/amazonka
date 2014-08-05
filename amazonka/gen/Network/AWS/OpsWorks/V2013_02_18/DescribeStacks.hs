{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeStacks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of one or more stacks. Required Permissions: To use
-- this action, an IAM user must have a Show, Deploy, or Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribeStacks where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeStacks' request.
describeStacks :: DescribeStacks
describeStacks = DescribeStacks
    { _dssStackIds = mempty
    }

data DescribeStacks = DescribeStacks
    { _dssStackIds :: [Text]
      -- ^ An array of stack IDs that specify the stacks to be described. If
      -- you omit this parameter, DescribeStacks returns a description of
      -- every stack.
    } deriving (Show, Generic)

makeLenses ''DescribeStacks

instance ToPath DescribeStacks

instance ToQuery DescribeStacks

instance ToHeaders DescribeStacks

instance ToJSON DescribeStacks

data DescribeStacksResponse = DescribeStacksResponse
    { _dstStacks :: [Stack]
      -- ^ An array of Stack objects that describe the stacks.
    } deriving (Show, Generic)

makeLenses ''DescribeStacksResponse

instance FromJSON DescribeStacksResponse

instance AWSRequest DescribeStacks where
    type Sv DescribeStacks = OpsWorks
    type Rs DescribeStacks = DescribeStacksResponse

    request = get
    response _ = undefined
