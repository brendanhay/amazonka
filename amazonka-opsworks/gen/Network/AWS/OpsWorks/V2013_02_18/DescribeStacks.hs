{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.OpsWorks.V2013_02_18.DescribeStacks
    (
    -- * Request
      DescribeStacks
    -- ** Request constructor
    , mkDescribeStacks
    -- ** Request lenses
    , ds2StackIds

    -- * Response
    , DescribeStacksResponse
    -- ** Response lenses
    , dsrsStacks
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

newtype DescribeStacks = DescribeStacks
    { _ds2StackIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeStacks' request.
mkDescribeStacks :: DescribeStacks
mkDescribeStacks = DescribeStacks
    { _ds2StackIds = mempty
    }

-- | An array of stack IDs that specify the stacks to be described. If you omit
-- this parameter, DescribeStacks returns a description of every stack.
ds2StackIds :: Lens' DescribeStacks [Text]
ds2StackIds = lens _ds2StackIds (\s a -> s { _ds2StackIds = a })

instance ToPath DescribeStacks

instance ToQuery DescribeStacks

instance ToHeaders DescribeStacks

instance ToJSON DescribeStacks

-- | Contains the response to a DescribeStacks request.
newtype DescribeStacksResponse = DescribeStacksResponse
    { _dsrsStacks :: [Stack]
    } deriving (Show, Generic)

-- | An array of Stack objects that describe the stacks.
dsrsStacks :: Lens' DescribeStacksResponse [Stack]
dsrsStacks = lens _dsrsStacks (\s a -> s { _dsrsStacks = a })

instance FromJSON DescribeStacksResponse

instance AWSRequest DescribeStacks where
    type Sv DescribeStacks = OpsWorks
    type Rs DescribeStacks = DescribeStacksResponse

    request = get
    response _ = jsonResponse
