{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeStacks
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
module Network.AWS.OpsWorks.DescribeStacks
    (
    -- * Request
      DescribeStacks
    -- ** Request constructor
    , describeStacks
    -- ** Request lenses
    , ds2StackIds

    -- * Response
    , DescribeStacksResponse
    -- ** Response constructor
    , describeStacksResponse
    -- ** Response lenses
    , dsrStacks
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DescribeStacks = DescribeStacks
    { _ds2StackIds :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeStacks' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackIds ::@ @[Text]@
--
describeStacks :: DescribeStacks
describeStacks = DescribeStacks
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
    { _dsrStacks :: [Stack]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeStacksResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Stacks ::@ @[Stack]@
--
describeStacksResponse :: DescribeStacksResponse
describeStacksResponse = DescribeStacksResponse
    { _dsrStacks = mempty
    }

-- | An array of Stack objects that describe the stacks.
dsrStacks :: Lens' DescribeStacksResponse [Stack]
dsrStacks = lens _dsrStacks (\s a -> s { _dsrStacks = a })

instance FromJSON DescribeStacksResponse

instance AWSRequest DescribeStacks where
    type Sv DescribeStacks = OpsWorks
    type Rs DescribeStacks = DescribeStacksResponse

    request = get
    response _ = jsonResponse
