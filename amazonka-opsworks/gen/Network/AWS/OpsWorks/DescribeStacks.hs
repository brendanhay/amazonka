{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
    , dsStackIds

    -- * Response
    , DescribeStacksResponse
    -- ** Response constructor
    , describeStacksResponse
    -- ** Response lenses
    , dsrStacks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

newtype DescribeStacks = DescribeStacks
    { _dsStackIds :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeStacks where
    type Item DescribeStacks = Text

    fromList = DescribeStacks . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dsStackIds

-- | 'DescribeStacks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsStackIds' @::@ ['Text']
--
describeStacks :: DescribeStacks
describeStacks = DescribeStacks
    { _dsStackIds = mempty
    }

-- | An array of stack IDs that specify the stacks to be described. If you
-- omit this parameter, DescribeStacks returns a description of every stack.
dsStackIds :: Lens' DescribeStacks [Text]
dsStackIds = lens _dsStackIds (\s a -> s { _dsStackIds = a })

instance ToPath DescribeStacks where
    toPath = const "/"

instance ToQuery DescribeStacks where
    toQuery = const mempty

instance ToHeaders DescribeStacks

instance ToBody DescribeStacks where
    toBody = toBody . encode . _dsStackIds

newtype DescribeStacksResponse = DescribeStacksResponse
    { _dsrStacks :: [Stack]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeStacksResponse where
    type Item DescribeStacksResponse = Stack

    fromList = DescribeStacksResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dsrStacks

-- | 'DescribeStacksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrStacks' @::@ ['Stack']
--
describeStacksResponse :: DescribeStacksResponse
describeStacksResponse = DescribeStacksResponse
    { _dsrStacks = mempty
    }

-- | An array of Stack objects that describe the stacks.
dsrStacks :: Lens' DescribeStacksResponse [Stack]
dsrStacks = lens _dsrStacks (\s a -> s { _dsrStacks = a })

-- FromJSON

instance AWSRequest DescribeStacks where
    type Sv DescribeStacks = OpsWorks
    type Rs DescribeStacks = DescribeStacksResponse

    request  = post'
    response = jsonResponse $ \h o -> DescribeStacksResponse
        <$> o .: "Stacks"
