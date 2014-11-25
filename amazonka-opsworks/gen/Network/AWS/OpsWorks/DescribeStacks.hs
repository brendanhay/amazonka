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

-- Module      : Network.AWS.OpsWorks.DescribeStacks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of one or more stacks.
--
-- Required Permissions: To use this action, an IAM user must have a Show,
-- Deploy, or Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeStacks.html>
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
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DescribeStacks = DescribeStacks
    { _dsStackIds :: List "InstanceIds" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

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

-- | An array of stack IDs that specify the stacks to be described. If you omit
-- this parameter, 'DescribeStacks' returns a description of every stack.
--
dsStackIds :: Lens' DescribeStacks [Text]
dsStackIds = lens _dsStackIds (\s a -> s { _dsStackIds = a }) . _List

newtype DescribeStacksResponse = DescribeStacksResponse
    { _dsrStacks :: List "Stacks" Stack
    } deriving (Eq, Show, Monoid, Semigroup)

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

-- | An array of 'Stack' objects that describe the stacks.
--
dsrStacks :: Lens' DescribeStacksResponse [Stack]
dsrStacks = lens _dsrStacks (\s a -> s { _dsrStacks = a }) . _List

instance ToPath DescribeStacks where
    toPath = const "/"

instance ToQuery DescribeStacks where
    toQuery = const mempty

instance ToHeaders DescribeStacks

instance ToJSON DescribeStacks where
    toJSON DescribeStacks{..} = object
        [ "StackIds" .= _dsStackIds
        ]

instance AWSRequest DescribeStacks where
    type Sv DescribeStacks = OpsWorks
    type Rs DescribeStacks = DescribeStacksResponse

    request  = post "DescribeStacks"
    response = jsonResponse

instance FromJSON DescribeStacksResponse where
    parseJSON = withObject "DescribeStacksResponse" $ \o -> DescribeStacksResponse
        <$> o .:  "Stacks"
