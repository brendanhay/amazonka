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

-- Module      : Network.AWS.CloudFormation.DescribeStacks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the description for the specified stack; if no stack name was
-- specified, then it returns the description for all the stacks created.
module Network.AWS.CloudFormation.DescribeStacks
    (
    -- * Request
      DescribeStacks
    -- ** Request constructor
    , describeStacks
    -- ** Request lenses
    , ds1NextToken
    , ds1StackName

    -- * Response
    , DescribeStacksResponse
    -- ** Response constructor
    , describeStacksResponse
    -- ** Response lenses
    , dsrNextToken
    , dsrStacks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

data DescribeStacks = DescribeStacks
    { _ds1NextToken :: Maybe Text
    , _ds1StackName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeStacks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ds1NextToken' @::@ 'Maybe' 'Text'
--
-- * 'ds1StackName' @::@ 'Maybe' 'Text'
--
describeStacks :: DescribeStacks
describeStacks = DescribeStacks
    { _ds1StackName = Nothing
    , _ds1NextToken = Nothing
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one.
ds1NextToken :: Lens' DescribeStacks (Maybe Text)
ds1NextToken = lens _ds1NextToken (\s a -> s { _ds1NextToken = a })

-- | The name or the unique identifier associated with the stack, which are
-- not always interchangeable: Running stacks: You can specify either the
-- stack's name or its unique stack ID. Deleted stacks: You must specify the
-- unique stack ID. Default: There is no default value.
ds1StackName :: Lens' DescribeStacks (Maybe Text)
ds1StackName = lens _ds1StackName (\s a -> s { _ds1StackName = a })

instance ToQuery DescribeStacks

instance ToPath DescribeStacks where
    toPath = const "/"

data DescribeStacksResponse = DescribeStacksResponse
    { _dsrNextToken :: Maybe Text
    , _dsrStacks    :: [Stack]
    } deriving (Eq, Show, Generic)

-- | 'DescribeStacksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsrStacks' @::@ ['Stack']
--
describeStacksResponse :: DescribeStacksResponse
describeStacksResponse = DescribeStacksResponse
    { _dsrStacks    = mempty
    , _dsrNextToken = Nothing
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one.
dsrNextToken :: Lens' DescribeStacksResponse (Maybe Text)
dsrNextToken = lens _dsrNextToken (\s a -> s { _dsrNextToken = a })

-- | A list of stack structures.
dsrStacks :: Lens' DescribeStacksResponse [Stack]
dsrStacks = lens _dsrStacks (\s a -> s { _dsrStacks = a })

instance FromXML DescribeStacksResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeStacksResponse"

instance AWSRequest DescribeStacks where
    type Sv DescribeStacks = CloudFormation
    type Rs DescribeStacks = DescribeStacksResponse

    request  = post "DescribeStacks"
    response = xmlResponse $ \h x -> DescribeStacksResponse
        <$> x %| "NextToken"
        <*> x %| "Stacks"
