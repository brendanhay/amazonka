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
      DescribeStacksInput
    -- ** Request constructor
    , describeStacksInput
    -- ** Request lenses
    , dsiNextToken
    , dsiStackName

    -- * Response
    , DescribeStacksOutput
    -- ** Response constructor
    , describeStacksOutput
    -- ** Response lenses
    , dsoNextToken
    , dsoStacks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

data DescribeStacksInput = DescribeStacksInput
    { _dsiNextToken :: Maybe Text
    , _dsiStackName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeStacksInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsiNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsiStackName' @::@ 'Maybe' 'Text'
--
describeStacksInput :: DescribeStacksInput
describeStacksInput = DescribeStacksInput
    { _dsiStackName = Nothing
    , _dsiNextToken = Nothing
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one.
dsiNextToken :: Lens' DescribeStacksInput (Maybe Text)
dsiNextToken = lens _dsiNextToken (\s a -> s { _dsiNextToken = a })

-- | The name or the unique identifier associated with the stack, which are
-- not always interchangeable: Running stacks: You can specify either the
-- stack's name or its unique stack ID. Deleted stacks: You must specify the
-- unique stack ID. Default: There is no default value.
dsiStackName :: Lens' DescribeStacksInput (Maybe Text)
dsiStackName = lens _dsiStackName (\s a -> s { _dsiStackName = a })

instance ToPath DescribeStacksInput where
    toPath = const "/"

instance ToQuery DescribeStacksInput

data DescribeStacksOutput = DescribeStacksOutput
    { _dsoNextToken :: Maybe Text
    , _dsoStacks    :: [Stack]
    } deriving (Eq, Show, Generic)

-- | 'DescribeStacksOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsoNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsoStacks' @::@ ['Stack']
--
describeStacksOutput :: DescribeStacksOutput
describeStacksOutput = DescribeStacksOutput
    { _dsoStacks    = mempty
    , _dsoNextToken = Nothing
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one.
dsoNextToken :: Lens' DescribeStacksOutput (Maybe Text)
dsoNextToken = lens _dsoNextToken (\s a -> s { _dsoNextToken = a })

-- | A list of stack structures.
dsoStacks :: Lens' DescribeStacksOutput [Stack]
dsoStacks = lens _dsoStacks (\s a -> s { _dsoStacks = a })

instance AWSRequest DescribeStacksInput where
    type Sv DescribeStacksInput = CloudFormation
    type Rs DescribeStacksInput = DescribeStacksOutput

    request  = post "DescribeStacks"
    response = const . xmlResponse $ \h x -> DescribeStacksOutput
record
