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

-- Module      : Network.AWS.CloudFormation.ListStacks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the summary information for stacks whose status matches the
-- specified StackStatusFilter. Summary information for stacks that have been
-- deleted is kept for 90 days after the stack is deleted. If no
-- StackStatusFilter is specified, summary information for all stacks is
-- returned (including existing stacks and stacks that have been deleted).
module Network.AWS.CloudFormation.ListStacks
    (
    -- * Request
      ListStacksInput
    -- ** Request constructor
    , listStacks
    -- ** Request lenses
    , lsiNextToken
    , lsiStackStatusFilter

    -- * Response
    , ListStacksOutput
    -- ** Response constructor
    , listStacksResponse
    -- ** Response lenses
    , lsoNextToken
    , lsoStackSummaries
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

data ListStacksInput = ListStacksInput
    { _lsiNextToken         :: Maybe Text
    , _lsiStackStatusFilter :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListStacksInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsiNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lsiStackStatusFilter' @::@ ['Text']
--
listStacks :: ListStacksInput
listStacks = ListStacksInput
    { _lsiNextToken         = Nothing
    , _lsiStackStatusFilter = mempty
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one. Default: There is no default value.
lsiNextToken :: Lens' ListStacksInput (Maybe Text)
lsiNextToken = lens _lsiNextToken (\s a -> s { _lsiNextToken = a })

-- | Stack status to use as a filter. Specify one or more stack status codes
-- to list only stacks with the specified status codes. For a complete list
-- of stack status codes, see the StackStatus parameter of the Stack data
-- type.
lsiStackStatusFilter :: Lens' ListStacksInput [Text]
lsiStackStatusFilter =
    lens _lsiStackStatusFilter (\s a -> s { _lsiStackStatusFilter = a })

instance ToQuery ListStacksInput

instance ToPath ListStacksInput where
    toPath = const "/"

data ListStacksOutput = ListStacksOutput
    { _lsoNextToken      :: Maybe Text
    , _lsoStackSummaries :: [StackSummary]
    } deriving (Eq, Show, Generic)

-- | 'ListStacksOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsoNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lsoStackSummaries' @::@ ['StackSummary']
--
listStacksResponse :: ListStacksOutput
listStacksResponse = ListStacksOutput
    { _lsoStackSummaries = mempty
    , _lsoNextToken      = Nothing
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one.
lsoNextToken :: Lens' ListStacksOutput (Maybe Text)
lsoNextToken = lens _lsoNextToken (\s a -> s { _lsoNextToken = a })

-- | A list of StackSummary structures containing information about the
-- specified stacks.
lsoStackSummaries :: Lens' ListStacksOutput [StackSummary]
lsoStackSummaries =
    lens _lsoStackSummaries (\s a -> s { _lsoStackSummaries = a })

instance FromXML ListStacksOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListStacksOutput"

instance AWSRequest ListStacksInput where
    type Sv ListStacksInput = CloudFormation
    type Rs ListStacksInput = ListStacksOutput

    request  = post "ListStacks"
    response = xmlResponse $ \h x -> ListStacksOutput
        <$> x %| "NextToken"
        <*> x %| "StackSummaries"
