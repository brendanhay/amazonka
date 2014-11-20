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
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ListStacks.html>
module Network.AWS.CloudFormation.ListStacks
    (
    -- * Request
      ListStacks
    -- ** Request constructor
    , listStacks
    -- ** Request lenses
    , lsNextToken
    , lsStackStatusFilter

    -- * Response
    , ListStacksResponse
    -- ** Response constructor
    , listStacksResponse
    -- ** Response lenses
    , lsr1NextToken
    , lsr1StackSummaries
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import qualified GHC.Exts

data ListStacks = ListStacks
    { _lsNextToken         :: Maybe Text
    , _lsStackStatusFilter :: List "StackStatusFilter" Text
    } deriving (Eq, Ord, Show)

-- | 'ListStacks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lsStackStatusFilter' @::@ ['Text']
--
listStacks :: ListStacks
listStacks = ListStacks
    { _lsNextToken         = Nothing
    , _lsStackStatusFilter = mempty
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one. Default: There is no default value.
lsNextToken :: Lens' ListStacks (Maybe Text)
lsNextToken = lens _lsNextToken (\s a -> s { _lsNextToken = a })

-- | Stack status to use as a filter. Specify one or more stack status codes
-- to list only stacks with the specified status codes. For a complete list
-- of stack status codes, see the StackStatus parameter of the Stack data
-- type.
lsStackStatusFilter :: Lens' ListStacks [Text]
lsStackStatusFilter =
    lens _lsStackStatusFilter (\s a -> s { _lsStackStatusFilter = a })
        . _List

data ListStacksResponse = ListStacksResponse
    { _lsr1NextToken      :: Maybe Text
    , _lsr1StackSummaries :: List "StackSummaries" StackSummary
    } deriving (Eq, Show)

-- | 'ListStacksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsr1NextToken' @::@ 'Maybe' 'Text'
--
-- * 'lsr1StackSummaries' @::@ ['StackSummary']
--
listStacksResponse :: ListStacksResponse
listStacksResponse = ListStacksResponse
    { _lsr1StackSummaries = mempty
    , _lsr1NextToken      = Nothing
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one.
lsr1NextToken :: Lens' ListStacksResponse (Maybe Text)
lsr1NextToken = lens _lsr1NextToken (\s a -> s { _lsr1NextToken = a })

-- | A list of StackSummary structures containing information about the
-- specified stacks.
lsr1StackSummaries :: Lens' ListStacksResponse [StackSummary]
lsr1StackSummaries =
    lens _lsr1StackSummaries (\s a -> s { _lsr1StackSummaries = a })
        . _List

instance ToPath ListStacks where
    toPath = const "/"

instance ToQuery ListStacks where
    toQuery ListStacks{..} = mconcat
        [ "NextToken"         =? _lsNextToken
        , "StackStatusFilter" =? _lsStackStatusFilter
        ]

instance ToHeaders ListStacks

query

instance AWSRequest ListStacks where
    type Sv ListStacks = CloudFormation
    type Rs ListStacks = ListStacksResponse

    request  = post "ListStacks"
    response = xmlResponse

instance FromXML ListStacksResponse where
    parseXML = withElement "ListStacksResult" $ \x -> ListStacksResponse
        <$> x .@? "NextToken"
        <*> x .@  "StackSummaries"

instance AWSPager ListStacks where
    page rq rs
        | stop (rq ^. lsNextToken) = Nothing
        | otherwise = (\x -> rq & lsNextToken ?~ x)
            <$> (rs ^. lsr1NextToken)
