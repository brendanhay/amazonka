{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=ListStacks
-- &StackStatusFilter.member.1=CREATE_IN_PROGRESS
-- &StackStatusFilter.member.2=DELETE_COMPLETE &Version=2010-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature]
-- arn:aws:cloudformation:us-east-1:1234567:stack/TestCreate1/aaaaa
-- CREATE_IN_PROGRESS vpc1 2011-05-23T15:47:44Z Creates one EC2 instance and a
-- load balancer.
-- arn:aws:cloudformation:us-east-1:1234567:stack/TestDelete2/bbbbb
-- DELETE_COMPLETE 2011-03-10T16:20:51Z WP1 2011-03-05T19:57:58Z A simple
-- basic Cloudformation Template.
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
    , lsr1StackSummaries
    , lsr1NextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import Network.AWS.Prelude

-- | The input for ListStacks action.
data ListStacks = ListStacks
    { _lsNextToken :: Maybe Text
    , _lsStackStatusFilter :: [StackStatus]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListStacks' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NextToken ::@ @Maybe Text@
--
-- * @StackStatusFilter ::@ @[StackStatus]@
--
listStacks :: ListStacks
listStacks = ListStacks
    { _lsNextToken = Nothing
    , _lsStackStatusFilter = mempty
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one. Default: There is no default value.
lsNextToken :: Lens' ListStacks (Maybe Text)
lsNextToken = lens _lsNextToken (\s a -> s { _lsNextToken = a })

-- | Stack status to use as a filter. Specify one or more stack status codes to
-- list only stacks with the specified status codes. For a complete list of
-- stack status codes, see the StackStatus parameter of the Stack data type.
lsStackStatusFilter :: Lens' ListStacks [StackStatus]
lsStackStatusFilter =
    lens _lsStackStatusFilter (\s a -> s { _lsStackStatusFilter = a })

instance ToQuery ListStacks where
    toQuery = genericQuery def

-- | The output for ListStacks action.
data ListStacksResponse = ListStacksResponse
    { _lsr1StackSummaries :: [StackSummary]
    , _lsr1NextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListStacksResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackSummaries ::@ @[StackSummary]@
--
-- * @NextToken ::@ @Maybe Text@
--
listStacksResponse :: ListStacksResponse
listStacksResponse = ListStacksResponse
    { _lsr1StackSummaries = mempty
    , _lsr1NextToken = Nothing
    }

-- | A list of StackSummary structures containing information about the
-- specified stacks.
lsr1StackSummaries :: Lens' ListStacksResponse [StackSummary]
lsr1StackSummaries =
    lens _lsr1StackSummaries (\s a -> s { _lsr1StackSummaries = a })

-- | String that identifies the start of the next list of stacks, if there is
-- one.
lsr1NextToken :: Lens' ListStacksResponse (Maybe Text)
lsr1NextToken = lens _lsr1NextToken (\s a -> s { _lsr1NextToken = a })

instance FromXML ListStacksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListStacks where
    type Sv ListStacks = CloudFormation
    type Rs ListStacks = ListStacksResponse

    request = post "ListStacks"
    response _ = xmlResponse

instance AWSPager ListStacks where
    next rq rs = (\x -> rq & lsNextToken ?~ x)
        <$> (rs ^. lsr1NextToken)
