{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.ListStacks
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
module Network.AWS.CloudFormation.V2010_05_15.ListStacks
    (
    -- * Request
      ListStacks
    -- ** Request constructor
    , mkListStacksInput
    -- ** Request lenses
    , lsiNextToken
    , lsiStackStatusFilter

    -- * Response
    , ListStacksResponse
    -- ** Response lenses
    , lsoStackSummaries
    , lsoNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListStacks' request.
mkListStacksInput :: ListStacks
mkListStacksInput = ListStacks
    { _lsiNextToken = Nothing
    , _lsiStackStatusFilter = mempty
    }
{-# INLINE mkListStacksInput #-}

data ListStacks = ListStacks
    { _lsiNextToken :: Maybe Text
      -- ^ String that identifies the start of the next list of stacks, if
      -- there is one. Default: There is no default value.
    , _lsiStackStatusFilter :: [StackStatus]
      -- ^ Stack status to use as a filter. Specify one or more stack status
      -- codes to list only stacks with the specified status codes. For a
      -- complete list of stack status codes, see the StackStatus
      -- parameter of the Stack data type.
    } deriving (Show, Generic)

-- | String that identifies the start of the next list of stacks, if there is
-- one. Default: There is no default value.
lsiNextToken :: Lens' ListStacks (Maybe Text)
lsiNextToken = lens _lsiNextToken (\s a -> s { _lsiNextToken = a })
{-# INLINE lsiNextToken #-}

-- | Stack status to use as a filter. Specify one or more stack status codes to
-- list only stacks with the specified status codes. For a complete list of
-- stack status codes, see the StackStatus parameter of the Stack data type.
lsiStackStatusFilter :: Lens' ListStacks ([StackStatus])
lsiStackStatusFilter = lens _lsiStackStatusFilter (\s a -> s { _lsiStackStatusFilter = a })
{-# INLINE lsiStackStatusFilter #-}

instance ToQuery ListStacks where
    toQuery = genericQuery def

data ListStacksResponse = ListStacksResponse
    { _lsoStackSummaries :: [StackSummary]
      -- ^ A list of StackSummary structures containing information about
      -- the specified stacks.
    , _lsoNextToken :: Maybe Text
      -- ^ String that identifies the start of the next list of stacks, if
      -- there is one.
    } deriving (Show, Generic)

-- | A list of StackSummary structures containing information about the
-- specified stacks.
lsoStackSummaries :: Lens' ListStacksResponse ([StackSummary])
lsoStackSummaries = lens _lsoStackSummaries (\s a -> s { _lsoStackSummaries = a })
{-# INLINE lsoStackSummaries #-}

-- | String that identifies the start of the next list of stacks, if there is
-- one.
lsoNextToken :: Lens' ListStacksResponse (Maybe Text)
lsoNextToken = lens _lsoNextToken (\s a -> s { _lsoNextToken = a })
{-# INLINE lsoNextToken #-}

instance FromXML ListStacksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListStacks where
    type Sv ListStacks = CloudFormation
    type Rs ListStacks = ListStacksResponse

    request = post "ListStacks"
    response _ = xmlResponse

instance AWSPager ListStacks where
    next rq rs = (\x -> rq { _lsiNextToken = Just x })
        <$> (_lsoNextToken rs)
