{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.CloudFormation.V2010_05_15.ListStacks where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListStacks' request.
listStacks :: ListStacks
listStacks = ListStacks
    { _lsiNextToken = Nothing
    , _lsiStackStatusFilter = mempty
    }

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

makeLenses ''ListStacks

instance ToQuery ListStacks where
    toQuery = genericToQuery def

data ListStacksResponse = ListStacksResponse
    { _lsoNextToken :: Maybe Text
      -- ^ String that identifies the start of the next list of stacks, if
      -- there is one.
    , _lsoStackSummaries :: [StackSummary]
      -- ^ A list of StackSummary structures containing information about
      -- the specified stacks.
    } deriving (Show, Generic)

makeLenses ''ListStacksResponse

instance AWSRequest ListStacks where
    type Sv ListStacks = CloudFormation
    type Rs ListStacks = ListStacksResponse

    request = post "ListStacks"
    response _ = cursorResponse $ \hs xml ->
        pure ListStacksResponse
            <*> xml %|? "NextToken"
            <*> xml %| "StackSummaries"

instance AWSPager ListStacks where
    next rq rs = (\x -> rq { _lsiNextToken = Just x })
        <$> (_lsoNextToken rs)
