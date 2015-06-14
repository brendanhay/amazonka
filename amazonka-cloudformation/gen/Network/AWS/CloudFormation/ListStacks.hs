{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFormation.ListStacks
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the summary information for stacks whose status matches the
-- specified StackStatusFilter. Summary information for stacks that have
-- been deleted is kept for 90 days after the stack is deleted. If no
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
    , lsStackStatusFilter
    , lsNextToken

    -- * Response
    , ListStacksResponse
    -- ** Response constructor
    , listStacksResponse
    -- ** Response lenses
    , lisStackSummaries
    , lisNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFormation.Types

-- | /See:/ 'listStacks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsStackStatusFilter'
--
-- * 'lsNextToken'
data ListStacks = ListStacks'{_lsStackStatusFilter :: [StackStatus], _lsNextToken :: Text} deriving (Eq, Read, Show)

-- | 'ListStacks' smart constructor.
listStacks :: Text -> ListStacks
listStacks pNextToken = ListStacks'{_lsStackStatusFilter = mempty, _lsNextToken = pNextToken};

-- | Stack status to use as a filter. Specify one or more stack status codes
-- to list only stacks with the specified status codes. For a complete list
-- of stack status codes, see the @StackStatus@ parameter of the Stack data
-- type.
lsStackStatusFilter :: Lens' ListStacks [StackStatus]
lsStackStatusFilter = lens _lsStackStatusFilter (\ s a -> s{_lsStackStatusFilter = a});

-- | String that identifies the start of the next list of stacks, if there is
-- one.
--
-- Default: There is no default value.
lsNextToken :: Lens' ListStacks Text
lsNextToken = lens _lsNextToken (\ s a -> s{_lsNextToken = a});

instance AWSRequest ListStacks where
        type Sv ListStacks = CloudFormation
        type Rs ListStacks = ListStacksResponse
        request = post
        response
          = receiveXMLWrapper "ListStacksResult"
              (\ s h x ->
                 ListStacksResponse' <$>
                   (x .@? "StackSummaries" .!@ mempty >>=
                      parseXMLList "member")
                     <*> x .@ "NextToken")

instance ToHeaders ListStacks where
        toHeaders = const mempty

instance ToPath ListStacks where
        toPath = const "/"

instance ToQuery ListStacks where
        toQuery ListStacks'{..}
          = mconcat
              ["Action" =: ("ListStacks" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackStatusFilter" =:
                 "member" =: _lsStackStatusFilter,
               "NextToken" =: _lsNextToken]

-- | /See:/ 'listStacksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lisStackSummaries'
--
-- * 'lisNextToken'
data ListStacksResponse = ListStacksResponse'{_lisStackSummaries :: [StackSummary], _lisNextToken :: Text} deriving (Eq, Read, Show)

-- | 'ListStacksResponse' smart constructor.
listStacksResponse :: Text -> ListStacksResponse
listStacksResponse pNextToken = ListStacksResponse'{_lisStackSummaries = mempty, _lisNextToken = pNextToken};

-- | A list of @StackSummary@ structures containing information about the
-- specified stacks.
lisStackSummaries :: Lens' ListStacksResponse [StackSummary]
lisStackSummaries = lens _lisStackSummaries (\ s a -> s{_lisStackSummaries = a});

-- | String that identifies the start of the next list of stacks, if there is
-- one.
lisNextToken :: Lens' ListStacksResponse Text
lisNextToken = lens _lisNextToken (\ s a -> s{_lisNextToken = a});
