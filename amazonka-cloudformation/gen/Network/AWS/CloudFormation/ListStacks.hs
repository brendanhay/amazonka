{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStacks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the summary information for stacks whose status matches the
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
    , lsNextToken
    , lsStackStatusFilter

    -- * Response
    , ListStacksResponse
    -- ** Response constructor
    , listStacksResponse
    -- ** Response lenses
    , lsrsStackSummaries
    , lsrsNextToken
    , lsrsStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for ListStacks action.
--
-- /See:/ 'listStacks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsNextToken'
--
-- * 'lsStackStatusFilter'
data ListStacks = ListStacks'
    { _lsNextToken         :: !(Maybe Text)
    , _lsStackStatusFilter :: !(Maybe [StackStatus])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListStacks' smart constructor.
listStacks :: ListStacks
listStacks =
    ListStacks'
    { _lsNextToken = Nothing
    , _lsStackStatusFilter = Nothing
    }

-- | String that identifies the start of the next list of stacks, if there is
-- one.
--
-- Default: There is no default value.
lsNextToken :: Lens' ListStacks (Maybe Text)
lsNextToken = lens _lsNextToken (\ s a -> s{_lsNextToken = a});

-- | Stack status to use as a filter. Specify one or more stack status codes
-- to list only stacks with the specified status codes. For a complete list
-- of stack status codes, see the @StackStatus@ parameter of the Stack data
-- type.
lsStackStatusFilter :: Lens' ListStacks [StackStatus]
lsStackStatusFilter = lens _lsStackStatusFilter (\ s a -> s{_lsStackStatusFilter = a}) . _Default . _Coerce;

instance AWSPager ListStacks where
        page rq rs
          | stop (rs ^. lsrsNextToken) = Nothing
          | stop (rs ^. lsrsStackSummaries) = Nothing
          | otherwise =
            Just $ rq & lsNextToken .~ rs ^. lsrsNextToken

instance AWSRequest ListStacks where
        type Sv ListStacks = CloudFormation
        type Rs ListStacks = ListStacksResponse
        request = postQuery
        response
          = receiveXMLWrapper "ListStacksResult"
              (\ s h x ->
                 ListStacksResponse' <$>
                   (x .@? "StackSummaries" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListStacks where
        toHeaders = const mempty

instance ToPath ListStacks where
        toPath = const mempty

instance ToQuery ListStacks where
        toQuery ListStacks'{..}
          = mconcat
              ["Action" =: ("ListStacks" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "NextToken" =: _lsNextToken,
               "StackStatusFilter" =:
                 toQuery
                   (toQueryList "member" <$> _lsStackStatusFilter)]

-- | The output for ListStacks action.
--
-- /See:/ 'listStacksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrsStackSummaries'
--
-- * 'lsrsNextToken'
--
-- * 'lsrsStatus'
data ListStacksResponse = ListStacksResponse'
    { _lsrsStackSummaries :: !(Maybe [StackSummary])
    , _lsrsNextToken      :: !(Maybe Text)
    , _lsrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListStacksResponse' smart constructor.
listStacksResponse :: Int -> ListStacksResponse
listStacksResponse pStatus_ =
    ListStacksResponse'
    { _lsrsStackSummaries = Nothing
    , _lsrsNextToken = Nothing
    , _lsrsStatus = pStatus_
    }

-- | A list of @StackSummary@ structures containing information about the
-- specified stacks.
lsrsStackSummaries :: Lens' ListStacksResponse [StackSummary]
lsrsStackSummaries = lens _lsrsStackSummaries (\ s a -> s{_lsrsStackSummaries = a}) . _Default . _Coerce;

-- | String that identifies the start of the next list of stacks, if there is
-- one.
lsrsNextToken :: Lens' ListStacksResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\ s a -> s{_lsrsNextToken = a});

-- | FIXME: Undocumented member.
lsrsStatus :: Lens' ListStacksResponse Int
lsrsStatus = lens _lsrsStatus (\ s a -> s{_lsrsStatus = a});
