{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStackResources
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions of all resources of the specified stack.
--
-- For deleted stacks, ListStackResources returns resource information for
-- up to 90 days after the stack has been deleted.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ListStackResources.html>
module Network.AWS.CloudFormation.ListStackResources
    (
    -- * Request
      ListStackResources
    -- ** Request constructor
    , listStackResources
    -- ** Request lenses
    , lsrrqNextToken
    , lsrrqStackName

    -- * Response
    , ListStackResourcesResponse
    -- ** Response constructor
    , listStackResourcesResponse
    -- ** Response lenses
    , lsrrsNextToken
    , lsrrsStackResourceSummaries
    , lsrrsStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the ListStackResource action.
--
-- /See:/ 'listStackResources' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrrqNextToken'
--
-- * 'lsrrqStackName'
data ListStackResources = ListStackResources'
    { _lsrrqNextToken :: !(Maybe Text)
    , _lsrrqStackName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListStackResources' smart constructor.
listStackResources :: Text -> ListStackResources
listStackResources pStackName =
    ListStackResources'
    { _lsrrqNextToken = Nothing
    , _lsrrqStackName = pStackName
    }

-- | String that identifies the start of the next list of stack resource
-- summaries, if there is one.
--
-- Default: There is no default value.
lsrrqNextToken :: Lens' ListStackResources (Maybe Text)
lsrrqNextToken = lens _lsrrqNextToken (\ s a -> s{_lsrrqNextToken = a});

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
lsrrqStackName :: Lens' ListStackResources Text
lsrrqStackName = lens _lsrrqStackName (\ s a -> s{_lsrrqStackName = a});

instance AWSPager ListStackResources where
        page rq rs
          | stop (rs ^. lsrrsNextToken) = Nothing
          | stop (rs ^. lsrrsStackResourceSummaries) = Nothing
          | otherwise =
            Just $ rq & lsrrqNextToken .~ rs ^. lsrrsNextToken

instance AWSRequest ListStackResources where
        type Sv ListStackResources = CloudFormation
        type Rs ListStackResources =
             ListStackResourcesResponse
        request = post
        response
          = receiveXMLWrapper "ListStackResourcesResult"
              (\ s h x ->
                 ListStackResourcesResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "StackResourceSummaries" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders ListStackResources where
        toHeaders = const mempty

instance ToPath ListStackResources where
        toPath = const "/"

instance ToQuery ListStackResources where
        toQuery ListStackResources'{..}
          = mconcat
              ["Action" =: ("ListStackResources" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "NextToken" =: _lsrrqNextToken,
               "StackName" =: _lsrrqStackName]

-- | The output for a ListStackResources action.
--
-- /See:/ 'listStackResourcesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrrsNextToken'
--
-- * 'lsrrsStackResourceSummaries'
--
-- * 'lsrrsStatus'
data ListStackResourcesResponse = ListStackResourcesResponse'
    { _lsrrsNextToken              :: !(Maybe Text)
    , _lsrrsStackResourceSummaries :: !(Maybe [StackResourceSummary])
    , _lsrrsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListStackResourcesResponse' smart constructor.
listStackResourcesResponse :: Int -> ListStackResourcesResponse
listStackResourcesResponse pStatus =
    ListStackResourcesResponse'
    { _lsrrsNextToken = Nothing
    , _lsrrsStackResourceSummaries = Nothing
    , _lsrrsStatus = pStatus
    }

-- | String that identifies the start of the next list of stack resources, if
-- there is one.
lsrrsNextToken :: Lens' ListStackResourcesResponse (Maybe Text)
lsrrsNextToken = lens _lsrrsNextToken (\ s a -> s{_lsrrsNextToken = a});

-- | A list of @StackResourceSummary@ structures.
lsrrsStackResourceSummaries :: Lens' ListStackResourcesResponse [StackResourceSummary]
lsrrsStackResourceSummaries = lens _lsrrsStackResourceSummaries (\ s a -> s{_lsrrsStackResourceSummaries = a}) . _Default;

-- | FIXME: Undocumented member.
lsrrsStatus :: Lens' ListStackResourcesResponse Int
lsrrsStatus = lens _lsrrsStatus (\ s a -> s{_lsrrsStatus = a});
