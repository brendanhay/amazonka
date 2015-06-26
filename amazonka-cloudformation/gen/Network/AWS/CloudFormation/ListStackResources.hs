{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFormation.ListStackResources
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

-- | Returns descriptions of all resources of the specified stack.
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
    , lsrNextToken
    , lsrStackName

    -- * Response
    , ListStackResourcesResponse
    -- ** Response constructor
    , listStackResourcesResponse
    -- ** Response lenses
    , lsrrNextToken
    , lsrrStackResourceSummaries
    , lsrrStatusCode
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the ListStackResource action.
--
-- /See:/ 'listStackResources' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrNextToken'
--
-- * 'lsrStackName'
data ListStackResources = ListStackResources'{_lsrNextToken :: Maybe Text, _lsrStackName :: Text} deriving (Eq, Read, Show)

-- | 'ListStackResources' smart constructor.
listStackResources :: Text -> ListStackResources
listStackResources pStackName = ListStackResources'{_lsrNextToken = Nothing, _lsrStackName = pStackName};

-- | String that identifies the start of the next list of stack resource
-- summaries, if there is one.
--
-- Default: There is no default value.
lsrNextToken :: Lens' ListStackResources (Maybe Text)
lsrNextToken = lens _lsrNextToken (\ s a -> s{_lsrNextToken = a});

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
lsrStackName :: Lens' ListStackResources Text
lsrStackName = lens _lsrStackName (\ s a -> s{_lsrStackName = a});

instance AWSPager ListStackResources where
        page rq rs
          | stop (rs ^. lsrrNextToken) = Nothing
          | stop (rs ^. lsrrStackResourceSummaries) = Nothing
          | otherwise =
            Just $ rq & lsrNextToken .~ rs ^. lsrrNextToken

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
               "NextToken" =: _lsrNextToken,
               "StackName" =: _lsrStackName]

-- | The output for a ListStackResources action.
--
-- /See:/ 'listStackResourcesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrrNextToken'
--
-- * 'lsrrStackResourceSummaries'
--
-- * 'lsrrStatusCode'
data ListStackResourcesResponse = ListStackResourcesResponse'{_lsrrNextToken :: Maybe Text, _lsrrStackResourceSummaries :: Maybe [StackResourceSummary], _lsrrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'ListStackResourcesResponse' smart constructor.
listStackResourcesResponse :: Int -> ListStackResourcesResponse
listStackResourcesResponse pStatusCode = ListStackResourcesResponse'{_lsrrNextToken = Nothing, _lsrrStackResourceSummaries = Nothing, _lsrrStatusCode = pStatusCode};

-- | String that identifies the start of the next list of stack resources, if
-- there is one.
lsrrNextToken :: Lens' ListStackResourcesResponse (Maybe Text)
lsrrNextToken = lens _lsrrNextToken (\ s a -> s{_lsrrNextToken = a});

-- | A list of @StackResourceSummary@ structures.
lsrrStackResourceSummaries :: Lens' ListStackResourcesResponse [StackResourceSummary]
lsrrStackResourceSummaries = lens _lsrrStackResourceSummaries (\ s a -> s{_lsrrStackResourceSummaries = a}) . _Default;

-- | FIXME: Undocumented member.
lsrrStatusCode :: Lens' ListStackResourcesResponse Int
lsrrStatusCode = lens _lsrrStatusCode (\ s a -> s{_lsrrStatusCode = a});
