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
    , lsrStackName
    , lsrNextToken

    -- * Response
    , ListStackResourcesResponse
    -- ** Response constructor
    , listStackResourcesResponse
    -- ** Response lenses
    , lsrrStackResourceSummaries
    , lsrrNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFormation.Types

-- | /See:/ 'listStackResources' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrStackName'
--
-- * 'lsrNextToken'
data ListStackResources = ListStackResources'{_lsrStackName :: Text, _lsrNextToken :: Text} deriving (Eq, Read, Show)

-- | 'ListStackResources' smart constructor.
listStackResources :: Text -> Text -> ListStackResources
listStackResources pStackName pNextToken = ListStackResources'{_lsrStackName = pStackName, _lsrNextToken = pNextToken};

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

-- | String that identifies the start of the next list of stack resource
-- summaries, if there is one.
--
-- Default: There is no default value.
lsrNextToken :: Lens' ListStackResources Text
lsrNextToken = lens _lsrNextToken (\ s a -> s{_lsrNextToken = a});

instance AWSRequest ListStackResources where
        type Sv ListStackResources = CloudFormation
        type Rs ListStackResources =
             ListStackResourcesResponse
        request = post
        response
          = receiveXMLWrapper "ListStackResourcesResult"
              (\ s h x ->
                 ListStackResourcesResponse' <$>
                   (x .@? "StackResourceSummaries" .!@ mempty >>=
                      parseXMLList "member")
                     <*> x .@ "NextToken")

instance ToHeaders ListStackResources where
        toHeaders = const mempty

instance ToPath ListStackResources where
        toPath = const "/"

instance ToQuery ListStackResources where
        toQuery ListStackResources'{..}
          = mconcat
              ["Action" =: ("ListStackResources" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackName" =: _lsrStackName,
               "NextToken" =: _lsrNextToken]

-- | /See:/ 'listStackResourcesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrrStackResourceSummaries'
--
-- * 'lsrrNextToken'
data ListStackResourcesResponse = ListStackResourcesResponse'{_lsrrStackResourceSummaries :: [StackResourceSummary], _lsrrNextToken :: Text} deriving (Eq, Read, Show)

-- | 'ListStackResourcesResponse' smart constructor.
listStackResourcesResponse :: Text -> ListStackResourcesResponse
listStackResourcesResponse pNextToken = ListStackResourcesResponse'{_lsrrStackResourceSummaries = mempty, _lsrrNextToken = pNextToken};

-- | A list of @StackResourceSummary@ structures.
lsrrStackResourceSummaries :: Lens' ListStackResourcesResponse [StackResourceSummary]
lsrrStackResourceSummaries = lens _lsrrStackResourceSummaries (\ s a -> s{_lsrrStackResourceSummaries = a});

-- | String that identifies the start of the next list of stack resources, if
-- there is one.
lsrrNextToken :: Lens' ListStackResourcesResponse Text
lsrrNextToken = lens _lsrrNextToken (\ s a -> s{_lsrrNextToken = a});
