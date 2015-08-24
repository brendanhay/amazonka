{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStackResources
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions of all resources of the specified stack.
--
-- For deleted stacks, ListStackResources returns resource information for
-- up to 90 days after the stack has been deleted.
--
-- /See:/ <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ListStackResources.html AWS API Reference> for ListStackResources.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStackResources
    (
    -- * Creating a Request
      listStackResources
    , ListStackResources
    -- * Request Lenses
    , lsrNextToken
    , lsrStackName

    -- * Destructuring the Response
    , listStackResourcesResponse
    , ListStackResourcesResponse
    -- * Response Lenses
    , lsrrsNextToken
    , lsrrsStackResourceSummaries
    , lsrrsStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.CloudFormation.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the ListStackResource action.
--
-- /See:/ 'listStackResources' smart constructor.
data ListStackResources = ListStackResources'
    { _lsrNextToken :: !(Maybe Text)
    , _lsrStackName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListStackResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrNextToken'
--
-- * 'lsrStackName'
listStackResources
    :: Text -- ^ 'lsrStackName'
    -> ListStackResources
listStackResources pStackName_ =
    ListStackResources'
    { _lsrNextToken = Nothing
    , _lsrStackName = pStackName_
    }

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
          | stop (rs ^. lsrrsNextToken) = Nothing
          | stop (rs ^. lsrrsStackResourceSummaries) = Nothing
          | otherwise =
            Just $ rq & lsrNextToken .~ rs ^. lsrrsNextToken

instance AWSRequest ListStackResources where
        type Rs ListStackResources =
             ListStackResourcesResponse
        request = postQuery cloudFormation
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
data ListStackResourcesResponse = ListStackResourcesResponse'
    { _lsrrsNextToken              :: !(Maybe Text)
    , _lsrrsStackResourceSummaries :: !(Maybe [StackResourceSummary])
    , _lsrrsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListStackResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrrsNextToken'
--
-- * 'lsrrsStackResourceSummaries'
--
-- * 'lsrrsStatus'
listStackResourcesResponse
    :: Int -- ^ 'lsrrsStatus'
    -> ListStackResourcesResponse
listStackResourcesResponse pStatus_ =
    ListStackResourcesResponse'
    { _lsrrsNextToken = Nothing
    , _lsrrsStackResourceSummaries = Nothing
    , _lsrrsStatus = pStatus_
    }

-- | String that identifies the start of the next list of stack resources, if
-- there is one.
lsrrsNextToken :: Lens' ListStackResourcesResponse (Maybe Text)
lsrrsNextToken = lens _lsrrsNextToken (\ s a -> s{_lsrrsNextToken = a});

-- | A list of 'StackResourceSummary' structures.
lsrrsStackResourceSummaries :: Lens' ListStackResourcesResponse [StackResourceSummary]
lsrrsStackResourceSummaries = lens _lsrrsStackResourceSummaries (\ s a -> s{_lsrrsStackResourceSummaries = a}) . _Default . _Coerce;

-- | The response status code.
lsrrsStatus :: Lens' ListStackResourcesResponse Int
lsrrsStatus = lens _lsrrsStatus (\ s a -> s{_lsrrsStatus = a});
