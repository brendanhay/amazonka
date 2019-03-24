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
-- Module      : Network.AWS.Connect.ListUserHierarchyGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @UserHierarchyGroupSummaryList@ , which is an array of @HierarchyGroupSummary@ objects that contain information about the hierarchy groups in your instance.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListUserHierarchyGroups
    (
    -- * Creating a Request
      listUserHierarchyGroups
    , ListUserHierarchyGroups
    -- * Request Lenses
    , luhgNextToken
    , luhgMaxResults
    , luhgInstanceId

    -- * Destructuring the Response
    , listUserHierarchyGroupsResponse
    , ListUserHierarchyGroupsResponse
    -- * Response Lenses
    , luhgrsNextToken
    , luhgrsUserHierarchyGroupSummaryList
    , luhgrsResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listUserHierarchyGroups' smart constructor.
data ListUserHierarchyGroups = ListUserHierarchyGroups'
  { _luhgNextToken  :: !(Maybe Text)
  , _luhgMaxResults :: !(Maybe Nat)
  , _luhgInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUserHierarchyGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luhgNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'luhgMaxResults' - The maximum number of hierarchy groups to return.
--
-- * 'luhgInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
listUserHierarchyGroups
    :: Text -- ^ 'luhgInstanceId'
    -> ListUserHierarchyGroups
listUserHierarchyGroups pInstanceId_ =
  ListUserHierarchyGroups'
    { _luhgNextToken = Nothing
    , _luhgMaxResults = Nothing
    , _luhgInstanceId = pInstanceId_
    }


-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
luhgNextToken :: Lens' ListUserHierarchyGroups (Maybe Text)
luhgNextToken = lens _luhgNextToken (\ s a -> s{_luhgNextToken = a})

-- | The maximum number of hierarchy groups to return.
luhgMaxResults :: Lens' ListUserHierarchyGroups (Maybe Natural)
luhgMaxResults = lens _luhgMaxResults (\ s a -> s{_luhgMaxResults = a}) . mapping _Nat

-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
luhgInstanceId :: Lens' ListUserHierarchyGroups Text
luhgInstanceId = lens _luhgInstanceId (\ s a -> s{_luhgInstanceId = a})

instance AWSPager ListUserHierarchyGroups where
        page rq rs
          | stop (rs ^. luhgrsNextToken) = Nothing
          | stop (rs ^. luhgrsUserHierarchyGroupSummaryList) =
            Nothing
          | otherwise =
            Just $ rq & luhgNextToken .~ rs ^. luhgrsNextToken

instance AWSRequest ListUserHierarchyGroups where
        type Rs ListUserHierarchyGroups =
             ListUserHierarchyGroupsResponse
        request = get connect
        response
          = receiveJSON
              (\ s h x ->
                 ListUserHierarchyGroupsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "UserHierarchyGroupSummaryList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListUserHierarchyGroups where

instance NFData ListUserHierarchyGroups where

instance ToHeaders ListUserHierarchyGroups where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListUserHierarchyGroups where
        toPath ListUserHierarchyGroups'{..}
          = mconcat
              ["/user-hierarchy-groups-summary/",
               toBS _luhgInstanceId]

instance ToQuery ListUserHierarchyGroups where
        toQuery ListUserHierarchyGroups'{..}
          = mconcat
              ["nextToken" =: _luhgNextToken,
               "maxResults" =: _luhgMaxResults]

-- | /See:/ 'listUserHierarchyGroupsResponse' smart constructor.
data ListUserHierarchyGroupsResponse = ListUserHierarchyGroupsResponse'
  { _luhgrsNextToken                     :: !(Maybe Text)
  , _luhgrsUserHierarchyGroupSummaryList :: !(Maybe [HierarchyGroupSummary])
  , _luhgrsResponseStatus                :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUserHierarchyGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luhgrsNextToken' - A string returned in the response. Use the value returned in the response as the value of the NextToken in a subsequent request to retrieve the next set of results.
--
-- * 'luhgrsUserHierarchyGroupSummaryList' - An array of @HierarchyGroupSummary@ objects.
--
-- * 'luhgrsResponseStatus' - -- | The response status code.
listUserHierarchyGroupsResponse
    :: Int -- ^ 'luhgrsResponseStatus'
    -> ListUserHierarchyGroupsResponse
listUserHierarchyGroupsResponse pResponseStatus_ =
  ListUserHierarchyGroupsResponse'
    { _luhgrsNextToken = Nothing
    , _luhgrsUserHierarchyGroupSummaryList = Nothing
    , _luhgrsResponseStatus = pResponseStatus_
    }


-- | A string returned in the response. Use the value returned in the response as the value of the NextToken in a subsequent request to retrieve the next set of results.
luhgrsNextToken :: Lens' ListUserHierarchyGroupsResponse (Maybe Text)
luhgrsNextToken = lens _luhgrsNextToken (\ s a -> s{_luhgrsNextToken = a})

-- | An array of @HierarchyGroupSummary@ objects.
luhgrsUserHierarchyGroupSummaryList :: Lens' ListUserHierarchyGroupsResponse [HierarchyGroupSummary]
luhgrsUserHierarchyGroupSummaryList = lens _luhgrsUserHierarchyGroupSummaryList (\ s a -> s{_luhgrsUserHierarchyGroupSummaryList = a}) . _Default . _Coerce

-- | -- | The response status code.
luhgrsResponseStatus :: Lens' ListUserHierarchyGroupsResponse Int
luhgrsResponseStatus = lens _luhgrsResponseStatus (\ s a -> s{_luhgrsResponseStatus = a})

instance NFData ListUserHierarchyGroupsResponse where
