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
-- Module      : Network.AWS.WorkDocs.DescribeGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the groups specified by query.
--
--
module Network.AWS.WorkDocs.DescribeGroups
    (
    -- * Creating a Request
      describeGroups
    , DescribeGroups
    -- * Request Lenses
    , dgAuthenticationToken
    , dgMarker
    , dgLimit
    , dgOrganizationId
    , dgSearchQuery

    -- * Destructuring the Response
    , describeGroupsResponse
    , DescribeGroupsResponse
    -- * Response Lenses
    , dgrsGroups
    , dgrsMarker
    , dgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'describeGroups' smart constructor.
data DescribeGroups = DescribeGroups'
  { _dgAuthenticationToken :: !(Maybe (Sensitive Text))
  , _dgMarker              :: !(Maybe Text)
  , _dgLimit               :: !(Maybe Nat)
  , _dgOrganizationId      :: !(Maybe Text)
  , _dgSearchQuery         :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'dgMarker' - The marker for the next set of results. (You received this marker from a previous call.)
--
-- * 'dgLimit' - The maximum number of items to return with this call.
--
-- * 'dgOrganizationId' - The ID of the organization.
--
-- * 'dgSearchQuery' - A query to describe groups by group name.
describeGroups
    :: Text -- ^ 'dgSearchQuery'
    -> DescribeGroups
describeGroups pSearchQuery_ =
  DescribeGroups'
    { _dgAuthenticationToken = Nothing
    , _dgMarker = Nothing
    , _dgLimit = Nothing
    , _dgOrganizationId = Nothing
    , _dgSearchQuery = _Sensitive # pSearchQuery_
    }


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
dgAuthenticationToken :: Lens' DescribeGroups (Maybe Text)
dgAuthenticationToken = lens _dgAuthenticationToken (\ s a -> s{_dgAuthenticationToken = a}) . mapping _Sensitive

-- | The marker for the next set of results. (You received this marker from a previous call.)
dgMarker :: Lens' DescribeGroups (Maybe Text)
dgMarker = lens _dgMarker (\ s a -> s{_dgMarker = a})

-- | The maximum number of items to return with this call.
dgLimit :: Lens' DescribeGroups (Maybe Natural)
dgLimit = lens _dgLimit (\ s a -> s{_dgLimit = a}) . mapping _Nat

-- | The ID of the organization.
dgOrganizationId :: Lens' DescribeGroups (Maybe Text)
dgOrganizationId = lens _dgOrganizationId (\ s a -> s{_dgOrganizationId = a})

-- | A query to describe groups by group name.
dgSearchQuery :: Lens' DescribeGroups Text
dgSearchQuery = lens _dgSearchQuery (\ s a -> s{_dgSearchQuery = a}) . _Sensitive

instance AWSRequest DescribeGroups where
        type Rs DescribeGroups = DescribeGroupsResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeGroupsResponse' <$>
                   (x .?> "Groups" .!@ mempty) <*> (x .?> "Marker") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeGroups where

instance NFData DescribeGroups where

instance ToHeaders DescribeGroups where
        toHeaders DescribeGroups'{..}
          = mconcat
              ["Authentication" =# _dgAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DescribeGroups where
        toPath = const "/api/v1/groups"

instance ToQuery DescribeGroups where
        toQuery DescribeGroups'{..}
          = mconcat
              ["marker" =: _dgMarker, "limit" =: _dgLimit,
               "organizationId" =: _dgOrganizationId,
               "searchQuery" =: _dgSearchQuery]

-- | /See:/ 'describeGroupsResponse' smart constructor.
data DescribeGroupsResponse = DescribeGroupsResponse'
  { _dgrsGroups         :: !(Maybe [GroupMetadata])
  , _dgrsMarker         :: !(Maybe Text)
  , _dgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgrsGroups' - The list of groups.
--
-- * 'dgrsMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'dgrsResponseStatus' - -- | The response status code.
describeGroupsResponse
    :: Int -- ^ 'dgrsResponseStatus'
    -> DescribeGroupsResponse
describeGroupsResponse pResponseStatus_ =
  DescribeGroupsResponse'
    { _dgrsGroups = Nothing
    , _dgrsMarker = Nothing
    , _dgrsResponseStatus = pResponseStatus_
    }


-- | The list of groups.
dgrsGroups :: Lens' DescribeGroupsResponse [GroupMetadata]
dgrsGroups = lens _dgrsGroups (\ s a -> s{_dgrsGroups = a}) . _Default . _Coerce

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
dgrsMarker :: Lens' DescribeGroupsResponse (Maybe Text)
dgrsMarker = lens _dgrsMarker (\ s a -> s{_dgrsMarker = a})

-- | -- | The response status code.
dgrsResponseStatus :: Lens' DescribeGroupsResponse Int
dgrsResponseStatus = lens _dgrsResponseStatus (\ s a -> s{_dgrsResponseStatus = a})

instance NFData DescribeGroupsResponse where
