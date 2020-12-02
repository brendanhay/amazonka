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
-- Module      : Network.AWS.WorkMail.ListGroupMembers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an overview of the members of a group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListGroupMembers
    (
    -- * Creating a Request
      listGroupMembers
    , ListGroupMembers
    -- * Request Lenses
    , lgmNextToken
    , lgmMaxResults
    , lgmOrganizationId
    , lgmGroupId

    -- * Destructuring the Response
    , listGroupMembersResponse
    , ListGroupMembersResponse
    -- * Response Lenses
    , lgmrsMembers
    , lgmrsNextToken
    , lgmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'listGroupMembers' smart constructor.
data ListGroupMembers = ListGroupMembers'
  { _lgmNextToken      :: !(Maybe Text)
  , _lgmMaxResults     :: !(Maybe Nat)
  , _lgmOrganizationId :: !Text
  , _lgmGroupId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroupMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgmNextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- * 'lgmMaxResults' - The maximum number of results to return in a single call.
--
-- * 'lgmOrganizationId' - The identifier for the organization under which the group exists.
--
-- * 'lgmGroupId' - The identifier for the group to which the members are associated.
listGroupMembers
    :: Text -- ^ 'lgmOrganizationId'
    -> Text -- ^ 'lgmGroupId'
    -> ListGroupMembers
listGroupMembers pOrganizationId_ pGroupId_ =
  ListGroupMembers'
    { _lgmNextToken = Nothing
    , _lgmMaxResults = Nothing
    , _lgmOrganizationId = pOrganizationId_
    , _lgmGroupId = pGroupId_
    }


-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
lgmNextToken :: Lens' ListGroupMembers (Maybe Text)
lgmNextToken = lens _lgmNextToken (\ s a -> s{_lgmNextToken = a})

-- | The maximum number of results to return in a single call.
lgmMaxResults :: Lens' ListGroupMembers (Maybe Natural)
lgmMaxResults = lens _lgmMaxResults (\ s a -> s{_lgmMaxResults = a}) . mapping _Nat

-- | The identifier for the organization under which the group exists.
lgmOrganizationId :: Lens' ListGroupMembers Text
lgmOrganizationId = lens _lgmOrganizationId (\ s a -> s{_lgmOrganizationId = a})

-- | The identifier for the group to which the members are associated.
lgmGroupId :: Lens' ListGroupMembers Text
lgmGroupId = lens _lgmGroupId (\ s a -> s{_lgmGroupId = a})

instance AWSPager ListGroupMembers where
        page rq rs
          | stop (rs ^. lgmrsNextToken) = Nothing
          | stop (rs ^. lgmrsMembers) = Nothing
          | otherwise =
            Just $ rq & lgmNextToken .~ rs ^. lgmrsNextToken

instance AWSRequest ListGroupMembers where
        type Rs ListGroupMembers = ListGroupMembersResponse
        request = postJSON workMail
        response
          = receiveJSON
              (\ s h x ->
                 ListGroupMembersResponse' <$>
                   (x .?> "Members" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListGroupMembers where

instance NFData ListGroupMembers where

instance ToHeaders ListGroupMembers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.ListGroupMembers" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListGroupMembers where
        toJSON ListGroupMembers'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lgmNextToken,
                  ("MaxResults" .=) <$> _lgmMaxResults,
                  Just ("OrganizationId" .= _lgmOrganizationId),
                  Just ("GroupId" .= _lgmGroupId)])

instance ToPath ListGroupMembers where
        toPath = const "/"

instance ToQuery ListGroupMembers where
        toQuery = const mempty

-- | /See:/ 'listGroupMembersResponse' smart constructor.
data ListGroupMembersResponse = ListGroupMembersResponse'
  { _lgmrsMembers        :: !(Maybe [Member])
  , _lgmrsNextToken      :: !(Maybe Text)
  , _lgmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroupMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgmrsMembers' - The members associated to the group.
--
-- * 'lgmrsNextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- * 'lgmrsResponseStatus' - -- | The response status code.
listGroupMembersResponse
    :: Int -- ^ 'lgmrsResponseStatus'
    -> ListGroupMembersResponse
listGroupMembersResponse pResponseStatus_ =
  ListGroupMembersResponse'
    { _lgmrsMembers = Nothing
    , _lgmrsNextToken = Nothing
    , _lgmrsResponseStatus = pResponseStatus_
    }


-- | The members associated to the group.
lgmrsMembers :: Lens' ListGroupMembersResponse [Member]
lgmrsMembers = lens _lgmrsMembers (\ s a -> s{_lgmrsMembers = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
lgmrsNextToken :: Lens' ListGroupMembersResponse (Maybe Text)
lgmrsNextToken = lens _lgmrsNextToken (\ s a -> s{_lgmrsNextToken = a})

-- | -- | The response status code.
lgmrsResponseStatus :: Lens' ListGroupMembersResponse Int
lgmrsResponseStatus = lens _lgmrsResponseStatus (\ s a -> s{_lgmrsResponseStatus = a})

instance NFData ListGroupMembersResponse where
