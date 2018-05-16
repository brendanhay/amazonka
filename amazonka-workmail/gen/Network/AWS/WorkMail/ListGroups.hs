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
-- Module      : Network.AWS.WorkMail.ListGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the organization's groups.
--
--
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListGroups
    (
    -- * Creating a Request
      listGroups
    , ListGroups
    -- * Request Lenses
    , lgNextToken
    , lgMaxResults
    , lgOrganizationId

    -- * Destructuring the Response
    , listGroupsResponse
    , ListGroupsResponse
    -- * Response Lenses
    , lgrsGroups
    , lgrsNextToken
    , lgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'listGroups' smart constructor.
data ListGroups = ListGroups'
  { _lgNextToken      :: !(Maybe Text)
  , _lgMaxResults     :: !(Maybe Nat)
  , _lgOrganizationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgNextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- * 'lgMaxResults' - The maximum number of results to return in a single call.
--
-- * 'lgOrganizationId' - The identifier for the organization under which the groups exist.
listGroups
    :: Text -- ^ 'lgOrganizationId'
    -> ListGroups
listGroups pOrganizationId_ =
  ListGroups'
    { _lgNextToken = Nothing
    , _lgMaxResults = Nothing
    , _lgOrganizationId = pOrganizationId_
    }


-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
lgNextToken :: Lens' ListGroups (Maybe Text)
lgNextToken = lens _lgNextToken (\ s a -> s{_lgNextToken = a})

-- | The maximum number of results to return in a single call.
lgMaxResults :: Lens' ListGroups (Maybe Natural)
lgMaxResults = lens _lgMaxResults (\ s a -> s{_lgMaxResults = a}) . mapping _Nat

-- | The identifier for the organization under which the groups exist.
lgOrganizationId :: Lens' ListGroups Text
lgOrganizationId = lens _lgOrganizationId (\ s a -> s{_lgOrganizationId = a})

instance AWSPager ListGroups where
        page rq rs
          | stop (rs ^. lgrsNextToken) = Nothing
          | stop (rs ^. lgrsGroups) = Nothing
          | otherwise =
            Just $ rq & lgNextToken .~ rs ^. lgrsNextToken

instance AWSRequest ListGroups where
        type Rs ListGroups = ListGroupsResponse
        request = postJSON workMail
        response
          = receiveJSON
              (\ s h x ->
                 ListGroupsResponse' <$>
                   (x .?> "Groups" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListGroups where

instance NFData ListGroups where

instance ToHeaders ListGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.ListGroups" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListGroups where
        toJSON ListGroups'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lgNextToken,
                  ("MaxResults" .=) <$> _lgMaxResults,
                  Just ("OrganizationId" .= _lgOrganizationId)])

instance ToPath ListGroups where
        toPath = const "/"

instance ToQuery ListGroups where
        toQuery = const mempty

-- | /See:/ 'listGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { _lgrsGroups         :: !(Maybe [Group])
  , _lgrsNextToken      :: !(Maybe Text)
  , _lgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrsGroups' - The overview of groups for an organization.
--
-- * 'lgrsNextToken' - The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
--
-- * 'lgrsResponseStatus' - -- | The response status code.
listGroupsResponse
    :: Int -- ^ 'lgrsResponseStatus'
    -> ListGroupsResponse
listGroupsResponse pResponseStatus_ =
  ListGroupsResponse'
    { _lgrsGroups = Nothing
    , _lgrsNextToken = Nothing
    , _lgrsResponseStatus = pResponseStatus_
    }


-- | The overview of groups for an organization.
lgrsGroups :: Lens' ListGroupsResponse [Group]
lgrsGroups = lens _lgrsGroups (\ s a -> s{_lgrsGroups = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
lgrsNextToken :: Lens' ListGroupsResponse (Maybe Text)
lgrsNextToken = lens _lgrsNextToken (\ s a -> s{_lgrsNextToken = a})

-- | -- | The response status code.
lgrsResponseStatus :: Lens' ListGroupsResponse Int
lgrsResponseStatus = lens _lgrsResponseStatus (\ s a -> s{_lgrsResponseStatus = a})

instance NFData ListGroupsResponse where
