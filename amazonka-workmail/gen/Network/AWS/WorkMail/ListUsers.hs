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
-- Module      : Network.AWS.WorkMail.ListUsers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the organization's users.
--
--
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListUsers
    (
    -- * Creating a Request
      listUsers
    , ListUsers
    -- * Request Lenses
    , luNextToken
    , luMaxResults
    , luOrganizationId

    -- * Destructuring the Response
    , listUsersResponse
    , ListUsersResponse
    -- * Response Lenses
    , lursUsers
    , lursNextToken
    , lursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'listUsers' smart constructor.
data ListUsers = ListUsers'
  { _luNextToken      :: !(Maybe Text)
  , _luMaxResults     :: !(Maybe Nat)
  , _luOrganizationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'luNextToken' - TBD
--
-- * 'luMaxResults' - The maximum number of results to return in a single call.
--
-- * 'luOrganizationId' - The identifier for the organization under which the users exist.
listUsers
    :: Text -- ^ 'luOrganizationId'
    -> ListUsers
listUsers pOrganizationId_ =
  ListUsers'
    { _luNextToken = Nothing
    , _luMaxResults = Nothing
    , _luOrganizationId = pOrganizationId_
    }


-- | TBD
luNextToken :: Lens' ListUsers (Maybe Text)
luNextToken = lens _luNextToken (\ s a -> s{_luNextToken = a})

-- | The maximum number of results to return in a single call.
luMaxResults :: Lens' ListUsers (Maybe Natural)
luMaxResults = lens _luMaxResults (\ s a -> s{_luMaxResults = a}) . mapping _Nat

-- | The identifier for the organization under which the users exist.
luOrganizationId :: Lens' ListUsers Text
luOrganizationId = lens _luOrganizationId (\ s a -> s{_luOrganizationId = a})

instance AWSPager ListUsers where
        page rq rs
          | stop (rs ^. lursNextToken) = Nothing
          | stop (rs ^. lursUsers) = Nothing
          | otherwise =
            Just $ rq & luNextToken .~ rs ^. lursNextToken

instance AWSRequest ListUsers where
        type Rs ListUsers = ListUsersResponse
        request = postJSON workMail
        response
          = receiveJSON
              (\ s h x ->
                 ListUsersResponse' <$>
                   (x .?> "Users" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListUsers where

instance NFData ListUsers where

instance ToHeaders ListUsers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.ListUsers" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListUsers where
        toJSON ListUsers'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _luNextToken,
                  ("MaxResults" .=) <$> _luMaxResults,
                  Just ("OrganizationId" .= _luOrganizationId)])

instance ToPath ListUsers where
        toPath = const "/"

instance ToQuery ListUsers where
        toQuery = const mempty

-- | /See:/ 'listUsersResponse' smart constructor.
data ListUsersResponse = ListUsersResponse'
  { _lursUsers          :: !(Maybe [User])
  , _lursNextToken      :: !(Maybe Text)
  , _lursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListUsersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lursUsers' - The overview of users for an organization.
--
-- * 'lursNextToken' - The token to use to retrieve the next page of results. This value is `null` when there are no more results to return.
--
-- * 'lursResponseStatus' - -- | The response status code.
listUsersResponse
    :: Int -- ^ 'lursResponseStatus'
    -> ListUsersResponse
listUsersResponse pResponseStatus_ =
  ListUsersResponse'
    { _lursUsers = Nothing
    , _lursNextToken = Nothing
    , _lursResponseStatus = pResponseStatus_
    }


-- | The overview of users for an organization.
lursUsers :: Lens' ListUsersResponse [User]
lursUsers = lens _lursUsers (\ s a -> s{_lursUsers = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is `null` when there are no more results to return.
lursNextToken :: Lens' ListUsersResponse (Maybe Text)
lursNextToken = lens _lursNextToken (\ s a -> s{_lursNextToken = a})

-- | -- | The response status code.
lursResponseStatus :: Lens' ListUsersResponse Int
lursResponseStatus = lens _lursResponseStatus (\ s a -> s{_lursResponseStatus = a})

instance NFData ListUsersResponse where
