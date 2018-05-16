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
-- Module      : Network.AWS.WorkMail.ListMailboxPermissions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the mailbox permissions associated with a mailbox.
--
--
module Network.AWS.WorkMail.ListMailboxPermissions
    (
    -- * Creating a Request
      listMailboxPermissions
    , ListMailboxPermissions
    -- * Request Lenses
    , lmpNextToken
    , lmpMaxResults
    , lmpOrganizationId
    , lmpEntityId

    -- * Destructuring the Response
    , listMailboxPermissionsResponse
    , ListMailboxPermissionsResponse
    -- * Response Lenses
    , lmprsNextToken
    , lmprsPermissions
    , lmprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'listMailboxPermissions' smart constructor.
data ListMailboxPermissions = ListMailboxPermissions'
  { _lmpNextToken      :: !(Maybe Text)
  , _lmpMaxResults     :: !(Maybe Nat)
  , _lmpOrganizationId :: !Text
  , _lmpEntityId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMailboxPermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmpNextToken' - The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- * 'lmpMaxResults' - The maximum number of results to return in a single call.
--
-- * 'lmpOrganizationId' - The identifier of the organization under which the entity (user or group) exists.
--
-- * 'lmpEntityId' - The identifier of the entity (user or group) for which to list mailbox permissions.
listMailboxPermissions
    :: Text -- ^ 'lmpOrganizationId'
    -> Text -- ^ 'lmpEntityId'
    -> ListMailboxPermissions
listMailboxPermissions pOrganizationId_ pEntityId_ =
  ListMailboxPermissions'
    { _lmpNextToken = Nothing
    , _lmpMaxResults = Nothing
    , _lmpOrganizationId = pOrganizationId_
    , _lmpEntityId = pEntityId_
    }


-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
lmpNextToken :: Lens' ListMailboxPermissions (Maybe Text)
lmpNextToken = lens _lmpNextToken (\ s a -> s{_lmpNextToken = a})

-- | The maximum number of results to return in a single call.
lmpMaxResults :: Lens' ListMailboxPermissions (Maybe Natural)
lmpMaxResults = lens _lmpMaxResults (\ s a -> s{_lmpMaxResults = a}) . mapping _Nat

-- | The identifier of the organization under which the entity (user or group) exists.
lmpOrganizationId :: Lens' ListMailboxPermissions Text
lmpOrganizationId = lens _lmpOrganizationId (\ s a -> s{_lmpOrganizationId = a})

-- | The identifier of the entity (user or group) for which to list mailbox permissions.
lmpEntityId :: Lens' ListMailboxPermissions Text
lmpEntityId = lens _lmpEntityId (\ s a -> s{_lmpEntityId = a})

instance AWSRequest ListMailboxPermissions where
        type Rs ListMailboxPermissions =
             ListMailboxPermissionsResponse
        request = postJSON workMail
        response
          = receiveJSON
              (\ s h x ->
                 ListMailboxPermissionsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Permissions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListMailboxPermissions where

instance NFData ListMailboxPermissions where

instance ToHeaders ListMailboxPermissions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.ListMailboxPermissions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListMailboxPermissions where
        toJSON ListMailboxPermissions'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lmpNextToken,
                  ("MaxResults" .=) <$> _lmpMaxResults,
                  Just ("OrganizationId" .= _lmpOrganizationId),
                  Just ("EntityId" .= _lmpEntityId)])

instance ToPath ListMailboxPermissions where
        toPath = const "/"

instance ToQuery ListMailboxPermissions where
        toQuery = const mempty

-- | /See:/ 'listMailboxPermissionsResponse' smart constructor.
data ListMailboxPermissionsResponse = ListMailboxPermissionsResponse'
  { _lmprsNextToken      :: !(Maybe Text)
  , _lmprsPermissions    :: !(Maybe [Permission])
  , _lmprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMailboxPermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmprsNextToken' - The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
--
-- * 'lmprsPermissions' - One page of the entity's mailbox permissions.
--
-- * 'lmprsResponseStatus' - -- | The response status code.
listMailboxPermissionsResponse
    :: Int -- ^ 'lmprsResponseStatus'
    -> ListMailboxPermissionsResponse
listMailboxPermissionsResponse pResponseStatus_ =
  ListMailboxPermissionsResponse'
    { _lmprsNextToken = Nothing
    , _lmprsPermissions = Nothing
    , _lmprsResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
lmprsNextToken :: Lens' ListMailboxPermissionsResponse (Maybe Text)
lmprsNextToken = lens _lmprsNextToken (\ s a -> s{_lmprsNextToken = a})

-- | One page of the entity's mailbox permissions.
lmprsPermissions :: Lens' ListMailboxPermissionsResponse [Permission]
lmprsPermissions = lens _lmprsPermissions (\ s a -> s{_lmprsPermissions = a}) . _Default . _Coerce

-- | -- | The response status code.
lmprsResponseStatus :: Lens' ListMailboxPermissionsResponse Int
lmprsResponseStatus = lens _lmprsResponseStatus (\ s a -> s{_lmprsResponseStatus = a})

instance NFData ListMailboxPermissionsResponse where
