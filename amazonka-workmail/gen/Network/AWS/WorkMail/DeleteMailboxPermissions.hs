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
-- Module      : Network.AWS.WorkMail.DeleteMailboxPermissions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes permissions granted to a user or group.
--
--
module Network.AWS.WorkMail.DeleteMailboxPermissions
    (
    -- * Creating a Request
      deleteMailboxPermissions
    , DeleteMailboxPermissions
    -- * Request Lenses
    , dmpOrganizationId
    , dmpEntityId
    , dmpGranteeId

    -- * Destructuring the Response
    , deleteMailboxPermissionsResponse
    , DeleteMailboxPermissionsResponse
    -- * Response Lenses
    , dmprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'deleteMailboxPermissions' smart constructor.
data DeleteMailboxPermissions = DeleteMailboxPermissions'
  { _dmpOrganizationId :: !Text
  , _dmpEntityId       :: !Text
  , _dmpGranteeId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMailboxPermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmpOrganizationId' - The identifier of the organization under which the entity (user or group) exists.
--
-- * 'dmpEntityId' - The identifier of the entity (user or group) for which to delete mailbox permissions.
--
-- * 'dmpGranteeId' - The identifier of the entity (user or group) for which to delete granted permissions.
deleteMailboxPermissions
    :: Text -- ^ 'dmpOrganizationId'
    -> Text -- ^ 'dmpEntityId'
    -> Text -- ^ 'dmpGranteeId'
    -> DeleteMailboxPermissions
deleteMailboxPermissions pOrganizationId_ pEntityId_ pGranteeId_ =
  DeleteMailboxPermissions'
    { _dmpOrganizationId = pOrganizationId_
    , _dmpEntityId = pEntityId_
    , _dmpGranteeId = pGranteeId_
    }


-- | The identifier of the organization under which the entity (user or group) exists.
dmpOrganizationId :: Lens' DeleteMailboxPermissions Text
dmpOrganizationId = lens _dmpOrganizationId (\ s a -> s{_dmpOrganizationId = a})

-- | The identifier of the entity (user or group) for which to delete mailbox permissions.
dmpEntityId :: Lens' DeleteMailboxPermissions Text
dmpEntityId = lens _dmpEntityId (\ s a -> s{_dmpEntityId = a})

-- | The identifier of the entity (user or group) for which to delete granted permissions.
dmpGranteeId :: Lens' DeleteMailboxPermissions Text
dmpGranteeId = lens _dmpGranteeId (\ s a -> s{_dmpGranteeId = a})

instance AWSRequest DeleteMailboxPermissions where
        type Rs DeleteMailboxPermissions =
             DeleteMailboxPermissionsResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteMailboxPermissionsResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteMailboxPermissions where

instance NFData DeleteMailboxPermissions where

instance ToHeaders DeleteMailboxPermissions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.DeleteMailboxPermissions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteMailboxPermissions where
        toJSON DeleteMailboxPermissions'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _dmpOrganizationId),
                  Just ("EntityId" .= _dmpEntityId),
                  Just ("GranteeId" .= _dmpGranteeId)])

instance ToPath DeleteMailboxPermissions where
        toPath = const "/"

instance ToQuery DeleteMailboxPermissions where
        toQuery = const mempty

-- | /See:/ 'deleteMailboxPermissionsResponse' smart constructor.
newtype DeleteMailboxPermissionsResponse = DeleteMailboxPermissionsResponse'
  { _dmprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMailboxPermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmprsResponseStatus' - -- | The response status code.
deleteMailboxPermissionsResponse
    :: Int -- ^ 'dmprsResponseStatus'
    -> DeleteMailboxPermissionsResponse
deleteMailboxPermissionsResponse pResponseStatus_ =
  DeleteMailboxPermissionsResponse' {_dmprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dmprsResponseStatus :: Lens' DeleteMailboxPermissionsResponse Int
dmprsResponseStatus = lens _dmprsResponseStatus (\ s a -> s{_dmprsResponseStatus = a})

instance NFData DeleteMailboxPermissionsResponse
         where
