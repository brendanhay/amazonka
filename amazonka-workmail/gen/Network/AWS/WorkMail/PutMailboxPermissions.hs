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
-- Module      : Network.AWS.WorkMail.PutMailboxPermissions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets permissions for a user or group. This replaces any pre-existing permissions set for the entity.
--
--
module Network.AWS.WorkMail.PutMailboxPermissions
    (
    -- * Creating a Request
      putMailboxPermissions
    , PutMailboxPermissions
    -- * Request Lenses
    , pmpOrganizationId
    , pmpEntityId
    , pmpGranteeId
    , pmpPermissionValues

    -- * Destructuring the Response
    , putMailboxPermissionsResponse
    , PutMailboxPermissionsResponse
    -- * Response Lenses
    , pmprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'putMailboxPermissions' smart constructor.
data PutMailboxPermissions = PutMailboxPermissions'
  { _pmpOrganizationId   :: !Text
  , _pmpEntityId         :: !Text
  , _pmpGranteeId        :: !Text
  , _pmpPermissionValues :: ![PermissionType]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutMailboxPermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmpOrganizationId' - The identifier of the organization under which the entity (user or group) exists.
--
-- * 'pmpEntityId' - The identifier of the entity (user or group) for which to update mailbox permissions.
--
-- * 'pmpGranteeId' - The identifier of the entity (user or group) to which to grant the permissions.
--
-- * 'pmpPermissionValues' - The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
putMailboxPermissions
    :: Text -- ^ 'pmpOrganizationId'
    -> Text -- ^ 'pmpEntityId'
    -> Text -- ^ 'pmpGranteeId'
    -> PutMailboxPermissions
putMailboxPermissions pOrganizationId_ pEntityId_ pGranteeId_ =
  PutMailboxPermissions'
    { _pmpOrganizationId = pOrganizationId_
    , _pmpEntityId = pEntityId_
    , _pmpGranteeId = pGranteeId_
    , _pmpPermissionValues = mempty
    }


-- | The identifier of the organization under which the entity (user or group) exists.
pmpOrganizationId :: Lens' PutMailboxPermissions Text
pmpOrganizationId = lens _pmpOrganizationId (\ s a -> s{_pmpOrganizationId = a})

-- | The identifier of the entity (user or group) for which to update mailbox permissions.
pmpEntityId :: Lens' PutMailboxPermissions Text
pmpEntityId = lens _pmpEntityId (\ s a -> s{_pmpEntityId = a})

-- | The identifier of the entity (user or group) to which to grant the permissions.
pmpGranteeId :: Lens' PutMailboxPermissions Text
pmpGranteeId = lens _pmpGranteeId (\ s a -> s{_pmpGranteeId = a})

-- | The permissions granted to the grantee. SEND_AS allows the grantee to send email as the owner of the mailbox (the grantee is not mentioned on these emails). SEND_ON_BEHALF allows the grantee to send email on behalf of the owner of the mailbox (the grantee is not mentioned as the physical sender of these emails). FULL_ACCESS allows the grantee full access to the mailbox, irrespective of other folder-level permissions set on the mailbox.
pmpPermissionValues :: Lens' PutMailboxPermissions [PermissionType]
pmpPermissionValues = lens _pmpPermissionValues (\ s a -> s{_pmpPermissionValues = a}) . _Coerce

instance AWSRequest PutMailboxPermissions where
        type Rs PutMailboxPermissions =
             PutMailboxPermissionsResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 PutMailboxPermissionsResponse' <$>
                   (pure (fromEnum s)))

instance Hashable PutMailboxPermissions where

instance NFData PutMailboxPermissions where

instance ToHeaders PutMailboxPermissions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.PutMailboxPermissions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutMailboxPermissions where
        toJSON PutMailboxPermissions'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _pmpOrganizationId),
                  Just ("EntityId" .= _pmpEntityId),
                  Just ("GranteeId" .= _pmpGranteeId),
                  Just ("PermissionValues" .= _pmpPermissionValues)])

instance ToPath PutMailboxPermissions where
        toPath = const "/"

instance ToQuery PutMailboxPermissions where
        toQuery = const mempty

-- | /See:/ 'putMailboxPermissionsResponse' smart constructor.
newtype PutMailboxPermissionsResponse = PutMailboxPermissionsResponse'
  { _pmprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutMailboxPermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmprsResponseStatus' - -- | The response status code.
putMailboxPermissionsResponse
    :: Int -- ^ 'pmprsResponseStatus'
    -> PutMailboxPermissionsResponse
putMailboxPermissionsResponse pResponseStatus_ =
  PutMailboxPermissionsResponse' {_pmprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
pmprsResponseStatus :: Lens' PutMailboxPermissionsResponse Int
pmprsResponseStatus = lens _pmprsResponseStatus (\ s a -> s{_pmprsResponseStatus = a})

instance NFData PutMailboxPermissionsResponse where
