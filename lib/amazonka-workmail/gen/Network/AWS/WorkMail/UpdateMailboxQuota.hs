{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.UpdateMailboxQuota
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user's current mailbox quota for a specified organization and user.
module Network.AWS.WorkMail.UpdateMailboxQuota
  ( -- * Creating a Request
    updateMailboxQuota,
    UpdateMailboxQuota,

    -- * Request Lenses
    umqOrganizationId,
    umqUserId,
    umqMailboxQuota,

    -- * Destructuring the Response
    updateMailboxQuotaResponse,
    UpdateMailboxQuotaResponse,

    -- * Response Lenses
    umqrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'updateMailboxQuota' smart constructor.
data UpdateMailboxQuota = UpdateMailboxQuota'
  { _umqOrganizationId ::
      !Text,
    _umqUserId :: !Text,
    _umqMailboxQuota :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMailboxQuota' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umqOrganizationId' - The identifier for the organization that contains the user for whom to update the mailbox quota.
--
-- * 'umqUserId' - The identifer for the user for whom to update the mailbox quota.
--
-- * 'umqMailboxQuota' - The updated mailbox quota, in MB, for the specified user.
updateMailboxQuota ::
  -- | 'umqOrganizationId'
  Text ->
  -- | 'umqUserId'
  Text ->
  -- | 'umqMailboxQuota'
  Natural ->
  UpdateMailboxQuota
updateMailboxQuota pOrganizationId_ pUserId_ pMailboxQuota_ =
  UpdateMailboxQuota'
    { _umqOrganizationId = pOrganizationId_,
      _umqUserId = pUserId_,
      _umqMailboxQuota = _Nat # pMailboxQuota_
    }

-- | The identifier for the organization that contains the user for whom to update the mailbox quota.
umqOrganizationId :: Lens' UpdateMailboxQuota Text
umqOrganizationId = lens _umqOrganizationId (\s a -> s {_umqOrganizationId = a})

-- | The identifer for the user for whom to update the mailbox quota.
umqUserId :: Lens' UpdateMailboxQuota Text
umqUserId = lens _umqUserId (\s a -> s {_umqUserId = a})

-- | The updated mailbox quota, in MB, for the specified user.
umqMailboxQuota :: Lens' UpdateMailboxQuota Natural
umqMailboxQuota = lens _umqMailboxQuota (\s a -> s {_umqMailboxQuota = a}) . _Nat

instance AWSRequest UpdateMailboxQuota where
  type Rs UpdateMailboxQuota = UpdateMailboxQuotaResponse
  request = postJSON workMail
  response =
    receiveEmpty
      (\s h x -> UpdateMailboxQuotaResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateMailboxQuota

instance NFData UpdateMailboxQuota

instance ToHeaders UpdateMailboxQuota where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.UpdateMailboxQuota" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateMailboxQuota where
  toJSON UpdateMailboxQuota' {..} =
    object
      ( catMaybes
          [ Just ("OrganizationId" .= _umqOrganizationId),
            Just ("UserId" .= _umqUserId),
            Just ("MailboxQuota" .= _umqMailboxQuota)
          ]
      )

instance ToPath UpdateMailboxQuota where
  toPath = const "/"

instance ToQuery UpdateMailboxQuota where
  toQuery = const mempty

-- | /See:/ 'updateMailboxQuotaResponse' smart constructor.
newtype UpdateMailboxQuotaResponse = UpdateMailboxQuotaResponse'
  { _umqrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMailboxQuotaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umqrsResponseStatus' - -- | The response status code.
updateMailboxQuotaResponse ::
  -- | 'umqrsResponseStatus'
  Int ->
  UpdateMailboxQuotaResponse
updateMailboxQuotaResponse pResponseStatus_ =
  UpdateMailboxQuotaResponse'
    { _umqrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
umqrsResponseStatus :: Lens' UpdateMailboxQuotaResponse Int
umqrsResponseStatus = lens _umqrsResponseStatus (\s a -> s {_umqrsResponseStatus = a})

instance NFData UpdateMailboxQuotaResponse
