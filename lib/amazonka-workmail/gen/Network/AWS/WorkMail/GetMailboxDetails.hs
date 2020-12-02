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
-- Module      : Network.AWS.WorkMail.GetMailboxDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a user's mailbox details for a specified organization and user.
module Network.AWS.WorkMail.GetMailboxDetails
  ( -- * Creating a Request
    getMailboxDetails,
    GetMailboxDetails,

    -- * Request Lenses
    gmdOrganizationId,
    gmdUserId,

    -- * Destructuring the Response
    getMailboxDetailsResponse,
    GetMailboxDetailsResponse,

    -- * Response Lenses
    gmdrsMailboxQuota,
    gmdrsMailboxSize,
    gmdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'getMailboxDetails' smart constructor.
data GetMailboxDetails = GetMailboxDetails'
  { _gmdOrganizationId ::
      !Text,
    _gmdUserId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMailboxDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmdOrganizationId' - The identifier for the organization that contains the user whose mailbox details are being requested.
--
-- * 'gmdUserId' - The identifier for the user whose mailbox details are being requested.
getMailboxDetails ::
  -- | 'gmdOrganizationId'
  Text ->
  -- | 'gmdUserId'
  Text ->
  GetMailboxDetails
getMailboxDetails pOrganizationId_ pUserId_ =
  GetMailboxDetails'
    { _gmdOrganizationId = pOrganizationId_,
      _gmdUserId = pUserId_
    }

-- | The identifier for the organization that contains the user whose mailbox details are being requested.
gmdOrganizationId :: Lens' GetMailboxDetails Text
gmdOrganizationId = lens _gmdOrganizationId (\s a -> s {_gmdOrganizationId = a})

-- | The identifier for the user whose mailbox details are being requested.
gmdUserId :: Lens' GetMailboxDetails Text
gmdUserId = lens _gmdUserId (\s a -> s {_gmdUserId = a})

instance AWSRequest GetMailboxDetails where
  type Rs GetMailboxDetails = GetMailboxDetailsResponse
  request = postJSON workMail
  response =
    receiveJSON
      ( \s h x ->
          GetMailboxDetailsResponse'
            <$> (x .?> "MailboxQuota")
            <*> (x .?> "MailboxSize")
            <*> (pure (fromEnum s))
      )

instance Hashable GetMailboxDetails

instance NFData GetMailboxDetails

instance ToHeaders GetMailboxDetails where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.GetMailboxDetails" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetMailboxDetails where
  toJSON GetMailboxDetails' {..} =
    object
      ( catMaybes
          [ Just ("OrganizationId" .= _gmdOrganizationId),
            Just ("UserId" .= _gmdUserId)
          ]
      )

instance ToPath GetMailboxDetails where
  toPath = const "/"

instance ToQuery GetMailboxDetails where
  toQuery = const mempty

-- | /See:/ 'getMailboxDetailsResponse' smart constructor.
data GetMailboxDetailsResponse = GetMailboxDetailsResponse'
  { _gmdrsMailboxQuota ::
      !(Maybe Nat),
    _gmdrsMailboxSize :: !(Maybe Double),
    _gmdrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMailboxDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmdrsMailboxQuota' - The maximum allowed mailbox size, in MB, for the specified user.
--
-- * 'gmdrsMailboxSize' - The current mailbox size, in MB, for the specified user.
--
-- * 'gmdrsResponseStatus' - -- | The response status code.
getMailboxDetailsResponse ::
  -- | 'gmdrsResponseStatus'
  Int ->
  GetMailboxDetailsResponse
getMailboxDetailsResponse pResponseStatus_ =
  GetMailboxDetailsResponse'
    { _gmdrsMailboxQuota = Nothing,
      _gmdrsMailboxSize = Nothing,
      _gmdrsResponseStatus = pResponseStatus_
    }

-- | The maximum allowed mailbox size, in MB, for the specified user.
gmdrsMailboxQuota :: Lens' GetMailboxDetailsResponse (Maybe Natural)
gmdrsMailboxQuota = lens _gmdrsMailboxQuota (\s a -> s {_gmdrsMailboxQuota = a}) . mapping _Nat

-- | The current mailbox size, in MB, for the specified user.
gmdrsMailboxSize :: Lens' GetMailboxDetailsResponse (Maybe Double)
gmdrsMailboxSize = lens _gmdrsMailboxSize (\s a -> s {_gmdrsMailboxSize = a})

-- | -- | The response status code.
gmdrsResponseStatus :: Lens' GetMailboxDetailsResponse Int
gmdrsResponseStatus = lens _gmdrsResponseStatus (\s a -> s {_gmdrsResponseStatus = a})

instance NFData GetMailboxDetailsResponse
