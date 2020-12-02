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
-- Module      : Network.AWS.GuardDuty.DisableOrganizationAdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an AWS account within the Organization as the GuardDuty delegated administrator.
module Network.AWS.GuardDuty.DisableOrganizationAdminAccount
  ( -- * Creating a Request
    disableOrganizationAdminAccount,
    DisableOrganizationAdminAccount,

    -- * Request Lenses
    doaaAdminAccountId,

    -- * Destructuring the Response
    disableOrganizationAdminAccountResponse,
    DisableOrganizationAdminAccountResponse,

    -- * Response Lenses
    doaarsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableOrganizationAdminAccount' smart constructor.
newtype DisableOrganizationAdminAccount = DisableOrganizationAdminAccount'
  { _doaaAdminAccountId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableOrganizationAdminAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doaaAdminAccountId' - The AWS Account ID for the organizations account to be disabled as a GuardDuty delegated administrator.
disableOrganizationAdminAccount ::
  -- | 'doaaAdminAccountId'
  Text ->
  DisableOrganizationAdminAccount
disableOrganizationAdminAccount pAdminAccountId_ =
  DisableOrganizationAdminAccount'
    { _doaaAdminAccountId =
        pAdminAccountId_
    }

-- | The AWS Account ID for the organizations account to be disabled as a GuardDuty delegated administrator.
doaaAdminAccountId :: Lens' DisableOrganizationAdminAccount Text
doaaAdminAccountId = lens _doaaAdminAccountId (\s a -> s {_doaaAdminAccountId = a})

instance AWSRequest DisableOrganizationAdminAccount where
  type
    Rs DisableOrganizationAdminAccount =
      DisableOrganizationAdminAccountResponse
  request = postJSON guardDuty
  response =
    receiveEmpty
      ( \s h x ->
          DisableOrganizationAdminAccountResponse' <$> (pure (fromEnum s))
      )

instance Hashable DisableOrganizationAdminAccount

instance NFData DisableOrganizationAdminAccount

instance ToHeaders DisableOrganizationAdminAccount where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON DisableOrganizationAdminAccount where
  toJSON DisableOrganizationAdminAccount' {..} =
    object
      (catMaybes [Just ("adminAccountId" .= _doaaAdminAccountId)])

instance ToPath DisableOrganizationAdminAccount where
  toPath = const "/admin/disable"

instance ToQuery DisableOrganizationAdminAccount where
  toQuery = const mempty

-- | /See:/ 'disableOrganizationAdminAccountResponse' smart constructor.
newtype DisableOrganizationAdminAccountResponse = DisableOrganizationAdminAccountResponse'
  { _doaarsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DisableOrganizationAdminAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doaarsResponseStatus' - -- | The response status code.
disableOrganizationAdminAccountResponse ::
  -- | 'doaarsResponseStatus'
  Int ->
  DisableOrganizationAdminAccountResponse
disableOrganizationAdminAccountResponse pResponseStatus_ =
  DisableOrganizationAdminAccountResponse'
    { _doaarsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
doaarsResponseStatus :: Lens' DisableOrganizationAdminAccountResponse Int
doaarsResponseStatus = lens _doaarsResponseStatus (\s a -> s {_doaarsResponseStatus = a})

instance NFData DisableOrganizationAdminAccountResponse
