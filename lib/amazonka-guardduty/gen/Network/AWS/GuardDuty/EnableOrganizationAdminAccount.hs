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
-- Module      : Network.AWS.GuardDuty.EnableOrganizationAdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables an AWS account within the organization as the GuardDuty delegated administrator.
module Network.AWS.GuardDuty.EnableOrganizationAdminAccount
  ( -- * Creating a Request
    enableOrganizationAdminAccount,
    EnableOrganizationAdminAccount,

    -- * Request Lenses
    eoaaAdminAccountId,

    -- * Destructuring the Response
    enableOrganizationAdminAccountResponse,
    EnableOrganizationAdminAccountResponse,

    -- * Response Lenses
    eoaarsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableOrganizationAdminAccount' smart constructor.
newtype EnableOrganizationAdminAccount = EnableOrganizationAdminAccount'
  { _eoaaAdminAccountId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableOrganizationAdminAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eoaaAdminAccountId' - The AWS Account ID for the organization account to be enabled as a GuardDuty delegated administrator.
enableOrganizationAdminAccount ::
  -- | 'eoaaAdminAccountId'
  Text ->
  EnableOrganizationAdminAccount
enableOrganizationAdminAccount pAdminAccountId_ =
  EnableOrganizationAdminAccount'
    { _eoaaAdminAccountId =
        pAdminAccountId_
    }

-- | The AWS Account ID for the organization account to be enabled as a GuardDuty delegated administrator.
eoaaAdminAccountId :: Lens' EnableOrganizationAdminAccount Text
eoaaAdminAccountId = lens _eoaaAdminAccountId (\s a -> s {_eoaaAdminAccountId = a})

instance AWSRequest EnableOrganizationAdminAccount where
  type
    Rs EnableOrganizationAdminAccount =
      EnableOrganizationAdminAccountResponse
  request = postJSON guardDuty
  response =
    receiveEmpty
      ( \s h x ->
          EnableOrganizationAdminAccountResponse' <$> (pure (fromEnum s))
      )

instance Hashable EnableOrganizationAdminAccount

instance NFData EnableOrganizationAdminAccount

instance ToHeaders EnableOrganizationAdminAccount where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON EnableOrganizationAdminAccount where
  toJSON EnableOrganizationAdminAccount' {..} =
    object
      (catMaybes [Just ("adminAccountId" .= _eoaaAdminAccountId)])

instance ToPath EnableOrganizationAdminAccount where
  toPath = const "/admin/enable"

instance ToQuery EnableOrganizationAdminAccount where
  toQuery = const mempty

-- | /See:/ 'enableOrganizationAdminAccountResponse' smart constructor.
newtype EnableOrganizationAdminAccountResponse = EnableOrganizationAdminAccountResponse'
  { _eoaarsResponseStatus ::
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

-- | Creates a value of 'EnableOrganizationAdminAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eoaarsResponseStatus' - -- | The response status code.
enableOrganizationAdminAccountResponse ::
  -- | 'eoaarsResponseStatus'
  Int ->
  EnableOrganizationAdminAccountResponse
enableOrganizationAdminAccountResponse pResponseStatus_ =
  EnableOrganizationAdminAccountResponse'
    { _eoaarsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
eoaarsResponseStatus :: Lens' EnableOrganizationAdminAccountResponse Int
eoaarsResponseStatus = lens _eoaarsResponseStatus (\s a -> s {_eoaarsResponseStatus = a})

instance NFData EnableOrganizationAdminAccountResponse
