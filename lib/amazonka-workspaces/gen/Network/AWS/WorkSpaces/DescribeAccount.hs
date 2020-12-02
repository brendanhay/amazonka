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
-- Module      : Network.AWS.WorkSpaces.DescribeAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the configuration of Bring Your Own License (BYOL) for the specified account.
module Network.AWS.WorkSpaces.DescribeAccount
  ( -- * Creating a Request
    describeAccount,
    DescribeAccount,

    -- * Destructuring the Response
    describeAccountResponse,
    DescribeAccountResponse,

    -- * Response Lenses
    darsDedicatedTenancySupport,
    darsDedicatedTenancyManagementCidrRange,
    darsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'describeAccount' smart constructor.
data DescribeAccount = DescribeAccount'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAccount' with the minimum fields required to make a request.
describeAccount ::
  DescribeAccount
describeAccount = DescribeAccount'

instance AWSRequest DescribeAccount where
  type Rs DescribeAccount = DescribeAccountResponse
  request = postJSON workSpaces
  response =
    receiveJSON
      ( \s h x ->
          DescribeAccountResponse'
            <$> (x .?> "DedicatedTenancySupport")
            <*> (x .?> "DedicatedTenancyManagementCidrRange")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAccount

instance NFData DescribeAccount

instance ToHeaders DescribeAccount where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.DescribeAccount" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeAccount where
  toJSON = const (Object mempty)

instance ToPath DescribeAccount where
  toPath = const "/"

instance ToQuery DescribeAccount where
  toQuery = const mempty

-- | /See:/ 'describeAccountResponse' smart constructor.
data DescribeAccountResponse = DescribeAccountResponse'
  { _darsDedicatedTenancySupport ::
      !(Maybe DedicatedTenancySupportResultEnum),
    _darsDedicatedTenancyManagementCidrRange ::
      !(Maybe Text),
    _darsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsDedicatedTenancySupport' - The status of BYOL (whether BYOL is enabled or disabled).
--
-- * 'darsDedicatedTenancyManagementCidrRange' - The IP address range, specified as an IPv4 CIDR block, used for the management network interface. The management network interface is connected to a secure Amazon WorkSpaces management network. It is used for interactive streaming of the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon WorkSpaces to manage the WorkSpace.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeAccountResponse ::
  -- | 'darsResponseStatus'
  Int ->
  DescribeAccountResponse
describeAccountResponse pResponseStatus_ =
  DescribeAccountResponse'
    { _darsDedicatedTenancySupport = Nothing,
      _darsDedicatedTenancyManagementCidrRange = Nothing,
      _darsResponseStatus = pResponseStatus_
    }

-- | The status of BYOL (whether BYOL is enabled or disabled).
darsDedicatedTenancySupport :: Lens' DescribeAccountResponse (Maybe DedicatedTenancySupportResultEnum)
darsDedicatedTenancySupport = lens _darsDedicatedTenancySupport (\s a -> s {_darsDedicatedTenancySupport = a})

-- | The IP address range, specified as an IPv4 CIDR block, used for the management network interface. The management network interface is connected to a secure Amazon WorkSpaces management network. It is used for interactive streaming of the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon WorkSpaces to manage the WorkSpace.
darsDedicatedTenancyManagementCidrRange :: Lens' DescribeAccountResponse (Maybe Text)
darsDedicatedTenancyManagementCidrRange = lens _darsDedicatedTenancyManagementCidrRange (\s a -> s {_darsDedicatedTenancyManagementCidrRange = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeAccountResponse Int
darsResponseStatus = lens _darsResponseStatus (\s a -> s {_darsResponseStatus = a})

instance NFData DescribeAccountResponse
