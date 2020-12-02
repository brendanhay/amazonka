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
-- Module      : Network.AWS.WorkSpaces.ModifyAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the configuration of Bring Your Own License (BYOL) for the specified account.
module Network.AWS.WorkSpaces.ModifyAccount
  ( -- * Creating a Request
    modifyAccount,
    ModifyAccount,

    -- * Request Lenses
    maDedicatedTenancySupport,
    maDedicatedTenancyManagementCidrRange,

    -- * Destructuring the Response
    modifyAccountResponse,
    ModifyAccountResponse,

    -- * Response Lenses
    marsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'modifyAccount' smart constructor.
data ModifyAccount = ModifyAccount'
  { _maDedicatedTenancySupport ::
      !(Maybe DedicatedTenancySupportEnum),
    _maDedicatedTenancyManagementCidrRange :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maDedicatedTenancySupport' - The status of BYOL.
--
-- * 'maDedicatedTenancyManagementCidrRange' - The IP address range, specified as an IPv4 CIDR block, for the management network interface. Specify an IP address range that is compatible with your network and in CIDR notation (that is, specify the range as an IPv4 CIDR block). The CIDR block size must be /16 (for example, 203.0.113.25/16). It must also be specified as available by the @ListAvailableManagementCidrRanges@ operation.
modifyAccount ::
  ModifyAccount
modifyAccount =
  ModifyAccount'
    { _maDedicatedTenancySupport = Nothing,
      _maDedicatedTenancyManagementCidrRange = Nothing
    }

-- | The status of BYOL.
maDedicatedTenancySupport :: Lens' ModifyAccount (Maybe DedicatedTenancySupportEnum)
maDedicatedTenancySupport = lens _maDedicatedTenancySupport (\s a -> s {_maDedicatedTenancySupport = a})

-- | The IP address range, specified as an IPv4 CIDR block, for the management network interface. Specify an IP address range that is compatible with your network and in CIDR notation (that is, specify the range as an IPv4 CIDR block). The CIDR block size must be /16 (for example, 203.0.113.25/16). It must also be specified as available by the @ListAvailableManagementCidrRanges@ operation.
maDedicatedTenancyManagementCidrRange :: Lens' ModifyAccount (Maybe Text)
maDedicatedTenancyManagementCidrRange = lens _maDedicatedTenancyManagementCidrRange (\s a -> s {_maDedicatedTenancyManagementCidrRange = a})

instance AWSRequest ModifyAccount where
  type Rs ModifyAccount = ModifyAccountResponse
  request = postJSON workSpaces
  response =
    receiveEmpty
      (\s h x -> ModifyAccountResponse' <$> (pure (fromEnum s)))

instance Hashable ModifyAccount

instance NFData ModifyAccount

instance ToHeaders ModifyAccount where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.ModifyAccount" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ModifyAccount where
  toJSON ModifyAccount' {..} =
    object
      ( catMaybes
          [ ("DedicatedTenancySupport" .=) <$> _maDedicatedTenancySupport,
            ("DedicatedTenancyManagementCidrRange" .=)
              <$> _maDedicatedTenancyManagementCidrRange
          ]
      )

instance ToPath ModifyAccount where
  toPath = const "/"

instance ToQuery ModifyAccount where
  toQuery = const mempty

-- | /See:/ 'modifyAccountResponse' smart constructor.
newtype ModifyAccountResponse = ModifyAccountResponse'
  { _marsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'marsResponseStatus' - -- | The response status code.
modifyAccountResponse ::
  -- | 'marsResponseStatus'
  Int ->
  ModifyAccountResponse
modifyAccountResponse pResponseStatus_ =
  ModifyAccountResponse' {_marsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
marsResponseStatus :: Lens' ModifyAccountResponse Int
marsResponseStatus = lens _marsResponseStatus (\s a -> s {_marsResponseStatus = a})

instance NFData ModifyAccountResponse
