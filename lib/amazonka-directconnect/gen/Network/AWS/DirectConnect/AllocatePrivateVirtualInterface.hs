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
-- Module      : Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a private virtual interface to be owned by the specified AWS account.
--
--
-- Virtual interfaces created using this action must be confirmed by the owner using 'ConfirmPrivateVirtualInterface' . Until then, the virtual interface is in the @Confirming@ state and is not available to handle traffic.
module Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
  ( -- * Creating a Request
    allocatePrivateVirtualInterface,
    AllocatePrivateVirtualInterface,

    -- * Request Lenses
    apviConnectionId,
    apviOwnerAccount,
    apviNewPrivateVirtualInterfaceAllocation,

    -- * Destructuring the Response
    virtualInterface,
    VirtualInterface,

    -- * Response Lenses
    viBgpPeers,
    viVirtualGatewayId,
    viMtu,
    viRouteFilterPrefixes,
    viCustomerAddress,
    viVlan,
    viLocation,
    viAmazonAddress,
    viAddressFamily,
    viVirtualInterfaceState,
    viConnectionId,
    viDirectConnectGatewayId,
    viAmazonSideASN,
    viVirtualInterfaceType,
    viAsn,
    viAuthKey,
    viJumboFrameCapable,
    viCustomerRouterConfig,
    viOwnerAccount,
    viRegion,
    viVirtualInterfaceName,
    viAwsDeviceV2,
    viVirtualInterfaceId,
    viTags,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'allocatePrivateVirtualInterface' smart constructor.
data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface'
  { _apviConnectionId ::
      !Text,
    _apviOwnerAccount :: !Text,
    _apviNewPrivateVirtualInterfaceAllocation ::
      !NewPrivateVirtualInterfaceAllocation
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AllocatePrivateVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apviConnectionId' - The ID of the connection on which the private virtual interface is provisioned.
--
-- * 'apviOwnerAccount' - The ID of the AWS account that owns the virtual private interface.
--
-- * 'apviNewPrivateVirtualInterfaceAllocation' - Information about the private virtual interface.
allocatePrivateVirtualInterface ::
  -- | 'apviConnectionId'
  Text ->
  -- | 'apviOwnerAccount'
  Text ->
  -- | 'apviNewPrivateVirtualInterfaceAllocation'
  NewPrivateVirtualInterfaceAllocation ->
  AllocatePrivateVirtualInterface
allocatePrivateVirtualInterface
  pConnectionId_
  pOwnerAccount_
  pNewPrivateVirtualInterfaceAllocation_ =
    AllocatePrivateVirtualInterface'
      { _apviConnectionId =
          pConnectionId_,
        _apviOwnerAccount = pOwnerAccount_,
        _apviNewPrivateVirtualInterfaceAllocation =
          pNewPrivateVirtualInterfaceAllocation_
      }

-- | The ID of the connection on which the private virtual interface is provisioned.
apviConnectionId :: Lens' AllocatePrivateVirtualInterface Text
apviConnectionId = lens _apviConnectionId (\s a -> s {_apviConnectionId = a})

-- | The ID of the AWS account that owns the virtual private interface.
apviOwnerAccount :: Lens' AllocatePrivateVirtualInterface Text
apviOwnerAccount = lens _apviOwnerAccount (\s a -> s {_apviOwnerAccount = a})

-- | Information about the private virtual interface.
apviNewPrivateVirtualInterfaceAllocation :: Lens' AllocatePrivateVirtualInterface NewPrivateVirtualInterfaceAllocation
apviNewPrivateVirtualInterfaceAllocation = lens _apviNewPrivateVirtualInterfaceAllocation (\s a -> s {_apviNewPrivateVirtualInterfaceAllocation = a})

instance AWSRequest AllocatePrivateVirtualInterface where
  type Rs AllocatePrivateVirtualInterface = VirtualInterface
  request = postJSON directConnect
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable AllocatePrivateVirtualInterface

instance NFData AllocatePrivateVirtualInterface

instance ToHeaders AllocatePrivateVirtualInterface where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("OvertureService.AllocatePrivateVirtualInterface" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AllocatePrivateVirtualInterface where
  toJSON AllocatePrivateVirtualInterface' {..} =
    object
      ( catMaybes
          [ Just ("connectionId" .= _apviConnectionId),
            Just ("ownerAccount" .= _apviOwnerAccount),
            Just
              ( "newPrivateVirtualInterfaceAllocation"
                  .= _apviNewPrivateVirtualInterfaceAllocation
              )
          ]
      )

instance ToPath AllocatePrivateVirtualInterface where
  toPath = const "/"

instance ToQuery AllocatePrivateVirtualInterface where
  toQuery = const mempty
