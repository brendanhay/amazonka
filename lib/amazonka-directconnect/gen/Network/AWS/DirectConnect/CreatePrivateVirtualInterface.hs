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
-- Module      : Network.AWS.DirectConnect.CreatePrivateVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a private virtual interface. A virtual interface is the VLAN that transports AWS Direct Connect traffic. A private virtual interface can be connected to either a Direct Connect gateway or a Virtual Private Gateway (VGW). Connecting the private virtual interface to a Direct Connect gateway enables the possibility for connecting to multiple VPCs, including VPCs in different AWS Regions. Connecting the private virtual interface to a VGW only provides access to a single VPC within the same Region.
--
--
-- Setting the MTU of a virtual interface to 9001 (jumbo frames) can cause an update to the underlying physical connection if it wasn't updated to support jumbo frames. Updating the connection disrupts network connectivity for all virtual interfaces associated with the connection for up to 30 seconds. To check whether your connection supports jumbo frames, call 'DescribeConnections' . To check whether your virtual interface supports jumbo frames, call 'DescribeVirtualInterfaces' .
module Network.AWS.DirectConnect.CreatePrivateVirtualInterface
  ( -- * Creating a Request
    createPrivateVirtualInterface,
    CreatePrivateVirtualInterface,

    -- * Request Lenses
    creConnectionId,
    creNewPrivateVirtualInterface,

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

-- | /See:/ 'createPrivateVirtualInterface' smart constructor.
data CreatePrivateVirtualInterface = CreatePrivateVirtualInterface'
  { _creConnectionId ::
      !Text,
    _creNewPrivateVirtualInterface ::
      !NewPrivateVirtualInterface
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePrivateVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creConnectionId' - The ID of the connection.
--
-- * 'creNewPrivateVirtualInterface' - Information about the private virtual interface.
createPrivateVirtualInterface ::
  -- | 'creConnectionId'
  Text ->
  -- | 'creNewPrivateVirtualInterface'
  NewPrivateVirtualInterface ->
  CreatePrivateVirtualInterface
createPrivateVirtualInterface
  pConnectionId_
  pNewPrivateVirtualInterface_ =
    CreatePrivateVirtualInterface'
      { _creConnectionId = pConnectionId_,
        _creNewPrivateVirtualInterface = pNewPrivateVirtualInterface_
      }

-- | The ID of the connection.
creConnectionId :: Lens' CreatePrivateVirtualInterface Text
creConnectionId = lens _creConnectionId (\s a -> s {_creConnectionId = a})

-- | Information about the private virtual interface.
creNewPrivateVirtualInterface :: Lens' CreatePrivateVirtualInterface NewPrivateVirtualInterface
creNewPrivateVirtualInterface = lens _creNewPrivateVirtualInterface (\s a -> s {_creNewPrivateVirtualInterface = a})

instance AWSRequest CreatePrivateVirtualInterface where
  type Rs CreatePrivateVirtualInterface = VirtualInterface
  request = postJSON directConnect
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable CreatePrivateVirtualInterface

instance NFData CreatePrivateVirtualInterface

instance ToHeaders CreatePrivateVirtualInterface where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("OvertureService.CreatePrivateVirtualInterface" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreatePrivateVirtualInterface where
  toJSON CreatePrivateVirtualInterface' {..} =
    object
      ( catMaybes
          [ Just ("connectionId" .= _creConnectionId),
            Just
              ("newPrivateVirtualInterface" .= _creNewPrivateVirtualInterface)
          ]
      )

instance ToPath CreatePrivateVirtualInterface where
  toPath = const "/"

instance ToQuery CreatePrivateVirtualInterface where
  toQuery = const mempty
