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
-- Module      : Network.AWS.DirectConnect.AllocatePublicVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a public virtual interface to be owned by the specified AWS account.
--
--
-- The owner of a connection calls this function to provision a public virtual interface to be owned by the specified AWS account.
--
-- Virtual interfaces created using this function must be confirmed by the owner using 'ConfirmPublicVirtualInterface' . Until this step has been completed, the virtual interface is in the @confirming@ state and is not available to handle traffic.
--
-- When creating an IPv6 public virtual interface, omit the Amazon address and customer address. IPv6 addresses are automatically assigned from the Amazon pool of IPv6 addresses; you cannot specify custom IPv6 addresses.
module Network.AWS.DirectConnect.AllocatePublicVirtualInterface
  ( -- * Creating a Request
    allocatePublicVirtualInterface,
    AllocatePublicVirtualInterface,

    -- * Request Lenses
    aConnectionId,
    aOwnerAccount,
    aNewPublicVirtualInterfaceAllocation,

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

-- | /See:/ 'allocatePublicVirtualInterface' smart constructor.
data AllocatePublicVirtualInterface = AllocatePublicVirtualInterface'
  { _aConnectionId ::
      !Text,
    _aOwnerAccount :: !Text,
    _aNewPublicVirtualInterfaceAllocation ::
      !NewPublicVirtualInterfaceAllocation
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AllocatePublicVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aConnectionId' - The ID of the connection on which the public virtual interface is provisioned.
--
-- * 'aOwnerAccount' - The ID of the AWS account that owns the public virtual interface.
--
-- * 'aNewPublicVirtualInterfaceAllocation' - Information about the public virtual interface.
allocatePublicVirtualInterface ::
  -- | 'aConnectionId'
  Text ->
  -- | 'aOwnerAccount'
  Text ->
  -- | 'aNewPublicVirtualInterfaceAllocation'
  NewPublicVirtualInterfaceAllocation ->
  AllocatePublicVirtualInterface
allocatePublicVirtualInterface
  pConnectionId_
  pOwnerAccount_
  pNewPublicVirtualInterfaceAllocation_ =
    AllocatePublicVirtualInterface'
      { _aConnectionId = pConnectionId_,
        _aOwnerAccount = pOwnerAccount_,
        _aNewPublicVirtualInterfaceAllocation =
          pNewPublicVirtualInterfaceAllocation_
      }

-- | The ID of the connection on which the public virtual interface is provisioned.
aConnectionId :: Lens' AllocatePublicVirtualInterface Text
aConnectionId = lens _aConnectionId (\s a -> s {_aConnectionId = a})

-- | The ID of the AWS account that owns the public virtual interface.
aOwnerAccount :: Lens' AllocatePublicVirtualInterface Text
aOwnerAccount = lens _aOwnerAccount (\s a -> s {_aOwnerAccount = a})

-- | Information about the public virtual interface.
aNewPublicVirtualInterfaceAllocation :: Lens' AllocatePublicVirtualInterface NewPublicVirtualInterfaceAllocation
aNewPublicVirtualInterfaceAllocation = lens _aNewPublicVirtualInterfaceAllocation (\s a -> s {_aNewPublicVirtualInterfaceAllocation = a})

instance AWSRequest AllocatePublicVirtualInterface where
  type Rs AllocatePublicVirtualInterface = VirtualInterface
  request = postJSON directConnect
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable AllocatePublicVirtualInterface

instance NFData AllocatePublicVirtualInterface

instance ToHeaders AllocatePublicVirtualInterface where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("OvertureService.AllocatePublicVirtualInterface" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AllocatePublicVirtualInterface where
  toJSON AllocatePublicVirtualInterface' {..} =
    object
      ( catMaybes
          [ Just ("connectionId" .= _aConnectionId),
            Just ("ownerAccount" .= _aOwnerAccount),
            Just
              ( "newPublicVirtualInterfaceAllocation"
                  .= _aNewPublicVirtualInterfaceAllocation
              )
          ]
      )

instance ToPath AllocatePublicVirtualInterface where
  toPath = const "/"

instance ToQuery AllocatePublicVirtualInterface where
  toQuery = const mempty
