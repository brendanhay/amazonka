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
-- Module      : Network.AWS.DirectConnect.AllocatePublicVirtualInterface
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a public virtual interface to be owned by a different customer.
--
--
-- The owner of a connection calls this function to provision a public virtual interface which will be owned by another AWS customer.
--
-- Virtual interfaces created using this function must be confirmed by the virtual interface owner by calling ConfirmPublicVirtualInterface. Until this step has been completed, the virtual interface will be in 'Confirming' state, and will not be available for handling traffic.
--
-- When creating an IPv6 public virtual interface (addressFamily is 'ipv6'), the customer and amazon address fields should be left blank to use auto-assigned IPv6 space. Custom IPv6 Addresses are currently not supported.
--
module Network.AWS.DirectConnect.AllocatePublicVirtualInterface
    (
    -- * Creating a Request
      allocatePublicVirtualInterface
    , AllocatePublicVirtualInterface
    -- * Request Lenses
    , aConnectionId
    , aOwnerAccount
    , aNewPublicVirtualInterfaceAllocation

    -- * Destructuring the Response
    , virtualInterface
    , VirtualInterface
    -- * Response Lenses
    , viBgpPeers
    , viVirtualGatewayId
    , viRouteFilterPrefixes
    , viCustomerAddress
    , viVlan
    , viLocation
    , viAmazonAddress
    , viAddressFamily
    , viVirtualInterfaceState
    , viConnectionId
    , viDirectConnectGatewayId
    , viAmazonSideASN
    , viVirtualInterfaceType
    , viAsn
    , viAuthKey
    , viCustomerRouterConfig
    , viOwnerAccount
    , viVirtualInterfaceName
    , viVirtualInterfaceId
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the AllocatePublicVirtualInterface operation.
--
--
--
-- /See:/ 'allocatePublicVirtualInterface' smart constructor.
data AllocatePublicVirtualInterface = AllocatePublicVirtualInterface'
  { _aConnectionId :: !Text
  , _aOwnerAccount :: !Text
  , _aNewPublicVirtualInterfaceAllocation :: !NewPublicVirtualInterfaceAllocation
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AllocatePublicVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aConnectionId' - The connection ID on which the public virtual interface is provisioned. Default: None
--
-- * 'aOwnerAccount' - The AWS account that will own the new public virtual interface. Default: None
--
-- * 'aNewPublicVirtualInterfaceAllocation' - Detailed information for the public virtual interface to be provisioned. Default: None
allocatePublicVirtualInterface
    :: Text -- ^ 'aConnectionId'
    -> Text -- ^ 'aOwnerAccount'
    -> NewPublicVirtualInterfaceAllocation -- ^ 'aNewPublicVirtualInterfaceAllocation'
    -> AllocatePublicVirtualInterface
allocatePublicVirtualInterface pConnectionId_ pOwnerAccount_ pNewPublicVirtualInterfaceAllocation_ =
  AllocatePublicVirtualInterface'
    { _aConnectionId = pConnectionId_
    , _aOwnerAccount = pOwnerAccount_
    , _aNewPublicVirtualInterfaceAllocation =
        pNewPublicVirtualInterfaceAllocation_
    }


-- | The connection ID on which the public virtual interface is provisioned. Default: None
aConnectionId :: Lens' AllocatePublicVirtualInterface Text
aConnectionId = lens _aConnectionId (\ s a -> s{_aConnectionId = a})

-- | The AWS account that will own the new public virtual interface. Default: None
aOwnerAccount :: Lens' AllocatePublicVirtualInterface Text
aOwnerAccount = lens _aOwnerAccount (\ s a -> s{_aOwnerAccount = a})

-- | Detailed information for the public virtual interface to be provisioned. Default: None
aNewPublicVirtualInterfaceAllocation :: Lens' AllocatePublicVirtualInterface NewPublicVirtualInterfaceAllocation
aNewPublicVirtualInterfaceAllocation = lens _aNewPublicVirtualInterfaceAllocation (\ s a -> s{_aNewPublicVirtualInterfaceAllocation = a})

instance AWSRequest AllocatePublicVirtualInterface
         where
        type Rs AllocatePublicVirtualInterface =
             VirtualInterface
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable AllocatePublicVirtualInterface
         where

instance NFData AllocatePublicVirtualInterface where

instance ToHeaders AllocatePublicVirtualInterface
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.AllocatePublicVirtualInterface" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AllocatePublicVirtualInterface where
        toJSON AllocatePublicVirtualInterface'{..}
          = object
              (catMaybes
                 [Just ("connectionId" .= _aConnectionId),
                  Just ("ownerAccount" .= _aOwnerAccount),
                  Just
                    ("newPublicVirtualInterfaceAllocation" .=
                       _aNewPublicVirtualInterfaceAllocation)])

instance ToPath AllocatePublicVirtualInterface where
        toPath = const "/"

instance ToQuery AllocatePublicVirtualInterface where
        toQuery = const mempty
