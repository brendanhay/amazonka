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
-- Module      : Network.AWS.DirectConnect.UpdateVirtualInterfaceAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the specified virtual private interface.
--
--
-- Setting the MTU of a virtual interface to 9001 (jumbo frames) can cause an update to the underlying physical connection if it wasn't updated to support jumbo frames. Updating the connection disrupts network connectivity for all virtual interfaces associated with the connection for up to 30 seconds. To check whether your connection supports jumbo frames, call 'DescribeConnections' . To check whether your virtual interface supports jumbo frames, call 'DescribeVirtualInterfaces' .
--
module Network.AWS.DirectConnect.UpdateVirtualInterfaceAttributes
    (
    -- * Creating a Request
      updateVirtualInterfaceAttributes
    , UpdateVirtualInterfaceAttributes
    -- * Request Lenses
    , uviaMtu
    , uviaVirtualInterfaceId

    -- * Destructuring the Response
    , virtualInterface
    , VirtualInterface
    -- * Response Lenses
    , viBgpPeers
    , viVirtualGatewayId
    , viMtu
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
    , viJumboFrameCapable
    , viCustomerRouterConfig
    , viOwnerAccount
    , viRegion
    , viVirtualInterfaceName
    , viAwsDeviceV2
    , viVirtualInterfaceId
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateVirtualInterfaceAttributes' smart constructor.
data UpdateVirtualInterfaceAttributes = UpdateVirtualInterfaceAttributes'
  { _uviaMtu                :: !(Maybe Int)
  , _uviaVirtualInterfaceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateVirtualInterfaceAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uviaMtu' - The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- * 'uviaVirtualInterfaceId' - The ID of the virtual private interface.
updateVirtualInterfaceAttributes
    :: Text -- ^ 'uviaVirtualInterfaceId'
    -> UpdateVirtualInterfaceAttributes
updateVirtualInterfaceAttributes pVirtualInterfaceId_ =
  UpdateVirtualInterfaceAttributes'
    {_uviaMtu = Nothing, _uviaVirtualInterfaceId = pVirtualInterfaceId_}


-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
uviaMtu :: Lens' UpdateVirtualInterfaceAttributes (Maybe Int)
uviaMtu = lens _uviaMtu (\ s a -> s{_uviaMtu = a})

-- | The ID of the virtual private interface.
uviaVirtualInterfaceId :: Lens' UpdateVirtualInterfaceAttributes Text
uviaVirtualInterfaceId = lens _uviaVirtualInterfaceId (\ s a -> s{_uviaVirtualInterfaceId = a})

instance AWSRequest UpdateVirtualInterfaceAttributes
         where
        type Rs UpdateVirtualInterfaceAttributes =
             VirtualInterface
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateVirtualInterfaceAttributes
         where

instance NFData UpdateVirtualInterfaceAttributes
         where

instance ToHeaders UpdateVirtualInterfaceAttributes
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.UpdateVirtualInterfaceAttributes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateVirtualInterfaceAttributes
         where
        toJSON UpdateVirtualInterfaceAttributes'{..}
          = object
              (catMaybes
                 [("mtu" .=) <$> _uviaMtu,
                  Just
                    ("virtualInterfaceId" .= _uviaVirtualInterfaceId)])

instance ToPath UpdateVirtualInterfaceAttributes
         where
        toPath = const "/"

instance ToQuery UpdateVirtualInterfaceAttributes
         where
        toQuery = const mempty
