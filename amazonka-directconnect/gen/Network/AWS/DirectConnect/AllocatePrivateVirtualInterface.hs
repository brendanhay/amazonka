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
-- Module      : Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a private virtual interface to be owned by a different
-- customer.
--
-- The owner of a connection calls this function to provision a private
-- virtual interface which will be owned by another AWS customer.
--
-- Virtual interfaces created using this function must be confirmed by the
-- virtual interface owner by calling ConfirmPrivateVirtualInterface. Until
-- this step has been completed, the virtual interface will be in
-- \'Confirming\' state, and will not be available for handling traffic.
--
-- /See:/ <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_AllocatePrivateVirtualInterface.html AWS API Reference> for AllocatePrivateVirtualInterface.
module Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
    (
    -- * Creating a Request
      allocatePrivateVirtualInterface
    , AllocatePrivateVirtualInterface
    -- * Request Lenses
    , apviConnectionId
    , apviOwnerAccount
    , apviNewPrivateVirtualInterfaceAllocation

    -- * Destructuring the Response
    , virtualInterface
    , VirtualInterface
    -- * Response Lenses
    , viVirtualGatewayId
    , viRouteFilterPrefixes
    , viCustomerAddress
    , viVlan
    , viLocation
    , viAmazonAddress
    , viVirtualInterfaceState
    , viConnectionId
    , viAsn
    , viVirtualInterfaceType
    , viAuthKey
    , viCustomerRouterConfig
    , viOwnerAccount
    , viVirtualInterfaceName
    , viVirtualInterfaceId
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.DirectConnect.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the AllocatePrivateVirtualInterface
-- operation.
--
-- /See:/ 'allocatePrivateVirtualInterface' smart constructor.
data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface'
    { _apviConnectionId                         :: !Text
    , _apviOwnerAccount                         :: !Text
    , _apviNewPrivateVirtualInterfaceAllocation :: !NewPrivateVirtualInterfaceAllocation
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AllocatePrivateVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apviConnectionId'
--
-- * 'apviOwnerAccount'
--
-- * 'apviNewPrivateVirtualInterfaceAllocation'
allocatePrivateVirtualInterface
    :: Text -- ^ 'apviConnectionId'
    -> Text -- ^ 'apviOwnerAccount'
    -> NewPrivateVirtualInterfaceAllocation -- ^ 'apviNewPrivateVirtualInterfaceAllocation'
    -> AllocatePrivateVirtualInterface
allocatePrivateVirtualInterface pConnectionId_ pOwnerAccount_ pNewPrivateVirtualInterfaceAllocation_ =
    AllocatePrivateVirtualInterface'
    { _apviConnectionId = pConnectionId_
    , _apviOwnerAccount = pOwnerAccount_
    , _apviNewPrivateVirtualInterfaceAllocation = pNewPrivateVirtualInterfaceAllocation_
    }

-- | The connection ID on which the private virtual interface is provisioned.
--
-- Default: None
apviConnectionId :: Lens' AllocatePrivateVirtualInterface Text
apviConnectionId = lens _apviConnectionId (\ s a -> s{_apviConnectionId = a});

-- | The AWS account that will own the new private virtual interface.
--
-- Default: None
apviOwnerAccount :: Lens' AllocatePrivateVirtualInterface Text
apviOwnerAccount = lens _apviOwnerAccount (\ s a -> s{_apviOwnerAccount = a});

-- | Detailed information for the private virtual interface to be
-- provisioned.
--
-- Default: None
apviNewPrivateVirtualInterfaceAllocation :: Lens' AllocatePrivateVirtualInterface NewPrivateVirtualInterfaceAllocation
apviNewPrivateVirtualInterfaceAllocation = lens _apviNewPrivateVirtualInterfaceAllocation (\ s a -> s{_apviNewPrivateVirtualInterfaceAllocation = a});

instance AWSRequest AllocatePrivateVirtualInterface
         where
        type Sv AllocatePrivateVirtualInterface =
             DirectConnect
        type Rs AllocatePrivateVirtualInterface =
             VirtualInterface
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders AllocatePrivateVirtualInterface
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.AllocatePrivateVirtualInterface" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AllocatePrivateVirtualInterface where
        toJSON AllocatePrivateVirtualInterface'{..}
          = object
              (catMaybes
                 [Just ("connectionId" .= _apviConnectionId),
                  Just ("ownerAccount" .= _apviOwnerAccount),
                  Just
                    ("newPrivateVirtualInterfaceAllocation" .=
                       _apviNewPrivateVirtualInterfaceAllocation)])

instance ToPath AllocatePrivateVirtualInterface where
        toPath = const "/"

instance ToQuery AllocatePrivateVirtualInterface
         where
        toQuery = const mempty
