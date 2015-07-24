{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_AllocatePrivateVirtualInterface.html>
module Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
    (
    -- * Request
      AllocatePrivateVirtualInterface
    -- ** Request constructor
    , allocatePrivateVirtualInterface
    -- ** Request lenses
    , apviConnectionId
    , apviOwnerAccount
    , apviNewPrivateVirtualInterfaceAllocation

    -- * Response
    , VirtualInterface
    -- ** Response constructor
    , virtualInterface
    -- ** Response lenses
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
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the AllocatePrivateVirtualInterface
-- operation.
--
-- /See:/ 'allocatePrivateVirtualInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apviConnectionId'
--
-- * 'apviOwnerAccount'
--
-- * 'apviNewPrivateVirtualInterfaceAllocation'
data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface'
    { _apviConnectionId                         :: !Text
    , _apviOwnerAccount                         :: !Text
    , _apviNewPrivateVirtualInterfaceAllocation :: !NewPrivateVirtualInterfaceAllocation
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AllocatePrivateVirtualInterface' smart constructor.
allocatePrivateVirtualInterface :: Text -> Text -> NewPrivateVirtualInterfaceAllocation -> AllocatePrivateVirtualInterface
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
        request = postJSON "AllocatePrivateVirtualInterface"
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
              ["connectionId" .= _apviConnectionId,
               "ownerAccount" .= _apviOwnerAccount,
               "newPrivateVirtualInterfaceAllocation" .=
                 _apviNewPrivateVirtualInterfaceAllocation]

instance ToPath AllocatePrivateVirtualInterface where
        toPath = const "/"

instance ToQuery AllocatePrivateVirtualInterface
         where
        toQuery = const mempty
