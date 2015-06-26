{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DirectConnect.AllocatePublicVirtualInterface
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Provisions a public virtual interface to be owned by a different
-- customer.
--
-- The owner of a connection calls this function to provision a public
-- virtual interface which will be owned by another AWS customer.
--
-- Virtual interfaces created using this function must be confirmed by the
-- virtual interface owner by calling ConfirmPublicVirtualInterface. Until
-- this step has been completed, the virtual interface will be in
-- \'Confirming\' state, and will not be available for handling traffic.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_AllocatePublicVirtualInterface.html>
module Network.AWS.DirectConnect.AllocatePublicVirtualInterface
    (
    -- * Request
      AllocatePublicVirtualInterface
    -- ** Request constructor
    , allocatePublicVirtualInterface
    -- ** Request lenses
    , allConnectionId
    , allOwnerAccount
    , allNewPublicVirtualInterfaceAllocation

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

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the AllocatePublicVirtualInterface
-- operation.
--
-- /See:/ 'allocatePublicVirtualInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'allConnectionId'
--
-- * 'allOwnerAccount'
--
-- * 'allNewPublicVirtualInterfaceAllocation'
data AllocatePublicVirtualInterface = AllocatePublicVirtualInterface'{_allConnectionId :: Text, _allOwnerAccount :: Text, _allNewPublicVirtualInterfaceAllocation :: NewPublicVirtualInterfaceAllocation} deriving (Eq, Read, Show)

-- | 'AllocatePublicVirtualInterface' smart constructor.
allocatePublicVirtualInterface :: Text -> Text -> NewPublicVirtualInterfaceAllocation -> AllocatePublicVirtualInterface
allocatePublicVirtualInterface pConnectionId pOwnerAccount pNewPublicVirtualInterfaceAllocation = AllocatePublicVirtualInterface'{_allConnectionId = pConnectionId, _allOwnerAccount = pOwnerAccount, _allNewPublicVirtualInterfaceAllocation = pNewPublicVirtualInterfaceAllocation};

-- | The connection ID on which the public virtual interface is provisioned.
--
-- Default: None
allConnectionId :: Lens' AllocatePublicVirtualInterface Text
allConnectionId = lens _allConnectionId (\ s a -> s{_allConnectionId = a});

-- | The AWS account that will own the new public virtual interface.
--
-- Default: None
allOwnerAccount :: Lens' AllocatePublicVirtualInterface Text
allOwnerAccount = lens _allOwnerAccount (\ s a -> s{_allOwnerAccount = a});

-- | Detailed information for the public virtual interface to be provisioned.
--
-- Default: None
allNewPublicVirtualInterfaceAllocation :: Lens' AllocatePublicVirtualInterface NewPublicVirtualInterfaceAllocation
allNewPublicVirtualInterfaceAllocation = lens _allNewPublicVirtualInterfaceAllocation (\ s a -> s{_allNewPublicVirtualInterfaceAllocation = a});

instance AWSRequest AllocatePublicVirtualInterface
         where
        type Sv AllocatePublicVirtualInterface =
             DirectConnect
        type Rs AllocatePublicVirtualInterface =
             VirtualInterface
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

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
              ["connectionId" .= _allConnectionId,
               "ownerAccount" .= _allOwnerAccount,
               "newPublicVirtualInterfaceAllocation" .=
                 _allNewPublicVirtualInterfaceAllocation]

instance ToPath AllocatePublicVirtualInterface where
        toPath = const "/"

instance ToQuery AllocatePublicVirtualInterface where
        toQuery = const mempty
