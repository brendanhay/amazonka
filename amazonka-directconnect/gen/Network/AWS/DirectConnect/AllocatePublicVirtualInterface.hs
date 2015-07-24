{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AllocatePublicVirtualInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Provisions a public virtual interface to be owned by a different
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
    , aConnectionId
    , aOwnerAccount
    , aNewPublicVirtualInterfaceAllocation

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

-- | Container for the parameters to the AllocatePublicVirtualInterface
-- operation.
--
-- /See:/ 'allocatePublicVirtualInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aConnectionId'
--
-- * 'aOwnerAccount'
--
-- * 'aNewPublicVirtualInterfaceAllocation'
data AllocatePublicVirtualInterface = AllocatePublicVirtualInterface'
    { _aConnectionId                        :: !Text
    , _aOwnerAccount                        :: !Text
    , _aNewPublicVirtualInterfaceAllocation :: !NewPublicVirtualInterfaceAllocation
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AllocatePublicVirtualInterface' smart constructor.
allocatePublicVirtualInterface :: Text -> Text -> NewPublicVirtualInterfaceAllocation -> AllocatePublicVirtualInterface
allocatePublicVirtualInterface pConnectionId_ pOwnerAccount_ pNewPublicVirtualInterfaceAllocation_ =
    AllocatePublicVirtualInterface'
    { _aConnectionId = pConnectionId_
    , _aOwnerAccount = pOwnerAccount_
    , _aNewPublicVirtualInterfaceAllocation = pNewPublicVirtualInterfaceAllocation_
    }

-- | The connection ID on which the public virtual interface is provisioned.
--
-- Default: None
aConnectionId :: Lens' AllocatePublicVirtualInterface Text
aConnectionId = lens _aConnectionId (\ s a -> s{_aConnectionId = a});

-- | The AWS account that will own the new public virtual interface.
--
-- Default: None
aOwnerAccount :: Lens' AllocatePublicVirtualInterface Text
aOwnerAccount = lens _aOwnerAccount (\ s a -> s{_aOwnerAccount = a});

-- | Detailed information for the public virtual interface to be provisioned.
--
-- Default: None
aNewPublicVirtualInterfaceAllocation :: Lens' AllocatePublicVirtualInterface NewPublicVirtualInterfaceAllocation
aNewPublicVirtualInterfaceAllocation = lens _aNewPublicVirtualInterfaceAllocation (\ s a -> s{_aNewPublicVirtualInterfaceAllocation = a});

instance AWSRequest AllocatePublicVirtualInterface
         where
        type Sv AllocatePublicVirtualInterface =
             DirectConnect
        type Rs AllocatePublicVirtualInterface =
             VirtualInterface
        request = postJSON "AllocatePublicVirtualInterface"
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
              ["connectionId" .= _aConnectionId,
               "ownerAccount" .= _aOwnerAccount,
               "newPublicVirtualInterfaceAllocation" .=
                 _aNewPublicVirtualInterfaceAllocation]

instance ToPath AllocatePublicVirtualInterface where
        toPath = const "/"

instance ToQuery AllocatePublicVirtualInterface where
        toQuery = const mempty
