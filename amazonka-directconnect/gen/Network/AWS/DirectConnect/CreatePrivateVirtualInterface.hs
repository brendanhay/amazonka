{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.DirectConnect.CreatePrivateVirtualInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a new private virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A private virtual interface
-- supports sending traffic to a single virtual private cloud (VPC).
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_CreatePrivateVirtualInterface.html>
module Network.AWS.DirectConnect.CreatePrivateVirtualInterface
    (
    -- * Request
      CreatePrivateVirtualInterface
    -- ** Request constructor
    , createPrivateVirtualInterface
    -- ** Request lenses
    , creConnectionId
    , creNewPrivateVirtualInterface

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

-- | Container for the parameters to the CreatePrivateVirtualInterface
-- operation.
--
-- /See:/ 'createPrivateVirtualInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creConnectionId'
--
-- * 'creNewPrivateVirtualInterface'
data CreatePrivateVirtualInterface = CreatePrivateVirtualInterface'
    { _creConnectionId               :: !Text
    , _creNewPrivateVirtualInterface :: !NewPrivateVirtualInterface
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePrivateVirtualInterface' smart constructor.
createPrivateVirtualInterface :: Text -> NewPrivateVirtualInterface -> CreatePrivateVirtualInterface
createPrivateVirtualInterface pConnectionId pNewPrivateVirtualInterface =
    CreatePrivateVirtualInterface'
    { _creConnectionId = pConnectionId
    , _creNewPrivateVirtualInterface = pNewPrivateVirtualInterface
    }

-- | FIXME: Undocumented member.
creConnectionId :: Lens' CreatePrivateVirtualInterface Text
creConnectionId = lens _creConnectionId (\ s a -> s{_creConnectionId = a});

-- | Detailed information for the private virtual interface to be created.
--
-- Default: None
creNewPrivateVirtualInterface :: Lens' CreatePrivateVirtualInterface NewPrivateVirtualInterface
creNewPrivateVirtualInterface = lens _creNewPrivateVirtualInterface (\ s a -> s{_creNewPrivateVirtualInterface = a});

instance AWSRequest CreatePrivateVirtualInterface
         where
        type Sv CreatePrivateVirtualInterface = DirectConnect
        type Rs CreatePrivateVirtualInterface =
             VirtualInterface
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CreatePrivateVirtualInterface
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.CreatePrivateVirtualInterface" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePrivateVirtualInterface where
        toJSON CreatePrivateVirtualInterface'{..}
          = object
              ["connectionId" .= _creConnectionId,
               "newPrivateVirtualInterface" .=
                 _creNewPrivateVirtualInterface]

instance ToPath CreatePrivateVirtualInterface where
        toPath = const "/"

instance ToQuery CreatePrivateVirtualInterface where
        toQuery = const mempty
