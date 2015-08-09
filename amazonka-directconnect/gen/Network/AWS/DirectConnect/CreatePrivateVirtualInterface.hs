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
-- Module      : Network.AWS.DirectConnect.CreatePrivateVirtualInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new private virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A private virtual interface
-- supports sending traffic to a single virtual private cloud (VPC).
--
-- /See:/ <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_CreatePrivateVirtualInterface.html AWS API Reference> for CreatePrivateVirtualInterface.
module Network.AWS.DirectConnect.CreatePrivateVirtualInterface
    (
    -- * Creating a Request
      CreatePrivateVirtualInterface
    , createPrivateVirtualInterface
    -- * Request Lenses
    , creConnectionId
    , creNewPrivateVirtualInterface

    -- * Destructuring the Response
    , VirtualInterface
    , virtualInterface
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
createPrivateVirtualInterface pConnectionId_ pNewPrivateVirtualInterface_ =
    CreatePrivateVirtualInterface'
    { _creConnectionId = pConnectionId_
    , _creNewPrivateVirtualInterface = pNewPrivateVirtualInterface_
    }

-- | Undocumented member.
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
