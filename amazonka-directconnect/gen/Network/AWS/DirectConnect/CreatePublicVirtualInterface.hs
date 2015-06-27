{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DirectConnect.CreatePublicVirtualInterface
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

-- | Creates a new public virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A public virtual interface
-- supports sending traffic to public services of AWS such as Amazon Simple
-- Storage Service (Amazon S3).
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_CreatePublicVirtualInterface.html>
module Network.AWS.DirectConnect.CreatePublicVirtualInterface
    (
    -- * Request
      CreatePublicVirtualInterface
    -- ** Request constructor
    , createPublicVirtualInterface
    -- ** Request lenses
    , cpviConnectionId
    , cpviNewPublicVirtualInterface

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

-- | Container for the parameters to the CreatePublicVirtualInterface
-- operation.
--
-- /See:/ 'createPublicVirtualInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpviConnectionId'
--
-- * 'cpviNewPublicVirtualInterface'
data CreatePublicVirtualInterface = CreatePublicVirtualInterface'
    { _cpviConnectionId              :: Text
    , _cpviNewPublicVirtualInterface :: NewPublicVirtualInterface
    } deriving (Eq,Read,Show)

-- | 'CreatePublicVirtualInterface' smart constructor.
createPublicVirtualInterface :: Text -> NewPublicVirtualInterface -> CreatePublicVirtualInterface
createPublicVirtualInterface pConnectionId pNewPublicVirtualInterface =
    CreatePublicVirtualInterface'
    { _cpviConnectionId = pConnectionId
    , _cpviNewPublicVirtualInterface = pNewPublicVirtualInterface
    }

-- | FIXME: Undocumented member.
cpviConnectionId :: Lens' CreatePublicVirtualInterface Text
cpviConnectionId = lens _cpviConnectionId (\ s a -> s{_cpviConnectionId = a});

-- | Detailed information for the public virtual interface to be created.
--
-- Default: None
cpviNewPublicVirtualInterface :: Lens' CreatePublicVirtualInterface NewPublicVirtualInterface
cpviNewPublicVirtualInterface = lens _cpviNewPublicVirtualInterface (\ s a -> s{_cpviNewPublicVirtualInterface = a});

instance AWSRequest CreatePublicVirtualInterface
         where
        type Sv CreatePublicVirtualInterface = DirectConnect
        type Rs CreatePublicVirtualInterface =
             VirtualInterface
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CreatePublicVirtualInterface where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.CreatePublicVirtualInterface" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePublicVirtualInterface where
        toJSON CreatePublicVirtualInterface'{..}
          = object
              ["connectionId" .= _cpviConnectionId,
               "newPublicVirtualInterface" .=
                 _cpviNewPublicVirtualInterface]

instance ToPath CreatePublicVirtualInterface where
        toPath = const "/"

instance ToQuery CreatePublicVirtualInterface where
        toQuery = const mempty
