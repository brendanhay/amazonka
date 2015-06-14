{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
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

-- | Accept ownership of a private virtual interface created by another
-- customer.
--
-- After the virtual interface owner calls this function, the virtual
-- interface will be created and attached to the given virtual private
-- gateway, and will be available for handling traffic.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_ConfirmPrivateVirtualInterface.html>
module Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
    (
    -- * Request
      ConfirmPrivateVirtualInterface
    -- ** Request constructor
    , confirmPrivateVirtualInterface
    -- ** Request lenses
    , cpviVirtualInterfaceId
    , cpviVirtualGatewayId

    -- * Response
    , ConfirmPrivateVirtualInterfaceResponse
    -- ** Response constructor
    , confirmPrivateVirtualInterfaceResponse
    -- ** Response lenses
    , cpvirVirtualInterfaceState
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.DirectConnect.Types

-- | /See:/ 'confirmPrivateVirtualInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpviVirtualInterfaceId'
--
-- * 'cpviVirtualGatewayId'
data ConfirmPrivateVirtualInterface = ConfirmPrivateVirtualInterface'{_cpviVirtualInterfaceId :: Text, _cpviVirtualGatewayId :: Text} deriving (Eq, Read, Show)

-- | 'ConfirmPrivateVirtualInterface' smart constructor.
confirmPrivateVirtualInterface :: Text -> Text -> ConfirmPrivateVirtualInterface
confirmPrivateVirtualInterface pVirtualInterfaceId pVirtualGatewayId = ConfirmPrivateVirtualInterface'{_cpviVirtualInterfaceId = pVirtualInterfaceId, _cpviVirtualGatewayId = pVirtualGatewayId};

-- | FIXME: Undocumented member.
cpviVirtualInterfaceId :: Lens' ConfirmPrivateVirtualInterface Text
cpviVirtualInterfaceId = lens _cpviVirtualInterfaceId (\ s a -> s{_cpviVirtualInterfaceId = a});

-- | ID of the virtual private gateway that will be attached to the virtual
-- interface.
--
-- A virtual private gateway can be managed via the Amazon Virtual Private
-- Cloud (VPC) console or the
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html EC2 CreateVpnGateway>
-- action.
--
-- Default: None
cpviVirtualGatewayId :: Lens' ConfirmPrivateVirtualInterface Text
cpviVirtualGatewayId = lens _cpviVirtualGatewayId (\ s a -> s{_cpviVirtualGatewayId = a});

instance AWSRequest ConfirmPrivateVirtualInterface
         where
        type Sv ConfirmPrivateVirtualInterface =
             DirectConnect
        type Rs ConfirmPrivateVirtualInterface =
             ConfirmPrivateVirtualInterfaceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ConfirmPrivateVirtualInterfaceResponse' <$>
                   x .?> "virtualInterfaceState")

instance ToHeaders ConfirmPrivateVirtualInterface
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.ConfirmPrivateVirtualInterface" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ConfirmPrivateVirtualInterface where
        toJSON ConfirmPrivateVirtualInterface'{..}
          = object
              ["virtualInterfaceId" .= _cpviVirtualInterfaceId,
               "virtualGatewayId" .= _cpviVirtualGatewayId]

instance ToPath ConfirmPrivateVirtualInterface where
        toPath = const "/"

instance ToQuery ConfirmPrivateVirtualInterface where
        toQuery = const mempty

-- | /See:/ 'confirmPrivateVirtualInterfaceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpvirVirtualInterfaceState'
newtype ConfirmPrivateVirtualInterfaceResponse = ConfirmPrivateVirtualInterfaceResponse'{_cpvirVirtualInterfaceState :: Maybe VirtualInterfaceState} deriving (Eq, Read, Show)

-- | 'ConfirmPrivateVirtualInterfaceResponse' smart constructor.
confirmPrivateVirtualInterfaceResponse :: ConfirmPrivateVirtualInterfaceResponse
confirmPrivateVirtualInterfaceResponse = ConfirmPrivateVirtualInterfaceResponse'{_cpvirVirtualInterfaceState = Nothing};

-- | FIXME: Undocumented member.
cpvirVirtualInterfaceState :: Lens' ConfirmPrivateVirtualInterfaceResponse (Maybe VirtualInterfaceState)
cpvirVirtualInterfaceState = lens _cpvirVirtualInterfaceState (\ s a -> s{_cpvirVirtualInterfaceState = a});
