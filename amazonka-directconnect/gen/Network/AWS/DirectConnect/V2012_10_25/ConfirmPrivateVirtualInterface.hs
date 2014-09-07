{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.ConfirmPrivateVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Accept ownership of a private virtual interface created by another
-- customer. After the virtual interface owner calls this function, the
-- virtual interface will be created and attached to the given virtual private
-- gateway, and will be available for handling traffic.
module Network.AWS.DirectConnect.V2012_10_25.ConfirmPrivateVirtualInterface
    (
    -- * Request
      ConfirmPrivateVirtualInterface
    -- ** Request constructor
    , mkConfirmPrivateVirtualInterface
    -- ** Request lenses
    , cpviVirtualInterfaceId
    , cpviVirtualGatewayId

    -- * Response
    , ConfirmPrivateVirtualInterfaceResponse
    -- ** Response lenses
    , cpvirsVirtualInterfaceState
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Container for the parameters to the ConfirmPrivateVirtualInterface
-- operation.
data ConfirmPrivateVirtualInterface = ConfirmPrivateVirtualInterface
    { _cpviVirtualInterfaceId :: Text
    , _cpviVirtualGatewayId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ConfirmPrivateVirtualInterface' request.
mkConfirmPrivateVirtualInterface :: Text -- ^ 'cpviVirtualInterfaceId'
                                 -> Text -- ^ 'cpviVirtualGatewayId'
                                 -> ConfirmPrivateVirtualInterface
mkConfirmPrivateVirtualInterface p1 p2 = ConfirmPrivateVirtualInterface
    { _cpviVirtualInterfaceId = p1
    , _cpviVirtualGatewayId = p2
    }

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
cpviVirtualInterfaceId :: Lens' ConfirmPrivateVirtualInterface Text
cpviVirtualInterfaceId =
    lens _cpviVirtualInterfaceId (\s a -> s { _cpviVirtualInterfaceId = a })

-- | ID of the virtual private gateway that will be attached to the virtual
-- interface. A virtual private gateway can be managed via the Amazon Virtual
-- Private Cloud (VPC) console or the EC2 CreateVpnGateway action. Default:
-- None.
cpviVirtualGatewayId :: Lens' ConfirmPrivateVirtualInterface Text
cpviVirtualGatewayId =
    lens _cpviVirtualGatewayId (\s a -> s { _cpviVirtualGatewayId = a })

instance ToPath ConfirmPrivateVirtualInterface

instance ToQuery ConfirmPrivateVirtualInterface

instance ToHeaders ConfirmPrivateVirtualInterface

instance ToJSON ConfirmPrivateVirtualInterface

-- | The response received when ConfirmPrivateVirtualInterface is called.
newtype ConfirmPrivateVirtualInterfaceResponse = ConfirmPrivateVirtualInterfaceResponse
    { _cpvirsVirtualInterfaceState :: Maybe VirtualInterfaceState
    } deriving (Show, Generic)

-- | State of the virtual interface. Confirming: The creation of the virtual
-- interface is pending confirmation from the virtual interface owner. If the
-- owner of the virtual interface is different from the owner of the
-- connection on which it is provisioned, then the virtual interface will
-- remain in this state until it is confirmed by the virtual interface owner.
-- Verifying: This state only applies to public virtual interfaces. Each
-- public virtual interface needs validation before the virtual interface can
-- be created. Pending: A virtual interface is in this state from the time
-- that it is created until the virtual interface is ready to forward traffic.
-- Available: A virtual interface that is able to forward traffic. Deleting: A
-- virtual interface is in this state immediately after calling
-- DeleteVirtualInterface until it can no longer forward traffic. Deleted: A
-- virtual interface that cannot forward traffic. Rejected: The virtual
-- interface owner has declined creation of the virtual interface. If a
-- virtual interface in the 'Confirming' state is deleted by the virtual
-- interface owner, the virtual interface will enter the 'Rejected' state.
cpvirsVirtualInterfaceState :: Lens' ConfirmPrivateVirtualInterfaceResponse (Maybe VirtualInterfaceState)
cpvirsVirtualInterfaceState =
    lens _cpvirsVirtualInterfaceState
         (\s a -> s { _cpvirsVirtualInterfaceState = a })

instance FromJSON ConfirmPrivateVirtualInterfaceResponse

instance AWSRequest ConfirmPrivateVirtualInterface where
    type Sv ConfirmPrivateVirtualInterface = DirectConnect
    type Rs ConfirmPrivateVirtualInterface = ConfirmPrivateVirtualInterfaceResponse

    request = get
    response _ = jsonResponse
