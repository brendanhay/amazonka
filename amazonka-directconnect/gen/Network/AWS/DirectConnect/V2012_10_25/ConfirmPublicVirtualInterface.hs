{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.ConfirmPublicVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Accept ownership of a public virtual interface created by another customer.
-- After the virtual interface owner calls this function, the specified
-- virtual interface will be created and made available for handling traffic.
module Network.AWS.DirectConnect.V2012_10_25.ConfirmPublicVirtualInterface
    (
    -- * Request
      ConfirmPublicVirtualInterface
    -- ** Request constructor
    , mkConfirmPublicVirtualInterface
    -- ** Request lenses
    , cpvi1VirtualInterfaceId

    -- * Response
    , ConfirmPublicVirtualInterfaceResponse
    -- ** Response lenses
    , cpvirsrsVirtualInterfaceState
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Container for the parameters to the ConfirmPublicVirtualInterface
-- operation.
newtype ConfirmPublicVirtualInterface = ConfirmPublicVirtualInterface
    { _cpvi1VirtualInterfaceId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ConfirmPublicVirtualInterface' request.
mkConfirmPublicVirtualInterface :: Text -- ^ 'cpvi1VirtualInterfaceId'
                                -> ConfirmPublicVirtualInterface
mkConfirmPublicVirtualInterface p1 = ConfirmPublicVirtualInterface
    { _cpvi1VirtualInterfaceId = p1
    }
{-# INLINE mkConfirmPublicVirtualInterface #-}

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
cpvi1VirtualInterfaceId :: Lens' ConfirmPublicVirtualInterface Text
cpvi1VirtualInterfaceId =
    lens _cpvi1VirtualInterfaceId
         (\s a -> s { _cpvi1VirtualInterfaceId = a })
{-# INLINE cpvi1VirtualInterfaceId #-}

instance ToPath ConfirmPublicVirtualInterface

instance ToQuery ConfirmPublicVirtualInterface

instance ToHeaders ConfirmPublicVirtualInterface

instance ToJSON ConfirmPublicVirtualInterface

-- | The response received when ConfirmPublicVirtualInterface is called.
newtype ConfirmPublicVirtualInterfaceResponse = ConfirmPublicVirtualInterfaceResponse
    { _cpvirsrsVirtualInterfaceState :: Maybe VirtualInterfaceState
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
cpvirsrsVirtualInterfaceState :: Lens' ConfirmPublicVirtualInterfaceResponse (Maybe VirtualInterfaceState)
cpvirsrsVirtualInterfaceState =
    lens _cpvirsrsVirtualInterfaceState
         (\s a -> s { _cpvirsrsVirtualInterfaceState = a })
{-# INLINE cpvirsrsVirtualInterfaceState #-}

instance FromJSON ConfirmPublicVirtualInterfaceResponse

instance AWSRequest ConfirmPublicVirtualInterface where
    type Sv ConfirmPublicVirtualInterface = DirectConnect
    type Rs ConfirmPublicVirtualInterface = ConfirmPublicVirtualInterfaceResponse

    request = get
    response _ = jsonResponse
