{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.ConfirmConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Confirm the creation of a hosted connection on an interconnect. Upon
-- creation, the hosted connection is initially in the 'Ordering' state, and
-- will remain in this state until the owner calls ConfirmConnection to
-- confirm creation of the hosted connection.
module Network.AWS.DirectConnect.ConfirmConnection
    (
    -- * Request
      ConfirmConnection
    -- ** Request constructor
    , confirmConnection
    -- ** Request lenses
    , ccConnectionId

    -- * Response
    , ConfirmConnectionResponse
    -- ** Response constructor
    , confirmConnectionResponse
    -- ** Response lenses
    , ccrConnectionState
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the ConfirmConnection operation.
newtype ConfirmConnection = ConfirmConnection
    { _ccConnectionId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ConfirmConnection' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ConnectionId ::@ @Text@
--
confirmConnection :: Text -- ^ 'ccConnectionId'
                    -> ConfirmConnection
confirmConnection p1 = ConfirmConnection
    { _ccConnectionId = p1
    }

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
ccConnectionId :: Lens' ConfirmConnection Text
ccConnectionId = lens _ccConnectionId (\s a -> s { _ccConnectionId = a })

instance ToPath ConfirmConnection

instance ToQuery ConfirmConnection

instance ToHeaders ConfirmConnection

instance ToJSON ConfirmConnection

-- | The response received when ConfirmConnection is called.
newtype ConfirmConnectionResponse = ConfirmConnectionResponse
    { _ccrConnectionState :: Maybe ConnectionState
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ConfirmConnectionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ConnectionState ::@ @Maybe ConnectionState@
--
confirmConnectionResponse :: ConfirmConnectionResponse
confirmConnectionResponse = ConfirmConnectionResponse
    { _ccrConnectionState = Nothing
    }

-- | State of the connection. Ordering: The initial state of a hosted connection
-- provisioned on an interconnect. The connection stays in the ordering state
-- until the owner of the hosted connection confirms or declines the
-- connection order. Requested: The initial state of a standard connection.
-- The connection stays in the requested state until the Letter of
-- Authorization (LOA) is sent to the customer. Pending: The connection has
-- been approved, and is being initialized. Available: The network link is up,
-- and the connection is ready for use. Down: The network link is down.
-- Deleted: The connection has been deleted. Rejected: A hosted connection in
-- the 'Ordering' state will enter the 'Rejected' state if it is deleted by
-- the end customer.
ccrConnectionState :: Lens' ConfirmConnectionResponse (Maybe ConnectionState)
ccrConnectionState =
    lens _ccrConnectionState (\s a -> s { _ccrConnectionState = a })

instance FromJSON ConfirmConnectionResponse

instance AWSRequest ConfirmConnection where
    type Sv ConfirmConnection = DirectConnect
    type Rs ConfirmConnection = ConfirmConnectionResponse

    request = get
    response _ = jsonResponse
