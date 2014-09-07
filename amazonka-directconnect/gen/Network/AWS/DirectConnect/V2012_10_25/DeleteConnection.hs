{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.DeleteConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the connection. Deleting a connection only stops the AWS Direct
-- Connect port hour and data transfer charges. You need to cancel separately
-- with the providers any services or charges for cross-connects or network
-- circuits that connect you to the AWS Direct Connect location.
module Network.AWS.DirectConnect.V2012_10_25.DeleteConnection
    (
    -- * Request
      DeleteConnection
    -- ** Request constructor
    , mkDeleteConnection
    -- ** Request lenses
    , dcConnectionId

    -- * Response
    , DeleteConnectionResponse
    -- ** Response lenses
    , dcrsOwnerAccount
    , dcrsConnectionId
    , dcrsConnectionName
    , dcrsConnectionState
    , dcrsRegion
    , dcrsLocation
    , dcrsBandwidth
    , dcrsVlan
    , dcrsPartnerName
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Container for the parameters to the DeleteConnection operation.
newtype DeleteConnection = DeleteConnection
    { _dcConnectionId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteConnection' request.
mkDeleteConnection :: Text -- ^ 'dcConnectionId'
                   -> DeleteConnection
mkDeleteConnection p1 = DeleteConnection
    { _dcConnectionId = p1
    }

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
dcConnectionId :: Lens' DeleteConnection Text
dcConnectionId = lens _dcConnectionId (\s a -> s { _dcConnectionId = a })

instance ToPath DeleteConnection

instance ToQuery DeleteConnection

instance ToHeaders DeleteConnection

instance ToJSON DeleteConnection

-- | A connection represents the physical network connection between the AWS
-- Direct Connect location and the customer.
data DeleteConnectionResponse = DeleteConnectionResponse
    { _dcrsOwnerAccount :: Maybe Text
    , _dcrsConnectionId :: Maybe Text
    , _dcrsConnectionName :: Maybe Text
    , _dcrsConnectionState :: Maybe ConnectionState
    , _dcrsRegion :: Maybe Text
    , _dcrsLocation :: Maybe Text
    , _dcrsBandwidth :: Maybe Text
    , _dcrsVlan :: Maybe Integer
    , _dcrsPartnerName :: Maybe Text
    } deriving (Show, Generic)

dcrsOwnerAccount :: Lens' DeleteConnectionResponse (Maybe Text)
dcrsOwnerAccount =
    lens _dcrsOwnerAccount (\s a -> s { _dcrsOwnerAccount = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
dcrsConnectionId :: Lens' DeleteConnectionResponse (Maybe Text)
dcrsConnectionId =
    lens _dcrsConnectionId (\s a -> s { _dcrsConnectionId = a })

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
dcrsConnectionName :: Lens' DeleteConnectionResponse (Maybe Text)
dcrsConnectionName =
    lens _dcrsConnectionName (\s a -> s { _dcrsConnectionName = a })

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
dcrsConnectionState :: Lens' DeleteConnectionResponse (Maybe ConnectionState)
dcrsConnectionState =
    lens _dcrsConnectionState (\s a -> s { _dcrsConnectionState = a })

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
dcrsRegion :: Lens' DeleteConnectionResponse (Maybe Text)
dcrsRegion = lens _dcrsRegion (\s a -> s { _dcrsRegion = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
dcrsLocation :: Lens' DeleteConnectionResponse (Maybe Text)
dcrsLocation = lens _dcrsLocation (\s a -> s { _dcrsLocation = a })

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
dcrsBandwidth :: Lens' DeleteConnectionResponse (Maybe Text)
dcrsBandwidth = lens _dcrsBandwidth (\s a -> s { _dcrsBandwidth = a })

-- | The VLAN ID. Example: 101.
dcrsVlan :: Lens' DeleteConnectionResponse (Maybe Integer)
dcrsVlan = lens _dcrsVlan (\s a -> s { _dcrsVlan = a })

dcrsPartnerName :: Lens' DeleteConnectionResponse (Maybe Text)
dcrsPartnerName = lens _dcrsPartnerName (\s a -> s { _dcrsPartnerName = a })

instance FromJSON DeleteConnectionResponse

instance AWSRequest DeleteConnection where
    type Sv DeleteConnection = DirectConnect
    type Rs DeleteConnection = DeleteConnectionResponse

    request = get
    response _ = jsonResponse
