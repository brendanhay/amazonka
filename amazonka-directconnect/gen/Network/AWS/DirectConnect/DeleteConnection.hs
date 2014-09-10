{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DeleteConnection
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
module Network.AWS.DirectConnect
    (
    -- * Request
      DeleteConnection
    -- ** Request constructor
    , mkDeleteConnection
    -- ** Request lenses
    , dcConnectionId

    -- * Response
    , DeleteConnectionResponse
    -- ** Response constructor
    , mkDeleteConnectionResponse
    -- ** Response lenses
    , dcrOwnerAccount
    , dcrConnectionId
    , dcrConnectionName
    , dcrConnectionState
    , dcrRegion
    , dcrLocation
    , dcrBandwidth
    , dcrVlan
    , dcrPartnerName
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the DeleteConnection operation.
newtype DeleteConnection = DeleteConnection
    { _dcConnectionId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteConnection' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ConnectionId ::@ @Text@
--
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
    { _dcrOwnerAccount :: !(Maybe Text)
    , _dcrConnectionId :: !(Maybe Text)
    , _dcrConnectionName :: !(Maybe Text)
    , _dcrConnectionState :: Maybe ConnectionState
    , _dcrRegion :: !(Maybe Text)
    , _dcrLocation :: !(Maybe Text)
    , _dcrBandwidth :: !(Maybe Text)
    , _dcrVlan :: !(Maybe Integer)
    , _dcrPartnerName :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteConnectionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OwnerAccount ::@ @Maybe Text@
--
-- * @ConnectionId ::@ @Maybe Text@
--
-- * @ConnectionName ::@ @Maybe Text@
--
-- * @ConnectionState ::@ @Maybe ConnectionState@
--
-- * @Region ::@ @Maybe Text@
--
-- * @Location ::@ @Maybe Text@
--
-- * @Bandwidth ::@ @Maybe Text@
--
-- * @Vlan ::@ @Maybe Integer@
--
-- * @PartnerName ::@ @Maybe Text@
--
mkDeleteConnectionResponse :: DeleteConnectionResponse
mkDeleteConnectionResponse = DeleteConnectionResponse
    { _dcrOwnerAccount = Nothing
    , _dcrConnectionId = Nothing
    , _dcrConnectionName = Nothing
    , _dcrConnectionState = Nothing
    , _dcrRegion = Nothing
    , _dcrLocation = Nothing
    , _dcrBandwidth = Nothing
    , _dcrVlan = Nothing
    , _dcrPartnerName = Nothing
    }

dcrOwnerAccount :: Lens' DeleteConnectionResponse (Maybe Text)
dcrOwnerAccount = lens _dcrOwnerAccount (\s a -> s { _dcrOwnerAccount = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
dcrConnectionId :: Lens' DeleteConnectionResponse (Maybe Text)
dcrConnectionId = lens _dcrConnectionId (\s a -> s { _dcrConnectionId = a })

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
dcrConnectionName :: Lens' DeleteConnectionResponse (Maybe Text)
dcrConnectionName =
    lens _dcrConnectionName (\s a -> s { _dcrConnectionName = a })

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
dcrConnectionState :: Lens' DeleteConnectionResponse (Maybe ConnectionState)
dcrConnectionState =
    lens _dcrConnectionState (\s a -> s { _dcrConnectionState = a })

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
dcrRegion :: Lens' DeleteConnectionResponse (Maybe Text)
dcrRegion = lens _dcrRegion (\s a -> s { _dcrRegion = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
dcrLocation :: Lens' DeleteConnectionResponse (Maybe Text)
dcrLocation = lens _dcrLocation (\s a -> s { _dcrLocation = a })

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
dcrBandwidth :: Lens' DeleteConnectionResponse (Maybe Text)
dcrBandwidth = lens _dcrBandwidth (\s a -> s { _dcrBandwidth = a })

-- | The VLAN ID. Example: 101.
dcrVlan :: Lens' DeleteConnectionResponse (Maybe Integer)
dcrVlan = lens _dcrVlan (\s a -> s { _dcrVlan = a })

dcrPartnerName :: Lens' DeleteConnectionResponse (Maybe Text)
dcrPartnerName = lens _dcrPartnerName (\s a -> s { _dcrPartnerName = a })

instance FromJSON DeleteConnectionResponse

instance AWSRequest DeleteConnection where
    type Sv DeleteConnection = DirectConnect
    type Rs DeleteConnection = DeleteConnectionResponse

    request = get
    response _ = jsonResponse
