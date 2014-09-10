{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a hosted connection on an interconnect. Allocates a VLAN number and
-- a specified amount of bandwidth for use by a hosted connection on the given
-- interconnect.
module Network.AWS.DirectConnect
    (
    -- * Request
      AllocateConnectionOnInterconnect
    -- ** Request constructor
    , mkAllocateConnectionOnInterconnect
    -- ** Request lenses
    , acoiBandwidth
    , acoiConnectionName
    , acoiOwnerAccount
    , acoiInterconnectId
    , acoiVlan

    -- * Response
    , AllocateConnectionOnInterconnectResponse
    -- ** Response constructor
    , mkAllocateConnectionOnInterconnectResponse
    -- ** Response lenses
    , acoirOwnerAccount
    , acoirConnectionId
    , acoirConnectionName
    , acoirConnectionState
    , acoirRegion
    , acoirLocation
    , acoirBandwidth
    , acoirVlan
    , acoirPartnerName
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the AllocateConnectionOnInterconnect
-- operation.
data AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnect
    { _acoiBandwidth :: Text
    , _acoiConnectionName :: Text
    , _acoiOwnerAccount :: Text
    , _acoiInterconnectId :: Text
    , _acoiVlan :: Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocateConnectionOnInterconnect' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bandwidth ::@ @Text@
--
-- * @ConnectionName ::@ @Text@
--
-- * @OwnerAccount ::@ @Text@
--
-- * @InterconnectId ::@ @Text@
--
-- * @Vlan ::@ @Integer@
--
mkAllocateConnectionOnInterconnect :: Text -- ^ 'acoiBandwidth'
                                   -> Text -- ^ 'acoiConnectionName'
                                   -> Text -- ^ 'acoiOwnerAccount'
                                   -> Text -- ^ 'acoiInterconnectId'
                                   -> Integer -- ^ 'acoiVlan'
                                   -> AllocateConnectionOnInterconnect
mkAllocateConnectionOnInterconnect p1 p2 p3 p4 p5 = AllocateConnectionOnInterconnect
    { _acoiBandwidth = p1
    , _acoiConnectionName = p2
    , _acoiOwnerAccount = p3
    , _acoiInterconnectId = p4
    , _acoiVlan = p5
    }

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
acoiBandwidth :: Lens' AllocateConnectionOnInterconnect Text
acoiBandwidth = lens _acoiBandwidth (\s a -> s { _acoiBandwidth = a })

-- | Name of the provisioned connection. Example: "500M Connection to AWS"
-- Default: None.
acoiConnectionName :: Lens' AllocateConnectionOnInterconnect Text
acoiConnectionName =
    lens _acoiConnectionName (\s a -> s { _acoiConnectionName = a })

-- | Numeric account Id of the customer for whom the connection will be
-- provisioned. Example: 123443215678 Default: None.
acoiOwnerAccount :: Lens' AllocateConnectionOnInterconnect Text
acoiOwnerAccount =
    lens _acoiOwnerAccount (\s a -> s { _acoiOwnerAccount = a })

-- | ID of the interconnect on which the connection will be provisioned.
-- Example: dxcon-456abc78 Default: None.
acoiInterconnectId :: Lens' AllocateConnectionOnInterconnect Text
acoiInterconnectId =
    lens _acoiInterconnectId (\s a -> s { _acoiInterconnectId = a })

-- | The dedicated VLAN provisioned to the connection. Example: 101 Default:
-- None.
acoiVlan :: Lens' AllocateConnectionOnInterconnect Integer
acoiVlan = lens _acoiVlan (\s a -> s { _acoiVlan = a })

instance ToPath AllocateConnectionOnInterconnect

instance ToQuery AllocateConnectionOnInterconnect

instance ToHeaders AllocateConnectionOnInterconnect

instance ToJSON AllocateConnectionOnInterconnect

-- | A connection represents the physical network connection between the AWS
-- Direct Connect location and the customer.
data AllocateConnectionOnInterconnectResponse = AllocateConnectionOnInterconnectResponse
    { _acoirOwnerAccount :: Maybe Text
    , _acoirConnectionId :: Maybe Text
    , _acoirConnectionName :: Maybe Text
    , _acoirConnectionState :: Maybe ConnectionState
    , _acoirRegion :: Maybe Text
    , _acoirLocation :: Maybe Text
    , _acoirBandwidth :: Maybe Text
    , _acoirVlan :: Maybe Integer
    , _acoirPartnerName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AllocateConnectionOnInterconnectResponse' response.
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
mkAllocateConnectionOnInterconnectResponse :: AllocateConnectionOnInterconnectResponse
mkAllocateConnectionOnInterconnectResponse = AllocateConnectionOnInterconnectResponse
    { _acoirOwnerAccount = Nothing
    , _acoirConnectionId = Nothing
    , _acoirConnectionName = Nothing
    , _acoirConnectionState = Nothing
    , _acoirRegion = Nothing
    , _acoirLocation = Nothing
    , _acoirBandwidth = Nothing
    , _acoirVlan = Nothing
    , _acoirPartnerName = Nothing
    }

acoirOwnerAccount :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirOwnerAccount =
    lens _acoirOwnerAccount (\s a -> s { _acoirOwnerAccount = a })

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
acoirConnectionId :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirConnectionId =
    lens _acoirConnectionId (\s a -> s { _acoirConnectionId = a })

-- | The name of the connection. Example: "1G Connection to AWS" Default: None.
acoirConnectionName :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirConnectionName =
    lens _acoirConnectionName (\s a -> s { _acoirConnectionName = a })

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
acoirConnectionState :: Lens' AllocateConnectionOnInterconnectResponse (Maybe ConnectionState)
acoirConnectionState =
    lens _acoirConnectionState (\s a -> s { _acoirConnectionState = a })

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
acoirRegion :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirRegion = lens _acoirRegion (\s a -> s { _acoirRegion = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
acoirLocation :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirLocation = lens _acoirLocation (\s a -> s { _acoirLocation = a })

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
acoirBandwidth :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirBandwidth = lens _acoirBandwidth (\s a -> s { _acoirBandwidth = a })

-- | The VLAN ID. Example: 101.
acoirVlan :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Integer)
acoirVlan = lens _acoirVlan (\s a -> s { _acoirVlan = a })

acoirPartnerName :: Lens' AllocateConnectionOnInterconnectResponse (Maybe Text)
acoirPartnerName =
    lens _acoirPartnerName (\s a -> s { _acoirPartnerName = a })

instance FromJSON AllocateConnectionOnInterconnectResponse

instance AWSRequest AllocateConnectionOnInterconnect where
    type Sv AllocateConnectionOnInterconnect = DirectConnect
    type Rs AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnectResponse

    request = get
    response _ = jsonResponse
