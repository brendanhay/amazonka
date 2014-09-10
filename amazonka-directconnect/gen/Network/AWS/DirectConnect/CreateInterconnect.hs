{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.CreateInterconnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new interconnect between a AWS Direct Connect partner's network
-- and a specific AWS Direct Connect location. An interconnect is a connection
-- which is capable of hosting other connections. The AWS Direct Connect
-- partner can use an interconnect to provide sub-1Gbps AWS Direct Connect
-- service to tier 2 customers who do not have their own connections. Like a
-- standard connection, an interconnect links the AWS Direct Connect partner's
-- network to an AWS Direct Connect location over a standard 1 Gbps or 10 Gbps
-- Ethernet fiber-optic cable. One end is connected to the partner's router,
-- the other to an AWS Direct Connect router. For each end customer, the AWS
-- Direct Connect partner provisions a connection on their interconnect by
-- calling AllocateConnectionOnInterconnect. The end customer can then connect
-- to AWS resources by creating a virtual interface on their connection, using
-- the VLAN assigned to them by the AWS Direct Connect partner.
module Network.AWS.DirectConnect.CreateInterconnect
    (
    -- * Request
      CreateInterconnect
    -- ** Request constructor
    , mkCreateInterconnect
    -- ** Request lenses
    , ciInterconnectName
    , ciBandwidth
    , ciLocation

    -- * Response
    , CreateInterconnectResponse
    -- ** Response constructor
    , mkCreateInterconnectResponse
    -- ** Response lenses
    , cirInterconnectId
    , cirInterconnectName
    , cirInterconnectState
    , cirRegion
    , cirLocation
    , cirBandwidth
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the CreateInterconnect operation.
data CreateInterconnect = CreateInterconnect
    { _ciInterconnectName :: !Text
    , _ciBandwidth :: !Text
    , _ciLocation :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateInterconnect' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InterconnectName ::@ @Text@
--
-- * @Bandwidth ::@ @Text@
--
-- * @Location ::@ @Text@
--
mkCreateInterconnect :: Text -- ^ 'ciInterconnectName'
                     -> Text -- ^ 'ciBandwidth'
                     -> Text -- ^ 'ciLocation'
                     -> CreateInterconnect
mkCreateInterconnect p1 p2 p3 = CreateInterconnect
    { _ciInterconnectName = p1
    , _ciBandwidth = p2
    , _ciLocation = p3
    }

-- | The name of the interconnect. Example: "1G Interconnect to AWS" Default:
-- None.
ciInterconnectName :: Lens' CreateInterconnect Text
ciInterconnectName =
    lens _ciInterconnectName (\s a -> s { _ciInterconnectName = a })

-- | The port bandwidth Example: 1Gbps Default: None Available values:
-- 1Gbps,10Gbps.
ciBandwidth :: Lens' CreateInterconnect Text
ciBandwidth = lens _ciBandwidth (\s a -> s { _ciBandwidth = a })

-- | Where the interconnect is located Example: EqSV5 Default: None.
ciLocation :: Lens' CreateInterconnect Text
ciLocation = lens _ciLocation (\s a -> s { _ciLocation = a })

instance ToPath CreateInterconnect

instance ToQuery CreateInterconnect

instance ToHeaders CreateInterconnect

instance ToJSON CreateInterconnect

-- | An interconnect is a connection that can host other connections. Like a
-- standard AWS Direct Connect connection, an interconnect represents the
-- physical connection between an AWS Direct Connect partner's network and a
-- specific Direct Connect location. An AWS Direct Connect partner who owns an
-- interconnect can provision hosted connections on the interconnect for their
-- end customers, thereby providing the end customers with connectivity to AWS
-- services. The resources of the interconnect, including bandwidth and VLAN
-- numbers, are shared by all of the hosted connections on the interconnect,
-- and the owner of the interconnect determines how these resources are
-- assigned.
data CreateInterconnectResponse = CreateInterconnectResponse
    { _cirInterconnectId :: !(Maybe Text)
    , _cirInterconnectName :: !(Maybe Text)
    , _cirInterconnectState :: Maybe InterconnectState
    , _cirRegion :: !(Maybe Text)
    , _cirLocation :: !(Maybe Text)
    , _cirBandwidth :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateInterconnectResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InterconnectId ::@ @Maybe Text@
--
-- * @InterconnectName ::@ @Maybe Text@
--
-- * @InterconnectState ::@ @Maybe InterconnectState@
--
-- * @Region ::@ @Maybe Text@
--
-- * @Location ::@ @Maybe Text@
--
-- * @Bandwidth ::@ @Maybe Text@
--
mkCreateInterconnectResponse :: CreateInterconnectResponse
mkCreateInterconnectResponse = CreateInterconnectResponse
    { _cirInterconnectId = Nothing
    , _cirInterconnectName = Nothing
    , _cirInterconnectState = Nothing
    , _cirRegion = Nothing
    , _cirLocation = Nothing
    , _cirBandwidth = Nothing
    }

-- | The ID of the interconnect. Example: dxcon-abc123.
cirInterconnectId :: Lens' CreateInterconnectResponse (Maybe Text)
cirInterconnectId =
    lens _cirInterconnectId (\s a -> s { _cirInterconnectId = a })

-- | The name of the interconnect. Example: "1G Interconnect to AWS".
cirInterconnectName :: Lens' CreateInterconnectResponse (Maybe Text)
cirInterconnectName =
    lens _cirInterconnectName (\s a -> s { _cirInterconnectName = a })

-- | State of the interconnect. Requested: The initial state of an interconnect.
-- The interconnect stays in the requested state until the Letter of
-- Authorization (LOA) is sent to the customer. Pending: The interconnect has
-- been approved, and is being initialized. Available: The network link is up,
-- and the interconnect is ready for use. Down: The network link is down.
-- Deleted: The interconnect has been deleted.
cirInterconnectState :: Lens' CreateInterconnectResponse (Maybe InterconnectState)
cirInterconnectState =
    lens _cirInterconnectState (\s a -> s { _cirInterconnectState = a })

-- | The AWS region where the connection is located. Example: us-east-1 Default:
-- None.
cirRegion :: Lens' CreateInterconnectResponse (Maybe Text)
cirRegion = lens _cirRegion (\s a -> s { _cirRegion = a })

-- | Where the connection is located. Example: EqSV5 Default: None.
cirLocation :: Lens' CreateInterconnectResponse (Maybe Text)
cirLocation = lens _cirLocation (\s a -> s { _cirLocation = a })

-- | Bandwidth of the connection. Example: 1Gbps Default: None.
cirBandwidth :: Lens' CreateInterconnectResponse (Maybe Text)
cirBandwidth = lens _cirBandwidth (\s a -> s { _cirBandwidth = a })

instance FromJSON CreateInterconnectResponse

instance AWSRequest CreateInterconnect where
    type Sv CreateInterconnect = DirectConnect
    type Rs CreateInterconnect = CreateInterconnectResponse

    request = get
    response _ = jsonResponse
