{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Direct Connect links your internal network to an AWS Direct Connect
-- location over a standard 1 gigabit or 10 gigabit Ethernet fiber-optic
-- cable. One end of the cable is connected to your router, the other to an
-- AWS Direct Connect router. With this connection in place, you can create
-- virtual interfaces directly to the AWS cloud and Amazon Virtual Private
-- Cloud, bypassing Internet service providers in your network path.
--
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.DirectConnect.V2012_10_25.Trans
    (
    -- * AllocateConnectionOnInterconnect
      allocateConnectionOnInterconnect
    -- * AllocatePrivateVirtualInterface
    , allocatePrivateVirtualInterface
    -- * AllocatePublicVirtualInterface
    , allocatePublicVirtualInterface
    -- * ConfirmConnection
    , confirmConnection
    -- * ConfirmPrivateVirtualInterface
    , confirmPrivateVirtualInterface
    -- * ConfirmPublicVirtualInterface
    , confirmPublicVirtualInterface
    -- * CreateConnection
    , createConnection
    -- * CreateInterconnect
    , createInterconnect
    -- * CreatePrivateVirtualInterface
    , createPrivateVirtualInterface
    -- * CreatePublicVirtualInterface
    , createPublicVirtualInterface
    -- * DeleteConnection
    , deleteConnection
    -- * DeleteInterconnect
    , deleteInterconnect
    -- * DeleteVirtualInterface
    , deleteVirtualInterface
    -- * DescribeConnections
    , describeConnections
    -- * DescribeConnectionsOnInterconnect
    , describeConnectionsOnInterconnect
    -- * DescribeInterconnects
    , describeInterconnects
    -- * DescribeLocations
    , describeLocations
    -- * DescribeVirtualGateways
    , describeVirtualGateways
    -- * DescribeVirtualInterfaces
    , describeVirtualInterfaces

    -- * Re-exported
    , module AWS
    , module Network.AWS.DirectConnect.V2012_10_25
    -- ** Lenses
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.DirectConnect.V2012_10_25

-- | Creates a hosted connection on an interconnect. Allocates a VLAN number and
-- a specified amount of bandwidth for use by a hosted connection on the given
-- interconnect.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.AllocateConnectionOnInterconnect'
allocateConnectionOnInterconnect :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadError AWS.Error m
                                    , MonadReader Env m
                                    )
                                 => Text -- ^ 'acoiBandwidth'
                                 -> Text -- ^ 'acoiConnectionName'
                                 -> Text -- ^ 'acoiOwnerAccount'
                                 -> Text -- ^ 'acoiInterconnectId'
                                 -> Integer -- ^ 'acoiVlan'
                                 -> State AllocateConnectionOnInterconnect a
                                 -> m AllocateConnectionOnInterconnectResponse
allocateConnectionOnInterconnect p1 p2 p3 p4 p5 s =
    send $ (mkAllocateConnectionOnInterconnect p1 p2 p3 p4 p5) &~ s

-- | Provisions a private virtual interface to be owned by a different customer.
-- The owner of a connection calls this function to provision a private
-- virtual interface which will be owned by another AWS customer. Virtual
-- interfaces created using this function must be confirmed by the virtual
-- interface owner by calling ConfirmPrivateVirtualInterface. Until this step
-- has been completed, the virtual interface will be in 'Confirming' state,
-- and will not be available for handling traffic.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.AllocatePrivateVirtualInterface'
allocatePrivateVirtualInterface :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadError AWS.Error m
                                   , MonadReader Env m
                                   )
                                => Text -- ^ 'apviConnectionId'
                                -> Text -- ^ 'apviOwnerAccount'
                                -> NewPrivateVirtualInterfaceAllocation -- ^ 'apviNewPrivateVirtualInterfaceAllocation'
                                -> State AllocatePrivateVirtualInterface a
                                -> m AllocatePrivateVirtualInterfaceResponse
allocatePrivateVirtualInterface p1 p2 p3 s =
    send $ (mkAllocatePrivateVirtualInterface p1 p2 p3) &~ s

-- | Provisions a public virtual interface to be owned by a different customer.
-- The owner of a connection calls this function to provision a public virtual
-- interface which will be owned by another AWS customer. Virtual interfaces
-- created using this function must be confirmed by the virtual interface
-- owner by calling ConfirmPublicVirtualInterface. Until this step has been
-- completed, the virtual interface will be in 'Confirming' state, and will
-- not be available for handling traffic.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.AllocatePublicVirtualInterface'
allocatePublicVirtualInterface :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  )
                               => Text -- ^ 'apvi1ConnectionId'
                               -> Text -- ^ 'apvi1OwnerAccount'
                               -> NewPublicVirtualInterfaceAllocation -- ^ 'apvi1NewPublicVirtualInterfaceAllocation'
                               -> State AllocatePublicVirtualInterface a
                               -> m AllocatePublicVirtualInterfaceResponse
allocatePublicVirtualInterface p1 p2 p3 s =
    send $ (mkAllocatePublicVirtualInterface p1 p2 p3) &~ s

-- | Confirm the creation of a hosted connection on an interconnect. Upon
-- creation, the hosted connection is initially in the 'Ordering' state, and
-- will remain in this state until the owner calls ConfirmConnection to
-- confirm creation of the hosted connection.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.ConfirmConnection'
confirmConnection :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
                  => Text -- ^ 'ccConnectionId'
                  -> State ConfirmConnection a
                  -> m ConfirmConnectionResponse
confirmConnection p1 s =
    send $ (mkConfirmConnection p1) &~ s

-- | Accept ownership of a private virtual interface created by another
-- customer. After the virtual interface owner calls this function, the
-- virtual interface will be created and attached to the given virtual private
-- gateway, and will be available for handling traffic.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.ConfirmPrivateVirtualInterface'
confirmPrivateVirtualInterface :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  )
                               => Text -- ^ 'cpviVirtualInterfaceId'
                               -> Text -- ^ 'cpviVirtualGatewayId'
                               -> State ConfirmPrivateVirtualInterface a
                               -> m ConfirmPrivateVirtualInterfaceResponse
confirmPrivateVirtualInterface p1 p2 s =
    send $ (mkConfirmPrivateVirtualInterface p1 p2) &~ s

-- | Accept ownership of a public virtual interface created by another customer.
-- After the virtual interface owner calls this function, the specified
-- virtual interface will be created and made available for handling traffic.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.ConfirmPublicVirtualInterface'
confirmPublicVirtualInterface :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError AWS.Error m
                                 , MonadReader Env m
                                 )
                              => Text -- ^ 'cpvi1VirtualInterfaceId'
                              -> State ConfirmPublicVirtualInterface a
                              -> m ConfirmPublicVirtualInterfaceResponse
confirmPublicVirtualInterface p1 s =
    send $ (mkConfirmPublicVirtualInterface p1) &~ s

-- | Creates a new connection between the customer network and a specific AWS
-- Direct Connect location. A connection links your internal network to an AWS
-- Direct Connect location over a standard 1 gigabit or 10 gigabit Ethernet
-- fiber-optic cable. One end of the cable is connected to your router, the
-- other to an AWS Direct Connect router. An AWS Direct Connect location
-- provides access to Amazon Web Services in the region it is associated with.
-- You can establish connections with AWS Direct Connect locations in multiple
-- regions, but a connection in one region does not provide connectivity to
-- other regions.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.CreateConnection'
createConnection :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
                 => Text -- ^ 'cc1Location'
                 -> Text -- ^ 'cc1Bandwidth'
                 -> Text -- ^ 'cc1ConnectionName'
                 -> State CreateConnection a
                 -> m CreateConnectionResponse
createConnection p1 p2 p3 s =
    send $ (mkCreateConnection p1 p2 p3) &~ s

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
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.CreateInterconnect'
createInterconnect :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
                   => Text -- ^ 'ciInterconnectName'
                   -> Text -- ^ 'ciBandwidth'
                   -> Text -- ^ 'ciLocation'
                   -> State CreateInterconnect a
                   -> m CreateInterconnectResponse
createInterconnect p1 p2 p3 s =
    send $ (mkCreateInterconnect p1 p2 p3) &~ s

-- | Creates a new private virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A private virtual interface
-- supports sending traffic to a single virtual private cloud (VPC).
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.CreatePrivateVirtualInterface'
createPrivateVirtualInterface :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError AWS.Error m
                                 , MonadReader Env m
                                 )
                              => Text -- ^ 'cpvi2ConnectionId'
                              -> NewPrivateVirtualInterface -- ^ 'cpvi2NewPrivateVirtualInterface'
                              -> State CreatePrivateVirtualInterface a
                              -> m CreatePrivateVirtualInterfaceResponse
createPrivateVirtualInterface p1 p2 s =
    send $ (mkCreatePrivateVirtualInterface p1 p2) &~ s

-- | Creates a new public virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A public virtual interface
-- supports sending traffic to public services of AWS such as Amazon Simple
-- Storage Service (Amazon S3).
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.CreatePublicVirtualInterface'
createPublicVirtualInterface :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                )
                             => Text -- ^ 'cpvi3ConnectionId'
                             -> NewPublicVirtualInterface -- ^ 'cpvi3NewPublicVirtualInterface'
                             -> State CreatePublicVirtualInterface a
                             -> m CreatePublicVirtualInterfaceResponse
createPublicVirtualInterface p1 p2 s =
    send $ (mkCreatePublicVirtualInterface p1 p2) &~ s

-- | Deletes the connection. Deleting a connection only stops the AWS Direct
-- Connect port hour and data transfer charges. You need to cancel separately
-- with the providers any services or charges for cross-connects or network
-- circuits that connect you to the AWS Direct Connect location.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.DeleteConnection'
deleteConnection :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
                 => Text -- ^ 'dcConnectionId'
                 -> State DeleteConnection a
                 -> m DeleteConnectionResponse
deleteConnection p1 s =
    send $ (mkDeleteConnection p1) &~ s

-- | Deletes the specified interconnect.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.DeleteInterconnect'
deleteInterconnect :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
                   => Text -- ^ 'diInterconnectId'
                   -> State DeleteInterconnect a
                   -> m DeleteInterconnectResponse
deleteInterconnect p1 s =
    send $ (mkDeleteInterconnect p1) &~ s

-- | Deletes a virtual interface.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.DeleteVirtualInterface'
deleteVirtualInterface :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
                       => Text -- ^ 'dviVirtualInterfaceId'
                       -> State DeleteVirtualInterface a
                       -> m DeleteVirtualInterfaceResponse
deleteVirtualInterface p1 s =
    send $ (mkDeleteVirtualInterface p1) &~ s

-- | Displays all connections in this region. If a connection ID is provided,
-- the call returns only that particular connection.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.DescribeConnections'
describeConnections :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
                    => State DescribeConnections a
                    -> m DescribeConnectionsResponse
describeConnections s =
    send (mkDescribeConnections &~ s)

-- | Return a list of connections that have been provisioned on the given
-- interconnect.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.DescribeConnectionsOnInterconnect'
describeConnectionsOnInterconnect :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadError AWS.Error m
                                     , MonadReader Env m
                                     )
                                  => Text -- ^ 'dcoiInterconnectId'
                                  -> State DescribeConnectionsOnInterconnect a
                                  -> m DescribeConnectionsOnInterconnectResponse
describeConnectionsOnInterconnect p1 s =
    send $ (mkDescribeConnectionsOnInterconnect p1) &~ s

-- | Returns a list of interconnects owned by the AWS account. If an
-- interconnect ID is provided, it will only return this particular
-- interconnect.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.DescribeInterconnects'
describeInterconnects :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
                      => State DescribeInterconnects a
                      -> m DescribeInterconnectsResponse
describeInterconnects s =
    send (mkDescribeInterconnects &~ s)

-- | Returns the list of AWS Direct Connect locations in the current AWS region.
-- These are the locations that may be selected when calling CreateConnection
-- or CreateInterconnect.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.DescribeLocations'
describeLocations :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
                  => State DescribeLocations a
                  -> m DescribeLocationsResponse
describeLocations s =
    send (mkDescribeLocations &~ s)

-- | Returns a list of virtual private gateways owned by the AWS account. You
-- can create one or more AWS Direct Connect private virtual interfaces
-- linking to a virtual private gateway. A virtual private gateway can be
-- managed via Amazon Virtual Private Cloud (VPC) console or the EC2
-- CreateVpnGateway action.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.DescribeVirtualGateways'
describeVirtualGateways :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
                        => State DescribeVirtualGateways a
                        -> m DescribeVirtualGatewaysResponse
describeVirtualGateways s =
    send (mkDescribeVirtualGateways &~ s)

-- | Displays all virtual interfaces for an AWS account. Virtual interfaces
-- deleted fewer than 15 minutes before DescribeVirtualInterfaces is called
-- are also returned. If a connection ID is included then only virtual
-- interfaces associated with this connection will be returned. If a virtual
-- interface ID is included then only a single virtual interface will be
-- returned. A virtual interface (VLAN) transmits the traffic between the AWS
-- Direct Connect location and the customer. If a connection ID is provided,
-- only virtual interfaces provisioned on the specified connection will be
-- returned. If a virtual interface ID is provided, only this particular
-- virtual interface will be returned.
--
-- See: 'Network.AWS.DirectConnect.V2012_10_25.DescribeVirtualInterfaces'
describeVirtualInterfaces :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
                          => State DescribeVirtualInterfaces a
                          -> m DescribeVirtualInterfacesResponse
describeVirtualInterfaces s =
    send (mkDescribeVirtualInterfaces &~ s)
