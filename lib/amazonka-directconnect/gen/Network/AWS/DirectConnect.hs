{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Direct Connect links your internal network to an AWS Direct Connect location over a standard 1 gigabit or 10 gigabit Ethernet fiber-optic cable. One end of the cable is connected to your router, the other to an AWS Direct Connect router. With this connection in place, you can create virtual interfaces directly to the AWS cloud (for example, to Amazon Elastic Compute Cloud (Amazon EC2) and Amazon Simple Storage Service (Amazon S3)) and to Amazon Virtual Private Cloud (Amazon VPC), bypassing Internet service providers in your network path. An AWS Direct Connect location provides access to AWS in the region it is associated with, as well as access to other US regions. For example, you can provision a single connection to any AWS Direct Connect location in the US and use it to access public AWS services in all US Regions and AWS GovCloud (US).
--
--
module Network.AWS.DirectConnect
    (
    -- * Service Configuration
      directConnect

    -- * Errors
    -- $errors

    -- ** DirectConnectClientException
    , _DirectConnectClientException

    -- ** DuplicateTagKeysException
    , _DuplicateTagKeysException

    -- ** TooManyTagsException
    , _TooManyTagsException

    -- ** DirectConnectServerException
    , _DirectConnectServerException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeDirectConnectGatewayAssociations
    , module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations

    -- ** DescribeInterconnects
    , module Network.AWS.DirectConnect.DescribeInterconnects

    -- ** DescribeTags
    , module Network.AWS.DirectConnect.DescribeTags

    -- ** DescribeLoa
    , module Network.AWS.DirectConnect.DescribeLoa

    -- ** DeleteConnection
    , module Network.AWS.DirectConnect.DeleteConnection

    -- ** AssociateConnectionWithLag
    , module Network.AWS.DirectConnect.AssociateConnectionWithLag

    -- ** CreateConnection
    , module Network.AWS.DirectConnect.CreateConnection

    -- ** DescribeDirectConnectGateways
    , module Network.AWS.DirectConnect.DescribeDirectConnectGateways

    -- ** AssociateVirtualInterface
    , module Network.AWS.DirectConnect.AssociateVirtualInterface

    -- ** DescribeConnections
    , module Network.AWS.DirectConnect.DescribeConnections

    -- ** DeleteInterconnect
    , module Network.AWS.DirectConnect.DeleteInterconnect

    -- ** ConfirmPrivateVirtualInterface
    , module Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface

    -- ** DeleteDirectConnectGatewayAssociation
    , module Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation

    -- ** DescribeLocations
    , module Network.AWS.DirectConnect.DescribeLocations

    -- ** CreateDirectConnectGatewayAssociation
    , module Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation

    -- ** CreatePublicVirtualInterface
    , module Network.AWS.DirectConnect.CreatePublicVirtualInterface

    -- ** AllocatePrivateVirtualInterface
    , module Network.AWS.DirectConnect.AllocatePrivateVirtualInterface

    -- ** DescribeLags
    , module Network.AWS.DirectConnect.DescribeLags

    -- ** ConfirmConnection
    , module Network.AWS.DirectConnect.ConfirmConnection

    -- ** DescribeDirectConnectGatewayAttachments
    , module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments

    -- ** ConfirmPublicVirtualInterface
    , module Network.AWS.DirectConnect.ConfirmPublicVirtualInterface

    -- ** DescribeVirtualGateways
    , module Network.AWS.DirectConnect.DescribeVirtualGateways

    -- ** CreateDirectConnectGateway
    , module Network.AWS.DirectConnect.CreateDirectConnectGateway

    -- ** DeleteDirectConnectGateway
    , module Network.AWS.DirectConnect.DeleteDirectConnectGateway

    -- ** DescribeVirtualInterfaces
    , module Network.AWS.DirectConnect.DescribeVirtualInterfaces

    -- ** AllocateHostedConnection
    , module Network.AWS.DirectConnect.AllocateHostedConnection

    -- ** DeleteVirtualInterface
    , module Network.AWS.DirectConnect.DeleteVirtualInterface

    -- ** CreatePrivateVirtualInterface
    , module Network.AWS.DirectConnect.CreatePrivateVirtualInterface

    -- ** AllocatePublicVirtualInterface
    , module Network.AWS.DirectConnect.AllocatePublicVirtualInterface

    -- ** DisassociateConnectionFromLag
    , module Network.AWS.DirectConnect.DisassociateConnectionFromLag

    -- ** TagResource
    , module Network.AWS.DirectConnect.TagResource

    -- ** DeleteLag
    , module Network.AWS.DirectConnect.DeleteLag

    -- ** UpdateLag
    , module Network.AWS.DirectConnect.UpdateLag

    -- ** UntagResource
    , module Network.AWS.DirectConnect.UntagResource

    -- ** CreateBGPPeer
    , module Network.AWS.DirectConnect.CreateBGPPeer

    -- ** AssociateHostedConnection
    , module Network.AWS.DirectConnect.AssociateHostedConnection

    -- ** CreateInterconnect
    , module Network.AWS.DirectConnect.CreateInterconnect

    -- ** DeleteBGPPeer
    , module Network.AWS.DirectConnect.DeleteBGPPeer

    -- ** CreateLag
    , module Network.AWS.DirectConnect.CreateLag

    -- ** DescribeHostedConnections
    , module Network.AWS.DirectConnect.DescribeHostedConnections

    -- * Types

    -- ** AddressFamily
    , AddressFamily (..)

    -- ** BGPPeerState
    , BGPPeerState (..)

    -- ** BGPStatus
    , BGPStatus (..)

    -- ** ConnectionState
    , ConnectionState (..)

    -- ** DirectConnectGatewayAssociationState
    , DirectConnectGatewayAssociationState (..)

    -- ** DirectConnectGatewayAttachmentState
    , DirectConnectGatewayAttachmentState (..)

    -- ** DirectConnectGatewayState
    , DirectConnectGatewayState (..)

    -- ** InterconnectState
    , InterconnectState (..)

    -- ** LagState
    , LagState (..)

    -- ** LoaContentType
    , LoaContentType (..)

    -- ** VirtualInterfaceState
    , VirtualInterfaceState (..)

    -- ** BGPPeer
    , BGPPeer
    , bgpPeer
    , bpCustomerAddress
    , bpAmazonAddress
    , bpAddressFamily
    , bpBgpStatus
    , bpAsn
    , bpAuthKey
    , bpBgpPeerState

    -- ** Connection
    , Connection
    , connection
    , cLagId
    , cVlan
    , cLocation
    , cAwsDevice
    , cConnectionId
    , cLoaIssueTime
    , cPartnerName
    , cConnectionName
    , cBandwidth
    , cOwnerAccount
    , cRegion
    , cConnectionState

    -- ** Connections
    , Connections
    , connections
    , cConnections

    -- ** DirectConnectGateway
    , DirectConnectGateway
    , directConnectGateway
    , dcgDirectConnectGatewayId
    , dcgStateChangeError
    , dcgAmazonSideASN
    , dcgDirectConnectGatewayName
    , dcgDirectConnectGatewayState
    , dcgOwnerAccount

    -- ** DirectConnectGatewayAssociation
    , DirectConnectGatewayAssociation
    , directConnectGatewayAssociation
    , dcgaVirtualGatewayId
    , dcgaDirectConnectGatewayId
    , dcgaVirtualGatewayOwnerAccount
    , dcgaStateChangeError
    , dcgaVirtualGatewayRegion
    , dcgaAssociationState

    -- ** DirectConnectGatewayAttachment
    , DirectConnectGatewayAttachment
    , directConnectGatewayAttachment
    , dDirectConnectGatewayId
    , dAttachmentState
    , dStateChangeError
    , dVirtualInterfaceRegion
    , dVirtualInterfaceOwnerAccount
    , dVirtualInterfaceId

    -- ** Interconnect
    , Interconnect
    , interconnect
    , iLagId
    , iInterconnectId
    , iLocation
    , iInterconnectName
    , iAwsDevice
    , iLoaIssueTime
    , iBandwidth
    , iInterconnectState
    , iRegion

    -- ** Lag
    , Lag
    , lag
    , lagLagId
    , lagConnectionsBandwidth
    , lagMinimumLinks
    , lagLagName
    , lagLocation
    , lagConnections
    , lagAwsDevice
    , lagAllowsHostedConnections
    , lagNumberOfConnections
    , lagLagState
    , lagOwnerAccount
    , lagRegion

    -- ** Location
    , Location
    , location
    , lLocationName
    , lLocationCode

    -- ** NewBGPPeer
    , NewBGPPeer
    , newBGPPeer
    , nbpCustomerAddress
    , nbpAmazonAddress
    , nbpAddressFamily
    , nbpAsn
    , nbpAuthKey

    -- ** NewPrivateVirtualInterface
    , NewPrivateVirtualInterface
    , newPrivateVirtualInterface
    , nVirtualGatewayId
    , nCustomerAddress
    , nAmazonAddress
    , nAddressFamily
    , nDirectConnectGatewayId
    , nAuthKey
    , nVirtualInterfaceName
    , nVlan
    , nAsn

    -- ** NewPrivateVirtualInterfaceAllocation
    , NewPrivateVirtualInterfaceAllocation
    , newPrivateVirtualInterfaceAllocation
    , npviaCustomerAddress
    , npviaAmazonAddress
    , npviaAddressFamily
    , npviaAuthKey
    , npviaVirtualInterfaceName
    , npviaVlan
    , npviaAsn

    -- ** NewPublicVirtualInterface
    , NewPublicVirtualInterface
    , newPublicVirtualInterface
    , npviRouteFilterPrefixes
    , npviCustomerAddress
    , npviAmazonAddress
    , npviAddressFamily
    , npviAuthKey
    , npviVirtualInterfaceName
    , npviVlan
    , npviAsn

    -- ** NewPublicVirtualInterfaceAllocation
    , NewPublicVirtualInterfaceAllocation
    , newPublicVirtualInterfaceAllocation
    , newRouteFilterPrefixes
    , newCustomerAddress
    , newAmazonAddress
    , newAddressFamily
    , newAuthKey
    , newVirtualInterfaceName
    , newVlan
    , newAsn

    -- ** ResourceTag
    , ResourceTag
    , resourceTag
    , rtResourceARN
    , rtTags

    -- ** RouteFilterPrefix
    , RouteFilterPrefix
    , routeFilterPrefix
    , rfpCidr

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** VirtualGateway
    , VirtualGateway
    , virtualGateway
    , vgVirtualGatewayId
    , vgVirtualGatewayState

    -- ** VirtualInterface
    , VirtualInterface
    , virtualInterface
    , viBgpPeers
    , viVirtualGatewayId
    , viRouteFilterPrefixes
    , viCustomerAddress
    , viVlan
    , viLocation
    , viAmazonAddress
    , viAddressFamily
    , viVirtualInterfaceState
    , viConnectionId
    , viDirectConnectGatewayId
    , viAmazonSideASN
    , viVirtualInterfaceType
    , viAsn
    , viAuthKey
    , viCustomerRouterConfig
    , viOwnerAccount
    , viVirtualInterfaceName
    , viVirtualInterfaceId
    ) where

import Network.AWS.DirectConnect.AllocateHostedConnection
import Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
import Network.AWS.DirectConnect.AllocatePublicVirtualInterface
import Network.AWS.DirectConnect.AssociateConnectionWithLag
import Network.AWS.DirectConnect.AssociateHostedConnection
import Network.AWS.DirectConnect.AssociateVirtualInterface
import Network.AWS.DirectConnect.ConfirmConnection
import Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
import Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
import Network.AWS.DirectConnect.CreateBGPPeer
import Network.AWS.DirectConnect.CreateConnection
import Network.AWS.DirectConnect.CreateDirectConnectGateway
import Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
import Network.AWS.DirectConnect.CreateInterconnect
import Network.AWS.DirectConnect.CreateLag
import Network.AWS.DirectConnect.CreatePrivateVirtualInterface
import Network.AWS.DirectConnect.CreatePublicVirtualInterface
import Network.AWS.DirectConnect.DeleteBGPPeer
import Network.AWS.DirectConnect.DeleteConnection
import Network.AWS.DirectConnect.DeleteDirectConnectGateway
import Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
import Network.AWS.DirectConnect.DeleteInterconnect
import Network.AWS.DirectConnect.DeleteLag
import Network.AWS.DirectConnect.DeleteVirtualInterface
import Network.AWS.DirectConnect.DescribeConnections
import Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations
import Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
import Network.AWS.DirectConnect.DescribeDirectConnectGateways
import Network.AWS.DirectConnect.DescribeHostedConnections
import Network.AWS.DirectConnect.DescribeInterconnects
import Network.AWS.DirectConnect.DescribeLags
import Network.AWS.DirectConnect.DescribeLoa
import Network.AWS.DirectConnect.DescribeLocations
import Network.AWS.DirectConnect.DescribeTags
import Network.AWS.DirectConnect.DescribeVirtualGateways
import Network.AWS.DirectConnect.DescribeVirtualInterfaces
import Network.AWS.DirectConnect.DisassociateConnectionFromLag
import Network.AWS.DirectConnect.TagResource
import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.UntagResource
import Network.AWS.DirectConnect.UpdateLag
import Network.AWS.DirectConnect.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DirectConnect'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
