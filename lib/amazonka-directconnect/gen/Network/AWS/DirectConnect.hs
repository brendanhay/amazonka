{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Direct Connect links your internal network to an AWS Direct Connect location over a standard Ethernet fiber-optic cable. One end of the cable is connected to your router, the other to an AWS Direct Connect router. With this connection in place, you can create virtual interfaces directly to the AWS cloud (for example, to Amazon EC2 and Amazon S3) and to Amazon VPC, bypassing Internet service providers in your network path. A connection provides access to all AWS Regions except the China (Beijing) and (China) Ningxia Regions. AWS resources in the China Regions can only be accessed through locations associated with those Regions.
module Network.AWS.DirectConnect
    (
    -- * Service configuration
      mkServiceConfig

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

    -- ** DescribeDirectConnectGatewayAssociations (Paginated)
    , module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations

    -- ** DescribeInterconnects 
    , module Network.AWS.DirectConnect.DescribeInterconnects

    -- ** DescribeTags 
    , module Network.AWS.DirectConnect.DescribeTags

    -- ** CreateTransitVirtualInterface 
    , module Network.AWS.DirectConnect.CreateTransitVirtualInterface

    -- ** DescribeLoa 
    , module Network.AWS.DirectConnect.DescribeLoa

    -- ** DeleteConnection 
    , module Network.AWS.DirectConnect.DeleteConnection

    -- ** StartBgpFailoverTest 
    , module Network.AWS.DirectConnect.StartBgpFailoverTest

    -- ** UpdateVirtualInterfaceAttributes 
    , module Network.AWS.DirectConnect.UpdateVirtualInterfaceAttributes

    -- ** AssociateConnectionWithLag 
    , module Network.AWS.DirectConnect.AssociateConnectionWithLag

    -- ** CreateDirectConnectGatewayAssociationProposal 
    , module Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociationProposal

    -- ** CreateConnection 
    , module Network.AWS.DirectConnect.CreateConnection

    -- ** DescribeDirectConnectGateways (Paginated)
    , module Network.AWS.DirectConnect.DescribeDirectConnectGateways

    -- ** AssociateVirtualInterface 
    , module Network.AWS.DirectConnect.AssociateVirtualInterface

    -- ** DescribeConnections 
    , module Network.AWS.DirectConnect.DescribeConnections

    -- ** DeleteInterconnect 
    , module Network.AWS.DirectConnect.DeleteInterconnect

    -- ** ConfirmPrivateVirtualInterface 
    , module Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface

    -- ** UpdateDirectConnectGatewayAssociation 
    , module Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation

    -- ** DeleteDirectConnectGatewayAssociation 
    , module Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation

    -- ** DescribeLocations 
    , module Network.AWS.DirectConnect.DescribeLocations

    -- ** CreateDirectConnectGatewayAssociation 
    , module Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation

    -- ** AcceptDirectConnectGatewayAssociationProposal 
    , module Network.AWS.DirectConnect.AcceptDirectConnectGatewayAssociationProposal

    -- ** CreatePublicVirtualInterface 
    , module Network.AWS.DirectConnect.CreatePublicVirtualInterface

    -- ** AllocatePrivateVirtualInterface 
    , module Network.AWS.DirectConnect.AllocatePrivateVirtualInterface

    -- ** DescribeLags 
    , module Network.AWS.DirectConnect.DescribeLags

    -- ** ConfirmConnection 
    , module Network.AWS.DirectConnect.ConfirmConnection

    -- ** DescribeDirectConnectGatewayAttachments (Paginated)
    , module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments

    -- ** ConfirmPublicVirtualInterface 
    , module Network.AWS.DirectConnect.ConfirmPublicVirtualInterface

    -- ** DescribeVirtualGateways 
    , module Network.AWS.DirectConnect.DescribeVirtualGateways

    -- ** DeleteDirectConnectGatewayAssociationProposal 
    , module Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociationProposal

    -- ** StopBgpFailoverTest 
    , module Network.AWS.DirectConnect.StopBgpFailoverTest

    -- ** CreateDirectConnectGateway 
    , module Network.AWS.DirectConnect.CreateDirectConnectGateway

    -- ** DeleteDirectConnectGateway 
    , module Network.AWS.DirectConnect.DeleteDirectConnectGateway

    -- ** DescribeVirtualInterfaces 
    , module Network.AWS.DirectConnect.DescribeVirtualInterfaces

    -- ** ListVirtualInterfaceTestHistory 
    , module Network.AWS.DirectConnect.ListVirtualInterfaceTestHistory

    -- ** AllocateHostedConnection 
    , module Network.AWS.DirectConnect.AllocateHostedConnection

    -- ** DeleteVirtualInterface 
    , module Network.AWS.DirectConnect.DeleteVirtualInterface

    -- ** CreatePrivateVirtualInterface 
    , module Network.AWS.DirectConnect.CreatePrivateVirtualInterface

    -- ** AllocatePublicVirtualInterface 
    , module Network.AWS.DirectConnect.AllocatePublicVirtualInterface

    -- ** DescribeDirectConnectGatewayAssociationProposals 
    , module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociationProposals

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

    -- ** AllocateTransitVirtualInterface 
    , module Network.AWS.DirectConnect.AllocateTransitVirtualInterface

    -- ** CreateLag 
    , module Network.AWS.DirectConnect.CreateLag

    -- ** ConfirmTransitVirtualInterface 
    , module Network.AWS.DirectConnect.ConfirmTransitVirtualInterface

    -- ** DescribeHostedConnections 
    , module Network.AWS.DirectConnect.DescribeHostedConnections

    -- * Types

    -- ** VirtualInterface
    , VirtualInterface (..)
    , mkVirtualInterface
    , viAddressFamily
    , viAmazonAddress
    , viAmazonSideAsn
    , viAsn
    , viAuthKey
    , viAwsDeviceV2
    , viBgpPeers
    , viConnectionId
    , viCustomerAddress
    , viCustomerRouterConfig
    , viDirectConnectGatewayId
    , viJumboFrameCapable
    , viLocation
    , viMtu
    , viOwnerAccount
    , viRegion
    , viRouteFilterPrefixes
    , viTags
    , viVirtualGatewayId
    , viVirtualInterfaceId
    , viVirtualInterfaceName
    , viVirtualInterfaceState
    , viVirtualInterfaceType
    , viVlan

    -- ** VirtualGatewayId
    , VirtualGatewayId (..)

    -- ** PaginationToken
    , PaginationToken (..)

    -- ** NewTransitVirtualInterface
    , NewTransitVirtualInterface (..)
    , mkNewTransitVirtualInterface
    , ntviAddressFamily
    , ntviAmazonAddress
    , ntviAsn
    , ntviAuthKey
    , ntviCustomerAddress
    , ntviDirectConnectGatewayId
    , ntviMtu
    , ntviTags
    , ntviVirtualInterfaceName
    , ntviVlan

    -- ** LagId
    , LagId (..)

    -- ** BGPPeer
    , BGPPeer (..)
    , mkBGPPeer
    , bgppAddressFamily
    , bgppAmazonAddress
    , bgppAsn
    , bgppAuthKey
    , bgppAwsDeviceV2
    , bgppBgpPeerId
    , bgppBgpPeerState
    , bgppBgpStatus
    , bgppCustomerAddress

    -- ** InterconnectId
    , InterconnectId (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** DirectConnectGatewayAssociation
    , DirectConnectGatewayAssociation (..)
    , mkDirectConnectGatewayAssociation
    , dcgaAllowedPrefixesToDirectConnectGateway
    , dcgaAssociatedGateway
    , dcgaAssociationId
    , dcgaAssociationState
    , dcgaDirectConnectGatewayId
    , dcgaDirectConnectGatewayOwnerAccount
    , dcgaStateChangeError
    , dcgaVirtualGatewayId
    , dcgaVirtualGatewayOwnerAccount
    , dcgaVirtualGatewayRegion

    -- ** BGPAuthKey
    , BGPAuthKey (..)

    -- ** CustomerAddress
    , CustomerAddress (..)

    -- ** DirectConnectGatewayAttachment
    , DirectConnectGatewayAttachment (..)
    , mkDirectConnectGatewayAttachment
    , dAttachmentState
    , dAttachmentType
    , dDirectConnectGatewayId
    , dStateChangeError
    , dVirtualInterfaceId
    , dVirtualInterfaceOwnerAccount
    , dVirtualInterfaceRegion

    -- ** AwsDevice
    , AwsDevice (..)

    -- ** Location
    , Location (..)
    , mkLocation
    , lAvailablePortSpeeds
    , lAvailableProviders
    , lLocationCode
    , lLocationName
    , lRegion

    -- ** LagName
    , LagName (..)

    -- ** InterconnectName
    , InterconnectName (..)

    -- ** Connections
    , Connections (..)
    , mkConnections
    , cConnections

    -- ** AssociatedGatewayId
    , AssociatedGatewayId (..)

    -- ** DirectConnectGatewayAttachmentState
    , DirectConnectGatewayAttachmentState (..)

    -- ** NewPrivateVirtualInterfaceAllocation
    , NewPrivateVirtualInterfaceAllocation (..)
    , mkNewPrivateVirtualInterfaceAllocation
    , npviaVirtualInterfaceName
    , npviaVlan
    , npviaAsn
    , npviaAddressFamily
    , npviaAmazonAddress
    , npviaAuthKey
    , npviaCustomerAddress
    , npviaMtu
    , npviaTags

    -- ** DirectConnectGatewayAssociationState
    , DirectConnectGatewayAssociationState (..)

    -- ** HasLogicalRedundancy
    , HasLogicalRedundancy (..)

    -- ** AddressFamily
    , AddressFamily (..)

    -- ** AmazonAddress
    , AmazonAddress (..)

    -- ** FailureTestHistoryStatus
    , FailureTestHistoryStatus (..)

    -- ** GatewayIdentifier
    , GatewayIdentifier (..)

    -- ** VirtualInterfaceState
    , VirtualInterfaceState (..)

    -- ** DirectConnectGatewayAssociationProposalId
    , DirectConnectGatewayAssociationProposalId (..)

    -- ** TestId
    , TestId (..)

    -- ** DirectConnectGateway
    , DirectConnectGateway (..)
    , mkDirectConnectGateway
    , dcgAmazonSideAsn
    , dcgDirectConnectGatewayId
    , dcgDirectConnectGatewayName
    , dcgDirectConnectGatewayState
    , dcgOwnerAccount
    , dcgStateChangeError

    -- ** ConnectionId
    , ConnectionId (..)

    -- ** DirectConnectGatewayId
    , DirectConnectGatewayId (..)

    -- ** DirectConnectGatewayAssociationProposal
    , DirectConnectGatewayAssociationProposal (..)
    , mkDirectConnectGatewayAssociationProposal
    , dcgapAssociatedGateway
    , dcgapDirectConnectGatewayId
    , dcgapDirectConnectGatewayOwnerAccount
    , dcgapExistingAllowedPrefixesToDirectConnectGateway
    , dcgapProposalId
    , dcgapProposalState
    , dcgapRequestedAllowedPrefixesToDirectConnectGateway

    -- ** PortSpeed
    , PortSpeed (..)

    -- ** Connection
    , Connection (..)
    , mkConnection
    , cAwsDevice
    , cAwsDeviceV2
    , cBandwidth
    , cConnectionId
    , cConnectionName
    , cConnectionState
    , cHasLogicalRedundancy
    , cJumboFrameCapable
    , cLagId
    , cLoaIssueTime
    , cLocation
    , cOwnerAccount
    , cPartnerName
    , cProviderName
    , cRegion
    , cTags
    , cVlan

    -- ** NewPublicVirtualInterface
    , NewPublicVirtualInterface (..)
    , mkNewPublicVirtualInterface
    , npviVirtualInterfaceName
    , npviVlan
    , npviAsn
    , npviAddressFamily
    , npviAmazonAddress
    , npviAuthKey
    , npviCustomerAddress
    , npviRouteFilterPrefixes
    , npviTags

    -- ** StateChangeError
    , StateChangeError (..)

    -- ** PartnerName
    , PartnerName (..)

    -- ** BGPStatus
    , BGPStatus (..)

    -- ** VirtualGatewayRegion
    , VirtualGatewayRegion (..)

    -- ** ConnectionName
    , ConnectionName (..)

    -- ** DirectConnectGatewayName
    , DirectConnectGatewayName (..)

    -- ** VirtualInterfaceType
    , VirtualInterfaceType (..)

    -- ** CIDR
    , CIDR (..)

    -- ** VirtualInterfaceRegion
    , VirtualInterfaceRegion (..)

    -- ** DirectConnectGatewayState
    , DirectConnectGatewayState (..)

    -- ** DirectConnectGatewayAssociationId
    , DirectConnectGatewayAssociationId (..)

    -- ** ResourceArn
    , ResourceArn (..)

    -- ** NewTransitVirtualInterfaceAllocation
    , NewTransitVirtualInterfaceAllocation (..)
    , mkNewTransitVirtualInterfaceAllocation
    , ntviaAddressFamily
    , ntviaAmazonAddress
    , ntviaAsn
    , ntviaAuthKey
    , ntviaCustomerAddress
    , ntviaMtu
    , ntviaTags
    , ntviaVirtualInterfaceName
    , ntviaVlan

    -- ** Bandwidth
    , Bandwidth (..)

    -- ** LocationName
    , LocationName (..)

    -- ** Lag
    , Lag (..)
    , mkLag
    , lfAllowsHostedConnections
    , lfAwsDevice
    , lfAwsDeviceV2
    , lfConnections
    , lfConnectionsBandwidth
    , lfHasLogicalRedundancy
    , lfJumboFrameCapable
    , lfLagId
    , lfLagName
    , lfLagState
    , lfLocation
    , lfMinimumLinks
    , lfNumberOfConnections
    , lfOwnerAccount
    , lfProviderName
    , lfRegion
    , lfTags

    -- ** DirectConnectGatewayAttachmentType
    , DirectConnectGatewayAttachmentType (..)

    -- ** Interconnect
    , Interconnect (..)
    , mkInterconnect
    , iAwsDevice
    , iAwsDeviceV2
    , iBandwidth
    , iHasLogicalRedundancy
    , iInterconnectId
    , iInterconnectName
    , iInterconnectState
    , iJumboFrameCapable
    , iLagId
    , iLoaIssueTime
    , iLocation
    , iProviderName
    , iRegion
    , iTags

    -- ** BGPPeerId
    , BGPPeerId (..)

    -- ** GatewayIdToAssociate
    , GatewayIdToAssociate (..)

    -- ** InterconnectState
    , InterconnectState (..)

    -- ** LagState
    , LagState (..)

    -- ** NewPrivateVirtualInterface
    , NewPrivateVirtualInterface (..)
    , mkNewPrivateVirtualInterface
    , nVirtualInterfaceName
    , nVlan
    , nAsn
    , nAddressFamily
    , nAmazonAddress
    , nAuthKey
    , nCustomerAddress
    , nDirectConnectGatewayId
    , nMtu
    , nTags
    , nVirtualGatewayId

    -- ** TagKey
    , TagKey (..)

    -- ** Region
    , Region (..)

    -- ** ResourceTag
    , ResourceTag (..)
    , mkResourceTag
    , rtResourceArn
    , rtTags

    -- ** LocationCode
    , LocationCode (..)

    -- ** VirtualInterfaceTestHistory
    , VirtualInterfaceTestHistory (..)
    , mkVirtualInterfaceTestHistory
    , vithBgpPeers
    , vithEndTime
    , vithOwnerAccount
    , vithStartTime
    , vithStatus
    , vithTestDurationInMinutes
    , vithTestId
    , vithVirtualInterfaceId

    -- ** OwnerAccount
    , OwnerAccount (..)

    -- ** AssociatedGateway
    , AssociatedGateway (..)
    , mkAssociatedGateway
    , agId
    , agOwnerAccount
    , agRegion
    , agType

    -- ** BGPPeerState
    , BGPPeerState (..)

    -- ** GatewayType
    , GatewayType (..)

    -- ** LoaContentType
    , LoaContentType (..)

    -- ** NewBGPPeer
    , NewBGPPeer (..)
    , mkNewBGPPeer
    , nbgppAddressFamily
    , nbgppAmazonAddress
    , nbgppAsn
    , nbgppAuthKey
    , nbgppCustomerAddress

    -- ** VirtualInterfaceName
    , VirtualInterfaceName (..)

    -- ** VirtualGatewayState
    , VirtualGatewayState (..)

    -- ** ProviderName
    , ProviderName (..)

    -- ** NewPublicVirtualInterfaceAllocation
    , NewPublicVirtualInterfaceAllocation (..)
    , mkNewPublicVirtualInterfaceAllocation
    , npviafVirtualInterfaceName
    , npviafVlan
    , npviafAsn
    , npviafAddressFamily
    , npviafAmazonAddress
    , npviafAuthKey
    , npviafCustomerAddress
    , npviafRouteFilterPrefixes
    , npviafTags

    -- ** ConnectionState
    , ConnectionState (..)

    -- ** VirtualGateway
    , VirtualGateway (..)
    , mkVirtualGateway
    , vgVirtualGatewayId
    , vgVirtualGatewayState

    -- ** VirtualInterfaceId
    , VirtualInterfaceId (..)

    -- ** AwsDeviceV2
    , AwsDeviceV2 (..)

    -- ** RouteFilterPrefix
    , RouteFilterPrefix (..)
    , mkRouteFilterPrefix
    , rfpCidr

    -- ** DirectConnectGatewayAssociationProposalState
    , DirectConnectGatewayAssociationProposalState (..)

    -- ** NextToken
    , NextToken (..)

    -- ** AuthKey
    , AuthKey (..)

    -- ** CustomerRouterConfig
    , CustomerRouterConfig (..)

    -- ** ProposalId
    , ProposalId (..)

    -- ** AssociatedGatewayOwnerAccount
    , AssociatedGatewayOwnerAccount (..)

    -- ** BgpPeerId
    , BgpPeerId (..)

    -- ** GatewayId
    , GatewayId (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** AssociationId
    , AssociationId (..)

    -- ** DirectConnectGatewayOwnerAccount
    , DirectConnectGatewayOwnerAccount (..)

    -- ** VirtualGatewayOwnerAccount
    , VirtualGatewayOwnerAccount (..)

    -- ** VirtualInterfaceOwnerAccount
    , VirtualInterfaceOwnerAccount (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Waiters
import Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations
import Network.AWS.DirectConnect.DescribeInterconnects
import Network.AWS.DirectConnect.DescribeTags
import Network.AWS.DirectConnect.CreateTransitVirtualInterface
import Network.AWS.DirectConnect.DescribeLoa
import Network.AWS.DirectConnect.DeleteConnection
import Network.AWS.DirectConnect.StartBgpFailoverTest
import Network.AWS.DirectConnect.UpdateVirtualInterfaceAttributes
import Network.AWS.DirectConnect.AssociateConnectionWithLag
import Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociationProposal
import Network.AWS.DirectConnect.CreateConnection
import Network.AWS.DirectConnect.DescribeDirectConnectGateways
import Network.AWS.DirectConnect.AssociateVirtualInterface
import Network.AWS.DirectConnect.DescribeConnections
import Network.AWS.DirectConnect.DeleteInterconnect
import Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
import Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation
import Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
import Network.AWS.DirectConnect.DescribeLocations
import Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
import Network.AWS.DirectConnect.AcceptDirectConnectGatewayAssociationProposal
import Network.AWS.DirectConnect.CreatePublicVirtualInterface
import Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
import Network.AWS.DirectConnect.DescribeLags
import Network.AWS.DirectConnect.ConfirmConnection
import Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
import Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
import Network.AWS.DirectConnect.DescribeVirtualGateways
import Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociationProposal
import Network.AWS.DirectConnect.StopBgpFailoverTest
import Network.AWS.DirectConnect.CreateDirectConnectGateway
import Network.AWS.DirectConnect.DeleteDirectConnectGateway
import Network.AWS.DirectConnect.DescribeVirtualInterfaces
import Network.AWS.DirectConnect.ListVirtualInterfaceTestHistory
import Network.AWS.DirectConnect.AllocateHostedConnection
import Network.AWS.DirectConnect.DeleteVirtualInterface
import Network.AWS.DirectConnect.CreatePrivateVirtualInterface
import Network.AWS.DirectConnect.AllocatePublicVirtualInterface
import Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociationProposals
import Network.AWS.DirectConnect.DisassociateConnectionFromLag
import Network.AWS.DirectConnect.TagResource
import Network.AWS.DirectConnect.DeleteLag
import Network.AWS.DirectConnect.UpdateLag
import Network.AWS.DirectConnect.UntagResource
import Network.AWS.DirectConnect.CreateBGPPeer
import Network.AWS.DirectConnect.AssociateHostedConnection
import Network.AWS.DirectConnect.CreateInterconnect
import Network.AWS.DirectConnect.DeleteBGPPeer
import Network.AWS.DirectConnect.AllocateTransitVirtualInterface
import Network.AWS.DirectConnect.CreateLag
import Network.AWS.DirectConnect.ConfirmTransitVirtualInterface
import Network.AWS.DirectConnect.DescribeHostedConnections
import qualified Network.AWS.Prelude as Lude

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
