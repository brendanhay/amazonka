{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.NetworkManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-07-05@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Transit Gateway Network Manager (Network Manager) enables you to create
-- a global network, in which you can monitor your AWS and on-premises
-- networks that are built around transit gateways.
--
-- The Network Manager APIs are supported in the US West (Oregon) Region
-- only. You must specify the @us-west-2@ Region in all requests made to
-- Network Manager.
module Amazonka.NetworkManager
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetLinkAssociations (Paginated)
    GetLinkAssociations (GetLinkAssociations'),
    newGetLinkAssociations,
    GetLinkAssociationsResponse (GetLinkAssociationsResponse'),
    newGetLinkAssociationsResponse,

    -- ** AssociateLink
    AssociateLink (AssociateLink'),
    newAssociateLink,
    AssociateLinkResponse (AssociateLinkResponse'),
    newAssociateLinkResponse,

    -- ** AssociateTransitGatewayConnectPeer
    AssociateTransitGatewayConnectPeer (AssociateTransitGatewayConnectPeer'),
    newAssociateTransitGatewayConnectPeer,
    AssociateTransitGatewayConnectPeerResponse (AssociateTransitGatewayConnectPeerResponse'),
    newAssociateTransitGatewayConnectPeerResponse,

    -- ** CreateSite
    CreateSite (CreateSite'),
    newCreateSite,
    CreateSiteResponse (CreateSiteResponse'),
    newCreateSiteResponse,

    -- ** DeleteConnection
    DeleteConnection (DeleteConnection'),
    newDeleteConnection,
    DeleteConnectionResponse (DeleteConnectionResponse'),
    newDeleteConnectionResponse,

    -- ** UpdateConnection
    UpdateConnection (UpdateConnection'),
    newUpdateConnection,
    UpdateConnectionResponse (UpdateConnectionResponse'),
    newUpdateConnectionResponse,

    -- ** DeregisterTransitGateway
    DeregisterTransitGateway (DeregisterTransitGateway'),
    newDeregisterTransitGateway,
    DeregisterTransitGatewayResponse (DeregisterTransitGatewayResponse'),
    newDeregisterTransitGatewayResponse,

    -- ** GetTransitGatewayConnectPeerAssociations (Paginated)
    GetTransitGatewayConnectPeerAssociations (GetTransitGatewayConnectPeerAssociations'),
    newGetTransitGatewayConnectPeerAssociations,
    GetTransitGatewayConnectPeerAssociationsResponse (GetTransitGatewayConnectPeerAssociationsResponse'),
    newGetTransitGatewayConnectPeerAssociationsResponse,

    -- ** UpdateSite
    UpdateSite (UpdateSite'),
    newUpdateSite,
    UpdateSiteResponse (UpdateSiteResponse'),
    newUpdateSiteResponse,

    -- ** DeleteSite
    DeleteSite (DeleteSite'),
    newDeleteSite,
    DeleteSiteResponse (DeleteSiteResponse'),
    newDeleteSiteResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DisassociateLink
    DisassociateLink (DisassociateLink'),
    newDisassociateLink,
    DisassociateLinkResponse (DisassociateLinkResponse'),
    newDisassociateLinkResponse,

    -- ** CreateConnection
    CreateConnection (CreateConnection'),
    newCreateConnection,
    CreateConnectionResponse (CreateConnectionResponse'),
    newCreateConnectionResponse,

    -- ** GetDevices (Paginated)
    GetDevices (GetDevices'),
    newGetDevices,
    GetDevicesResponse (GetDevicesResponse'),
    newGetDevicesResponse,

    -- ** GetLinks (Paginated)
    GetLinks (GetLinks'),
    newGetLinks,
    GetLinksResponse (GetLinksResponse'),
    newGetLinksResponse,

    -- ** DescribeGlobalNetworks (Paginated)
    DescribeGlobalNetworks (DescribeGlobalNetworks'),
    newDescribeGlobalNetworks,
    DescribeGlobalNetworksResponse (DescribeGlobalNetworksResponse'),
    newDescribeGlobalNetworksResponse,

    -- ** DisassociateCustomerGateway
    DisassociateCustomerGateway (DisassociateCustomerGateway'),
    newDisassociateCustomerGateway,
    DisassociateCustomerGatewayResponse (DisassociateCustomerGatewayResponse'),
    newDisassociateCustomerGatewayResponse,

    -- ** DisassociateTransitGatewayConnectPeer
    DisassociateTransitGatewayConnectPeer (DisassociateTransitGatewayConnectPeer'),
    newDisassociateTransitGatewayConnectPeer,
    DisassociateTransitGatewayConnectPeerResponse (DisassociateTransitGatewayConnectPeerResponse'),
    newDisassociateTransitGatewayConnectPeerResponse,

    -- ** CreateGlobalNetwork
    CreateGlobalNetwork (CreateGlobalNetwork'),
    newCreateGlobalNetwork,
    CreateGlobalNetworkResponse (CreateGlobalNetworkResponse'),
    newCreateGlobalNetworkResponse,

    -- ** CreateLink
    CreateLink (CreateLink'),
    newCreateLink,
    CreateLinkResponse (CreateLinkResponse'),
    newCreateLinkResponse,

    -- ** DeleteGlobalNetwork
    DeleteGlobalNetwork (DeleteGlobalNetwork'),
    newDeleteGlobalNetwork,
    DeleteGlobalNetworkResponse (DeleteGlobalNetworkResponse'),
    newDeleteGlobalNetworkResponse,

    -- ** UpdateGlobalNetwork
    UpdateGlobalNetwork (UpdateGlobalNetwork'),
    newUpdateGlobalNetwork,
    UpdateGlobalNetworkResponse (UpdateGlobalNetworkResponse'),
    newUpdateGlobalNetworkResponse,

    -- ** CreateDevice
    CreateDevice (CreateDevice'),
    newCreateDevice,
    CreateDeviceResponse (CreateDeviceResponse'),
    newCreateDeviceResponse,

    -- ** AssociateCustomerGateway
    AssociateCustomerGateway (AssociateCustomerGateway'),
    newAssociateCustomerGateway,
    AssociateCustomerGatewayResponse (AssociateCustomerGatewayResponse'),
    newAssociateCustomerGatewayResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetCustomerGatewayAssociations (Paginated)
    GetCustomerGatewayAssociations (GetCustomerGatewayAssociations'),
    newGetCustomerGatewayAssociations,
    GetCustomerGatewayAssociationsResponse (GetCustomerGatewayAssociationsResponse'),
    newGetCustomerGatewayAssociationsResponse,

    -- ** GetTransitGatewayRegistrations (Paginated)
    GetTransitGatewayRegistrations (GetTransitGatewayRegistrations'),
    newGetTransitGatewayRegistrations,
    GetTransitGatewayRegistrationsResponse (GetTransitGatewayRegistrationsResponse'),
    newGetTransitGatewayRegistrationsResponse,

    -- ** GetConnections (Paginated)
    GetConnections (GetConnections'),
    newGetConnections,
    GetConnectionsResponse (GetConnectionsResponse'),
    newGetConnectionsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** GetSites (Paginated)
    GetSites (GetSites'),
    newGetSites,
    GetSitesResponse (GetSitesResponse'),
    newGetSitesResponse,

    -- ** RegisterTransitGateway
    RegisterTransitGateway (RegisterTransitGateway'),
    newRegisterTransitGateway,
    RegisterTransitGatewayResponse (RegisterTransitGatewayResponse'),
    newRegisterTransitGatewayResponse,

    -- ** DeleteDevice
    DeleteDevice (DeleteDevice'),
    newDeleteDevice,
    DeleteDeviceResponse (DeleteDeviceResponse'),
    newDeleteDeviceResponse,

    -- ** UpdateDevice
    UpdateDevice (UpdateDevice'),
    newUpdateDevice,
    UpdateDeviceResponse (UpdateDeviceResponse'),
    newUpdateDeviceResponse,

    -- ** DeleteLink
    DeleteLink (DeleteLink'),
    newDeleteLink,
    DeleteLinkResponse (DeleteLinkResponse'),
    newDeleteLinkResponse,

    -- ** UpdateLink
    UpdateLink (UpdateLink'),
    newUpdateLink,
    UpdateLinkResponse (UpdateLinkResponse'),
    newUpdateLinkResponse,

    -- * Types

    -- ** ConnectionState
    ConnectionState (..),

    -- ** CustomerGatewayAssociationState
    CustomerGatewayAssociationState (..),

    -- ** DeviceState
    DeviceState (..),

    -- ** GlobalNetworkState
    GlobalNetworkState (..),

    -- ** LinkAssociationState
    LinkAssociationState (..),

    -- ** LinkState
    LinkState (..),

    -- ** SiteState
    SiteState (..),

    -- ** TransitGatewayConnectPeerAssociationState
    TransitGatewayConnectPeerAssociationState (..),

    -- ** TransitGatewayRegistrationState
    TransitGatewayRegistrationState (..),

    -- ** AWSLocation
    AWSLocation (AWSLocation'),
    newAWSLocation,

    -- ** Bandwidth
    Bandwidth (Bandwidth'),
    newBandwidth,

    -- ** Connection
    Connection (Connection'),
    newConnection,

    -- ** CustomerGatewayAssociation
    CustomerGatewayAssociation (CustomerGatewayAssociation'),
    newCustomerGatewayAssociation,

    -- ** Device
    Device (Device'),
    newDevice,

    -- ** GlobalNetwork
    GlobalNetwork (GlobalNetwork'),
    newGlobalNetwork,

    -- ** Link
    Link (Link'),
    newLink,

    -- ** LinkAssociation
    LinkAssociation (LinkAssociation'),
    newLinkAssociation,

    -- ** Location
    Location (Location'),
    newLocation,

    -- ** Site
    Site (Site'),
    newSite,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TransitGatewayConnectPeerAssociation
    TransitGatewayConnectPeerAssociation (TransitGatewayConnectPeerAssociation'),
    newTransitGatewayConnectPeerAssociation,

    -- ** TransitGatewayRegistration
    TransitGatewayRegistration (TransitGatewayRegistration'),
    newTransitGatewayRegistration,

    -- ** TransitGatewayRegistrationStateReason
    TransitGatewayRegistrationStateReason (TransitGatewayRegistrationStateReason'),
    newTransitGatewayRegistrationStateReason,
  )
where

import Amazonka.NetworkManager.AssociateCustomerGateway
import Amazonka.NetworkManager.AssociateLink
import Amazonka.NetworkManager.AssociateTransitGatewayConnectPeer
import Amazonka.NetworkManager.CreateConnection
import Amazonka.NetworkManager.CreateDevice
import Amazonka.NetworkManager.CreateGlobalNetwork
import Amazonka.NetworkManager.CreateLink
import Amazonka.NetworkManager.CreateSite
import Amazonka.NetworkManager.DeleteConnection
import Amazonka.NetworkManager.DeleteDevice
import Amazonka.NetworkManager.DeleteGlobalNetwork
import Amazonka.NetworkManager.DeleteLink
import Amazonka.NetworkManager.DeleteSite
import Amazonka.NetworkManager.DeregisterTransitGateway
import Amazonka.NetworkManager.DescribeGlobalNetworks
import Amazonka.NetworkManager.DisassociateCustomerGateway
import Amazonka.NetworkManager.DisassociateLink
import Amazonka.NetworkManager.DisassociateTransitGatewayConnectPeer
import Amazonka.NetworkManager.GetConnections
import Amazonka.NetworkManager.GetCustomerGatewayAssociations
import Amazonka.NetworkManager.GetDevices
import Amazonka.NetworkManager.GetLinkAssociations
import Amazonka.NetworkManager.GetLinks
import Amazonka.NetworkManager.GetSites
import Amazonka.NetworkManager.GetTransitGatewayConnectPeerAssociations
import Amazonka.NetworkManager.GetTransitGatewayRegistrations
import Amazonka.NetworkManager.Lens
import Amazonka.NetworkManager.ListTagsForResource
import Amazonka.NetworkManager.RegisterTransitGateway
import Amazonka.NetworkManager.TagResource
import Amazonka.NetworkManager.Types
import Amazonka.NetworkManager.UntagResource
import Amazonka.NetworkManager.UpdateConnection
import Amazonka.NetworkManager.UpdateDevice
import Amazonka.NetworkManager.UpdateGlobalNetwork
import Amazonka.NetworkManager.UpdateLink
import Amazonka.NetworkManager.UpdateSite
import Amazonka.NetworkManager.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'NetworkManager'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
