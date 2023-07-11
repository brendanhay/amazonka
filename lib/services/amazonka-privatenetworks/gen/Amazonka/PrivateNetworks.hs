{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.PrivateNetworks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-12-03@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Private 5G is a managed service that makes it easy
-- to deploy, operate, and scale your own private mobile network at your
-- on-premises location. Private 5G provides the pre-configured hardware
-- and software for mobile networks, helps automate setup, and scales
-- capacity on demand to support additional devices as needed.
module Amazonka.PrivateNetworks
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcknowledgeOrderReceipt
    AcknowledgeOrderReceipt (AcknowledgeOrderReceipt'),
    newAcknowledgeOrderReceipt,
    AcknowledgeOrderReceiptResponse (AcknowledgeOrderReceiptResponse'),
    newAcknowledgeOrderReceiptResponse,

    -- ** ActivateDeviceIdentifier
    ActivateDeviceIdentifier (ActivateDeviceIdentifier'),
    newActivateDeviceIdentifier,
    ActivateDeviceIdentifierResponse (ActivateDeviceIdentifierResponse'),
    newActivateDeviceIdentifierResponse,

    -- ** ActivateNetworkSite
    ActivateNetworkSite (ActivateNetworkSite'),
    newActivateNetworkSite,
    ActivateNetworkSiteResponse (ActivateNetworkSiteResponse'),
    newActivateNetworkSiteResponse,

    -- ** ConfigureAccessPoint
    ConfigureAccessPoint (ConfigureAccessPoint'),
    newConfigureAccessPoint,
    ConfigureAccessPointResponse (ConfigureAccessPointResponse'),
    newConfigureAccessPointResponse,

    -- ** CreateNetwork
    CreateNetwork (CreateNetwork'),
    newCreateNetwork,
    CreateNetworkResponse (CreateNetworkResponse'),
    newCreateNetworkResponse,

    -- ** CreateNetworkSite
    CreateNetworkSite (CreateNetworkSite'),
    newCreateNetworkSite,
    CreateNetworkSiteResponse (CreateNetworkSiteResponse'),
    newCreateNetworkSiteResponse,

    -- ** DeactivateDeviceIdentifier
    DeactivateDeviceIdentifier (DeactivateDeviceIdentifier'),
    newDeactivateDeviceIdentifier,
    DeactivateDeviceIdentifierResponse (DeactivateDeviceIdentifierResponse'),
    newDeactivateDeviceIdentifierResponse,

    -- ** DeleteNetwork
    DeleteNetwork (DeleteNetwork'),
    newDeleteNetwork,
    DeleteNetworkResponse (DeleteNetworkResponse'),
    newDeleteNetworkResponse,

    -- ** DeleteNetworkSite
    DeleteNetworkSite (DeleteNetworkSite'),
    newDeleteNetworkSite,
    DeleteNetworkSiteResponse (DeleteNetworkSiteResponse'),
    newDeleteNetworkSiteResponse,

    -- ** GetDeviceIdentifier
    GetDeviceIdentifier (GetDeviceIdentifier'),
    newGetDeviceIdentifier,
    GetDeviceIdentifierResponse (GetDeviceIdentifierResponse'),
    newGetDeviceIdentifierResponse,

    -- ** GetNetwork
    GetNetwork (GetNetwork'),
    newGetNetwork,
    GetNetworkResponse (GetNetworkResponse'),
    newGetNetworkResponse,

    -- ** GetNetworkResource
    GetNetworkResource (GetNetworkResource'),
    newGetNetworkResource,
    GetNetworkResourceResponse (GetNetworkResourceResponse'),
    newGetNetworkResourceResponse,

    -- ** GetNetworkSite
    GetNetworkSite (GetNetworkSite'),
    newGetNetworkSite,
    GetNetworkSiteResponse (GetNetworkSiteResponse'),
    newGetNetworkSiteResponse,

    -- ** GetOrder
    GetOrder (GetOrder'),
    newGetOrder,
    GetOrderResponse (GetOrderResponse'),
    newGetOrderResponse,

    -- ** ListDeviceIdentifiers (Paginated)
    ListDeviceIdentifiers (ListDeviceIdentifiers'),
    newListDeviceIdentifiers,
    ListDeviceIdentifiersResponse (ListDeviceIdentifiersResponse'),
    newListDeviceIdentifiersResponse,

    -- ** ListNetworkResources (Paginated)
    ListNetworkResources (ListNetworkResources'),
    newListNetworkResources,
    ListNetworkResourcesResponse (ListNetworkResourcesResponse'),
    newListNetworkResourcesResponse,

    -- ** ListNetworkSites (Paginated)
    ListNetworkSites (ListNetworkSites'),
    newListNetworkSites,
    ListNetworkSitesResponse (ListNetworkSitesResponse'),
    newListNetworkSitesResponse,

    -- ** ListNetworks (Paginated)
    ListNetworks (ListNetworks'),
    newListNetworks,
    ListNetworksResponse (ListNetworksResponse'),
    newListNetworksResponse,

    -- ** ListOrders (Paginated)
    ListOrders (ListOrders'),
    newListOrders,
    ListOrdersResponse (ListOrdersResponse'),
    newListOrdersResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** Ping
    Ping (Ping'),
    newPing,
    PingResponse (PingResponse'),
    newPingResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateNetworkSite
    UpdateNetworkSite (UpdateNetworkSite'),
    newUpdateNetworkSite,
    UpdateNetworkSiteResponse (UpdateNetworkSiteResponse'),
    newUpdateNetworkSiteResponse,

    -- ** UpdateNetworkSitePlan
    UpdateNetworkSitePlan (UpdateNetworkSitePlan'),
    newUpdateNetworkSitePlan,
    UpdateNetworkSiteResponse (UpdateNetworkSiteResponse'),
    newUpdateNetworkSiteResponse,

    -- * Types

    -- ** AcknowledgmentStatus
    AcknowledgmentStatus (..),

    -- ** DeviceIdentifierFilterKeys
    DeviceIdentifierFilterKeys (..),

    -- ** DeviceIdentifierStatus
    DeviceIdentifierStatus (..),

    -- ** ElevationReference
    ElevationReference (..),

    -- ** ElevationUnit
    ElevationUnit (..),

    -- ** HealthStatus
    HealthStatus (..),

    -- ** NetworkFilterKeys
    NetworkFilterKeys (..),

    -- ** NetworkResourceDefinitionType
    NetworkResourceDefinitionType (..),

    -- ** NetworkResourceFilterKeys
    NetworkResourceFilterKeys (..),

    -- ** NetworkResourceStatus
    NetworkResourceStatus (..),

    -- ** NetworkResourceType
    NetworkResourceType (..),

    -- ** NetworkSiteFilterKeys
    NetworkSiteFilterKeys (..),

    -- ** NetworkSiteStatus
    NetworkSiteStatus (..),

    -- ** NetworkStatus
    NetworkStatus (..),

    -- ** OrderFilterKeys
    OrderFilterKeys (..),

    -- ** Address
    Address (Address'),
    newAddress,

    -- ** DeviceIdentifier
    DeviceIdentifier (DeviceIdentifier'),
    newDeviceIdentifier,

    -- ** NameValuePair
    NameValuePair (NameValuePair'),
    newNameValuePair,

    -- ** Network
    Network (Network'),
    newNetwork,

    -- ** NetworkResource
    NetworkResource (NetworkResource'),
    newNetworkResource,

    -- ** NetworkResourceDefinition
    NetworkResourceDefinition (NetworkResourceDefinition'),
    newNetworkResourceDefinition,

    -- ** NetworkSite
    NetworkSite (NetworkSite'),
    newNetworkSite,

    -- ** Order
    Order (Order'),
    newOrder,

    -- ** Position
    Position (Position'),
    newPosition,

    -- ** SitePlan
    SitePlan (SitePlan'),
    newSitePlan,

    -- ** TrackingInformation
    TrackingInformation (TrackingInformation'),
    newTrackingInformation,

    -- ** UpdateNetworkSiteResponse
    UpdateNetworkSiteResponse (UpdateNetworkSiteResponse'),
    newUpdateNetworkSiteResponse,
  )
where

import Amazonka.PrivateNetworks.AcknowledgeOrderReceipt
import Amazonka.PrivateNetworks.ActivateDeviceIdentifier
import Amazonka.PrivateNetworks.ActivateNetworkSite
import Amazonka.PrivateNetworks.ConfigureAccessPoint
import Amazonka.PrivateNetworks.CreateNetwork
import Amazonka.PrivateNetworks.CreateNetworkSite
import Amazonka.PrivateNetworks.DeactivateDeviceIdentifier
import Amazonka.PrivateNetworks.DeleteNetwork
import Amazonka.PrivateNetworks.DeleteNetworkSite
import Amazonka.PrivateNetworks.GetDeviceIdentifier
import Amazonka.PrivateNetworks.GetNetwork
import Amazonka.PrivateNetworks.GetNetworkResource
import Amazonka.PrivateNetworks.GetNetworkSite
import Amazonka.PrivateNetworks.GetOrder
import Amazonka.PrivateNetworks.Lens
import Amazonka.PrivateNetworks.ListDeviceIdentifiers
import Amazonka.PrivateNetworks.ListNetworkResources
import Amazonka.PrivateNetworks.ListNetworkSites
import Amazonka.PrivateNetworks.ListNetworks
import Amazonka.PrivateNetworks.ListOrders
import Amazonka.PrivateNetworks.ListTagsForResource
import Amazonka.PrivateNetworks.Ping
import Amazonka.PrivateNetworks.TagResource
import Amazonka.PrivateNetworks.Types
import Amazonka.PrivateNetworks.UntagResource
import Amazonka.PrivateNetworks.UpdateNetworkSite
import Amazonka.PrivateNetworks.UpdateNetworkSitePlan
import Amazonka.PrivateNetworks.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'PrivateNetworks'.

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
