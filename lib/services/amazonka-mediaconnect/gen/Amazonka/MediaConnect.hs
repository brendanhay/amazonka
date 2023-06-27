{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MediaConnect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-11-14@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- API for AWS Elemental MediaConnect
module Amazonka.MediaConnect
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AddFlowOutputs420Exception
    _AddFlowOutputs420Exception,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** CreateBridge420Exception
    _CreateBridge420Exception,

    -- ** CreateFlow420Exception
    _CreateFlow420Exception,

    -- ** CreateGateway420Exception
    _CreateGateway420Exception,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** GrantFlowEntitlements420Exception
    _GrantFlowEntitlements420Exception,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- ** FlowActive
    newFlowActive,

    -- ** FlowDeleted
    newFlowDeleted,

    -- ** FlowStandby
    newFlowStandby,

    -- * Operations
    -- $operations

    -- ** AddBridgeOutputs
    AddBridgeOutputs (AddBridgeOutputs'),
    newAddBridgeOutputs,
    AddBridgeOutputsResponse (AddBridgeOutputsResponse'),
    newAddBridgeOutputsResponse,

    -- ** AddBridgeSources
    AddBridgeSources (AddBridgeSources'),
    newAddBridgeSources,
    AddBridgeSourcesResponse (AddBridgeSourcesResponse'),
    newAddBridgeSourcesResponse,

    -- ** AddFlowMediaStreams
    AddFlowMediaStreams (AddFlowMediaStreams'),
    newAddFlowMediaStreams,
    AddFlowMediaStreamsResponse (AddFlowMediaStreamsResponse'),
    newAddFlowMediaStreamsResponse,

    -- ** AddFlowOutputs
    AddFlowOutputs (AddFlowOutputs'),
    newAddFlowOutputs,
    AddFlowOutputsResponse (AddFlowOutputsResponse'),
    newAddFlowOutputsResponse,

    -- ** AddFlowSources
    AddFlowSources (AddFlowSources'),
    newAddFlowSources,
    AddFlowSourcesResponse (AddFlowSourcesResponse'),
    newAddFlowSourcesResponse,

    -- ** AddFlowVpcInterfaces
    AddFlowVpcInterfaces (AddFlowVpcInterfaces'),
    newAddFlowVpcInterfaces,
    AddFlowVpcInterfacesResponse (AddFlowVpcInterfacesResponse'),
    newAddFlowVpcInterfacesResponse,

    -- ** CreateBridge
    CreateBridge (CreateBridge'),
    newCreateBridge,
    CreateBridgeResponse (CreateBridgeResponse'),
    newCreateBridgeResponse,

    -- ** CreateFlow
    CreateFlow (CreateFlow'),
    newCreateFlow,
    CreateFlowResponse (CreateFlowResponse'),
    newCreateFlowResponse,

    -- ** CreateGateway
    CreateGateway (CreateGateway'),
    newCreateGateway,
    CreateGatewayResponse (CreateGatewayResponse'),
    newCreateGatewayResponse,

    -- ** DeleteBridge
    DeleteBridge (DeleteBridge'),
    newDeleteBridge,
    DeleteBridgeResponse (DeleteBridgeResponse'),
    newDeleteBridgeResponse,

    -- ** DeleteFlow
    DeleteFlow (DeleteFlow'),
    newDeleteFlow,
    DeleteFlowResponse (DeleteFlowResponse'),
    newDeleteFlowResponse,

    -- ** DeleteGateway
    DeleteGateway (DeleteGateway'),
    newDeleteGateway,
    DeleteGatewayResponse (DeleteGatewayResponse'),
    newDeleteGatewayResponse,

    -- ** DeregisterGatewayInstance
    DeregisterGatewayInstance (DeregisterGatewayInstance'),
    newDeregisterGatewayInstance,
    DeregisterGatewayInstanceResponse (DeregisterGatewayInstanceResponse'),
    newDeregisterGatewayInstanceResponse,

    -- ** DescribeBridge
    DescribeBridge (DescribeBridge'),
    newDescribeBridge,
    DescribeBridgeResponse (DescribeBridgeResponse'),
    newDescribeBridgeResponse,

    -- ** DescribeFlow
    DescribeFlow (DescribeFlow'),
    newDescribeFlow,
    DescribeFlowResponse (DescribeFlowResponse'),
    newDescribeFlowResponse,

    -- ** DescribeGateway
    DescribeGateway (DescribeGateway'),
    newDescribeGateway,
    DescribeGatewayResponse (DescribeGatewayResponse'),
    newDescribeGatewayResponse,

    -- ** DescribeGatewayInstance
    DescribeGatewayInstance (DescribeGatewayInstance'),
    newDescribeGatewayInstance,
    DescribeGatewayInstanceResponse (DescribeGatewayInstanceResponse'),
    newDescribeGatewayInstanceResponse,

    -- ** DescribeOffering
    DescribeOffering (DescribeOffering'),
    newDescribeOffering,
    DescribeOfferingResponse (DescribeOfferingResponse'),
    newDescribeOfferingResponse,

    -- ** DescribeReservation
    DescribeReservation (DescribeReservation'),
    newDescribeReservation,
    DescribeReservationResponse (DescribeReservationResponse'),
    newDescribeReservationResponse,

    -- ** GrantFlowEntitlements
    GrantFlowEntitlements (GrantFlowEntitlements'),
    newGrantFlowEntitlements,
    GrantFlowEntitlementsResponse (GrantFlowEntitlementsResponse'),
    newGrantFlowEntitlementsResponse,

    -- ** ListBridges (Paginated)
    ListBridges (ListBridges'),
    newListBridges,
    ListBridgesResponse (ListBridgesResponse'),
    newListBridgesResponse,

    -- ** ListEntitlements (Paginated)
    ListEntitlements (ListEntitlements'),
    newListEntitlements,
    ListEntitlementsResponse (ListEntitlementsResponse'),
    newListEntitlementsResponse,

    -- ** ListFlows (Paginated)
    ListFlows (ListFlows'),
    newListFlows,
    ListFlowsResponse (ListFlowsResponse'),
    newListFlowsResponse,

    -- ** ListGatewayInstances (Paginated)
    ListGatewayInstances (ListGatewayInstances'),
    newListGatewayInstances,
    ListGatewayInstancesResponse (ListGatewayInstancesResponse'),
    newListGatewayInstancesResponse,

    -- ** ListGateways (Paginated)
    ListGateways (ListGateways'),
    newListGateways,
    ListGatewaysResponse (ListGatewaysResponse'),
    newListGatewaysResponse,

    -- ** ListOfferings (Paginated)
    ListOfferings (ListOfferings'),
    newListOfferings,
    ListOfferingsResponse (ListOfferingsResponse'),
    newListOfferingsResponse,

    -- ** ListReservations (Paginated)
    ListReservations (ListReservations'),
    newListReservations,
    ListReservationsResponse (ListReservationsResponse'),
    newListReservationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PurchaseOffering
    PurchaseOffering (PurchaseOffering'),
    newPurchaseOffering,
    PurchaseOfferingResponse (PurchaseOfferingResponse'),
    newPurchaseOfferingResponse,

    -- ** RemoveBridgeOutput
    RemoveBridgeOutput (RemoveBridgeOutput'),
    newRemoveBridgeOutput,
    RemoveBridgeOutputResponse (RemoveBridgeOutputResponse'),
    newRemoveBridgeOutputResponse,

    -- ** RemoveBridgeSource
    RemoveBridgeSource (RemoveBridgeSource'),
    newRemoveBridgeSource,
    RemoveBridgeSourceResponse (RemoveBridgeSourceResponse'),
    newRemoveBridgeSourceResponse,

    -- ** RemoveFlowMediaStream
    RemoveFlowMediaStream (RemoveFlowMediaStream'),
    newRemoveFlowMediaStream,
    RemoveFlowMediaStreamResponse (RemoveFlowMediaStreamResponse'),
    newRemoveFlowMediaStreamResponse,

    -- ** RemoveFlowOutput
    RemoveFlowOutput (RemoveFlowOutput'),
    newRemoveFlowOutput,
    RemoveFlowOutputResponse (RemoveFlowOutputResponse'),
    newRemoveFlowOutputResponse,

    -- ** RemoveFlowSource
    RemoveFlowSource (RemoveFlowSource'),
    newRemoveFlowSource,
    RemoveFlowSourceResponse (RemoveFlowSourceResponse'),
    newRemoveFlowSourceResponse,

    -- ** RemoveFlowVpcInterface
    RemoveFlowVpcInterface (RemoveFlowVpcInterface'),
    newRemoveFlowVpcInterface,
    RemoveFlowVpcInterfaceResponse (RemoveFlowVpcInterfaceResponse'),
    newRemoveFlowVpcInterfaceResponse,

    -- ** RevokeFlowEntitlement
    RevokeFlowEntitlement (RevokeFlowEntitlement'),
    newRevokeFlowEntitlement,
    RevokeFlowEntitlementResponse (RevokeFlowEntitlementResponse'),
    newRevokeFlowEntitlementResponse,

    -- ** StartFlow
    StartFlow (StartFlow'),
    newStartFlow,
    StartFlowResponse (StartFlowResponse'),
    newStartFlowResponse,

    -- ** StopFlow
    StopFlow (StopFlow'),
    newStopFlow,
    StopFlowResponse (StopFlowResponse'),
    newStopFlowResponse,

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

    -- ** UpdateBridge
    UpdateBridge (UpdateBridge'),
    newUpdateBridge,
    UpdateBridgeResponse (UpdateBridgeResponse'),
    newUpdateBridgeResponse,

    -- ** UpdateBridgeOutput
    UpdateBridgeOutput (UpdateBridgeOutput'),
    newUpdateBridgeOutput,
    UpdateBridgeOutputResponse (UpdateBridgeOutputResponse'),
    newUpdateBridgeOutputResponse,

    -- ** UpdateBridgeSource
    UpdateBridgeSource (UpdateBridgeSource'),
    newUpdateBridgeSource,
    UpdateBridgeSourceResponse (UpdateBridgeSourceResponse'),
    newUpdateBridgeSourceResponse,

    -- ** UpdateBridgeState
    UpdateBridgeState (UpdateBridgeState'),
    newUpdateBridgeState,
    UpdateBridgeStateResponse (UpdateBridgeStateResponse'),
    newUpdateBridgeStateResponse,

    -- ** UpdateFlow
    UpdateFlow (UpdateFlow'),
    newUpdateFlow,
    UpdateFlowResponse (UpdateFlowResponse'),
    newUpdateFlowResponse,

    -- ** UpdateFlowEntitlement
    UpdateFlowEntitlement (UpdateFlowEntitlement'),
    newUpdateFlowEntitlement,
    UpdateFlowEntitlementResponse (UpdateFlowEntitlementResponse'),
    newUpdateFlowEntitlementResponse,

    -- ** UpdateFlowMediaStream
    UpdateFlowMediaStream (UpdateFlowMediaStream'),
    newUpdateFlowMediaStream,
    UpdateFlowMediaStreamResponse (UpdateFlowMediaStreamResponse'),
    newUpdateFlowMediaStreamResponse,

    -- ** UpdateFlowOutput
    UpdateFlowOutput (UpdateFlowOutput'),
    newUpdateFlowOutput,
    UpdateFlowOutputResponse (UpdateFlowOutputResponse'),
    newUpdateFlowOutputResponse,

    -- ** UpdateFlowSource
    UpdateFlowSource (UpdateFlowSource'),
    newUpdateFlowSource,
    UpdateFlowSourceResponse (UpdateFlowSourceResponse'),
    newUpdateFlowSourceResponse,

    -- ** UpdateGatewayInstance
    UpdateGatewayInstance (UpdateGatewayInstance'),
    newUpdateGatewayInstance,
    UpdateGatewayInstanceResponse (UpdateGatewayInstanceResponse'),
    newUpdateGatewayInstanceResponse,

    -- * Types

    -- ** Algorithm
    Algorithm (..),

    -- ** BridgePlacement
    BridgePlacement (..),

    -- ** BridgeState
    BridgeState (..),

    -- ** Colorimetry
    Colorimetry (..),

    -- ** ConnectionStatus
    ConnectionStatus (..),

    -- ** DesiredState
    DesiredState (..),

    -- ** DurationUnits
    DurationUnits (..),

    -- ** EncoderProfile
    EncoderProfile (..),

    -- ** EncodingName
    EncodingName (..),

    -- ** EntitlementStatus
    EntitlementStatus (..),

    -- ** FailoverMode
    FailoverMode (..),

    -- ** GatewayState
    GatewayState (..),

    -- ** InstanceState
    InstanceState (..),

    -- ** KeyType
    KeyType (..),

    -- ** MaintenanceDay
    MaintenanceDay (..),

    -- ** MediaStreamType
    MediaStreamType (..),

    -- ** NetworkInterfaceType
    NetworkInterfaceType (..),

    -- ** PriceUnits
    PriceUnits (..),

    -- ** Protocol
    Protocol (..),

    -- ** Range
    Range (..),

    -- ** ReservationState
    ReservationState (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** ScanMode
    ScanMode (..),

    -- ** SourceType
    SourceType (..),

    -- ** State
    State (..),

    -- ** Status
    Status (..),

    -- ** Tcs
    Tcs (..),

    -- ** AddBridgeFlowSourceRequest
    AddBridgeFlowSourceRequest (AddBridgeFlowSourceRequest'),
    newAddBridgeFlowSourceRequest,

    -- ** AddBridgeNetworkOutputRequest
    AddBridgeNetworkOutputRequest (AddBridgeNetworkOutputRequest'),
    newAddBridgeNetworkOutputRequest,

    -- ** AddBridgeNetworkSourceRequest
    AddBridgeNetworkSourceRequest (AddBridgeNetworkSourceRequest'),
    newAddBridgeNetworkSourceRequest,

    -- ** AddBridgeOutputRequest
    AddBridgeOutputRequest (AddBridgeOutputRequest'),
    newAddBridgeOutputRequest,

    -- ** AddBridgeSourceRequest
    AddBridgeSourceRequest (AddBridgeSourceRequest'),
    newAddBridgeSourceRequest,

    -- ** AddEgressGatewayBridgeRequest
    AddEgressGatewayBridgeRequest (AddEgressGatewayBridgeRequest'),
    newAddEgressGatewayBridgeRequest,

    -- ** AddIngressGatewayBridgeRequest
    AddIngressGatewayBridgeRequest (AddIngressGatewayBridgeRequest'),
    newAddIngressGatewayBridgeRequest,

    -- ** AddMaintenance
    AddMaintenance (AddMaintenance'),
    newAddMaintenance,

    -- ** AddMediaStreamRequest
    AddMediaStreamRequest (AddMediaStreamRequest'),
    newAddMediaStreamRequest,

    -- ** AddOutputRequest
    AddOutputRequest (AddOutputRequest'),
    newAddOutputRequest,

    -- ** Bridge
    Bridge (Bridge'),
    newBridge,

    -- ** BridgeFlowOutput
    BridgeFlowOutput (BridgeFlowOutput'),
    newBridgeFlowOutput,

    -- ** BridgeFlowSource
    BridgeFlowSource (BridgeFlowSource'),
    newBridgeFlowSource,

    -- ** BridgeNetworkOutput
    BridgeNetworkOutput (BridgeNetworkOutput'),
    newBridgeNetworkOutput,

    -- ** BridgeNetworkSource
    BridgeNetworkSource (BridgeNetworkSource'),
    newBridgeNetworkSource,

    -- ** BridgeOutput
    BridgeOutput (BridgeOutput'),
    newBridgeOutput,

    -- ** BridgeSource
    BridgeSource (BridgeSource'),
    newBridgeSource,

    -- ** DestinationConfiguration
    DestinationConfiguration (DestinationConfiguration'),
    newDestinationConfiguration,

    -- ** DestinationConfigurationRequest
    DestinationConfigurationRequest (DestinationConfigurationRequest'),
    newDestinationConfigurationRequest,

    -- ** EgressGatewayBridge
    EgressGatewayBridge (EgressGatewayBridge'),
    newEgressGatewayBridge,

    -- ** EncodingParameters
    EncodingParameters (EncodingParameters'),
    newEncodingParameters,

    -- ** EncodingParametersRequest
    EncodingParametersRequest (EncodingParametersRequest'),
    newEncodingParametersRequest,

    -- ** Encryption
    Encryption (Encryption'),
    newEncryption,

    -- ** Entitlement
    Entitlement (Entitlement'),
    newEntitlement,

    -- ** FailoverConfig
    FailoverConfig (FailoverConfig'),
    newFailoverConfig,

    -- ** Flow
    Flow (Flow'),
    newFlow,

    -- ** Fmtp
    Fmtp (Fmtp'),
    newFmtp,

    -- ** FmtpRequest
    FmtpRequest (FmtpRequest'),
    newFmtpRequest,

    -- ** Gateway
    Gateway (Gateway'),
    newGateway,

    -- ** GatewayBridgeSource
    GatewayBridgeSource (GatewayBridgeSource'),
    newGatewayBridgeSource,

    -- ** GatewayInstance
    GatewayInstance (GatewayInstance'),
    newGatewayInstance,

    -- ** GatewayNetwork
    GatewayNetwork (GatewayNetwork'),
    newGatewayNetwork,

    -- ** GrantEntitlementRequest
    GrantEntitlementRequest (GrantEntitlementRequest'),
    newGrantEntitlementRequest,

    -- ** IngressGatewayBridge
    IngressGatewayBridge (IngressGatewayBridge'),
    newIngressGatewayBridge,

    -- ** InputConfiguration
    InputConfiguration (InputConfiguration'),
    newInputConfiguration,

    -- ** InputConfigurationRequest
    InputConfigurationRequest (InputConfigurationRequest'),
    newInputConfigurationRequest,

    -- ** Interface
    Interface (Interface'),
    newInterface,

    -- ** InterfaceRequest
    InterfaceRequest (InterfaceRequest'),
    newInterfaceRequest,

    -- ** ListedBridge
    ListedBridge (ListedBridge'),
    newListedBridge,

    -- ** ListedEntitlement
    ListedEntitlement (ListedEntitlement'),
    newListedEntitlement,

    -- ** ListedFlow
    ListedFlow (ListedFlow'),
    newListedFlow,

    -- ** ListedGateway
    ListedGateway (ListedGateway'),
    newListedGateway,

    -- ** ListedGatewayInstance
    ListedGatewayInstance (ListedGatewayInstance'),
    newListedGatewayInstance,

    -- ** Maintenance
    Maintenance (Maintenance'),
    newMaintenance,

    -- ** MediaStream
    MediaStream (MediaStream'),
    newMediaStream,

    -- ** MediaStreamAttributes
    MediaStreamAttributes (MediaStreamAttributes'),
    newMediaStreamAttributes,

    -- ** MediaStreamAttributesRequest
    MediaStreamAttributesRequest (MediaStreamAttributesRequest'),
    newMediaStreamAttributesRequest,

    -- ** MediaStreamOutputConfiguration
    MediaStreamOutputConfiguration (MediaStreamOutputConfiguration'),
    newMediaStreamOutputConfiguration,

    -- ** MediaStreamOutputConfigurationRequest
    MediaStreamOutputConfigurationRequest (MediaStreamOutputConfigurationRequest'),
    newMediaStreamOutputConfigurationRequest,

    -- ** MediaStreamSourceConfiguration
    MediaStreamSourceConfiguration (MediaStreamSourceConfiguration'),
    newMediaStreamSourceConfiguration,

    -- ** MediaStreamSourceConfigurationRequest
    MediaStreamSourceConfigurationRequest (MediaStreamSourceConfigurationRequest'),
    newMediaStreamSourceConfigurationRequest,

    -- ** MessageDetail
    MessageDetail (MessageDetail'),
    newMessageDetail,

    -- ** Messages
    Messages (Messages'),
    newMessages,

    -- ** Offering
    Offering (Offering'),
    newOffering,

    -- ** Output
    Output (Output'),
    newOutput,

    -- ** Reservation
    Reservation (Reservation'),
    newReservation,

    -- ** ResourceSpecification
    ResourceSpecification (ResourceSpecification'),
    newResourceSpecification,

    -- ** SetGatewayBridgeSourceRequest
    SetGatewayBridgeSourceRequest (SetGatewayBridgeSourceRequest'),
    newSetGatewayBridgeSourceRequest,

    -- ** SetSourceRequest
    SetSourceRequest (SetSourceRequest'),
    newSetSourceRequest,

    -- ** Source
    Source (Source'),
    newSource,

    -- ** SourcePriority
    SourcePriority (SourcePriority'),
    newSourcePriority,

    -- ** Transport
    Transport (Transport'),
    newTransport,

    -- ** UpdateBridgeFlowSourceRequest
    UpdateBridgeFlowSourceRequest (UpdateBridgeFlowSourceRequest'),
    newUpdateBridgeFlowSourceRequest,

    -- ** UpdateBridgeNetworkOutputRequest
    UpdateBridgeNetworkOutputRequest (UpdateBridgeNetworkOutputRequest'),
    newUpdateBridgeNetworkOutputRequest,

    -- ** UpdateBridgeNetworkSourceRequest
    UpdateBridgeNetworkSourceRequest (UpdateBridgeNetworkSourceRequest'),
    newUpdateBridgeNetworkSourceRequest,

    -- ** UpdateEgressGatewayBridgeRequest
    UpdateEgressGatewayBridgeRequest (UpdateEgressGatewayBridgeRequest'),
    newUpdateEgressGatewayBridgeRequest,

    -- ** UpdateEncryption
    UpdateEncryption (UpdateEncryption'),
    newUpdateEncryption,

    -- ** UpdateFailoverConfig
    UpdateFailoverConfig (UpdateFailoverConfig'),
    newUpdateFailoverConfig,

    -- ** UpdateGatewayBridgeSourceRequest
    UpdateGatewayBridgeSourceRequest (UpdateGatewayBridgeSourceRequest'),
    newUpdateGatewayBridgeSourceRequest,

    -- ** UpdateIngressGatewayBridgeRequest
    UpdateIngressGatewayBridgeRequest (UpdateIngressGatewayBridgeRequest'),
    newUpdateIngressGatewayBridgeRequest,

    -- ** UpdateMaintenance
    UpdateMaintenance (UpdateMaintenance'),
    newUpdateMaintenance,

    -- ** VpcInterface
    VpcInterface (VpcInterface'),
    newVpcInterface,

    -- ** VpcInterfaceAttachment
    VpcInterfaceAttachment (VpcInterfaceAttachment'),
    newVpcInterfaceAttachment,

    -- ** VpcInterfaceRequest
    VpcInterfaceRequest (VpcInterfaceRequest'),
    newVpcInterfaceRequest,
  )
where

import Amazonka.MediaConnect.AddBridgeOutputs
import Amazonka.MediaConnect.AddBridgeSources
import Amazonka.MediaConnect.AddFlowMediaStreams
import Amazonka.MediaConnect.AddFlowOutputs
import Amazonka.MediaConnect.AddFlowSources
import Amazonka.MediaConnect.AddFlowVpcInterfaces
import Amazonka.MediaConnect.CreateBridge
import Amazonka.MediaConnect.CreateFlow
import Amazonka.MediaConnect.CreateGateway
import Amazonka.MediaConnect.DeleteBridge
import Amazonka.MediaConnect.DeleteFlow
import Amazonka.MediaConnect.DeleteGateway
import Amazonka.MediaConnect.DeregisterGatewayInstance
import Amazonka.MediaConnect.DescribeBridge
import Amazonka.MediaConnect.DescribeFlow
import Amazonka.MediaConnect.DescribeGateway
import Amazonka.MediaConnect.DescribeGatewayInstance
import Amazonka.MediaConnect.DescribeOffering
import Amazonka.MediaConnect.DescribeReservation
import Amazonka.MediaConnect.GrantFlowEntitlements
import Amazonka.MediaConnect.Lens
import Amazonka.MediaConnect.ListBridges
import Amazonka.MediaConnect.ListEntitlements
import Amazonka.MediaConnect.ListFlows
import Amazonka.MediaConnect.ListGatewayInstances
import Amazonka.MediaConnect.ListGateways
import Amazonka.MediaConnect.ListOfferings
import Amazonka.MediaConnect.ListReservations
import Amazonka.MediaConnect.ListTagsForResource
import Amazonka.MediaConnect.PurchaseOffering
import Amazonka.MediaConnect.RemoveBridgeOutput
import Amazonka.MediaConnect.RemoveBridgeSource
import Amazonka.MediaConnect.RemoveFlowMediaStream
import Amazonka.MediaConnect.RemoveFlowOutput
import Amazonka.MediaConnect.RemoveFlowSource
import Amazonka.MediaConnect.RemoveFlowVpcInterface
import Amazonka.MediaConnect.RevokeFlowEntitlement
import Amazonka.MediaConnect.StartFlow
import Amazonka.MediaConnect.StopFlow
import Amazonka.MediaConnect.TagResource
import Amazonka.MediaConnect.Types
import Amazonka.MediaConnect.UntagResource
import Amazonka.MediaConnect.UpdateBridge
import Amazonka.MediaConnect.UpdateBridgeOutput
import Amazonka.MediaConnect.UpdateBridgeSource
import Amazonka.MediaConnect.UpdateBridgeState
import Amazonka.MediaConnect.UpdateFlow
import Amazonka.MediaConnect.UpdateFlowEntitlement
import Amazonka.MediaConnect.UpdateFlowMediaStream
import Amazonka.MediaConnect.UpdateFlowOutput
import Amazonka.MediaConnect.UpdateFlowSource
import Amazonka.MediaConnect.UpdateGatewayInstance
import Amazonka.MediaConnect.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MediaConnect'.

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
