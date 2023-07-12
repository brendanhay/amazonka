{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MediaConnect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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

    -- ** CreateFlow420Exception
    _CreateFlow420Exception,

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

    -- ** CreateFlow
    CreateFlow (CreateFlow'),
    newCreateFlow,
    CreateFlowResponse (CreateFlowResponse'),
    newCreateFlowResponse,

    -- ** DeleteFlow
    DeleteFlow (DeleteFlow'),
    newDeleteFlow,
    DeleteFlowResponse (DeleteFlowResponse'),
    newDeleteFlowResponse,

    -- ** DescribeFlow
    DescribeFlow (DescribeFlow'),
    newDescribeFlow,
    DescribeFlowResponse (DescribeFlowResponse'),
    newDescribeFlowResponse,

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

    -- * Types

    -- ** Algorithm
    Algorithm (..),

    -- ** Colorimetry
    Colorimetry (..),

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

    -- ** AddMaintenance
    AddMaintenance (AddMaintenance'),
    newAddMaintenance,

    -- ** AddMediaStreamRequest
    AddMediaStreamRequest (AddMediaStreamRequest'),
    newAddMediaStreamRequest,

    -- ** AddOutputRequest
    AddOutputRequest (AddOutputRequest'),
    newAddOutputRequest,

    -- ** DestinationConfiguration
    DestinationConfiguration (DestinationConfiguration'),
    newDestinationConfiguration,

    -- ** DestinationConfigurationRequest
    DestinationConfigurationRequest (DestinationConfigurationRequest'),
    newDestinationConfigurationRequest,

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

    -- ** GrantEntitlementRequest
    GrantEntitlementRequest (GrantEntitlementRequest'),
    newGrantEntitlementRequest,

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

    -- ** ListedEntitlement
    ListedEntitlement (ListedEntitlement'),
    newListedEntitlement,

    -- ** ListedFlow
    ListedFlow (ListedFlow'),
    newListedFlow,

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

    -- ** UpdateEncryption
    UpdateEncryption (UpdateEncryption'),
    newUpdateEncryption,

    -- ** UpdateFailoverConfig
    UpdateFailoverConfig (UpdateFailoverConfig'),
    newUpdateFailoverConfig,

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

import Amazonka.MediaConnect.AddFlowMediaStreams
import Amazonka.MediaConnect.AddFlowOutputs
import Amazonka.MediaConnect.AddFlowSources
import Amazonka.MediaConnect.AddFlowVpcInterfaces
import Amazonka.MediaConnect.CreateFlow
import Amazonka.MediaConnect.DeleteFlow
import Amazonka.MediaConnect.DescribeFlow
import Amazonka.MediaConnect.DescribeOffering
import Amazonka.MediaConnect.DescribeReservation
import Amazonka.MediaConnect.GrantFlowEntitlements
import Amazonka.MediaConnect.Lens
import Amazonka.MediaConnect.ListEntitlements
import Amazonka.MediaConnect.ListFlows
import Amazonka.MediaConnect.ListOfferings
import Amazonka.MediaConnect.ListReservations
import Amazonka.MediaConnect.ListTagsForResource
import Amazonka.MediaConnect.PurchaseOffering
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
import Amazonka.MediaConnect.UpdateFlow
import Amazonka.MediaConnect.UpdateFlowEntitlement
import Amazonka.MediaConnect.UpdateFlowMediaStream
import Amazonka.MediaConnect.UpdateFlowOutput
import Amazonka.MediaConnect.UpdateFlowSource
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
