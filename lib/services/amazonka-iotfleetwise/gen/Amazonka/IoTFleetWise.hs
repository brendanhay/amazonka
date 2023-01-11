{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IoTFleetWise
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-06-17@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services IoT FleetWise is a fully managed service that you
-- can use to collect, model, and transfer vehicle data to the Amazon Web
-- Services cloud at scale. With Amazon Web Services IoT FleetWise, you can
-- standardize all of your vehicle data models, independent of the
-- in-vehicle communication architecture, and define data collection rules
-- to transfer only high-value data to the cloud.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-fleetwise/latest/developerguide/ What is Amazon Web Services IoT FleetWise?>
-- in the /Amazon Web Services IoT FleetWise Developer Guide/.
module Amazonka.IoTFleetWise
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** DecoderManifestValidationException
    _DecoderManifestValidationException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** InvalidNodeException
    _InvalidNodeException,

    -- ** InvalidSignalsException
    _InvalidSignalsException,

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

    -- ** AssociateVehicleFleet
    AssociateVehicleFleet (AssociateVehicleFleet'),
    newAssociateVehicleFleet,
    AssociateVehicleFleetResponse (AssociateVehicleFleetResponse'),
    newAssociateVehicleFleetResponse,

    -- ** BatchCreateVehicle
    BatchCreateVehicle (BatchCreateVehicle'),
    newBatchCreateVehicle,
    BatchCreateVehicleResponse (BatchCreateVehicleResponse'),
    newBatchCreateVehicleResponse,

    -- ** BatchUpdateVehicle
    BatchUpdateVehicle (BatchUpdateVehicle'),
    newBatchUpdateVehicle,
    BatchUpdateVehicleResponse (BatchUpdateVehicleResponse'),
    newBatchUpdateVehicleResponse,

    -- ** CreateCampaign
    CreateCampaign (CreateCampaign'),
    newCreateCampaign,
    CreateCampaignResponse (CreateCampaignResponse'),
    newCreateCampaignResponse,

    -- ** CreateDecoderManifest
    CreateDecoderManifest (CreateDecoderManifest'),
    newCreateDecoderManifest,
    CreateDecoderManifestResponse (CreateDecoderManifestResponse'),
    newCreateDecoderManifestResponse,

    -- ** CreateFleet
    CreateFleet (CreateFleet'),
    newCreateFleet,
    CreateFleetResponse (CreateFleetResponse'),
    newCreateFleetResponse,

    -- ** CreateModelManifest
    CreateModelManifest (CreateModelManifest'),
    newCreateModelManifest,
    CreateModelManifestResponse (CreateModelManifestResponse'),
    newCreateModelManifestResponse,

    -- ** CreateSignalCatalog
    CreateSignalCatalog (CreateSignalCatalog'),
    newCreateSignalCatalog,
    CreateSignalCatalogResponse (CreateSignalCatalogResponse'),
    newCreateSignalCatalogResponse,

    -- ** CreateVehicle
    CreateVehicle (CreateVehicle'),
    newCreateVehicle,
    CreateVehicleResponse (CreateVehicleResponse'),
    newCreateVehicleResponse,

    -- ** DeleteCampaign
    DeleteCampaign (DeleteCampaign'),
    newDeleteCampaign,
    DeleteCampaignResponse (DeleteCampaignResponse'),
    newDeleteCampaignResponse,

    -- ** DeleteDecoderManifest
    DeleteDecoderManifest (DeleteDecoderManifest'),
    newDeleteDecoderManifest,
    DeleteDecoderManifestResponse (DeleteDecoderManifestResponse'),
    newDeleteDecoderManifestResponse,

    -- ** DeleteFleet
    DeleteFleet (DeleteFleet'),
    newDeleteFleet,
    DeleteFleetResponse (DeleteFleetResponse'),
    newDeleteFleetResponse,

    -- ** DeleteModelManifest
    DeleteModelManifest (DeleteModelManifest'),
    newDeleteModelManifest,
    DeleteModelManifestResponse (DeleteModelManifestResponse'),
    newDeleteModelManifestResponse,

    -- ** DeleteSignalCatalog
    DeleteSignalCatalog (DeleteSignalCatalog'),
    newDeleteSignalCatalog,
    DeleteSignalCatalogResponse (DeleteSignalCatalogResponse'),
    newDeleteSignalCatalogResponse,

    -- ** DeleteVehicle
    DeleteVehicle (DeleteVehicle'),
    newDeleteVehicle,
    DeleteVehicleResponse (DeleteVehicleResponse'),
    newDeleteVehicleResponse,

    -- ** DisassociateVehicleFleet
    DisassociateVehicleFleet (DisassociateVehicleFleet'),
    newDisassociateVehicleFleet,
    DisassociateVehicleFleetResponse (DisassociateVehicleFleetResponse'),
    newDisassociateVehicleFleetResponse,

    -- ** GetCampaign
    GetCampaign (GetCampaign'),
    newGetCampaign,
    GetCampaignResponse (GetCampaignResponse'),
    newGetCampaignResponse,

    -- ** GetDecoderManifest
    GetDecoderManifest (GetDecoderManifest'),
    newGetDecoderManifest,
    GetDecoderManifestResponse (GetDecoderManifestResponse'),
    newGetDecoderManifestResponse,

    -- ** GetFleet
    GetFleet (GetFleet'),
    newGetFleet,
    GetFleetResponse (GetFleetResponse'),
    newGetFleetResponse,

    -- ** GetLoggingOptions
    GetLoggingOptions (GetLoggingOptions'),
    newGetLoggingOptions,
    GetLoggingOptionsResponse (GetLoggingOptionsResponse'),
    newGetLoggingOptionsResponse,

    -- ** GetModelManifest
    GetModelManifest (GetModelManifest'),
    newGetModelManifest,
    GetModelManifestResponse (GetModelManifestResponse'),
    newGetModelManifestResponse,

    -- ** GetRegisterAccountStatus
    GetRegisterAccountStatus (GetRegisterAccountStatus'),
    newGetRegisterAccountStatus,
    GetRegisterAccountStatusResponse (GetRegisterAccountStatusResponse'),
    newGetRegisterAccountStatusResponse,

    -- ** GetSignalCatalog
    GetSignalCatalog (GetSignalCatalog'),
    newGetSignalCatalog,
    GetSignalCatalogResponse (GetSignalCatalogResponse'),
    newGetSignalCatalogResponse,

    -- ** GetVehicle
    GetVehicle (GetVehicle'),
    newGetVehicle,
    GetVehicleResponse (GetVehicleResponse'),
    newGetVehicleResponse,

    -- ** GetVehicleStatus (Paginated)
    GetVehicleStatus (GetVehicleStatus'),
    newGetVehicleStatus,
    GetVehicleStatusResponse (GetVehicleStatusResponse'),
    newGetVehicleStatusResponse,

    -- ** ImportDecoderManifest
    ImportDecoderManifest (ImportDecoderManifest'),
    newImportDecoderManifest,
    ImportDecoderManifestResponse (ImportDecoderManifestResponse'),
    newImportDecoderManifestResponse,

    -- ** ImportSignalCatalog
    ImportSignalCatalog (ImportSignalCatalog'),
    newImportSignalCatalog,
    ImportSignalCatalogResponse (ImportSignalCatalogResponse'),
    newImportSignalCatalogResponse,

    -- ** ListCampaigns (Paginated)
    ListCampaigns (ListCampaigns'),
    newListCampaigns,
    ListCampaignsResponse (ListCampaignsResponse'),
    newListCampaignsResponse,

    -- ** ListDecoderManifestNetworkInterfaces (Paginated)
    ListDecoderManifestNetworkInterfaces (ListDecoderManifestNetworkInterfaces'),
    newListDecoderManifestNetworkInterfaces,
    ListDecoderManifestNetworkInterfacesResponse (ListDecoderManifestNetworkInterfacesResponse'),
    newListDecoderManifestNetworkInterfacesResponse,

    -- ** ListDecoderManifestSignals (Paginated)
    ListDecoderManifestSignals (ListDecoderManifestSignals'),
    newListDecoderManifestSignals,
    ListDecoderManifestSignalsResponse (ListDecoderManifestSignalsResponse'),
    newListDecoderManifestSignalsResponse,

    -- ** ListDecoderManifests (Paginated)
    ListDecoderManifests (ListDecoderManifests'),
    newListDecoderManifests,
    ListDecoderManifestsResponse (ListDecoderManifestsResponse'),
    newListDecoderManifestsResponse,

    -- ** ListFleets (Paginated)
    ListFleets (ListFleets'),
    newListFleets,
    ListFleetsResponse (ListFleetsResponse'),
    newListFleetsResponse,

    -- ** ListFleetsForVehicle (Paginated)
    ListFleetsForVehicle (ListFleetsForVehicle'),
    newListFleetsForVehicle,
    ListFleetsForVehicleResponse (ListFleetsForVehicleResponse'),
    newListFleetsForVehicleResponse,

    -- ** ListModelManifestNodes (Paginated)
    ListModelManifestNodes (ListModelManifestNodes'),
    newListModelManifestNodes,
    ListModelManifestNodesResponse (ListModelManifestNodesResponse'),
    newListModelManifestNodesResponse,

    -- ** ListModelManifests (Paginated)
    ListModelManifests (ListModelManifests'),
    newListModelManifests,
    ListModelManifestsResponse (ListModelManifestsResponse'),
    newListModelManifestsResponse,

    -- ** ListSignalCatalogNodes (Paginated)
    ListSignalCatalogNodes (ListSignalCatalogNodes'),
    newListSignalCatalogNodes,
    ListSignalCatalogNodesResponse (ListSignalCatalogNodesResponse'),
    newListSignalCatalogNodesResponse,

    -- ** ListSignalCatalogs (Paginated)
    ListSignalCatalogs (ListSignalCatalogs'),
    newListSignalCatalogs,
    ListSignalCatalogsResponse (ListSignalCatalogsResponse'),
    newListSignalCatalogsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListVehicles (Paginated)
    ListVehicles (ListVehicles'),
    newListVehicles,
    ListVehiclesResponse (ListVehiclesResponse'),
    newListVehiclesResponse,

    -- ** ListVehiclesInFleet (Paginated)
    ListVehiclesInFleet (ListVehiclesInFleet'),
    newListVehiclesInFleet,
    ListVehiclesInFleetResponse (ListVehiclesInFleetResponse'),
    newListVehiclesInFleetResponse,

    -- ** PutLoggingOptions
    PutLoggingOptions (PutLoggingOptions'),
    newPutLoggingOptions,
    PutLoggingOptionsResponse (PutLoggingOptionsResponse'),
    newPutLoggingOptionsResponse,

    -- ** RegisterAccount
    RegisterAccount (RegisterAccount'),
    newRegisterAccount,
    RegisterAccountResponse (RegisterAccountResponse'),
    newRegisterAccountResponse,

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

    -- ** UpdateCampaign
    UpdateCampaign (UpdateCampaign'),
    newUpdateCampaign,
    UpdateCampaignResponse (UpdateCampaignResponse'),
    newUpdateCampaignResponse,

    -- ** UpdateDecoderManifest
    UpdateDecoderManifest (UpdateDecoderManifest'),
    newUpdateDecoderManifest,
    UpdateDecoderManifestResponse (UpdateDecoderManifestResponse'),
    newUpdateDecoderManifestResponse,

    -- ** UpdateFleet
    UpdateFleet (UpdateFleet'),
    newUpdateFleet,
    UpdateFleetResponse (UpdateFleetResponse'),
    newUpdateFleetResponse,

    -- ** UpdateModelManifest
    UpdateModelManifest (UpdateModelManifest'),
    newUpdateModelManifest,
    UpdateModelManifestResponse (UpdateModelManifestResponse'),
    newUpdateModelManifestResponse,

    -- ** UpdateSignalCatalog
    UpdateSignalCatalog (UpdateSignalCatalog'),
    newUpdateSignalCatalog,
    UpdateSignalCatalogResponse (UpdateSignalCatalogResponse'),
    newUpdateSignalCatalogResponse,

    -- ** UpdateVehicle
    UpdateVehicle (UpdateVehicle'),
    newUpdateVehicle,
    UpdateVehicleResponse (UpdateVehicleResponse'),
    newUpdateVehicleResponse,

    -- * Types

    -- ** CampaignStatus
    CampaignStatus (..),

    -- ** Compression
    Compression (..),

    -- ** DiagnosticsMode
    DiagnosticsMode (..),

    -- ** LogType
    LogType (..),

    -- ** ManifestStatus
    ManifestStatus (..),

    -- ** NetworkInterfaceType
    NetworkInterfaceType (..),

    -- ** NodeDataType
    NodeDataType (..),

    -- ** RegistrationStatus
    RegistrationStatus (..),

    -- ** SignalDecoderType
    SignalDecoderType (..),

    -- ** SpoolingMode
    SpoolingMode (..),

    -- ** TriggerMode
    TriggerMode (..),

    -- ** UpdateCampaignAction
    UpdateCampaignAction (..),

    -- ** UpdateMode
    UpdateMode (..),

    -- ** VehicleAssociationBehavior
    VehicleAssociationBehavior (..),

    -- ** VehicleState
    VehicleState (..),

    -- ** Actuator
    Actuator (Actuator'),
    newActuator,

    -- ** Attribute
    Attribute (Attribute'),
    newAttribute,

    -- ** Branch
    Branch (Branch'),
    newBranch,

    -- ** CampaignSummary
    CampaignSummary (CampaignSummary'),
    newCampaignSummary,

    -- ** CanDbcDefinition
    CanDbcDefinition (CanDbcDefinition'),
    newCanDbcDefinition,

    -- ** CanInterface
    CanInterface (CanInterface'),
    newCanInterface,

    -- ** CanSignal
    CanSignal (CanSignal'),
    newCanSignal,

    -- ** CloudWatchLogDeliveryOptions
    CloudWatchLogDeliveryOptions (CloudWatchLogDeliveryOptions'),
    newCloudWatchLogDeliveryOptions,

    -- ** CollectionScheme
    CollectionScheme (CollectionScheme'),
    newCollectionScheme,

    -- ** ConditionBasedCollectionScheme
    ConditionBasedCollectionScheme (ConditionBasedCollectionScheme'),
    newConditionBasedCollectionScheme,

    -- ** CreateVehicleError
    CreateVehicleError (CreateVehicleError'),
    newCreateVehicleError,

    -- ** CreateVehicleRequestItem
    CreateVehicleRequestItem (CreateVehicleRequestItem'),
    newCreateVehicleRequestItem,

    -- ** CreateVehicleResponseItem
    CreateVehicleResponseItem (CreateVehicleResponseItem'),
    newCreateVehicleResponseItem,

    -- ** DecoderManifestSummary
    DecoderManifestSummary (DecoderManifestSummary'),
    newDecoderManifestSummary,

    -- ** FleetSummary
    FleetSummary (FleetSummary'),
    newFleetSummary,

    -- ** FormattedVss
    FormattedVss (FormattedVss'),
    newFormattedVss,

    -- ** IamRegistrationResponse
    IamRegistrationResponse (IamRegistrationResponse'),
    newIamRegistrationResponse,

    -- ** IamResources
    IamResources (IamResources'),
    newIamResources,

    -- ** ModelManifestSummary
    ModelManifestSummary (ModelManifestSummary'),
    newModelManifestSummary,

    -- ** NetworkFileDefinition
    NetworkFileDefinition (NetworkFileDefinition'),
    newNetworkFileDefinition,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** Node
    Node (Node'),
    newNode,

    -- ** NodeCounts
    NodeCounts (NodeCounts'),
    newNodeCounts,

    -- ** ObdInterface
    ObdInterface (ObdInterface'),
    newObdInterface,

    -- ** ObdSignal
    ObdSignal (ObdSignal'),
    newObdSignal,

    -- ** Sensor
    Sensor (Sensor'),
    newSensor,

    -- ** SignalCatalogSummary
    SignalCatalogSummary (SignalCatalogSummary'),
    newSignalCatalogSummary,

    -- ** SignalDecoder
    SignalDecoder (SignalDecoder'),
    newSignalDecoder,

    -- ** SignalInformation
    SignalInformation (SignalInformation'),
    newSignalInformation,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TimeBasedCollectionScheme
    TimeBasedCollectionScheme (TimeBasedCollectionScheme'),
    newTimeBasedCollectionScheme,

    -- ** TimestreamRegistrationResponse
    TimestreamRegistrationResponse (TimestreamRegistrationResponse'),
    newTimestreamRegistrationResponse,

    -- ** TimestreamResources
    TimestreamResources (TimestreamResources'),
    newTimestreamResources,

    -- ** UpdateVehicleError
    UpdateVehicleError (UpdateVehicleError'),
    newUpdateVehicleError,

    -- ** UpdateVehicleRequestItem
    UpdateVehicleRequestItem (UpdateVehicleRequestItem'),
    newUpdateVehicleRequestItem,

    -- ** UpdateVehicleResponseItem
    UpdateVehicleResponseItem (UpdateVehicleResponseItem'),
    newUpdateVehicleResponseItem,

    -- ** VehicleStatus
    VehicleStatus (VehicleStatus'),
    newVehicleStatus,

    -- ** VehicleSummary
    VehicleSummary (VehicleSummary'),
    newVehicleSummary,
  )
where

import Amazonka.IoTFleetWise.AssociateVehicleFleet
import Amazonka.IoTFleetWise.BatchCreateVehicle
import Amazonka.IoTFleetWise.BatchUpdateVehicle
import Amazonka.IoTFleetWise.CreateCampaign
import Amazonka.IoTFleetWise.CreateDecoderManifest
import Amazonka.IoTFleetWise.CreateFleet
import Amazonka.IoTFleetWise.CreateModelManifest
import Amazonka.IoTFleetWise.CreateSignalCatalog
import Amazonka.IoTFleetWise.CreateVehicle
import Amazonka.IoTFleetWise.DeleteCampaign
import Amazonka.IoTFleetWise.DeleteDecoderManifest
import Amazonka.IoTFleetWise.DeleteFleet
import Amazonka.IoTFleetWise.DeleteModelManifest
import Amazonka.IoTFleetWise.DeleteSignalCatalog
import Amazonka.IoTFleetWise.DeleteVehicle
import Amazonka.IoTFleetWise.DisassociateVehicleFleet
import Amazonka.IoTFleetWise.GetCampaign
import Amazonka.IoTFleetWise.GetDecoderManifest
import Amazonka.IoTFleetWise.GetFleet
import Amazonka.IoTFleetWise.GetLoggingOptions
import Amazonka.IoTFleetWise.GetModelManifest
import Amazonka.IoTFleetWise.GetRegisterAccountStatus
import Amazonka.IoTFleetWise.GetSignalCatalog
import Amazonka.IoTFleetWise.GetVehicle
import Amazonka.IoTFleetWise.GetVehicleStatus
import Amazonka.IoTFleetWise.ImportDecoderManifest
import Amazonka.IoTFleetWise.ImportSignalCatalog
import Amazonka.IoTFleetWise.Lens
import Amazonka.IoTFleetWise.ListCampaigns
import Amazonka.IoTFleetWise.ListDecoderManifestNetworkInterfaces
import Amazonka.IoTFleetWise.ListDecoderManifestSignals
import Amazonka.IoTFleetWise.ListDecoderManifests
import Amazonka.IoTFleetWise.ListFleets
import Amazonka.IoTFleetWise.ListFleetsForVehicle
import Amazonka.IoTFleetWise.ListModelManifestNodes
import Amazonka.IoTFleetWise.ListModelManifests
import Amazonka.IoTFleetWise.ListSignalCatalogNodes
import Amazonka.IoTFleetWise.ListSignalCatalogs
import Amazonka.IoTFleetWise.ListTagsForResource
import Amazonka.IoTFleetWise.ListVehicles
import Amazonka.IoTFleetWise.ListVehiclesInFleet
import Amazonka.IoTFleetWise.PutLoggingOptions
import Amazonka.IoTFleetWise.RegisterAccount
import Amazonka.IoTFleetWise.TagResource
import Amazonka.IoTFleetWise.Types
import Amazonka.IoTFleetWise.UntagResource
import Amazonka.IoTFleetWise.UpdateCampaign
import Amazonka.IoTFleetWise.UpdateDecoderManifest
import Amazonka.IoTFleetWise.UpdateFleet
import Amazonka.IoTFleetWise.UpdateModelManifest
import Amazonka.IoTFleetWise.UpdateSignalCatalog
import Amazonka.IoTFleetWise.UpdateVehicle
import Amazonka.IoTFleetWise.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoTFleetWise'.

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
