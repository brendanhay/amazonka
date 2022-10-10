{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTFleetWise.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Lens
  ( -- * Operations

    -- ** AssociateVehicleFleet
    associateVehicleFleet_vehicleName,
    associateVehicleFleet_fleetId,
    associateVehicleFleetResponse_httpStatus,

    -- ** BatchCreateVehicle
    batchCreateVehicle_vehicles,
    batchCreateVehicleResponse_vehicles,
    batchCreateVehicleResponse_errors,
    batchCreateVehicleResponse_httpStatus,

    -- ** BatchUpdateVehicle
    batchUpdateVehicle_vehicles,
    batchUpdateVehicleResponse_vehicles,
    batchUpdateVehicleResponse_errors,
    batchUpdateVehicleResponse_httpStatus,

    -- ** CreateCampaign
    createCampaign_tags,
    createCampaign_compression,
    createCampaign_dataExtraDimensions,
    createCampaign_expiryTime,
    createCampaign_diagnosticsMode,
    createCampaign_description,
    createCampaign_spoolingMode,
    createCampaign_postTriggerCollectionDuration,
    createCampaign_priority,
    createCampaign_startTime,
    createCampaign_signalsToCollect,
    createCampaign_name,
    createCampaign_signalCatalogArn,
    createCampaign_targetArn,
    createCampaign_collectionScheme,
    createCampaignResponse_name,
    createCampaignResponse_arn,
    createCampaignResponse_httpStatus,

    -- ** CreateDecoderManifest
    createDecoderManifest_tags,
    createDecoderManifest_signalDecoders,
    createDecoderManifest_description,
    createDecoderManifest_networkInterfaces,
    createDecoderManifest_name,
    createDecoderManifest_modelManifestArn,
    createDecoderManifestResponse_httpStatus,
    createDecoderManifestResponse_name,
    createDecoderManifestResponse_arn,

    -- ** CreateFleet
    createFleet_tags,
    createFleet_description,
    createFleet_fleetId,
    createFleet_signalCatalogArn,
    createFleetResponse_httpStatus,
    createFleetResponse_id,
    createFleetResponse_arn,

    -- ** CreateModelManifest
    createModelManifest_tags,
    createModelManifest_description,
    createModelManifest_name,
    createModelManifest_nodes,
    createModelManifest_signalCatalogArn,
    createModelManifestResponse_httpStatus,
    createModelManifestResponse_name,
    createModelManifestResponse_arn,

    -- ** CreateSignalCatalog
    createSignalCatalog_tags,
    createSignalCatalog_nodes,
    createSignalCatalog_description,
    createSignalCatalog_name,
    createSignalCatalogResponse_httpStatus,
    createSignalCatalogResponse_name,
    createSignalCatalogResponse_arn,

    -- ** CreateVehicle
    createVehicle_tags,
    createVehicle_attributes,
    createVehicle_associationBehavior,
    createVehicle_vehicleName,
    createVehicle_modelManifestArn,
    createVehicle_decoderManifestArn,
    createVehicleResponse_thingArn,
    createVehicleResponse_arn,
    createVehicleResponse_vehicleName,
    createVehicleResponse_httpStatus,

    -- ** DeleteCampaign
    deleteCampaign_name,
    deleteCampaignResponse_name,
    deleteCampaignResponse_arn,
    deleteCampaignResponse_httpStatus,

    -- ** DeleteDecoderManifest
    deleteDecoderManifest_name,
    deleteDecoderManifestResponse_httpStatus,
    deleteDecoderManifestResponse_name,
    deleteDecoderManifestResponse_arn,

    -- ** DeleteFleet
    deleteFleet_fleetId,
    deleteFleetResponse_arn,
    deleteFleetResponse_id,
    deleteFleetResponse_httpStatus,

    -- ** DeleteModelManifest
    deleteModelManifest_name,
    deleteModelManifestResponse_httpStatus,
    deleteModelManifestResponse_name,
    deleteModelManifestResponse_arn,

    -- ** DeleteSignalCatalog
    deleteSignalCatalog_name,
    deleteSignalCatalogResponse_httpStatus,
    deleteSignalCatalogResponse_name,
    deleteSignalCatalogResponse_arn,

    -- ** DeleteVehicle
    deleteVehicle_vehicleName,
    deleteVehicleResponse_httpStatus,
    deleteVehicleResponse_vehicleName,
    deleteVehicleResponse_arn,

    -- ** DisassociateVehicleFleet
    disassociateVehicleFleet_vehicleName,
    disassociateVehicleFleet_fleetId,
    disassociateVehicleFleetResponse_httpStatus,

    -- ** GetCampaign
    getCampaign_name,
    getCampaignResponse_lastModificationTime,
    getCampaignResponse_name,
    getCampaignResponse_compression,
    getCampaignResponse_dataExtraDimensions,
    getCampaignResponse_expiryTime,
    getCampaignResponse_targetArn,
    getCampaignResponse_arn,
    getCampaignResponse_status,
    getCampaignResponse_diagnosticsMode,
    getCampaignResponse_description,
    getCampaignResponse_spoolingMode,
    getCampaignResponse_postTriggerCollectionDuration,
    getCampaignResponse_priority,
    getCampaignResponse_creationTime,
    getCampaignResponse_signalCatalogArn,
    getCampaignResponse_collectionScheme,
    getCampaignResponse_startTime,
    getCampaignResponse_signalsToCollect,
    getCampaignResponse_httpStatus,

    -- ** GetDecoderManifest
    getDecoderManifest_name,
    getDecoderManifestResponse_modelManifestArn,
    getDecoderManifestResponse_status,
    getDecoderManifestResponse_description,
    getDecoderManifestResponse_httpStatus,
    getDecoderManifestResponse_name,
    getDecoderManifestResponse_arn,
    getDecoderManifestResponse_creationTime,
    getDecoderManifestResponse_lastModificationTime,

    -- ** GetFleet
    getFleet_fleetId,
    getFleetResponse_description,
    getFleetResponse_httpStatus,
    getFleetResponse_id,
    getFleetResponse_arn,
    getFleetResponse_signalCatalogArn,
    getFleetResponse_creationTime,
    getFleetResponse_lastModificationTime,

    -- ** GetLoggingOptions
    getLoggingOptionsResponse_httpStatus,
    getLoggingOptionsResponse_cloudWatchLogDelivery,

    -- ** GetModelManifest
    getModelManifest_name,
    getModelManifestResponse_status,
    getModelManifestResponse_description,
    getModelManifestResponse_signalCatalogArn,
    getModelManifestResponse_httpStatus,
    getModelManifestResponse_name,
    getModelManifestResponse_arn,
    getModelManifestResponse_creationTime,
    getModelManifestResponse_lastModificationTime,

    -- ** GetRegisterAccountStatus
    getRegisterAccountStatusResponse_httpStatus,
    getRegisterAccountStatusResponse_customerAccountId,
    getRegisterAccountStatusResponse_accountStatus,
    getRegisterAccountStatusResponse_timestreamRegistrationResponse,
    getRegisterAccountStatusResponse_iamRegistrationResponse,
    getRegisterAccountStatusResponse_creationTime,
    getRegisterAccountStatusResponse_lastModificationTime,

    -- ** GetSignalCatalog
    getSignalCatalog_name,
    getSignalCatalogResponse_nodeCounts,
    getSignalCatalogResponse_description,
    getSignalCatalogResponse_httpStatus,
    getSignalCatalogResponse_name,
    getSignalCatalogResponse_arn,
    getSignalCatalogResponse_creationTime,
    getSignalCatalogResponse_lastModificationTime,

    -- ** GetVehicle
    getVehicle_vehicleName,
    getVehicleResponse_lastModificationTime,
    getVehicleResponse_modelManifestArn,
    getVehicleResponse_arn,
    getVehicleResponse_vehicleName,
    getVehicleResponse_creationTime,
    getVehicleResponse_attributes,
    getVehicleResponse_decoderManifestArn,
    getVehicleResponse_httpStatus,

    -- ** GetVehicleStatus
    getVehicleStatus_nextToken,
    getVehicleStatus_maxResults,
    getVehicleStatus_vehicleName,
    getVehicleStatusResponse_nextToken,
    getVehicleStatusResponse_campaigns,
    getVehicleStatusResponse_httpStatus,

    -- ** ImportDecoderManifest
    importDecoderManifest_name,
    importDecoderManifest_networkFileDefinitions,
    importDecoderManifestResponse_httpStatus,
    importDecoderManifestResponse_name,
    importDecoderManifestResponse_arn,

    -- ** ImportSignalCatalog
    importSignalCatalog_tags,
    importSignalCatalog_description,
    importSignalCatalog_vss,
    importSignalCatalog_name,
    importSignalCatalogResponse_httpStatus,
    importSignalCatalogResponse_name,
    importSignalCatalogResponse_arn,

    -- ** ListCampaigns
    listCampaigns_nextToken,
    listCampaigns_status,
    listCampaigns_maxResults,
    listCampaignsResponse_campaignSummaries,
    listCampaignsResponse_nextToken,
    listCampaignsResponse_httpStatus,

    -- ** ListDecoderManifestNetworkInterfaces
    listDecoderManifestNetworkInterfaces_nextToken,
    listDecoderManifestNetworkInterfaces_maxResults,
    listDecoderManifestNetworkInterfaces_name,
    listDecoderManifestNetworkInterfacesResponse_nextToken,
    listDecoderManifestNetworkInterfacesResponse_networkInterfaces,
    listDecoderManifestNetworkInterfacesResponse_httpStatus,

    -- ** ListDecoderManifestSignals
    listDecoderManifestSignals_nextToken,
    listDecoderManifestSignals_maxResults,
    listDecoderManifestSignals_name,
    listDecoderManifestSignalsResponse_nextToken,
    listDecoderManifestSignalsResponse_signalDecoders,
    listDecoderManifestSignalsResponse_httpStatus,

    -- ** ListDecoderManifests
    listDecoderManifests_nextToken,
    listDecoderManifests_modelManifestArn,
    listDecoderManifests_maxResults,
    listDecoderManifestsResponse_nextToken,
    listDecoderManifestsResponse_summaries,
    listDecoderManifestsResponse_httpStatus,

    -- ** ListFleets
    listFleets_nextToken,
    listFleets_maxResults,
    listFleetsResponse_nextToken,
    listFleetsResponse_fleetSummaries,
    listFleetsResponse_httpStatus,

    -- ** ListFleetsForVehicle
    listFleetsForVehicle_nextToken,
    listFleetsForVehicle_maxResults,
    listFleetsForVehicle_vehicleName,
    listFleetsForVehicleResponse_nextToken,
    listFleetsForVehicleResponse_fleets,
    listFleetsForVehicleResponse_httpStatus,

    -- ** ListModelManifestNodes
    listModelManifestNodes_nextToken,
    listModelManifestNodes_maxResults,
    listModelManifestNodes_name,
    listModelManifestNodesResponse_nextToken,
    listModelManifestNodesResponse_nodes,
    listModelManifestNodesResponse_httpStatus,

    -- ** ListModelManifests
    listModelManifests_nextToken,
    listModelManifests_maxResults,
    listModelManifests_signalCatalogArn,
    listModelManifestsResponse_nextToken,
    listModelManifestsResponse_summaries,
    listModelManifestsResponse_httpStatus,

    -- ** ListSignalCatalogNodes
    listSignalCatalogNodes_nextToken,
    listSignalCatalogNodes_maxResults,
    listSignalCatalogNodes_name,
    listSignalCatalogNodesResponse_nextToken,
    listSignalCatalogNodesResponse_nodes,
    listSignalCatalogNodesResponse_httpStatus,

    -- ** ListSignalCatalogs
    listSignalCatalogs_nextToken,
    listSignalCatalogs_maxResults,
    listSignalCatalogsResponse_nextToken,
    listSignalCatalogsResponse_summaries,
    listSignalCatalogsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListVehicles
    listVehicles_nextToken,
    listVehicles_modelManifestArn,
    listVehicles_maxResults,
    listVehiclesResponse_nextToken,
    listVehiclesResponse_vehicleSummaries,
    listVehiclesResponse_httpStatus,

    -- ** ListVehiclesInFleet
    listVehiclesInFleet_nextToken,
    listVehiclesInFleet_maxResults,
    listVehiclesInFleet_fleetId,
    listVehiclesInFleetResponse_vehicles,
    listVehiclesInFleetResponse_nextToken,
    listVehiclesInFleetResponse_httpStatus,

    -- ** PutLoggingOptions
    putLoggingOptions_cloudWatchLogDelivery,
    putLoggingOptionsResponse_httpStatus,

    -- ** RegisterAccount
    registerAccount_iamResources,
    registerAccount_timestreamResources,
    registerAccountResponse_httpStatus,
    registerAccountResponse_registerAccountStatus,
    registerAccountResponse_timestreamResources,
    registerAccountResponse_iamResources,
    registerAccountResponse_creationTime,
    registerAccountResponse_lastModificationTime,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateCampaign
    updateCampaign_dataExtraDimensions,
    updateCampaign_description,
    updateCampaign_name,
    updateCampaign_action,
    updateCampaignResponse_name,
    updateCampaignResponse_arn,
    updateCampaignResponse_status,
    updateCampaignResponse_httpStatus,

    -- ** UpdateDecoderManifest
    updateDecoderManifest_networkInterfacesToUpdate,
    updateDecoderManifest_networkInterfacesToAdd,
    updateDecoderManifest_networkInterfacesToRemove,
    updateDecoderManifest_signalDecodersToUpdate,
    updateDecoderManifest_signalDecodersToAdd,
    updateDecoderManifest_status,
    updateDecoderManifest_description,
    updateDecoderManifest_signalDecodersToRemove,
    updateDecoderManifest_name,
    updateDecoderManifestResponse_httpStatus,
    updateDecoderManifestResponse_name,
    updateDecoderManifestResponse_arn,

    -- ** UpdateFleet
    updateFleet_description,
    updateFleet_fleetId,
    updateFleetResponse_arn,
    updateFleetResponse_id,
    updateFleetResponse_httpStatus,

    -- ** UpdateModelManifest
    updateModelManifest_nodesToRemove,
    updateModelManifest_nodesToAdd,
    updateModelManifest_status,
    updateModelManifest_description,
    updateModelManifest_name,
    updateModelManifestResponse_httpStatus,
    updateModelManifestResponse_name,
    updateModelManifestResponse_arn,

    -- ** UpdateSignalCatalog
    updateSignalCatalog_nodesToRemove,
    updateSignalCatalog_nodesToAdd,
    updateSignalCatalog_description,
    updateSignalCatalog_nodesToUpdate,
    updateSignalCatalog_name,
    updateSignalCatalogResponse_httpStatus,
    updateSignalCatalogResponse_name,
    updateSignalCatalogResponse_arn,

    -- ** UpdateVehicle
    updateVehicle_modelManifestArn,
    updateVehicle_attributes,
    updateVehicle_decoderManifestArn,
    updateVehicle_attributeUpdateMode,
    updateVehicle_vehicleName,
    updateVehicleResponse_arn,
    updateVehicleResponse_vehicleName,
    updateVehicleResponse_httpStatus,

    -- * Types

    -- ** Actuator
    actuator_max,
    actuator_description,
    actuator_min,
    actuator_assignedValue,
    actuator_allowedValues,
    actuator_unit,
    actuator_fullyQualifiedName,
    actuator_dataType,

    -- ** Attribute
    attribute_max,
    attribute_defaultValue,
    attribute_description,
    attribute_min,
    attribute_assignedValue,
    attribute_allowedValues,
    attribute_unit,
    attribute_fullyQualifiedName,
    attribute_dataType,

    -- ** Branch
    branch_description,
    branch_fullyQualifiedName,

    -- ** CampaignSummary
    campaignSummary_name,
    campaignSummary_targetArn,
    campaignSummary_arn,
    campaignSummary_status,
    campaignSummary_description,
    campaignSummary_signalCatalogArn,
    campaignSummary_creationTime,
    campaignSummary_lastModificationTime,

    -- ** CanDbcDefinition
    canDbcDefinition_signalsMap,
    canDbcDefinition_networkInterface,
    canDbcDefinition_canDbcFiles,

    -- ** CanInterface
    canInterface_protocolName,
    canInterface_protocolVersion,
    canInterface_name,

    -- ** CanSignal
    canSignal_name,
    canSignal_messageId,
    canSignal_isBigEndian,
    canSignal_isSigned,
    canSignal_startBit,
    canSignal_offset,
    canSignal_factor,
    canSignal_length,

    -- ** CloudWatchLogDeliveryOptions
    cloudWatchLogDeliveryOptions_logGroupName,
    cloudWatchLogDeliveryOptions_logType,

    -- ** CollectionScheme
    collectionScheme_timeBasedCollectionScheme,
    collectionScheme_conditionBasedCollectionScheme,

    -- ** ConditionBasedCollectionScheme
    conditionBasedCollectionScheme_conditionLanguageVersion,
    conditionBasedCollectionScheme_minimumTriggerIntervalMs,
    conditionBasedCollectionScheme_triggerMode,
    conditionBasedCollectionScheme_expression,

    -- ** CreateVehicleError
    createVehicleError_message,
    createVehicleError_code,
    createVehicleError_vehicleName,

    -- ** CreateVehicleRequestItem
    createVehicleRequestItem_tags,
    createVehicleRequestItem_attributes,
    createVehicleRequestItem_associationBehavior,
    createVehicleRequestItem_vehicleName,
    createVehicleRequestItem_modelManifestArn,
    createVehicleRequestItem_decoderManifestArn,

    -- ** CreateVehicleResponseItem
    createVehicleResponseItem_thingArn,
    createVehicleResponseItem_arn,
    createVehicleResponseItem_vehicleName,

    -- ** DecoderManifestSummary
    decoderManifestSummary_name,
    decoderManifestSummary_modelManifestArn,
    decoderManifestSummary_arn,
    decoderManifestSummary_status,
    decoderManifestSummary_description,
    decoderManifestSummary_creationTime,
    decoderManifestSummary_lastModificationTime,

    -- ** FleetSummary
    fleetSummary_lastModificationTime,
    fleetSummary_description,
    fleetSummary_id,
    fleetSummary_arn,
    fleetSummary_signalCatalogArn,
    fleetSummary_creationTime,

    -- ** FormattedVss
    formattedVss_vssJson,

    -- ** IamRegistrationResponse
    iamRegistrationResponse_errorMessage,
    iamRegistrationResponse_roleArn,
    iamRegistrationResponse_registrationStatus,

    -- ** IamResources
    iamResources_roleArn,

    -- ** ModelManifestSummary
    modelManifestSummary_name,
    modelManifestSummary_arn,
    modelManifestSummary_status,
    modelManifestSummary_description,
    modelManifestSummary_signalCatalogArn,
    modelManifestSummary_creationTime,
    modelManifestSummary_lastModificationTime,

    -- ** NetworkFileDefinition
    networkFileDefinition_canDbc,

    -- ** NetworkInterface
    networkInterface_obdInterface,
    networkInterface_canInterface,
    networkInterface_interfaceId,
    networkInterface_type,

    -- ** Node
    node_branch,
    node_attribute,
    node_actuator,
    node_sensor,

    -- ** NodeCounts
    nodeCounts_totalSensors,
    nodeCounts_totalNodes,
    nodeCounts_totalActuators,
    nodeCounts_totalAttributes,
    nodeCounts_totalBranches,

    -- ** ObdInterface
    obdInterface_pidRequestIntervalSeconds,
    obdInterface_obdStandard,
    obdInterface_hasTransmissionEcu,
    obdInterface_useExtendedIds,
    obdInterface_dtcRequestIntervalSeconds,
    obdInterface_name,
    obdInterface_requestMessageId,

    -- ** ObdSignal
    obdSignal_bitMaskLength,
    obdSignal_bitRightShift,
    obdSignal_pidResponseLength,
    obdSignal_serviceMode,
    obdSignal_pid,
    obdSignal_scaling,
    obdSignal_offset,
    obdSignal_startByte,
    obdSignal_byteLength,

    -- ** Sensor
    sensor_max,
    sensor_description,
    sensor_min,
    sensor_allowedValues,
    sensor_unit,
    sensor_fullyQualifiedName,
    sensor_dataType,

    -- ** SignalCatalogSummary
    signalCatalogSummary_lastModificationTime,
    signalCatalogSummary_name,
    signalCatalogSummary_arn,
    signalCatalogSummary_creationTime,

    -- ** SignalDecoder
    signalDecoder_canSignal,
    signalDecoder_obdSignal,
    signalDecoder_fullyQualifiedName,
    signalDecoder_type,
    signalDecoder_interfaceId,

    -- ** SignalInformation
    signalInformation_minimumSamplingIntervalMs,
    signalInformation_maxSampleCount,
    signalInformation_name,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TimeBasedCollectionScheme
    timeBasedCollectionScheme_periodMs,

    -- ** TimestreamRegistrationResponse
    timestreamRegistrationResponse_errorMessage,
    timestreamRegistrationResponse_timestreamTableArn,
    timestreamRegistrationResponse_timestreamDatabaseArn,
    timestreamRegistrationResponse_timestreamDatabaseName,
    timestreamRegistrationResponse_timestreamTableName,
    timestreamRegistrationResponse_registrationStatus,

    -- ** TimestreamResources
    timestreamResources_timestreamDatabaseName,
    timestreamResources_timestreamTableName,

    -- ** UpdateVehicleError
    updateVehicleError_message,
    updateVehicleError_code,
    updateVehicleError_vehicleName,

    -- ** UpdateVehicleRequestItem
    updateVehicleRequestItem_modelManifestArn,
    updateVehicleRequestItem_attributes,
    updateVehicleRequestItem_decoderManifestArn,
    updateVehicleRequestItem_attributeUpdateMode,
    updateVehicleRequestItem_vehicleName,

    -- ** UpdateVehicleResponseItem
    updateVehicleResponseItem_arn,
    updateVehicleResponseItem_vehicleName,

    -- ** VehicleStatus
    vehicleStatus_campaignName,
    vehicleStatus_status,
    vehicleStatus_vehicleName,

    -- ** VehicleSummary
    vehicleSummary_vehicleName,
    vehicleSummary_arn,
    vehicleSummary_modelManifestArn,
    vehicleSummary_decoderManifestArn,
    vehicleSummary_creationTime,
    vehicleSummary_lastModificationTime,
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
import Amazonka.IoTFleetWise.Types.Actuator
import Amazonka.IoTFleetWise.Types.Attribute
import Amazonka.IoTFleetWise.Types.Branch
import Amazonka.IoTFleetWise.Types.CampaignSummary
import Amazonka.IoTFleetWise.Types.CanDbcDefinition
import Amazonka.IoTFleetWise.Types.CanInterface
import Amazonka.IoTFleetWise.Types.CanSignal
import Amazonka.IoTFleetWise.Types.CloudWatchLogDeliveryOptions
import Amazonka.IoTFleetWise.Types.CollectionScheme
import Amazonka.IoTFleetWise.Types.ConditionBasedCollectionScheme
import Amazonka.IoTFleetWise.Types.CreateVehicleError
import Amazonka.IoTFleetWise.Types.CreateVehicleRequestItem
import Amazonka.IoTFleetWise.Types.CreateVehicleResponseItem
import Amazonka.IoTFleetWise.Types.DecoderManifestSummary
import Amazonka.IoTFleetWise.Types.FleetSummary
import Amazonka.IoTFleetWise.Types.FormattedVss
import Amazonka.IoTFleetWise.Types.IamRegistrationResponse
import Amazonka.IoTFleetWise.Types.IamResources
import Amazonka.IoTFleetWise.Types.ModelManifestSummary
import Amazonka.IoTFleetWise.Types.NetworkFileDefinition
import Amazonka.IoTFleetWise.Types.NetworkInterface
import Amazonka.IoTFleetWise.Types.Node
import Amazonka.IoTFleetWise.Types.NodeCounts
import Amazonka.IoTFleetWise.Types.ObdInterface
import Amazonka.IoTFleetWise.Types.ObdSignal
import Amazonka.IoTFleetWise.Types.Sensor
import Amazonka.IoTFleetWise.Types.SignalCatalogSummary
import Amazonka.IoTFleetWise.Types.SignalDecoder
import Amazonka.IoTFleetWise.Types.SignalInformation
import Amazonka.IoTFleetWise.Types.Tag
import Amazonka.IoTFleetWise.Types.TimeBasedCollectionScheme
import Amazonka.IoTFleetWise.Types.TimestreamRegistrationResponse
import Amazonka.IoTFleetWise.Types.TimestreamResources
import Amazonka.IoTFleetWise.Types.UpdateVehicleError
import Amazonka.IoTFleetWise.Types.UpdateVehicleRequestItem
import Amazonka.IoTFleetWise.Types.UpdateVehicleResponseItem
import Amazonka.IoTFleetWise.Types.VehicleStatus
import Amazonka.IoTFleetWise.Types.VehicleSummary
import Amazonka.IoTFleetWise.UntagResource
import Amazonka.IoTFleetWise.UpdateCampaign
import Amazonka.IoTFleetWise.UpdateDecoderManifest
import Amazonka.IoTFleetWise.UpdateFleet
import Amazonka.IoTFleetWise.UpdateModelManifest
import Amazonka.IoTFleetWise.UpdateSignalCatalog
import Amazonka.IoTFleetWise.UpdateVehicle
