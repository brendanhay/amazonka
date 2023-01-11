{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.BackupGateway.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupGateway.Lens
  ( -- * Operations

    -- ** AssociateGatewayToServer
    associateGatewayToServer_gatewayArn,
    associateGatewayToServer_serverArn,
    associateGatewayToServerResponse_gatewayArn,
    associateGatewayToServerResponse_httpStatus,

    -- ** CreateGateway
    createGateway_tags,
    createGateway_activationKey,
    createGateway_gatewayDisplayName,
    createGateway_gatewayType,
    createGatewayResponse_gatewayArn,
    createGatewayResponse_httpStatus,

    -- ** DeleteGateway
    deleteGateway_gatewayArn,
    deleteGatewayResponse_gatewayArn,
    deleteGatewayResponse_httpStatus,

    -- ** DeleteHypervisor
    deleteHypervisor_hypervisorArn,
    deleteHypervisorResponse_hypervisorArn,
    deleteHypervisorResponse_httpStatus,

    -- ** DisassociateGatewayFromServer
    disassociateGatewayFromServer_gatewayArn,
    disassociateGatewayFromServerResponse_gatewayArn,
    disassociateGatewayFromServerResponse_httpStatus,

    -- ** GetBandwidthRateLimitSchedule
    getBandwidthRateLimitSchedule_gatewayArn,
    getBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals,
    getBandwidthRateLimitScheduleResponse_gatewayArn,
    getBandwidthRateLimitScheduleResponse_httpStatus,

    -- ** GetGateway
    getGateway_gatewayArn,
    getGatewayResponse_gateway,
    getGatewayResponse_httpStatus,

    -- ** GetHypervisor
    getHypervisor_hypervisorArn,
    getHypervisorResponse_hypervisor,
    getHypervisorResponse_httpStatus,

    -- ** GetHypervisorPropertyMappings
    getHypervisorPropertyMappings_hypervisorArn,
    getHypervisorPropertyMappingsResponse_hypervisorArn,
    getHypervisorPropertyMappingsResponse_iamRoleArn,
    getHypervisorPropertyMappingsResponse_vmwareToAwsTagMappings,
    getHypervisorPropertyMappingsResponse_httpStatus,

    -- ** GetVirtualMachine
    getVirtualMachine_resourceArn,
    getVirtualMachineResponse_virtualMachine,
    getVirtualMachineResponse_httpStatus,

    -- ** ImportHypervisorConfiguration
    importHypervisorConfiguration_kmsKeyArn,
    importHypervisorConfiguration_password,
    importHypervisorConfiguration_tags,
    importHypervisorConfiguration_username,
    importHypervisorConfiguration_host,
    importHypervisorConfiguration_name,
    importHypervisorConfigurationResponse_hypervisorArn,
    importHypervisorConfigurationResponse_httpStatus,

    -- ** ListGateways
    listGateways_maxResults,
    listGateways_nextToken,
    listGatewaysResponse_gateways,
    listGatewaysResponse_nextToken,
    listGatewaysResponse_httpStatus,

    -- ** ListHypervisors
    listHypervisors_maxResults,
    listHypervisors_nextToken,
    listHypervisorsResponse_hypervisors,
    listHypervisorsResponse_nextToken,
    listHypervisorsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListVirtualMachines
    listVirtualMachines_hypervisorArn,
    listVirtualMachines_maxResults,
    listVirtualMachines_nextToken,
    listVirtualMachinesResponse_nextToken,
    listVirtualMachinesResponse_virtualMachines,
    listVirtualMachinesResponse_httpStatus,

    -- ** PutBandwidthRateLimitSchedule
    putBandwidthRateLimitSchedule_bandwidthRateLimitIntervals,
    putBandwidthRateLimitSchedule_gatewayArn,
    putBandwidthRateLimitScheduleResponse_gatewayArn,
    putBandwidthRateLimitScheduleResponse_httpStatus,

    -- ** PutHypervisorPropertyMappings
    putHypervisorPropertyMappings_hypervisorArn,
    putHypervisorPropertyMappings_iamRoleArn,
    putHypervisorPropertyMappings_vmwareToAwsTagMappings,
    putHypervisorPropertyMappingsResponse_hypervisorArn,
    putHypervisorPropertyMappingsResponse_httpStatus,

    -- ** PutMaintenanceStartTime
    putMaintenanceStartTime_dayOfMonth,
    putMaintenanceStartTime_dayOfWeek,
    putMaintenanceStartTime_gatewayArn,
    putMaintenanceStartTime_hourOfDay,
    putMaintenanceStartTime_minuteOfHour,
    putMaintenanceStartTimeResponse_gatewayArn,
    putMaintenanceStartTimeResponse_httpStatus,

    -- ** StartVirtualMachinesMetadataSync
    startVirtualMachinesMetadataSync_hypervisorArn,
    startVirtualMachinesMetadataSyncResponse_hypervisorArn,
    startVirtualMachinesMetadataSyncResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_resourceARN,
    tagResourceResponse_httpStatus,

    -- ** TestHypervisorConfiguration
    testHypervisorConfiguration_password,
    testHypervisorConfiguration_username,
    testHypervisorConfiguration_gatewayArn,
    testHypervisorConfiguration_host,
    testHypervisorConfigurationResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_resourceARN,
    untagResourceResponse_httpStatus,

    -- ** UpdateGatewayInformation
    updateGatewayInformation_gatewayDisplayName,
    updateGatewayInformation_gatewayArn,
    updateGatewayInformationResponse_gatewayArn,
    updateGatewayInformationResponse_httpStatus,

    -- ** UpdateGatewaySoftwareNow
    updateGatewaySoftwareNow_gatewayArn,
    updateGatewaySoftwareNowResponse_gatewayArn,
    updateGatewaySoftwareNowResponse_httpStatus,

    -- ** UpdateHypervisor
    updateHypervisor_host,
    updateHypervisor_logGroupArn,
    updateHypervisor_name,
    updateHypervisor_password,
    updateHypervisor_username,
    updateHypervisor_hypervisorArn,
    updateHypervisorResponse_hypervisorArn,
    updateHypervisorResponse_httpStatus,

    -- * Types

    -- ** BandwidthRateLimitInterval
    bandwidthRateLimitInterval_averageUploadRateLimitInBitsPerSec,
    bandwidthRateLimitInterval_daysOfWeek,
    bandwidthRateLimitInterval_endHourOfDay,
    bandwidthRateLimitInterval_endMinuteOfHour,
    bandwidthRateLimitInterval_startHourOfDay,
    bandwidthRateLimitInterval_startMinuteOfHour,

    -- ** Gateway
    gateway_gatewayArn,
    gateway_gatewayDisplayName,
    gateway_gatewayType,
    gateway_hypervisorId,
    gateway_lastSeenTime,

    -- ** GatewayDetails
    gatewayDetails_gatewayArn,
    gatewayDetails_gatewayDisplayName,
    gatewayDetails_gatewayType,
    gatewayDetails_hypervisorId,
    gatewayDetails_lastSeenTime,
    gatewayDetails_maintenanceStartTime,
    gatewayDetails_nextUpdateAvailabilityTime,
    gatewayDetails_vpcEndpoint,

    -- ** Hypervisor
    hypervisor_host,
    hypervisor_hypervisorArn,
    hypervisor_kmsKeyArn,
    hypervisor_name,
    hypervisor_state,

    -- ** HypervisorDetails
    hypervisorDetails_host,
    hypervisorDetails_hypervisorArn,
    hypervisorDetails_kmsKeyArn,
    hypervisorDetails_lastSuccessfulMetadataSyncTime,
    hypervisorDetails_latestMetadataSyncStatus,
    hypervisorDetails_latestMetadataSyncStatusMessage,
    hypervisorDetails_logGroupArn,
    hypervisorDetails_name,
    hypervisorDetails_state,

    -- ** MaintenanceStartTime
    maintenanceStartTime_dayOfMonth,
    maintenanceStartTime_dayOfWeek,
    maintenanceStartTime_hourOfDay,
    maintenanceStartTime_minuteOfHour,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** VirtualMachine
    virtualMachine_hostName,
    virtualMachine_hypervisorId,
    virtualMachine_lastBackupDate,
    virtualMachine_name,
    virtualMachine_path,
    virtualMachine_resourceArn,

    -- ** VirtualMachineDetails
    virtualMachineDetails_hostName,
    virtualMachineDetails_hypervisorId,
    virtualMachineDetails_lastBackupDate,
    virtualMachineDetails_name,
    virtualMachineDetails_path,
    virtualMachineDetails_resourceArn,
    virtualMachineDetails_vmwareTags,

    -- ** VmwareTag
    vmwareTag_vmwareCategory,
    vmwareTag_vmwareTagDescription,
    vmwareTag_vmwareTagName,

    -- ** VmwareToAwsTagMapping
    vmwareToAwsTagMapping_awsTagKey,
    vmwareToAwsTagMapping_awsTagValue,
    vmwareToAwsTagMapping_vmwareCategory,
    vmwareToAwsTagMapping_vmwareTagName,
  )
where

import Amazonka.BackupGateway.AssociateGatewayToServer
import Amazonka.BackupGateway.CreateGateway
import Amazonka.BackupGateway.DeleteGateway
import Amazonka.BackupGateway.DeleteHypervisor
import Amazonka.BackupGateway.DisassociateGatewayFromServer
import Amazonka.BackupGateway.GetBandwidthRateLimitSchedule
import Amazonka.BackupGateway.GetGateway
import Amazonka.BackupGateway.GetHypervisor
import Amazonka.BackupGateway.GetHypervisorPropertyMappings
import Amazonka.BackupGateway.GetVirtualMachine
import Amazonka.BackupGateway.ImportHypervisorConfiguration
import Amazonka.BackupGateway.ListGateways
import Amazonka.BackupGateway.ListHypervisors
import Amazonka.BackupGateway.ListTagsForResource
import Amazonka.BackupGateway.ListVirtualMachines
import Amazonka.BackupGateway.PutBandwidthRateLimitSchedule
import Amazonka.BackupGateway.PutHypervisorPropertyMappings
import Amazonka.BackupGateway.PutMaintenanceStartTime
import Amazonka.BackupGateway.StartVirtualMachinesMetadataSync
import Amazonka.BackupGateway.TagResource
import Amazonka.BackupGateway.TestHypervisorConfiguration
import Amazonka.BackupGateway.Types.BandwidthRateLimitInterval
import Amazonka.BackupGateway.Types.Gateway
import Amazonka.BackupGateway.Types.GatewayDetails
import Amazonka.BackupGateway.Types.Hypervisor
import Amazonka.BackupGateway.Types.HypervisorDetails
import Amazonka.BackupGateway.Types.MaintenanceStartTime
import Amazonka.BackupGateway.Types.Tag
import Amazonka.BackupGateway.Types.VirtualMachine
import Amazonka.BackupGateway.Types.VirtualMachineDetails
import Amazonka.BackupGateway.Types.VmwareTag
import Amazonka.BackupGateway.Types.VmwareToAwsTagMapping
import Amazonka.BackupGateway.UntagResource
import Amazonka.BackupGateway.UpdateGatewayInformation
import Amazonka.BackupGateway.UpdateGatewaySoftwareNow
import Amazonka.BackupGateway.UpdateHypervisor
