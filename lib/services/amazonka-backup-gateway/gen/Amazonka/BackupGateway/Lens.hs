{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.BackupGateway.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
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

    -- ** GetGateway
    getGateway_gatewayArn,
    getGatewayResponse_gateway,
    getGatewayResponse_httpStatus,

    -- ** ImportHypervisorConfiguration
    importHypervisorConfiguration_tags,
    importHypervisorConfiguration_password,
    importHypervisorConfiguration_username,
    importHypervisorConfiguration_kmsKeyArn,
    importHypervisorConfiguration_host,
    importHypervisorConfiguration_name,
    importHypervisorConfigurationResponse_hypervisorArn,
    importHypervisorConfigurationResponse_httpStatus,

    -- ** ListGateways
    listGateways_nextToken,
    listGateways_maxResults,
    listGatewaysResponse_nextToken,
    listGatewaysResponse_gateways,
    listGatewaysResponse_httpStatus,

    -- ** ListHypervisors
    listHypervisors_nextToken,
    listHypervisors_maxResults,
    listHypervisorsResponse_nextToken,
    listHypervisorsResponse_hypervisors,
    listHypervisorsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_httpStatus,

    -- ** ListVirtualMachines
    listVirtualMachines_nextToken,
    listVirtualMachines_maxResults,
    listVirtualMachinesResponse_nextToken,
    listVirtualMachinesResponse_virtualMachines,
    listVirtualMachinesResponse_httpStatus,

    -- ** PutMaintenanceStartTime
    putMaintenanceStartTime_dayOfWeek,
    putMaintenanceStartTime_dayOfMonth,
    putMaintenanceStartTime_gatewayArn,
    putMaintenanceStartTime_hourOfDay,
    putMaintenanceStartTime_minuteOfHour,
    putMaintenanceStartTimeResponse_gatewayArn,
    putMaintenanceStartTimeResponse_httpStatus,

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
    updateHypervisor_name,
    updateHypervisor_host,
    updateHypervisor_password,
    updateHypervisor_username,
    updateHypervisor_hypervisorArn,
    updateHypervisorResponse_hypervisorArn,
    updateHypervisorResponse_httpStatus,

    -- * Types

    -- ** Gateway
    gateway_gatewayType,
    gateway_gatewayArn,
    gateway_hypervisorId,
    gateway_lastSeenTime,
    gateway_gatewayDisplayName,

    -- ** GatewayDetails
    gatewayDetails_gatewayType,
    gatewayDetails_gatewayArn,
    gatewayDetails_nextUpdateAvailabilityTime,
    gatewayDetails_hypervisorId,
    gatewayDetails_lastSeenTime,
    gatewayDetails_gatewayDisplayName,
    gatewayDetails_vpcEndpoint,

    -- ** Hypervisor
    hypervisor_name,
    hypervisor_host,
    hypervisor_state,
    hypervisor_kmsKeyArn,
    hypervisor_hypervisorArn,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** VirtualMachine
    virtualMachine_name,
    virtualMachine_path,
    virtualMachine_hostName,
    virtualMachine_resourceArn,
    virtualMachine_hypervisorId,
    virtualMachine_lastBackupDate,
  )
where

import Amazonka.BackupGateway.AssociateGatewayToServer
import Amazonka.BackupGateway.CreateGateway
import Amazonka.BackupGateway.DeleteGateway
import Amazonka.BackupGateway.DeleteHypervisor
import Amazonka.BackupGateway.DisassociateGatewayFromServer
import Amazonka.BackupGateway.GetGateway
import Amazonka.BackupGateway.ImportHypervisorConfiguration
import Amazonka.BackupGateway.ListGateways
import Amazonka.BackupGateway.ListHypervisors
import Amazonka.BackupGateway.ListTagsForResource
import Amazonka.BackupGateway.ListVirtualMachines
import Amazonka.BackupGateway.PutMaintenanceStartTime
import Amazonka.BackupGateway.TagResource
import Amazonka.BackupGateway.TestHypervisorConfiguration
import Amazonka.BackupGateway.Types.Gateway
import Amazonka.BackupGateway.Types.GatewayDetails
import Amazonka.BackupGateway.Types.Hypervisor
import Amazonka.BackupGateway.Types.Tag
import Amazonka.BackupGateway.Types.VirtualMachine
import Amazonka.BackupGateway.UntagResource
import Amazonka.BackupGateway.UpdateGatewayInformation
import Amazonka.BackupGateway.UpdateGatewaySoftwareNow
import Amazonka.BackupGateway.UpdateHypervisor
