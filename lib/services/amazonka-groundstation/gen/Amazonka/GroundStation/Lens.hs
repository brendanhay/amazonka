{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GroundStation.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Lens
  ( -- * Operations

    -- ** CancelContact
    cancelContact_contactId,
    contactIdResponse_contactId,

    -- ** CreateConfig
    createConfig_tags,
    createConfig_configData,
    createConfig_name,
    configIdResponse_configId,
    configIdResponse_configType,
    configIdResponse_configArn,

    -- ** CreateDataflowEndpointGroup
    createDataflowEndpointGroup_tags,
    createDataflowEndpointGroup_endpointDetails,
    dataflowEndpointGroupIdResponse_dataflowEndpointGroupId,

    -- ** CreateMissionProfile
    createMissionProfile_tags,
    createMissionProfile_contactPrePassDurationSeconds,
    createMissionProfile_contactPostPassDurationSeconds,
    createMissionProfile_dataflowEdges,
    createMissionProfile_minimumViableContactDurationSeconds,
    createMissionProfile_name,
    createMissionProfile_trackingConfigArn,
    missionProfileIdResponse_missionProfileId,

    -- ** DeleteConfig
    deleteConfig_configId,
    deleteConfig_configType,
    configIdResponse_configId,
    configIdResponse_configType,
    configIdResponse_configArn,

    -- ** DeleteDataflowEndpointGroup
    deleteDataflowEndpointGroup_dataflowEndpointGroupId,
    dataflowEndpointGroupIdResponse_dataflowEndpointGroupId,

    -- ** DeleteMissionProfile
    deleteMissionProfile_missionProfileId,
    missionProfileIdResponse_missionProfileId,

    -- ** DescribeContact
    describeContact_contactId,
    describeContactResponse_tags,
    describeContactResponse_missionProfileArn,
    describeContactResponse_dataflowList,
    describeContactResponse_contactId,
    describeContactResponse_errorMessage,
    describeContactResponse_postPassEndTime,
    describeContactResponse_satelliteArn,
    describeContactResponse_contactStatus,
    describeContactResponse_endTime,
    describeContactResponse_region,
    describeContactResponse_prePassStartTime,
    describeContactResponse_groundStation,
    describeContactResponse_startTime,
    describeContactResponse_maximumElevation,
    describeContactResponse_httpStatus,

    -- ** GetConfig
    getConfig_configId,
    getConfig_configType,
    getConfigResponse_tags,
    getConfigResponse_configType,
    getConfigResponse_httpStatus,
    getConfigResponse_configArn,
    getConfigResponse_configData,
    getConfigResponse_configId,
    getConfigResponse_name,

    -- ** GetDataflowEndpointGroup
    getDataflowEndpointGroup_dataflowEndpointGroupId,
    getDataflowEndpointGroupResponse_tags,
    getDataflowEndpointGroupResponse_dataflowEndpointGroupArn,
    getDataflowEndpointGroupResponse_endpointsDetails,
    getDataflowEndpointGroupResponse_dataflowEndpointGroupId,
    getDataflowEndpointGroupResponse_httpStatus,

    -- ** GetMinuteUsage
    getMinuteUsage_month,
    getMinuteUsage_year,
    getMinuteUsageResponse_upcomingMinutesScheduled,
    getMinuteUsageResponse_totalScheduledMinutes,
    getMinuteUsageResponse_estimatedMinutesRemaining,
    getMinuteUsageResponse_totalReservedMinuteAllocation,
    getMinuteUsageResponse_isReservedMinutesCustomer,
    getMinuteUsageResponse_httpStatus,

    -- ** GetMissionProfile
    getMissionProfile_missionProfileId,
    getMissionProfileResponse_tags,
    getMissionProfileResponse_missionProfileArn,
    getMissionProfileResponse_name,
    getMissionProfileResponse_minimumViableContactDurationSeconds,
    getMissionProfileResponse_contactPrePassDurationSeconds,
    getMissionProfileResponse_dataflowEdges,
    getMissionProfileResponse_contactPostPassDurationSeconds,
    getMissionProfileResponse_missionProfileId,
    getMissionProfileResponse_region,
    getMissionProfileResponse_trackingConfigArn,
    getMissionProfileResponse_httpStatus,

    -- ** GetSatellite
    getSatellite_satelliteId,
    getSatelliteResponse_satelliteArn,
    getSatelliteResponse_satelliteId,
    getSatelliteResponse_noradSatelliteID,
    getSatelliteResponse_groundStations,
    getSatelliteResponse_httpStatus,

    -- ** ListConfigs
    listConfigs_nextToken,
    listConfigs_maxResults,
    listConfigsResponse_nextToken,
    listConfigsResponse_configList,
    listConfigsResponse_httpStatus,

    -- ** ListContacts
    listContacts_missionProfileArn,
    listContacts_nextToken,
    listContacts_satelliteArn,
    listContacts_maxResults,
    listContacts_groundStation,
    listContacts_endTime,
    listContacts_startTime,
    listContacts_statusList,
    listContactsResponse_nextToken,
    listContactsResponse_contactList,
    listContactsResponse_httpStatus,

    -- ** ListDataflowEndpointGroups
    listDataflowEndpointGroups_nextToken,
    listDataflowEndpointGroups_maxResults,
    listDataflowEndpointGroupsResponse_nextToken,
    listDataflowEndpointGroupsResponse_dataflowEndpointGroupList,
    listDataflowEndpointGroupsResponse_httpStatus,

    -- ** ListGroundStations
    listGroundStations_nextToken,
    listGroundStations_maxResults,
    listGroundStations_satelliteId,
    listGroundStationsResponse_nextToken,
    listGroundStationsResponse_groundStationList,
    listGroundStationsResponse_httpStatus,

    -- ** ListMissionProfiles
    listMissionProfiles_nextToken,
    listMissionProfiles_maxResults,
    listMissionProfilesResponse_nextToken,
    listMissionProfilesResponse_missionProfileList,
    listMissionProfilesResponse_httpStatus,

    -- ** ListSatellites
    listSatellites_nextToken,
    listSatellites_maxResults,
    listSatellitesResponse_nextToken,
    listSatellitesResponse_satellites,
    listSatellitesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ReserveContact
    reserveContact_tags,
    reserveContact_endTime,
    reserveContact_groundStation,
    reserveContact_missionProfileArn,
    reserveContact_satelliteArn,
    reserveContact_startTime,
    contactIdResponse_contactId,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateConfig
    updateConfig_configData,
    updateConfig_configId,
    updateConfig_configType,
    updateConfig_name,
    configIdResponse_configId,
    configIdResponse_configType,
    configIdResponse_configArn,

    -- ** UpdateMissionProfile
    updateMissionProfile_name,
    updateMissionProfile_minimumViableContactDurationSeconds,
    updateMissionProfile_contactPrePassDurationSeconds,
    updateMissionProfile_dataflowEdges,
    updateMissionProfile_contactPostPassDurationSeconds,
    updateMissionProfile_trackingConfigArn,
    updateMissionProfile_missionProfileId,
    missionProfileIdResponse_missionProfileId,

    -- * Types

    -- ** AntennaDemodDecodeDetails
    antennaDemodDecodeDetails_outputNode,

    -- ** AntennaDownlinkConfig
    antennaDownlinkConfig_spectrumConfig,

    -- ** AntennaDownlinkDemodDecodeConfig
    antennaDownlinkDemodDecodeConfig_decodeConfig,
    antennaDownlinkDemodDecodeConfig_demodulationConfig,
    antennaDownlinkDemodDecodeConfig_spectrumConfig,

    -- ** AntennaUplinkConfig
    antennaUplinkConfig_transmitDisabled,
    antennaUplinkConfig_spectrumConfig,
    antennaUplinkConfig_targetEirp,

    -- ** ConfigDetails
    configDetails_endpointDetails,
    configDetails_s3RecordingDetails,
    configDetails_antennaDemodDecodeDetails,

    -- ** ConfigIdResponse
    configIdResponse_configId,
    configIdResponse_configType,
    configIdResponse_configArn,

    -- ** ConfigListItem
    configListItem_name,
    configListItem_configId,
    configListItem_configType,
    configListItem_configArn,

    -- ** ConfigTypeData
    configTypeData_trackingConfig,
    configTypeData_dataflowEndpointConfig,
    configTypeData_uplinkEchoConfig,
    configTypeData_antennaDownlinkConfig,
    configTypeData_antennaDownlinkDemodDecodeConfig,
    configTypeData_antennaUplinkConfig,
    configTypeData_s3RecordingConfig,

    -- ** ContactData
    contactData_tags,
    contactData_missionProfileArn,
    contactData_contactId,
    contactData_errorMessage,
    contactData_postPassEndTime,
    contactData_satelliteArn,
    contactData_contactStatus,
    contactData_endTime,
    contactData_region,
    contactData_prePassStartTime,
    contactData_groundStation,
    contactData_startTime,
    contactData_maximumElevation,

    -- ** ContactIdResponse
    contactIdResponse_contactId,

    -- ** DataflowDetail
    dataflowDetail_destination,
    dataflowDetail_errorMessage,
    dataflowDetail_source,

    -- ** DataflowEndpoint
    dataflowEndpoint_name,
    dataflowEndpoint_status,
    dataflowEndpoint_address,
    dataflowEndpoint_mtu,

    -- ** DataflowEndpointConfig
    dataflowEndpointConfig_dataflowEndpointRegion,
    dataflowEndpointConfig_dataflowEndpointName,

    -- ** DataflowEndpointGroupIdResponse
    dataflowEndpointGroupIdResponse_dataflowEndpointGroupId,

    -- ** DataflowEndpointListItem
    dataflowEndpointListItem_dataflowEndpointGroupArn,
    dataflowEndpointListItem_dataflowEndpointGroupId,

    -- ** DecodeConfig
    decodeConfig_unvalidatedJSON,

    -- ** DemodulationConfig
    demodulationConfig_unvalidatedJSON,

    -- ** Destination
    destination_dataflowDestinationRegion,
    destination_configId,
    destination_configDetails,
    destination_configType,

    -- ** Eirp
    eirp_units,
    eirp_value,

    -- ** Elevation
    elevation_unit,
    elevation_value,

    -- ** EndpointDetails
    endpointDetails_endpoint,
    endpointDetails_securityDetails,

    -- ** Frequency
    frequency_units,
    frequency_value,

    -- ** FrequencyBandwidth
    frequencyBandwidth_units,
    frequencyBandwidth_value,

    -- ** GroundStationData
    groundStationData_groundStationName,
    groundStationData_region,
    groundStationData_groundStationId,

    -- ** MissionProfileIdResponse
    missionProfileIdResponse_missionProfileId,

    -- ** MissionProfileListItem
    missionProfileListItem_missionProfileArn,
    missionProfileListItem_name,
    missionProfileListItem_missionProfileId,
    missionProfileListItem_region,

    -- ** S3RecordingConfig
    s3RecordingConfig_prefix,
    s3RecordingConfig_bucketArn,
    s3RecordingConfig_roleArn,

    -- ** S3RecordingDetails
    s3RecordingDetails_keyTemplate,
    s3RecordingDetails_bucketArn,

    -- ** SatelliteListItem
    satelliteListItem_satelliteArn,
    satelliteListItem_satelliteId,
    satelliteListItem_noradSatelliteID,
    satelliteListItem_groundStations,

    -- ** SecurityDetails
    securityDetails_roleArn,
    securityDetails_securityGroupIds,
    securityDetails_subnetIds,

    -- ** SocketAddress
    socketAddress_name,
    socketAddress_port,

    -- ** Source
    source_dataflowSourceRegion,
    source_configId,
    source_configDetails,
    source_configType,

    -- ** SpectrumConfig
    spectrumConfig_polarization,
    spectrumConfig_bandwidth,
    spectrumConfig_centerFrequency,

    -- ** TrackingConfig
    trackingConfig_autotrack,

    -- ** UplinkEchoConfig
    uplinkEchoConfig_antennaUplinkConfigArn,
    uplinkEchoConfig_enabled,

    -- ** UplinkSpectrumConfig
    uplinkSpectrumConfig_polarization,
    uplinkSpectrumConfig_centerFrequency,
  )
where

import Amazonka.GroundStation.CancelContact
import Amazonka.GroundStation.CreateConfig
import Amazonka.GroundStation.CreateDataflowEndpointGroup
import Amazonka.GroundStation.CreateMissionProfile
import Amazonka.GroundStation.DeleteConfig
import Amazonka.GroundStation.DeleteDataflowEndpointGroup
import Amazonka.GroundStation.DeleteMissionProfile
import Amazonka.GroundStation.DescribeContact
import Amazonka.GroundStation.GetConfig
import Amazonka.GroundStation.GetDataflowEndpointGroup
import Amazonka.GroundStation.GetMinuteUsage
import Amazonka.GroundStation.GetMissionProfile
import Amazonka.GroundStation.GetSatellite
import Amazonka.GroundStation.ListConfigs
import Amazonka.GroundStation.ListContacts
import Amazonka.GroundStation.ListDataflowEndpointGroups
import Amazonka.GroundStation.ListGroundStations
import Amazonka.GroundStation.ListMissionProfiles
import Amazonka.GroundStation.ListSatellites
import Amazonka.GroundStation.ListTagsForResource
import Amazonka.GroundStation.ReserveContact
import Amazonka.GroundStation.TagResource
import Amazonka.GroundStation.Types.AntennaDemodDecodeDetails
import Amazonka.GroundStation.Types.AntennaDownlinkConfig
import Amazonka.GroundStation.Types.AntennaDownlinkDemodDecodeConfig
import Amazonka.GroundStation.Types.AntennaUplinkConfig
import Amazonka.GroundStation.Types.ConfigDetails
import Amazonka.GroundStation.Types.ConfigIdResponse
import Amazonka.GroundStation.Types.ConfigListItem
import Amazonka.GroundStation.Types.ConfigTypeData
import Amazonka.GroundStation.Types.ContactData
import Amazonka.GroundStation.Types.ContactIdResponse
import Amazonka.GroundStation.Types.DataflowDetail
import Amazonka.GroundStation.Types.DataflowEndpoint
import Amazonka.GroundStation.Types.DataflowEndpointConfig
import Amazonka.GroundStation.Types.DataflowEndpointGroupIdResponse
import Amazonka.GroundStation.Types.DataflowEndpointListItem
import Amazonka.GroundStation.Types.DecodeConfig
import Amazonka.GroundStation.Types.DemodulationConfig
import Amazonka.GroundStation.Types.Destination
import Amazonka.GroundStation.Types.Eirp
import Amazonka.GroundStation.Types.Elevation
import Amazonka.GroundStation.Types.EndpointDetails
import Amazonka.GroundStation.Types.Frequency
import Amazonka.GroundStation.Types.FrequencyBandwidth
import Amazonka.GroundStation.Types.GroundStationData
import Amazonka.GroundStation.Types.MissionProfileIdResponse
import Amazonka.GroundStation.Types.MissionProfileListItem
import Amazonka.GroundStation.Types.S3RecordingConfig
import Amazonka.GroundStation.Types.S3RecordingDetails
import Amazonka.GroundStation.Types.SatelliteListItem
import Amazonka.GroundStation.Types.SecurityDetails
import Amazonka.GroundStation.Types.SocketAddress
import Amazonka.GroundStation.Types.Source
import Amazonka.GroundStation.Types.SpectrumConfig
import Amazonka.GroundStation.Types.TrackingConfig
import Amazonka.GroundStation.Types.UplinkEchoConfig
import Amazonka.GroundStation.Types.UplinkSpectrumConfig
import Amazonka.GroundStation.UntagResource
import Amazonka.GroundStation.UpdateConfig
import Amazonka.GroundStation.UpdateMissionProfile
