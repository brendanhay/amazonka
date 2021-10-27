{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GroundStation.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GroundStation.Lens
  ( -- * Operations

    -- ** DeleteDataflowEndpointGroup
    deleteDataflowEndpointGroup_dataflowEndpointGroupId,
    dataflowEndpointGroupIdResponse_dataflowEndpointGroupId,

    -- ** ListSatellites
    listSatellites_nextToken,
    listSatellites_maxResults,
    listSatellitesResponse_satellites,
    listSatellitesResponse_nextToken,
    listSatellitesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetMinuteUsage
    getMinuteUsage_month,
    getMinuteUsage_year,
    getMinuteUsageResponse_isReservedMinutesCustomer,
    getMinuteUsageResponse_upcomingMinutesScheduled,
    getMinuteUsageResponse_totalScheduledMinutes,
    getMinuteUsageResponse_totalReservedMinuteAllocation,
    getMinuteUsageResponse_estimatedMinutesRemaining,
    getMinuteUsageResponse_httpStatus,

    -- ** DescribeContact
    describeContact_contactId,
    describeContactResponse_contactStatus,
    describeContactResponse_missionProfileArn,
    describeContactResponse_startTime,
    describeContactResponse_dataflowList,
    describeContactResponse_satelliteArn,
    describeContactResponse_maximumElevation,
    describeContactResponse_groundStation,
    describeContactResponse_endTime,
    describeContactResponse_contactId,
    describeContactResponse_region,
    describeContactResponse_postPassEndTime,
    describeContactResponse_prePassStartTime,
    describeContactResponse_errorMessage,
    describeContactResponse_tags,
    describeContactResponse_httpStatus,

    -- ** ReserveContact
    reserveContact_tags,
    reserveContact_endTime,
    reserveContact_groundStation,
    reserveContact_missionProfileArn,
    reserveContact_satelliteArn,
    reserveContact_startTime,
    contactIdResponse_contactId,

    -- ** CreateMissionProfile
    createMissionProfile_contactPrePassDurationSeconds,
    createMissionProfile_contactPostPassDurationSeconds,
    createMissionProfile_tags,
    createMissionProfile_dataflowEdges,
    createMissionProfile_minimumViableContactDurationSeconds,
    createMissionProfile_name,
    createMissionProfile_trackingConfigArn,
    missionProfileIdResponse_missionProfileId,

    -- ** ListGroundStations
    listGroundStations_satelliteId,
    listGroundStations_nextToken,
    listGroundStations_maxResults,
    listGroundStationsResponse_nextToken,
    listGroundStationsResponse_groundStationList,
    listGroundStationsResponse_httpStatus,

    -- ** CreateConfig
    createConfig_tags,
    createConfig_configData,
    createConfig_name,
    configIdResponse_configArn,
    configIdResponse_configId,
    configIdResponse_configType,

    -- ** ListMissionProfiles
    listMissionProfiles_nextToken,
    listMissionProfiles_maxResults,
    listMissionProfilesResponse_nextToken,
    listMissionProfilesResponse_missionProfileList,
    listMissionProfilesResponse_httpStatus,

    -- ** GetMissionProfile
    getMissionProfile_missionProfileId,
    getMissionProfileResponse_missionProfileId,
    getMissionProfileResponse_missionProfileArn,
    getMissionProfileResponse_trackingConfigArn,
    getMissionProfileResponse_contactPrePassDurationSeconds,
    getMissionProfileResponse_contactPostPassDurationSeconds,
    getMissionProfileResponse_name,
    getMissionProfileResponse_dataflowEdges,
    getMissionProfileResponse_region,
    getMissionProfileResponse_minimumViableContactDurationSeconds,
    getMissionProfileResponse_tags,
    getMissionProfileResponse_httpStatus,

    -- ** GetConfig
    getConfig_configId,
    getConfig_configType,
    getConfigResponse_configType,
    getConfigResponse_tags,
    getConfigResponse_httpStatus,
    getConfigResponse_configArn,
    getConfigResponse_configData,
    getConfigResponse_configId,
    getConfigResponse_name,

    -- ** ListDataflowEndpointGroups
    listDataflowEndpointGroups_nextToken,
    listDataflowEndpointGroups_maxResults,
    listDataflowEndpointGroupsResponse_dataflowEndpointGroupList,
    listDataflowEndpointGroupsResponse_nextToken,
    listDataflowEndpointGroupsResponse_httpStatus,

    -- ** CreateDataflowEndpointGroup
    createDataflowEndpointGroup_tags,
    createDataflowEndpointGroup_endpointDetails,
    dataflowEndpointGroupIdResponse_dataflowEndpointGroupId,

    -- ** GetSatellite
    getSatellite_satelliteId,
    getSatelliteResponse_satelliteId,
    getSatelliteResponse_satelliteArn,
    getSatelliteResponse_groundStations,
    getSatelliteResponse_noradSatelliteID,
    getSatelliteResponse_httpStatus,

    -- ** GetDataflowEndpointGroup
    getDataflowEndpointGroup_dataflowEndpointGroupId,
    getDataflowEndpointGroupResponse_endpointsDetails,
    getDataflowEndpointGroupResponse_dataflowEndpointGroupArn,
    getDataflowEndpointGroupResponse_dataflowEndpointGroupId,
    getDataflowEndpointGroupResponse_tags,
    getDataflowEndpointGroupResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListConfigs
    listConfigs_nextToken,
    listConfigs_maxResults,
    listConfigsResponse_configList,
    listConfigsResponse_nextToken,
    listConfigsResponse_httpStatus,

    -- ** UpdateConfig
    updateConfig_configData,
    updateConfig_configId,
    updateConfig_configType,
    updateConfig_name,
    configIdResponse_configArn,
    configIdResponse_configId,
    configIdResponse_configType,

    -- ** DeleteConfig
    deleteConfig_configId,
    deleteConfig_configType,
    configIdResponse_configArn,
    configIdResponse_configId,
    configIdResponse_configType,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateMissionProfile
    updateMissionProfile_trackingConfigArn,
    updateMissionProfile_contactPrePassDurationSeconds,
    updateMissionProfile_contactPostPassDurationSeconds,
    updateMissionProfile_name,
    updateMissionProfile_dataflowEdges,
    updateMissionProfile_minimumViableContactDurationSeconds,
    updateMissionProfile_missionProfileId,
    missionProfileIdResponse_missionProfileId,

    -- ** DeleteMissionProfile
    deleteMissionProfile_missionProfileId,
    missionProfileIdResponse_missionProfileId,

    -- ** CancelContact
    cancelContact_contactId,
    contactIdResponse_contactId,

    -- ** ListContacts
    listContacts_missionProfileArn,
    listContacts_satelliteArn,
    listContacts_nextToken,
    listContacts_groundStation,
    listContacts_maxResults,
    listContacts_endTime,
    listContacts_startTime,
    listContacts_statusList,
    listContactsResponse_contactList,
    listContactsResponse_nextToken,
    listContactsResponse_httpStatus,

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
    configDetails_antennaDemodDecodeDetails,
    configDetails_s3RecordingDetails,
    configDetails_endpointDetails,

    -- ** ConfigIdResponse
    configIdResponse_configArn,
    configIdResponse_configId,
    configIdResponse_configType,

    -- ** ConfigListItem
    configListItem_configArn,
    configListItem_name,
    configListItem_configId,
    configListItem_configType,

    -- ** ConfigTypeData
    configTypeData_antennaDownlinkDemodDecodeConfig,
    configTypeData_antennaDownlinkConfig,
    configTypeData_antennaUplinkConfig,
    configTypeData_uplinkEchoConfig,
    configTypeData_s3RecordingConfig,
    configTypeData_trackingConfig,
    configTypeData_dataflowEndpointConfig,

    -- ** ContactData
    contactData_contactStatus,
    contactData_missionProfileArn,
    contactData_startTime,
    contactData_satelliteArn,
    contactData_maximumElevation,
    contactData_groundStation,
    contactData_endTime,
    contactData_contactId,
    contactData_region,
    contactData_postPassEndTime,
    contactData_prePassStartTime,
    contactData_errorMessage,
    contactData_tags,

    -- ** ContactIdResponse
    contactIdResponse_contactId,

    -- ** DataflowDetail
    dataflowDetail_destination,
    dataflowDetail_source,
    dataflowDetail_errorMessage,

    -- ** DataflowEndpoint
    dataflowEndpoint_mtu,
    dataflowEndpoint_status,
    dataflowEndpoint_address,
    dataflowEndpoint_name,

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
    destination_configType,
    destination_configDetails,

    -- ** Eirp
    eirp_units,
    eirp_value,

    -- ** Elevation
    elevation_unit,
    elevation_value,

    -- ** EndpointDetails
    endpointDetails_securityDetails,
    endpointDetails_endpoint,

    -- ** Frequency
    frequency_units,
    frequency_value,

    -- ** FrequencyBandwidth
    frequencyBandwidth_units,
    frequencyBandwidth_value,

    -- ** GroundStationData
    groundStationData_groundStationId,
    groundStationData_groundStationName,
    groundStationData_region,

    -- ** MissionProfileIdResponse
    missionProfileIdResponse_missionProfileId,

    -- ** MissionProfileListItem
    missionProfileListItem_missionProfileId,
    missionProfileListItem_missionProfileArn,
    missionProfileListItem_name,
    missionProfileListItem_region,

    -- ** S3RecordingConfig
    s3RecordingConfig_prefix,
    s3RecordingConfig_bucketArn,
    s3RecordingConfig_roleArn,

    -- ** S3RecordingDetails
    s3RecordingDetails_keyTemplate,
    s3RecordingDetails_bucketArn,

    -- ** SatelliteListItem
    satelliteListItem_satelliteId,
    satelliteListItem_satelliteArn,
    satelliteListItem_groundStations,
    satelliteListItem_noradSatelliteID,

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
    source_configType,
    source_configDetails,

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

import Network.AWS.GroundStation.CancelContact
import Network.AWS.GroundStation.CreateConfig
import Network.AWS.GroundStation.CreateDataflowEndpointGroup
import Network.AWS.GroundStation.CreateMissionProfile
import Network.AWS.GroundStation.DeleteConfig
import Network.AWS.GroundStation.DeleteDataflowEndpointGroup
import Network.AWS.GroundStation.DeleteMissionProfile
import Network.AWS.GroundStation.DescribeContact
import Network.AWS.GroundStation.GetConfig
import Network.AWS.GroundStation.GetDataflowEndpointGroup
import Network.AWS.GroundStation.GetMinuteUsage
import Network.AWS.GroundStation.GetMissionProfile
import Network.AWS.GroundStation.GetSatellite
import Network.AWS.GroundStation.ListConfigs
import Network.AWS.GroundStation.ListContacts
import Network.AWS.GroundStation.ListDataflowEndpointGroups
import Network.AWS.GroundStation.ListGroundStations
import Network.AWS.GroundStation.ListMissionProfiles
import Network.AWS.GroundStation.ListSatellites
import Network.AWS.GroundStation.ListTagsForResource
import Network.AWS.GroundStation.ReserveContact
import Network.AWS.GroundStation.TagResource
import Network.AWS.GroundStation.Types.AntennaDemodDecodeDetails
import Network.AWS.GroundStation.Types.AntennaDownlinkConfig
import Network.AWS.GroundStation.Types.AntennaDownlinkDemodDecodeConfig
import Network.AWS.GroundStation.Types.AntennaUplinkConfig
import Network.AWS.GroundStation.Types.ConfigDetails
import Network.AWS.GroundStation.Types.ConfigIdResponse
import Network.AWS.GroundStation.Types.ConfigListItem
import Network.AWS.GroundStation.Types.ConfigTypeData
import Network.AWS.GroundStation.Types.ContactData
import Network.AWS.GroundStation.Types.ContactIdResponse
import Network.AWS.GroundStation.Types.DataflowDetail
import Network.AWS.GroundStation.Types.DataflowEndpoint
import Network.AWS.GroundStation.Types.DataflowEndpointConfig
import Network.AWS.GroundStation.Types.DataflowEndpointGroupIdResponse
import Network.AWS.GroundStation.Types.DataflowEndpointListItem
import Network.AWS.GroundStation.Types.DecodeConfig
import Network.AWS.GroundStation.Types.DemodulationConfig
import Network.AWS.GroundStation.Types.Destination
import Network.AWS.GroundStation.Types.Eirp
import Network.AWS.GroundStation.Types.Elevation
import Network.AWS.GroundStation.Types.EndpointDetails
import Network.AWS.GroundStation.Types.Frequency
import Network.AWS.GroundStation.Types.FrequencyBandwidth
import Network.AWS.GroundStation.Types.GroundStationData
import Network.AWS.GroundStation.Types.MissionProfileIdResponse
import Network.AWS.GroundStation.Types.MissionProfileListItem
import Network.AWS.GroundStation.Types.S3RecordingConfig
import Network.AWS.GroundStation.Types.S3RecordingDetails
import Network.AWS.GroundStation.Types.SatelliteListItem
import Network.AWS.GroundStation.Types.SecurityDetails
import Network.AWS.GroundStation.Types.SocketAddress
import Network.AWS.GroundStation.Types.Source
import Network.AWS.GroundStation.Types.SpectrumConfig
import Network.AWS.GroundStation.Types.TrackingConfig
import Network.AWS.GroundStation.Types.UplinkEchoConfig
import Network.AWS.GroundStation.Types.UplinkSpectrumConfig
import Network.AWS.GroundStation.UntagResource
import Network.AWS.GroundStation.UpdateConfig
import Network.AWS.GroundStation.UpdateMissionProfile
