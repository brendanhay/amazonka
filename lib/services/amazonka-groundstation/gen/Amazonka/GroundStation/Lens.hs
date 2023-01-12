{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GroundStation.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    configIdResponse_configArn,
    configIdResponse_configId,
    configIdResponse_configType,

    -- ** CreateDataflowEndpointGroup
    createDataflowEndpointGroup_tags,
    createDataflowEndpointGroup_endpointDetails,
    dataflowEndpointGroupIdResponse_dataflowEndpointGroupId,

    -- ** CreateEphemeris
    createEphemeris_enabled,
    createEphemeris_ephemeris,
    createEphemeris_expirationTime,
    createEphemeris_kmsKeyArn,
    createEphemeris_priority,
    createEphemeris_tags,
    createEphemeris_name,
    createEphemeris_satelliteId,
    ephemerisIdResponse_ephemerisId,

    -- ** CreateMissionProfile
    createMissionProfile_contactPostPassDurationSeconds,
    createMissionProfile_contactPrePassDurationSeconds,
    createMissionProfile_tags,
    createMissionProfile_dataflowEdges,
    createMissionProfile_minimumViableContactDurationSeconds,
    createMissionProfile_name,
    createMissionProfile_trackingConfigArn,
    missionProfileIdResponse_missionProfileId,

    -- ** DeleteConfig
    deleteConfig_configId,
    deleteConfig_configType,
    configIdResponse_configArn,
    configIdResponse_configId,
    configIdResponse_configType,

    -- ** DeleteDataflowEndpointGroup
    deleteDataflowEndpointGroup_dataflowEndpointGroupId,
    dataflowEndpointGroupIdResponse_dataflowEndpointGroupId,

    -- ** DeleteEphemeris
    deleteEphemeris_ephemerisId,
    ephemerisIdResponse_ephemerisId,

    -- ** DeleteMissionProfile
    deleteMissionProfile_missionProfileId,
    missionProfileIdResponse_missionProfileId,

    -- ** DescribeContact
    describeContact_contactId,
    describeContactResponse_contactId,
    describeContactResponse_contactStatus,
    describeContactResponse_dataflowList,
    describeContactResponse_endTime,
    describeContactResponse_errorMessage,
    describeContactResponse_groundStation,
    describeContactResponse_maximumElevation,
    describeContactResponse_missionProfileArn,
    describeContactResponse_postPassEndTime,
    describeContactResponse_prePassStartTime,
    describeContactResponse_region,
    describeContactResponse_satelliteArn,
    describeContactResponse_startTime,
    describeContactResponse_tags,
    describeContactResponse_httpStatus,

    -- ** DescribeEphemeris
    describeEphemeris_ephemerisId,
    describeEphemerisResponse_creationTime,
    describeEphemerisResponse_enabled,
    describeEphemerisResponse_ephemerisId,
    describeEphemerisResponse_invalidReason,
    describeEphemerisResponse_name,
    describeEphemerisResponse_priority,
    describeEphemerisResponse_satelliteId,
    describeEphemerisResponse_status,
    describeEphemerisResponse_suppliedData,
    describeEphemerisResponse_tags,
    describeEphemerisResponse_httpStatus,

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

    -- ** GetDataflowEndpointGroup
    getDataflowEndpointGroup_dataflowEndpointGroupId,
    getDataflowEndpointGroupResponse_dataflowEndpointGroupArn,
    getDataflowEndpointGroupResponse_dataflowEndpointGroupId,
    getDataflowEndpointGroupResponse_endpointsDetails,
    getDataflowEndpointGroupResponse_tags,
    getDataflowEndpointGroupResponse_httpStatus,

    -- ** GetMinuteUsage
    getMinuteUsage_month,
    getMinuteUsage_year,
    getMinuteUsageResponse_estimatedMinutesRemaining,
    getMinuteUsageResponse_isReservedMinutesCustomer,
    getMinuteUsageResponse_totalReservedMinuteAllocation,
    getMinuteUsageResponse_totalScheduledMinutes,
    getMinuteUsageResponse_upcomingMinutesScheduled,
    getMinuteUsageResponse_httpStatus,

    -- ** GetMissionProfile
    getMissionProfile_missionProfileId,
    getMissionProfileResponse_contactPostPassDurationSeconds,
    getMissionProfileResponse_contactPrePassDurationSeconds,
    getMissionProfileResponse_dataflowEdges,
    getMissionProfileResponse_minimumViableContactDurationSeconds,
    getMissionProfileResponse_missionProfileArn,
    getMissionProfileResponse_missionProfileId,
    getMissionProfileResponse_name,
    getMissionProfileResponse_region,
    getMissionProfileResponse_tags,
    getMissionProfileResponse_trackingConfigArn,
    getMissionProfileResponse_httpStatus,

    -- ** GetSatellite
    getSatellite_satelliteId,
    getSatelliteResponse_currentEphemeris,
    getSatelliteResponse_groundStations,
    getSatelliteResponse_noradSatelliteID,
    getSatelliteResponse_satelliteArn,
    getSatelliteResponse_satelliteId,
    getSatelliteResponse_httpStatus,

    -- ** ListConfigs
    listConfigs_maxResults,
    listConfigs_nextToken,
    listConfigsResponse_configList,
    listConfigsResponse_nextToken,
    listConfigsResponse_httpStatus,

    -- ** ListContacts
    listContacts_groundStation,
    listContacts_maxResults,
    listContacts_missionProfileArn,
    listContacts_nextToken,
    listContacts_satelliteArn,
    listContacts_endTime,
    listContacts_startTime,
    listContacts_statusList,
    listContactsResponse_contactList,
    listContactsResponse_nextToken,
    listContactsResponse_httpStatus,

    -- ** ListDataflowEndpointGroups
    listDataflowEndpointGroups_maxResults,
    listDataflowEndpointGroups_nextToken,
    listDataflowEndpointGroupsResponse_dataflowEndpointGroupList,
    listDataflowEndpointGroupsResponse_nextToken,
    listDataflowEndpointGroupsResponse_httpStatus,

    -- ** ListEphemerides
    listEphemerides_maxResults,
    listEphemerides_nextToken,
    listEphemerides_statusList,
    listEphemerides_endTime,
    listEphemerides_satelliteId,
    listEphemerides_startTime,
    listEphemeridesResponse_ephemerides,
    listEphemeridesResponse_nextToken,
    listEphemeridesResponse_httpStatus,

    -- ** ListGroundStations
    listGroundStations_maxResults,
    listGroundStations_nextToken,
    listGroundStations_satelliteId,
    listGroundStationsResponse_groundStationList,
    listGroundStationsResponse_nextToken,
    listGroundStationsResponse_httpStatus,

    -- ** ListMissionProfiles
    listMissionProfiles_maxResults,
    listMissionProfiles_nextToken,
    listMissionProfilesResponse_missionProfileList,
    listMissionProfilesResponse_nextToken,
    listMissionProfilesResponse_httpStatus,

    -- ** ListSatellites
    listSatellites_maxResults,
    listSatellites_nextToken,
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
    configIdResponse_configArn,
    configIdResponse_configId,
    configIdResponse_configType,

    -- ** UpdateEphemeris
    updateEphemeris_name,
    updateEphemeris_priority,
    updateEphemeris_enabled,
    updateEphemeris_ephemerisId,
    ephemerisIdResponse_ephemerisId,

    -- ** UpdateMissionProfile
    updateMissionProfile_contactPostPassDurationSeconds,
    updateMissionProfile_contactPrePassDurationSeconds,
    updateMissionProfile_dataflowEdges,
    updateMissionProfile_minimumViableContactDurationSeconds,
    updateMissionProfile_name,
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
    configDetails_antennaDemodDecodeDetails,
    configDetails_endpointDetails,
    configDetails_s3RecordingDetails,

    -- ** ConfigIdResponse
    configIdResponse_configArn,
    configIdResponse_configId,
    configIdResponse_configType,

    -- ** ConfigListItem
    configListItem_configArn,
    configListItem_configId,
    configListItem_configType,
    configListItem_name,

    -- ** ConfigTypeData
    configTypeData_antennaDownlinkConfig,
    configTypeData_antennaDownlinkDemodDecodeConfig,
    configTypeData_antennaUplinkConfig,
    configTypeData_dataflowEndpointConfig,
    configTypeData_s3RecordingConfig,
    configTypeData_trackingConfig,
    configTypeData_uplinkEchoConfig,

    -- ** ContactData
    contactData_contactId,
    contactData_contactStatus,
    contactData_endTime,
    contactData_errorMessage,
    contactData_groundStation,
    contactData_maximumElevation,
    contactData_missionProfileArn,
    contactData_postPassEndTime,
    contactData_prePassStartTime,
    contactData_region,
    contactData_satelliteArn,
    contactData_startTime,
    contactData_tags,

    -- ** ContactIdResponse
    contactIdResponse_contactId,

    -- ** DataflowDetail
    dataflowDetail_destination,
    dataflowDetail_errorMessage,
    dataflowDetail_source,

    -- ** DataflowEndpoint
    dataflowEndpoint_address,
    dataflowEndpoint_mtu,
    dataflowEndpoint_name,
    dataflowEndpoint_status,

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
    destination_configDetails,
    destination_configId,
    destination_configType,
    destination_dataflowDestinationRegion,

    -- ** Eirp
    eirp_units,
    eirp_value,

    -- ** Elevation
    elevation_unit,
    elevation_value,

    -- ** EndpointDetails
    endpointDetails_endpoint,
    endpointDetails_securityDetails,

    -- ** EphemerisData
    ephemerisData_oem,
    ephemerisData_tle,

    -- ** EphemerisDescription
    ephemerisDescription_ephemerisData,
    ephemerisDescription_sourceS3Object,

    -- ** EphemerisIdResponse
    ephemerisIdResponse_ephemerisId,

    -- ** EphemerisItem
    ephemerisItem_creationTime,
    ephemerisItem_enabled,
    ephemerisItem_ephemerisId,
    ephemerisItem_name,
    ephemerisItem_priority,
    ephemerisItem_sourceS3Object,
    ephemerisItem_status,

    -- ** EphemerisMetaData
    ephemerisMetaData_ephemerisId,
    ephemerisMetaData_epoch,
    ephemerisMetaData_name,
    ephemerisMetaData_source,

    -- ** EphemerisTypeDescription
    ephemerisTypeDescription_oem,
    ephemerisTypeDescription_tle,

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
    missionProfileListItem_missionProfileArn,
    missionProfileListItem_missionProfileId,
    missionProfileListItem_name,
    missionProfileListItem_region,

    -- ** OEMEphemeris
    oEMEphemeris_oemData,
    oEMEphemeris_s3Object,

    -- ** S3Object
    s3Object_bucket,
    s3Object_key,
    s3Object_version,

    -- ** S3RecordingConfig
    s3RecordingConfig_prefix,
    s3RecordingConfig_bucketArn,
    s3RecordingConfig_roleArn,

    -- ** S3RecordingDetails
    s3RecordingDetails_bucketArn,
    s3RecordingDetails_keyTemplate,

    -- ** SatelliteListItem
    satelliteListItem_currentEphemeris,
    satelliteListItem_groundStations,
    satelliteListItem_noradSatelliteID,
    satelliteListItem_satelliteArn,
    satelliteListItem_satelliteId,

    -- ** SecurityDetails
    securityDetails_roleArn,
    securityDetails_securityGroupIds,
    securityDetails_subnetIds,

    -- ** SocketAddress
    socketAddress_name,
    socketAddress_port,

    -- ** Source
    source_configDetails,
    source_configId,
    source_configType,
    source_dataflowSourceRegion,

    -- ** SpectrumConfig
    spectrumConfig_polarization,
    spectrumConfig_bandwidth,
    spectrumConfig_centerFrequency,

    -- ** TLEData
    tLEData_tleLine1,
    tLEData_tleLine2,
    tLEData_validTimeRange,

    -- ** TLEEphemeris
    tLEEphemeris_s3Object,
    tLEEphemeris_tleData,

    -- ** TimeRange
    timeRange_endTime,
    timeRange_startTime,

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
import Amazonka.GroundStation.CreateEphemeris
import Amazonka.GroundStation.CreateMissionProfile
import Amazonka.GroundStation.DeleteConfig
import Amazonka.GroundStation.DeleteDataflowEndpointGroup
import Amazonka.GroundStation.DeleteEphemeris
import Amazonka.GroundStation.DeleteMissionProfile
import Amazonka.GroundStation.DescribeContact
import Amazonka.GroundStation.DescribeEphemeris
import Amazonka.GroundStation.GetConfig
import Amazonka.GroundStation.GetDataflowEndpointGroup
import Amazonka.GroundStation.GetMinuteUsage
import Amazonka.GroundStation.GetMissionProfile
import Amazonka.GroundStation.GetSatellite
import Amazonka.GroundStation.ListConfigs
import Amazonka.GroundStation.ListContacts
import Amazonka.GroundStation.ListDataflowEndpointGroups
import Amazonka.GroundStation.ListEphemerides
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
import Amazonka.GroundStation.Types.EphemerisData
import Amazonka.GroundStation.Types.EphemerisDescription
import Amazonka.GroundStation.Types.EphemerisIdResponse
import Amazonka.GroundStation.Types.EphemerisItem
import Amazonka.GroundStation.Types.EphemerisMetaData
import Amazonka.GroundStation.Types.EphemerisTypeDescription
import Amazonka.GroundStation.Types.Frequency
import Amazonka.GroundStation.Types.FrequencyBandwidth
import Amazonka.GroundStation.Types.GroundStationData
import Amazonka.GroundStation.Types.MissionProfileIdResponse
import Amazonka.GroundStation.Types.MissionProfileListItem
import Amazonka.GroundStation.Types.OEMEphemeris
import Amazonka.GroundStation.Types.S3Object
import Amazonka.GroundStation.Types.S3RecordingConfig
import Amazonka.GroundStation.Types.S3RecordingDetails
import Amazonka.GroundStation.Types.SatelliteListItem
import Amazonka.GroundStation.Types.SecurityDetails
import Amazonka.GroundStation.Types.SocketAddress
import Amazonka.GroundStation.Types.Source
import Amazonka.GroundStation.Types.SpectrumConfig
import Amazonka.GroundStation.Types.TLEData
import Amazonka.GroundStation.Types.TLEEphemeris
import Amazonka.GroundStation.Types.TimeRange
import Amazonka.GroundStation.Types.TrackingConfig
import Amazonka.GroundStation.Types.UplinkEchoConfig
import Amazonka.GroundStation.Types.UplinkSpectrumConfig
import Amazonka.GroundStation.UntagResource
import Amazonka.GroundStation.UpdateConfig
import Amazonka.GroundStation.UpdateEphemeris
import Amazonka.GroundStation.UpdateMissionProfile
