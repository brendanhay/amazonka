{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.GroundStation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-05-23@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Welcome to the AWS Ground Station API Reference. AWS Ground Station is a
-- fully managed service that enables you to control satellite
-- communications, downlink and process satellite data, and scale your
-- satellite operations efficiently and cost-effectively without having to
-- build or manage your own ground station infrastructure.
module Amazonka.GroundStation
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** DependencyException
    _DependencyException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelContact
    CancelContact (CancelContact'),
    newCancelContact,
    ContactIdResponse (ContactIdResponse'),
    newContactIdResponse,

    -- ** CreateConfig
    CreateConfig (CreateConfig'),
    newCreateConfig,
    ConfigIdResponse (ConfigIdResponse'),
    newConfigIdResponse,

    -- ** CreateDataflowEndpointGroup
    CreateDataflowEndpointGroup (CreateDataflowEndpointGroup'),
    newCreateDataflowEndpointGroup,
    DataflowEndpointGroupIdResponse (DataflowEndpointGroupIdResponse'),
    newDataflowEndpointGroupIdResponse,

    -- ** CreateEphemeris
    CreateEphemeris (CreateEphemeris'),
    newCreateEphemeris,
    EphemerisIdResponse (EphemerisIdResponse'),
    newEphemerisIdResponse,

    -- ** CreateMissionProfile
    CreateMissionProfile (CreateMissionProfile'),
    newCreateMissionProfile,
    MissionProfileIdResponse (MissionProfileIdResponse'),
    newMissionProfileIdResponse,

    -- ** DeleteConfig
    DeleteConfig (DeleteConfig'),
    newDeleteConfig,
    ConfigIdResponse (ConfigIdResponse'),
    newConfigIdResponse,

    -- ** DeleteDataflowEndpointGroup
    DeleteDataflowEndpointGroup (DeleteDataflowEndpointGroup'),
    newDeleteDataflowEndpointGroup,
    DataflowEndpointGroupIdResponse (DataflowEndpointGroupIdResponse'),
    newDataflowEndpointGroupIdResponse,

    -- ** DeleteEphemeris
    DeleteEphemeris (DeleteEphemeris'),
    newDeleteEphemeris,
    EphemerisIdResponse (EphemerisIdResponse'),
    newEphemerisIdResponse,

    -- ** DeleteMissionProfile
    DeleteMissionProfile (DeleteMissionProfile'),
    newDeleteMissionProfile,
    MissionProfileIdResponse (MissionProfileIdResponse'),
    newMissionProfileIdResponse,

    -- ** DescribeContact
    DescribeContact (DescribeContact'),
    newDescribeContact,
    DescribeContactResponse (DescribeContactResponse'),
    newDescribeContactResponse,

    -- ** DescribeEphemeris
    DescribeEphemeris (DescribeEphemeris'),
    newDescribeEphemeris,
    DescribeEphemerisResponse (DescribeEphemerisResponse'),
    newDescribeEphemerisResponse,

    -- ** GetConfig
    GetConfig (GetConfig'),
    newGetConfig,
    GetConfigResponse (GetConfigResponse'),
    newGetConfigResponse,

    -- ** GetDataflowEndpointGroup
    GetDataflowEndpointGroup (GetDataflowEndpointGroup'),
    newGetDataflowEndpointGroup,
    GetDataflowEndpointGroupResponse (GetDataflowEndpointGroupResponse'),
    newGetDataflowEndpointGroupResponse,

    -- ** GetMinuteUsage
    GetMinuteUsage (GetMinuteUsage'),
    newGetMinuteUsage,
    GetMinuteUsageResponse (GetMinuteUsageResponse'),
    newGetMinuteUsageResponse,

    -- ** GetMissionProfile
    GetMissionProfile (GetMissionProfile'),
    newGetMissionProfile,
    GetMissionProfileResponse (GetMissionProfileResponse'),
    newGetMissionProfileResponse,

    -- ** GetSatellite
    GetSatellite (GetSatellite'),
    newGetSatellite,
    GetSatelliteResponse (GetSatelliteResponse'),
    newGetSatelliteResponse,

    -- ** ListConfigs (Paginated)
    ListConfigs (ListConfigs'),
    newListConfigs,
    ListConfigsResponse (ListConfigsResponse'),
    newListConfigsResponse,

    -- ** ListContacts (Paginated)
    ListContacts (ListContacts'),
    newListContacts,
    ListContactsResponse (ListContactsResponse'),
    newListContactsResponse,

    -- ** ListDataflowEndpointGroups (Paginated)
    ListDataflowEndpointGroups (ListDataflowEndpointGroups'),
    newListDataflowEndpointGroups,
    ListDataflowEndpointGroupsResponse (ListDataflowEndpointGroupsResponse'),
    newListDataflowEndpointGroupsResponse,

    -- ** ListEphemerides (Paginated)
    ListEphemerides (ListEphemerides'),
    newListEphemerides,
    ListEphemeridesResponse (ListEphemeridesResponse'),
    newListEphemeridesResponse,

    -- ** ListGroundStations (Paginated)
    ListGroundStations (ListGroundStations'),
    newListGroundStations,
    ListGroundStationsResponse (ListGroundStationsResponse'),
    newListGroundStationsResponse,

    -- ** ListMissionProfiles (Paginated)
    ListMissionProfiles (ListMissionProfiles'),
    newListMissionProfiles,
    ListMissionProfilesResponse (ListMissionProfilesResponse'),
    newListMissionProfilesResponse,

    -- ** ListSatellites (Paginated)
    ListSatellites (ListSatellites'),
    newListSatellites,
    ListSatellitesResponse (ListSatellitesResponse'),
    newListSatellitesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ReserveContact
    ReserveContact (ReserveContact'),
    newReserveContact,
    ContactIdResponse (ContactIdResponse'),
    newContactIdResponse,

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

    -- ** UpdateConfig
    UpdateConfig (UpdateConfig'),
    newUpdateConfig,
    ConfigIdResponse (ConfigIdResponse'),
    newConfigIdResponse,

    -- ** UpdateEphemeris
    UpdateEphemeris (UpdateEphemeris'),
    newUpdateEphemeris,
    EphemerisIdResponse (EphemerisIdResponse'),
    newEphemerisIdResponse,

    -- ** UpdateMissionProfile
    UpdateMissionProfile (UpdateMissionProfile'),
    newUpdateMissionProfile,
    MissionProfileIdResponse (MissionProfileIdResponse'),
    newMissionProfileIdResponse,

    -- * Types

    -- ** AngleUnits
    AngleUnits (..),

    -- ** BandwidthUnits
    BandwidthUnits (..),

    -- ** ConfigCapabilityType
    ConfigCapabilityType (..),

    -- ** ContactStatus
    ContactStatus (..),

    -- ** Criticality
    Criticality (..),

    -- ** EirpUnits
    EirpUnits (..),

    -- ** EndpointStatus
    EndpointStatus (..),

    -- ** EphemerisInvalidReason
    EphemerisInvalidReason (..),

    -- ** EphemerisSource
    EphemerisSource (..),

    -- ** EphemerisStatus
    EphemerisStatus (..),

    -- ** FrequencyUnits
    FrequencyUnits (..),

    -- ** Polarization
    Polarization (..),

    -- ** AntennaDemodDecodeDetails
    AntennaDemodDecodeDetails (AntennaDemodDecodeDetails'),
    newAntennaDemodDecodeDetails,

    -- ** AntennaDownlinkConfig
    AntennaDownlinkConfig (AntennaDownlinkConfig'),
    newAntennaDownlinkConfig,

    -- ** AntennaDownlinkDemodDecodeConfig
    AntennaDownlinkDemodDecodeConfig (AntennaDownlinkDemodDecodeConfig'),
    newAntennaDownlinkDemodDecodeConfig,

    -- ** AntennaUplinkConfig
    AntennaUplinkConfig (AntennaUplinkConfig'),
    newAntennaUplinkConfig,

    -- ** ConfigDetails
    ConfigDetails (ConfigDetails'),
    newConfigDetails,

    -- ** ConfigIdResponse
    ConfigIdResponse (ConfigIdResponse'),
    newConfigIdResponse,

    -- ** ConfigListItem
    ConfigListItem (ConfigListItem'),
    newConfigListItem,

    -- ** ConfigTypeData
    ConfigTypeData (ConfigTypeData'),
    newConfigTypeData,

    -- ** ContactData
    ContactData (ContactData'),
    newContactData,

    -- ** ContactIdResponse
    ContactIdResponse (ContactIdResponse'),
    newContactIdResponse,

    -- ** DataflowDetail
    DataflowDetail (DataflowDetail'),
    newDataflowDetail,

    -- ** DataflowEndpoint
    DataflowEndpoint (DataflowEndpoint'),
    newDataflowEndpoint,

    -- ** DataflowEndpointConfig
    DataflowEndpointConfig (DataflowEndpointConfig'),
    newDataflowEndpointConfig,

    -- ** DataflowEndpointGroupIdResponse
    DataflowEndpointGroupIdResponse (DataflowEndpointGroupIdResponse'),
    newDataflowEndpointGroupIdResponse,

    -- ** DataflowEndpointListItem
    DataflowEndpointListItem (DataflowEndpointListItem'),
    newDataflowEndpointListItem,

    -- ** DecodeConfig
    DecodeConfig (DecodeConfig'),
    newDecodeConfig,

    -- ** DemodulationConfig
    DemodulationConfig (DemodulationConfig'),
    newDemodulationConfig,

    -- ** Destination
    Destination (Destination'),
    newDestination,

    -- ** Eirp
    Eirp (Eirp'),
    newEirp,

    -- ** Elevation
    Elevation (Elevation'),
    newElevation,

    -- ** EndpointDetails
    EndpointDetails (EndpointDetails'),
    newEndpointDetails,

    -- ** EphemerisData
    EphemerisData (EphemerisData'),
    newEphemerisData,

    -- ** EphemerisDescription
    EphemerisDescription (EphemerisDescription'),
    newEphemerisDescription,

    -- ** EphemerisIdResponse
    EphemerisIdResponse (EphemerisIdResponse'),
    newEphemerisIdResponse,

    -- ** EphemerisItem
    EphemerisItem (EphemerisItem'),
    newEphemerisItem,

    -- ** EphemerisMetaData
    EphemerisMetaData (EphemerisMetaData'),
    newEphemerisMetaData,

    -- ** EphemerisTypeDescription
    EphemerisTypeDescription (EphemerisTypeDescription'),
    newEphemerisTypeDescription,

    -- ** Frequency
    Frequency (Frequency'),
    newFrequency,

    -- ** FrequencyBandwidth
    FrequencyBandwidth (FrequencyBandwidth'),
    newFrequencyBandwidth,

    -- ** GroundStationData
    GroundStationData (GroundStationData'),
    newGroundStationData,

    -- ** MissionProfileIdResponse
    MissionProfileIdResponse (MissionProfileIdResponse'),
    newMissionProfileIdResponse,

    -- ** MissionProfileListItem
    MissionProfileListItem (MissionProfileListItem'),
    newMissionProfileListItem,

    -- ** OEMEphemeris
    OEMEphemeris (OEMEphemeris'),
    newOEMEphemeris,

    -- ** S3Object
    S3Object (S3Object'),
    newS3Object,

    -- ** S3RecordingConfig
    S3RecordingConfig (S3RecordingConfig'),
    newS3RecordingConfig,

    -- ** S3RecordingDetails
    S3RecordingDetails (S3RecordingDetails'),
    newS3RecordingDetails,

    -- ** SatelliteListItem
    SatelliteListItem (SatelliteListItem'),
    newSatelliteListItem,

    -- ** SecurityDetails
    SecurityDetails (SecurityDetails'),
    newSecurityDetails,

    -- ** SocketAddress
    SocketAddress (SocketAddress'),
    newSocketAddress,

    -- ** Source
    Source (Source'),
    newSource,

    -- ** SpectrumConfig
    SpectrumConfig (SpectrumConfig'),
    newSpectrumConfig,

    -- ** TLEData
    TLEData (TLEData'),
    newTLEData,

    -- ** TLEEphemeris
    TLEEphemeris (TLEEphemeris'),
    newTLEEphemeris,

    -- ** TimeRange
    TimeRange (TimeRange'),
    newTimeRange,

    -- ** TrackingConfig
    TrackingConfig (TrackingConfig'),
    newTrackingConfig,

    -- ** UplinkEchoConfig
    UplinkEchoConfig (UplinkEchoConfig'),
    newUplinkEchoConfig,

    -- ** UplinkSpectrumConfig
    UplinkSpectrumConfig (UplinkSpectrumConfig'),
    newUplinkSpectrumConfig,
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
import Amazonka.GroundStation.Lens
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
import Amazonka.GroundStation.Types
import Amazonka.GroundStation.UntagResource
import Amazonka.GroundStation.UpdateConfig
import Amazonka.GroundStation.UpdateEphemeris
import Amazonka.GroundStation.UpdateMissionProfile
import Amazonka.GroundStation.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'GroundStation'.

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
