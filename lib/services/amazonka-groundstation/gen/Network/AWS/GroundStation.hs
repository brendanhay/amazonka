{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.GroundStation
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.GroundStation
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** DependencyException
    _DependencyException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteDataflowEndpointGroup
    DeleteDataflowEndpointGroup (DeleteDataflowEndpointGroup'),
    newDeleteDataflowEndpointGroup,
    DataflowEndpointGroupIdResponse (DataflowEndpointGroupIdResponse'),
    newDataflowEndpointGroupIdResponse,

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

    -- ** GetMinuteUsage
    GetMinuteUsage (GetMinuteUsage'),
    newGetMinuteUsage,
    GetMinuteUsageResponse (GetMinuteUsageResponse'),
    newGetMinuteUsageResponse,

    -- ** DescribeContact
    DescribeContact (DescribeContact'),
    newDescribeContact,
    DescribeContactResponse (DescribeContactResponse'),
    newDescribeContactResponse,

    -- ** ReserveContact
    ReserveContact (ReserveContact'),
    newReserveContact,
    ContactIdResponse (ContactIdResponse'),
    newContactIdResponse,

    -- ** CreateMissionProfile
    CreateMissionProfile (CreateMissionProfile'),
    newCreateMissionProfile,
    MissionProfileIdResponse (MissionProfileIdResponse'),
    newMissionProfileIdResponse,

    -- ** ListGroundStations (Paginated)
    ListGroundStations (ListGroundStations'),
    newListGroundStations,
    ListGroundStationsResponse (ListGroundStationsResponse'),
    newListGroundStationsResponse,

    -- ** CreateConfig
    CreateConfig (CreateConfig'),
    newCreateConfig,
    ConfigIdResponse (ConfigIdResponse'),
    newConfigIdResponse,

    -- ** ListMissionProfiles (Paginated)
    ListMissionProfiles (ListMissionProfiles'),
    newListMissionProfiles,
    ListMissionProfilesResponse (ListMissionProfilesResponse'),
    newListMissionProfilesResponse,

    -- ** GetMissionProfile
    GetMissionProfile (GetMissionProfile'),
    newGetMissionProfile,
    GetMissionProfileResponse (GetMissionProfileResponse'),
    newGetMissionProfileResponse,

    -- ** GetConfig
    GetConfig (GetConfig'),
    newGetConfig,
    GetConfigResponse (GetConfigResponse'),
    newGetConfigResponse,

    -- ** ListDataflowEndpointGroups (Paginated)
    ListDataflowEndpointGroups (ListDataflowEndpointGroups'),
    newListDataflowEndpointGroups,
    ListDataflowEndpointGroupsResponse (ListDataflowEndpointGroupsResponse'),
    newListDataflowEndpointGroupsResponse,

    -- ** CreateDataflowEndpointGroup
    CreateDataflowEndpointGroup (CreateDataflowEndpointGroup'),
    newCreateDataflowEndpointGroup,
    DataflowEndpointGroupIdResponse (DataflowEndpointGroupIdResponse'),
    newDataflowEndpointGroupIdResponse,

    -- ** GetSatellite
    GetSatellite (GetSatellite'),
    newGetSatellite,
    GetSatelliteResponse (GetSatelliteResponse'),
    newGetSatelliteResponse,

    -- ** GetDataflowEndpointGroup
    GetDataflowEndpointGroup (GetDataflowEndpointGroup'),
    newGetDataflowEndpointGroup,
    GetDataflowEndpointGroupResponse (GetDataflowEndpointGroupResponse'),
    newGetDataflowEndpointGroupResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListConfigs (Paginated)
    ListConfigs (ListConfigs'),
    newListConfigs,
    ListConfigsResponse (ListConfigsResponse'),
    newListConfigsResponse,

    -- ** UpdateConfig
    UpdateConfig (UpdateConfig'),
    newUpdateConfig,
    ConfigIdResponse (ConfigIdResponse'),
    newConfigIdResponse,

    -- ** DeleteConfig
    DeleteConfig (DeleteConfig'),
    newDeleteConfig,
    ConfigIdResponse (ConfigIdResponse'),
    newConfigIdResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateMissionProfile
    UpdateMissionProfile (UpdateMissionProfile'),
    newUpdateMissionProfile,
    MissionProfileIdResponse (MissionProfileIdResponse'),
    newMissionProfileIdResponse,

    -- ** DeleteMissionProfile
    DeleteMissionProfile (DeleteMissionProfile'),
    newDeleteMissionProfile,
    MissionProfileIdResponse (MissionProfileIdResponse'),
    newMissionProfileIdResponse,

    -- ** CancelContact
    CancelContact (CancelContact'),
    newCancelContact,
    ContactIdResponse (ContactIdResponse'),
    newContactIdResponse,

    -- ** ListContacts (Paginated)
    ListContacts (ListContacts'),
    newListContacts,
    ListContactsResponse (ListContactsResponse'),
    newListContactsResponse,

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
import Network.AWS.GroundStation.Lens
import Network.AWS.GroundStation.ListConfigs
import Network.AWS.GroundStation.ListContacts
import Network.AWS.GroundStation.ListDataflowEndpointGroups
import Network.AWS.GroundStation.ListGroundStations
import Network.AWS.GroundStation.ListMissionProfiles
import Network.AWS.GroundStation.ListSatellites
import Network.AWS.GroundStation.ListTagsForResource
import Network.AWS.GroundStation.ReserveContact
import Network.AWS.GroundStation.TagResource
import Network.AWS.GroundStation.Types
import Network.AWS.GroundStation.UntagResource
import Network.AWS.GroundStation.UpdateConfig
import Network.AWS.GroundStation.UpdateMissionProfile
import Network.AWS.GroundStation.Waiters

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
