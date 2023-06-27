{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.InternetMonitor.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Lens
  ( -- * Operations

    -- ** CreateMonitor
    createMonitor_clientToken,
    createMonitor_internetMeasurementsLogDelivery,
    createMonitor_maxCityNetworksToMonitor,
    createMonitor_resources,
    createMonitor_tags,
    createMonitor_trafficPercentageToMonitor,
    createMonitor_monitorName,
    createMonitorResponse_httpStatus,
    createMonitorResponse_arn,
    createMonitorResponse_status,

    -- ** DeleteMonitor
    deleteMonitor_monitorName,
    deleteMonitorResponse_httpStatus,

    -- ** GetHealthEvent
    getHealthEvent_monitorName,
    getHealthEvent_eventId,
    getHealthEventResponse_createdAt,
    getHealthEventResponse_endedAt,
    getHealthEventResponse_percentOfTotalTrafficImpacted,
    getHealthEventResponse_httpStatus,
    getHealthEventResponse_eventArn,
    getHealthEventResponse_eventId,
    getHealthEventResponse_startedAt,
    getHealthEventResponse_lastUpdatedAt,
    getHealthEventResponse_impactedLocations,
    getHealthEventResponse_status,
    getHealthEventResponse_impactType,

    -- ** GetMonitor
    getMonitor_monitorName,
    getMonitorResponse_internetMeasurementsLogDelivery,
    getMonitorResponse_maxCityNetworksToMonitor,
    getMonitorResponse_processingStatus,
    getMonitorResponse_processingStatusInfo,
    getMonitorResponse_tags,
    getMonitorResponse_trafficPercentageToMonitor,
    getMonitorResponse_httpStatus,
    getMonitorResponse_monitorName,
    getMonitorResponse_monitorArn,
    getMonitorResponse_resources,
    getMonitorResponse_status,
    getMonitorResponse_createdAt,
    getMonitorResponse_modifiedAt,

    -- ** ListHealthEvents
    listHealthEvents_endTime,
    listHealthEvents_eventStatus,
    listHealthEvents_maxResults,
    listHealthEvents_nextToken,
    listHealthEvents_startTime,
    listHealthEvents_monitorName,
    listHealthEventsResponse_nextToken,
    listHealthEventsResponse_httpStatus,
    listHealthEventsResponse_healthEvents,

    -- ** ListMonitors
    listMonitors_maxResults,
    listMonitors_monitorStatus,
    listMonitors_nextToken,
    listMonitorsResponse_nextToken,
    listMonitorsResponse_httpStatus,
    listMonitorsResponse_monitors,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateMonitor
    updateMonitor_clientToken,
    updateMonitor_internetMeasurementsLogDelivery,
    updateMonitor_maxCityNetworksToMonitor,
    updateMonitor_resourcesToAdd,
    updateMonitor_resourcesToRemove,
    updateMonitor_status,
    updateMonitor_trafficPercentageToMonitor,
    updateMonitor_monitorName,
    updateMonitorResponse_httpStatus,
    updateMonitorResponse_monitorArn,
    updateMonitorResponse_status,

    -- * Types

    -- ** AvailabilityMeasurement
    availabilityMeasurement_experienceScore,
    availabilityMeasurement_percentOfClientLocationImpacted,
    availabilityMeasurement_percentOfTotalTrafficImpacted,

    -- ** HealthEvent
    healthEvent_createdAt,
    healthEvent_endedAt,
    healthEvent_percentOfTotalTrafficImpacted,
    healthEvent_eventArn,
    healthEvent_eventId,
    healthEvent_startedAt,
    healthEvent_lastUpdatedAt,
    healthEvent_impactedLocations,
    healthEvent_status,
    healthEvent_impactType,

    -- ** ImpactedLocation
    impactedLocation_causedBy,
    impactedLocation_city,
    impactedLocation_countryCode,
    impactedLocation_internetHealth,
    impactedLocation_latitude,
    impactedLocation_longitude,
    impactedLocation_metro,
    impactedLocation_serviceLocation,
    impactedLocation_subdivision,
    impactedLocation_subdivisionCode,
    impactedLocation_aSName,
    impactedLocation_aSNumber,
    impactedLocation_country,
    impactedLocation_status,

    -- ** InternetHealth
    internetHealth_availability,
    internetHealth_performance,

    -- ** InternetMeasurementsLogDelivery
    internetMeasurementsLogDelivery_s3Config,

    -- ** Monitor
    monitor_processingStatus,
    monitor_monitorName,
    monitor_monitorArn,
    monitor_status,

    -- ** Network
    network_aSName,
    network_aSNumber,

    -- ** NetworkImpairment
    networkImpairment_networks,
    networkImpairment_asPath,
    networkImpairment_networkEventType,

    -- ** PerformanceMeasurement
    performanceMeasurement_experienceScore,
    performanceMeasurement_percentOfClientLocationImpacted,
    performanceMeasurement_percentOfTotalTrafficImpacted,
    performanceMeasurement_roundTripTime,

    -- ** RoundTripTime
    roundTripTime_p50,
    roundTripTime_p90,
    roundTripTime_p95,

    -- ** S3Config
    s3Config_bucketName,
    s3Config_bucketPrefix,
    s3Config_logDeliveryStatus,
  )
where

import Amazonka.InternetMonitor.CreateMonitor
import Amazonka.InternetMonitor.DeleteMonitor
import Amazonka.InternetMonitor.GetHealthEvent
import Amazonka.InternetMonitor.GetMonitor
import Amazonka.InternetMonitor.ListHealthEvents
import Amazonka.InternetMonitor.ListMonitors
import Amazonka.InternetMonitor.ListTagsForResource
import Amazonka.InternetMonitor.TagResource
import Amazonka.InternetMonitor.Types.AvailabilityMeasurement
import Amazonka.InternetMonitor.Types.HealthEvent
import Amazonka.InternetMonitor.Types.ImpactedLocation
import Amazonka.InternetMonitor.Types.InternetHealth
import Amazonka.InternetMonitor.Types.InternetMeasurementsLogDelivery
import Amazonka.InternetMonitor.Types.Monitor
import Amazonka.InternetMonitor.Types.Network
import Amazonka.InternetMonitor.Types.NetworkImpairment
import Amazonka.InternetMonitor.Types.PerformanceMeasurement
import Amazonka.InternetMonitor.Types.RoundTripTime
import Amazonka.InternetMonitor.Types.S3Config
import Amazonka.InternetMonitor.UntagResource
import Amazonka.InternetMonitor.UpdateMonitor
