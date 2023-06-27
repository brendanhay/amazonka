{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.InternetMonitor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-06-03@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon CloudWatch Internet Monitor provides visibility into how internet
-- issues impact the performance and availability between your applications
-- hosted on Amazon Web Services and your end users. It reduces the time it
-- takes for you to diagnose internet issues from days to minutes. Internet
-- Monitor uses the connectivity data that Amazon Web Services captures
-- from its global networking footprint to calculate a baseline of
-- performance and availability for internet traffic. This is the same data
-- that Amazon Web Services uses to monitor internet uptime and
-- availability. With those measurements as a baseline, Internet Monitor
-- raises awareness for you when there are significant problems for your
-- end users in the different geographic locations where your application
-- runs.
--
-- Internet Monitor publishes internet measurements to CloudWatch Logs and
-- CloudWatch Metrics, to easily support using CloudWatch tools with health
-- information for geographies and networks specific to your application.
-- Internet Monitor sends health events to Amazon EventBridge so that you
-- can set up notifications. If an issue is caused by the Amazon Web
-- Services network, you also automatically receive an Amazon Web Services
-- Health Dashboard notification with the steps that Amazon Web Services is
-- taking to mitigate the problem.
--
-- To use Internet Monitor, you create a /monitor/ and associate your
-- application\'s resources with it, VPCs, CloudFront distributions, or
-- WorkSpaces directories, to enable Internet Monitor to know where your
-- application\'s internet traffic is. Internet Monitor then provides
-- internet measurements from Amazon Web Services that are specific to the
-- locations and networks that communicate with your application.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-InternetMonitor.html Using Amazon CloudWatch Internet Monitor>
-- in the /Amazon CloudWatch User Guide/.
module Amazonka.InternetMonitor
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateMonitor
    CreateMonitor (CreateMonitor'),
    newCreateMonitor,
    CreateMonitorResponse (CreateMonitorResponse'),
    newCreateMonitorResponse,

    -- ** DeleteMonitor
    DeleteMonitor (DeleteMonitor'),
    newDeleteMonitor,
    DeleteMonitorResponse (DeleteMonitorResponse'),
    newDeleteMonitorResponse,

    -- ** GetHealthEvent
    GetHealthEvent (GetHealthEvent'),
    newGetHealthEvent,
    GetHealthEventResponse (GetHealthEventResponse'),
    newGetHealthEventResponse,

    -- ** GetMonitor
    GetMonitor (GetMonitor'),
    newGetMonitor,
    GetMonitorResponse (GetMonitorResponse'),
    newGetMonitorResponse,

    -- ** ListHealthEvents (Paginated)
    ListHealthEvents (ListHealthEvents'),
    newListHealthEvents,
    ListHealthEventsResponse (ListHealthEventsResponse'),
    newListHealthEventsResponse,

    -- ** ListMonitors (Paginated)
    ListMonitors (ListMonitors'),
    newListMonitors,
    ListMonitorsResponse (ListMonitorsResponse'),
    newListMonitorsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

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

    -- ** UpdateMonitor
    UpdateMonitor (UpdateMonitor'),
    newUpdateMonitor,
    UpdateMonitorResponse (UpdateMonitorResponse'),
    newUpdateMonitorResponse,

    -- * Types

    -- ** HealthEventImpactType
    HealthEventImpactType (..),

    -- ** HealthEventStatus
    HealthEventStatus (..),

    -- ** LogDeliveryStatus
    LogDeliveryStatus (..),

    -- ** MonitorConfigState
    MonitorConfigState (..),

    -- ** MonitorProcessingStatusCode
    MonitorProcessingStatusCode (..),

    -- ** TriangulationEventType
    TriangulationEventType (..),

    -- ** AvailabilityMeasurement
    AvailabilityMeasurement (AvailabilityMeasurement'),
    newAvailabilityMeasurement,

    -- ** HealthEvent
    HealthEvent (HealthEvent'),
    newHealthEvent,

    -- ** ImpactedLocation
    ImpactedLocation (ImpactedLocation'),
    newImpactedLocation,

    -- ** InternetHealth
    InternetHealth (InternetHealth'),
    newInternetHealth,

    -- ** InternetMeasurementsLogDelivery
    InternetMeasurementsLogDelivery (InternetMeasurementsLogDelivery'),
    newInternetMeasurementsLogDelivery,

    -- ** Monitor
    Monitor (Monitor'),
    newMonitor,

    -- ** Network
    Network (Network'),
    newNetwork,

    -- ** NetworkImpairment
    NetworkImpairment (NetworkImpairment'),
    newNetworkImpairment,

    -- ** PerformanceMeasurement
    PerformanceMeasurement (PerformanceMeasurement'),
    newPerformanceMeasurement,

    -- ** RoundTripTime
    RoundTripTime (RoundTripTime'),
    newRoundTripTime,

    -- ** S3Config
    S3Config (S3Config'),
    newS3Config,
  )
where

import Amazonka.InternetMonitor.CreateMonitor
import Amazonka.InternetMonitor.DeleteMonitor
import Amazonka.InternetMonitor.GetHealthEvent
import Amazonka.InternetMonitor.GetMonitor
import Amazonka.InternetMonitor.Lens
import Amazonka.InternetMonitor.ListHealthEvents
import Amazonka.InternetMonitor.ListMonitors
import Amazonka.InternetMonitor.ListTagsForResource
import Amazonka.InternetMonitor.TagResource
import Amazonka.InternetMonitor.Types
import Amazonka.InternetMonitor.UntagResource
import Amazonka.InternetMonitor.UpdateMonitor
import Amazonka.InternetMonitor.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'InternetMonitor'.

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
