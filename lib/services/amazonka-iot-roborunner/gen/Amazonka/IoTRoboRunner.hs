{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IoTRoboRunner
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- An example service, deployed with the Octane Service creator, which will
-- echo the string
module Amazonka.IoTRoboRunner
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ConflictException
    _ConflictException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateDestination
    CreateDestination (CreateDestination'),
    newCreateDestination,
    CreateDestinationResponse (CreateDestinationResponse'),
    newCreateDestinationResponse,

    -- ** CreateSite
    CreateSite (CreateSite'),
    newCreateSite,
    CreateSiteResponse (CreateSiteResponse'),
    newCreateSiteResponse,

    -- ** CreateWorker
    CreateWorker (CreateWorker'),
    newCreateWorker,
    CreateWorkerResponse (CreateWorkerResponse'),
    newCreateWorkerResponse,

    -- ** CreateWorkerFleet
    CreateWorkerFleet (CreateWorkerFleet'),
    newCreateWorkerFleet,
    CreateWorkerFleetResponse (CreateWorkerFleetResponse'),
    newCreateWorkerFleetResponse,

    -- ** DeleteDestination
    DeleteDestination (DeleteDestination'),
    newDeleteDestination,
    DeleteDestinationResponse (DeleteDestinationResponse'),
    newDeleteDestinationResponse,

    -- ** DeleteSite
    DeleteSite (DeleteSite'),
    newDeleteSite,
    DeleteSiteResponse (DeleteSiteResponse'),
    newDeleteSiteResponse,

    -- ** DeleteWorker
    DeleteWorker (DeleteWorker'),
    newDeleteWorker,
    DeleteWorkerResponse (DeleteWorkerResponse'),
    newDeleteWorkerResponse,

    -- ** DeleteWorkerFleet
    DeleteWorkerFleet (DeleteWorkerFleet'),
    newDeleteWorkerFleet,
    DeleteWorkerFleetResponse (DeleteWorkerFleetResponse'),
    newDeleteWorkerFleetResponse,

    -- ** GetDestination
    GetDestination (GetDestination'),
    newGetDestination,
    GetDestinationResponse (GetDestinationResponse'),
    newGetDestinationResponse,

    -- ** GetSite
    GetSite (GetSite'),
    newGetSite,
    GetSiteResponse (GetSiteResponse'),
    newGetSiteResponse,

    -- ** GetWorker
    GetWorker (GetWorker'),
    newGetWorker,
    GetWorkerResponse (GetWorkerResponse'),
    newGetWorkerResponse,

    -- ** GetWorkerFleet
    GetWorkerFleet (GetWorkerFleet'),
    newGetWorkerFleet,
    GetWorkerFleetResponse (GetWorkerFleetResponse'),
    newGetWorkerFleetResponse,

    -- ** ListDestinations (Paginated)
    ListDestinations (ListDestinations'),
    newListDestinations,
    ListDestinationsResponse (ListDestinationsResponse'),
    newListDestinationsResponse,

    -- ** ListSites (Paginated)
    ListSites (ListSites'),
    newListSites,
    ListSitesResponse (ListSitesResponse'),
    newListSitesResponse,

    -- ** ListWorkerFleets (Paginated)
    ListWorkerFleets (ListWorkerFleets'),
    newListWorkerFleets,
    ListWorkerFleetsResponse (ListWorkerFleetsResponse'),
    newListWorkerFleetsResponse,

    -- ** ListWorkers (Paginated)
    ListWorkers (ListWorkers'),
    newListWorkers,
    ListWorkersResponse (ListWorkersResponse'),
    newListWorkersResponse,

    -- ** UpdateDestination
    UpdateDestination (UpdateDestination'),
    newUpdateDestination,
    UpdateDestinationResponse (UpdateDestinationResponse'),
    newUpdateDestinationResponse,

    -- ** UpdateSite
    UpdateSite (UpdateSite'),
    newUpdateSite,
    UpdateSiteResponse (UpdateSiteResponse'),
    newUpdateSiteResponse,

    -- ** UpdateWorker
    UpdateWorker (UpdateWorker'),
    newUpdateWorker,
    UpdateWorkerResponse (UpdateWorkerResponse'),
    newUpdateWorkerResponse,

    -- ** UpdateWorkerFleet
    UpdateWorkerFleet (UpdateWorkerFleet'),
    newUpdateWorkerFleet,
    UpdateWorkerFleetResponse (UpdateWorkerFleetResponse'),
    newUpdateWorkerFleetResponse,

    -- * Types

    -- ** DestinationState
    DestinationState (..),

    -- ** CartesianCoordinates
    CartesianCoordinates (CartesianCoordinates'),
    newCartesianCoordinates,

    -- ** Destination
    Destination (Destination'),
    newDestination,

    -- ** Orientation
    Orientation (Orientation'),
    newOrientation,

    -- ** PositionCoordinates
    PositionCoordinates (PositionCoordinates'),
    newPositionCoordinates,

    -- ** Site
    Site (Site'),
    newSite,

    -- ** VendorProperties
    VendorProperties (VendorProperties'),
    newVendorProperties,

    -- ** Worker
    Worker (Worker'),
    newWorker,

    -- ** WorkerFleet
    WorkerFleet (WorkerFleet'),
    newWorkerFleet,
  )
where

import Amazonka.IoTRoboRunner.CreateDestination
import Amazonka.IoTRoboRunner.CreateSite
import Amazonka.IoTRoboRunner.CreateWorker
import Amazonka.IoTRoboRunner.CreateWorkerFleet
import Amazonka.IoTRoboRunner.DeleteDestination
import Amazonka.IoTRoboRunner.DeleteSite
import Amazonka.IoTRoboRunner.DeleteWorker
import Amazonka.IoTRoboRunner.DeleteWorkerFleet
import Amazonka.IoTRoboRunner.GetDestination
import Amazonka.IoTRoboRunner.GetSite
import Amazonka.IoTRoboRunner.GetWorker
import Amazonka.IoTRoboRunner.GetWorkerFleet
import Amazonka.IoTRoboRunner.Lens
import Amazonka.IoTRoboRunner.ListDestinations
import Amazonka.IoTRoboRunner.ListSites
import Amazonka.IoTRoboRunner.ListWorkerFleets
import Amazonka.IoTRoboRunner.ListWorkers
import Amazonka.IoTRoboRunner.Types
import Amazonka.IoTRoboRunner.UpdateDestination
import Amazonka.IoTRoboRunner.UpdateSite
import Amazonka.IoTRoboRunner.UpdateWorker
import Amazonka.IoTRoboRunner.UpdateWorkerFleet
import Amazonka.IoTRoboRunner.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoTRoboRunner'.

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
