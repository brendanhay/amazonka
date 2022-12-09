{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTRoboRunner.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTRoboRunner.Lens
  ( -- * Operations

    -- ** CreateDestination
    createDestination_additionalFixedProperties,
    createDestination_clientToken,
    createDestination_state,
    createDestination_name,
    createDestination_site,
    createDestinationResponse_httpStatus,
    createDestinationResponse_arn,
    createDestinationResponse_id,
    createDestinationResponse_createdAt,
    createDestinationResponse_updatedAt,
    createDestinationResponse_state,

    -- ** CreateSite
    createSite_clientToken,
    createSite_description,
    createSite_name,
    createSite_countryCode,
    createSiteResponse_httpStatus,
    createSiteResponse_arn,
    createSiteResponse_id,
    createSiteResponse_createdAt,
    createSiteResponse_updatedAt,

    -- ** CreateWorker
    createWorker_additionalFixedProperties,
    createWorker_additionalTransientProperties,
    createWorker_clientToken,
    createWorker_orientation,
    createWorker_position,
    createWorker_vendorProperties,
    createWorker_name,
    createWorker_fleet,
    createWorkerResponse_httpStatus,
    createWorkerResponse_arn,
    createWorkerResponse_id,
    createWorkerResponse_createdAt,
    createWorkerResponse_updatedAt,
    createWorkerResponse_site,

    -- ** CreateWorkerFleet
    createWorkerFleet_additionalFixedProperties,
    createWorkerFleet_clientToken,
    createWorkerFleet_name,
    createWorkerFleet_site,
    createWorkerFleetResponse_httpStatus,
    createWorkerFleetResponse_arn,
    createWorkerFleetResponse_id,
    createWorkerFleetResponse_createdAt,
    createWorkerFleetResponse_updatedAt,

    -- ** DeleteDestination
    deleteDestination_id,
    deleteDestinationResponse_httpStatus,

    -- ** DeleteSite
    deleteSite_id,
    deleteSiteResponse_httpStatus,

    -- ** DeleteWorker
    deleteWorker_id,
    deleteWorkerResponse_httpStatus,

    -- ** DeleteWorkerFleet
    deleteWorkerFleet_id,
    deleteWorkerFleetResponse_httpStatus,

    -- ** GetDestination
    getDestination_id,
    getDestinationResponse_additionalFixedProperties,
    getDestinationResponse_httpStatus,
    getDestinationResponse_arn,
    getDestinationResponse_id,
    getDestinationResponse_name,
    getDestinationResponse_site,
    getDestinationResponse_createdAt,
    getDestinationResponse_updatedAt,
    getDestinationResponse_state,

    -- ** GetSite
    getSite_id,
    getSiteResponse_description,
    getSiteResponse_httpStatus,
    getSiteResponse_arn,
    getSiteResponse_id,
    getSiteResponse_name,
    getSiteResponse_countryCode,
    getSiteResponse_createdAt,
    getSiteResponse_updatedAt,

    -- ** GetWorker
    getWorker_id,
    getWorkerResponse_additionalFixedProperties,
    getWorkerResponse_additionalTransientProperties,
    getWorkerResponse_orientation,
    getWorkerResponse_position,
    getWorkerResponse_vendorProperties,
    getWorkerResponse_httpStatus,
    getWorkerResponse_arn,
    getWorkerResponse_id,
    getWorkerResponse_fleet,
    getWorkerResponse_site,
    getWorkerResponse_createdAt,
    getWorkerResponse_updatedAt,
    getWorkerResponse_name,

    -- ** GetWorkerFleet
    getWorkerFleet_id,
    getWorkerFleetResponse_additionalFixedProperties,
    getWorkerFleetResponse_httpStatus,
    getWorkerFleetResponse_id,
    getWorkerFleetResponse_arn,
    getWorkerFleetResponse_name,
    getWorkerFleetResponse_site,
    getWorkerFleetResponse_createdAt,
    getWorkerFleetResponse_updatedAt,

    -- ** ListDestinations
    listDestinations_maxResults,
    listDestinations_nextToken,
    listDestinations_state,
    listDestinations_site,
    listDestinationsResponse_destinations,
    listDestinationsResponse_nextToken,
    listDestinationsResponse_httpStatus,

    -- ** ListSites
    listSites_maxResults,
    listSites_nextToken,
    listSitesResponse_nextToken,
    listSitesResponse_sites,
    listSitesResponse_httpStatus,

    -- ** ListWorkerFleets
    listWorkerFleets_maxResults,
    listWorkerFleets_nextToken,
    listWorkerFleets_site,
    listWorkerFleetsResponse_nextToken,
    listWorkerFleetsResponse_workerFleets,
    listWorkerFleetsResponse_httpStatus,

    -- ** ListWorkers
    listWorkers_fleet,
    listWorkers_maxResults,
    listWorkers_nextToken,
    listWorkers_site,
    listWorkersResponse_nextToken,
    listWorkersResponse_workers,
    listWorkersResponse_httpStatus,

    -- ** UpdateDestination
    updateDestination_additionalFixedProperties,
    updateDestination_name,
    updateDestination_state,
    updateDestination_id,
    updateDestinationResponse_additionalFixedProperties,
    updateDestinationResponse_httpStatus,
    updateDestinationResponse_arn,
    updateDestinationResponse_id,
    updateDestinationResponse_name,
    updateDestinationResponse_updatedAt,
    updateDestinationResponse_state,

    -- ** UpdateSite
    updateSite_countryCode,
    updateSite_description,
    updateSite_name,
    updateSite_id,
    updateSiteResponse_countryCode,
    updateSiteResponse_description,
    updateSiteResponse_httpStatus,
    updateSiteResponse_arn,
    updateSiteResponse_id,
    updateSiteResponse_name,
    updateSiteResponse_updatedAt,

    -- ** UpdateWorker
    updateWorker_additionalFixedProperties,
    updateWorker_additionalTransientProperties,
    updateWorker_name,
    updateWorker_orientation,
    updateWorker_position,
    updateWorker_vendorProperties,
    updateWorker_id,
    updateWorkerResponse_additionalFixedProperties,
    updateWorkerResponse_additionalTransientProperties,
    updateWorkerResponse_orientation,
    updateWorkerResponse_position,
    updateWorkerResponse_vendorProperties,
    updateWorkerResponse_httpStatus,
    updateWorkerResponse_arn,
    updateWorkerResponse_id,
    updateWorkerResponse_fleet,
    updateWorkerResponse_updatedAt,
    updateWorkerResponse_name,

    -- ** UpdateWorkerFleet
    updateWorkerFleet_additionalFixedProperties,
    updateWorkerFleet_name,
    updateWorkerFleet_id,
    updateWorkerFleetResponse_additionalFixedProperties,
    updateWorkerFleetResponse_httpStatus,
    updateWorkerFleetResponse_arn,
    updateWorkerFleetResponse_id,
    updateWorkerFleetResponse_name,
    updateWorkerFleetResponse_updatedAt,

    -- * Types

    -- ** CartesianCoordinates
    cartesianCoordinates_z,
    cartesianCoordinates_x,
    cartesianCoordinates_y,

    -- ** Destination
    destination_additionalFixedProperties,
    destination_arn,
    destination_id,
    destination_name,
    destination_site,
    destination_createdAt,
    destination_updatedAt,
    destination_state,

    -- ** Orientation
    orientation_degrees,

    -- ** PositionCoordinates
    positionCoordinates_cartesianCoordinates,

    -- ** Site
    site_arn,
    site_name,
    site_countryCode,
    site_createdAt,

    -- ** VendorProperties
    vendorProperties_vendorAdditionalFixedProperties,
    vendorProperties_vendorAdditionalTransientProperties,
    vendorProperties_vendorWorkerIpAddress,
    vendorProperties_vendorWorkerId,

    -- ** Worker
    worker_additionalFixedProperties,
    worker_additionalTransientProperties,
    worker_orientation,
    worker_position,
    worker_vendorProperties,
    worker_arn,
    worker_id,
    worker_fleet,
    worker_createdAt,
    worker_updatedAt,
    worker_name,
    worker_site,

    -- ** WorkerFleet
    workerFleet_additionalFixedProperties,
    workerFleet_arn,
    workerFleet_id,
    workerFleet_name,
    workerFleet_site,
    workerFleet_createdAt,
    workerFleet_updatedAt,
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
import Amazonka.IoTRoboRunner.ListDestinations
import Amazonka.IoTRoboRunner.ListSites
import Amazonka.IoTRoboRunner.ListWorkerFleets
import Amazonka.IoTRoboRunner.ListWorkers
import Amazonka.IoTRoboRunner.Types.CartesianCoordinates
import Amazonka.IoTRoboRunner.Types.Destination
import Amazonka.IoTRoboRunner.Types.Orientation
import Amazonka.IoTRoboRunner.Types.PositionCoordinates
import Amazonka.IoTRoboRunner.Types.Site
import Amazonka.IoTRoboRunner.Types.VendorProperties
import Amazonka.IoTRoboRunner.Types.Worker
import Amazonka.IoTRoboRunner.Types.WorkerFleet
import Amazonka.IoTRoboRunner.UpdateDestination
import Amazonka.IoTRoboRunner.UpdateSite
import Amazonka.IoTRoboRunner.UpdateWorker
import Amazonka.IoTRoboRunner.UpdateWorkerFleet
