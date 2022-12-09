{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MigrationHub
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-05-31@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The AWS Migration Hub API methods help to obtain server and application
-- migration status and integrate your resource-specific migration tool by
-- providing a programmatic interface to Migration Hub.
--
-- Remember that you must set your AWS Migration Hub home region before you
-- call any of these APIs, or a @HomeRegionNotSetException@ error will be
-- returned. Also, you must make the API calls while in your home region.
module Amazonka.MigrationHub
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** DryRunOperation
    _DryRunOperation,

    -- ** HomeRegionNotSetException
    _HomeRegionNotSetException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** PolicyErrorException
    _PolicyErrorException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** UnauthorizedOperation
    _UnauthorizedOperation,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateCreatedArtifact
    AssociateCreatedArtifact (AssociateCreatedArtifact'),
    newAssociateCreatedArtifact,
    AssociateCreatedArtifactResponse (AssociateCreatedArtifactResponse'),
    newAssociateCreatedArtifactResponse,

    -- ** AssociateDiscoveredResource
    AssociateDiscoveredResource (AssociateDiscoveredResource'),
    newAssociateDiscoveredResource,
    AssociateDiscoveredResourceResponse (AssociateDiscoveredResourceResponse'),
    newAssociateDiscoveredResourceResponse,

    -- ** CreateProgressUpdateStream
    CreateProgressUpdateStream (CreateProgressUpdateStream'),
    newCreateProgressUpdateStream,
    CreateProgressUpdateStreamResponse (CreateProgressUpdateStreamResponse'),
    newCreateProgressUpdateStreamResponse,

    -- ** DeleteProgressUpdateStream
    DeleteProgressUpdateStream (DeleteProgressUpdateStream'),
    newDeleteProgressUpdateStream,
    DeleteProgressUpdateStreamResponse (DeleteProgressUpdateStreamResponse'),
    newDeleteProgressUpdateStreamResponse,

    -- ** DescribeApplicationState
    DescribeApplicationState (DescribeApplicationState'),
    newDescribeApplicationState,
    DescribeApplicationStateResponse (DescribeApplicationStateResponse'),
    newDescribeApplicationStateResponse,

    -- ** DescribeMigrationTask
    DescribeMigrationTask (DescribeMigrationTask'),
    newDescribeMigrationTask,
    DescribeMigrationTaskResponse (DescribeMigrationTaskResponse'),
    newDescribeMigrationTaskResponse,

    -- ** DisassociateCreatedArtifact
    DisassociateCreatedArtifact (DisassociateCreatedArtifact'),
    newDisassociateCreatedArtifact,
    DisassociateCreatedArtifactResponse (DisassociateCreatedArtifactResponse'),
    newDisassociateCreatedArtifactResponse,

    -- ** DisassociateDiscoveredResource
    DisassociateDiscoveredResource (DisassociateDiscoveredResource'),
    newDisassociateDiscoveredResource,
    DisassociateDiscoveredResourceResponse (DisassociateDiscoveredResourceResponse'),
    newDisassociateDiscoveredResourceResponse,

    -- ** ImportMigrationTask
    ImportMigrationTask (ImportMigrationTask'),
    newImportMigrationTask,
    ImportMigrationTaskResponse (ImportMigrationTaskResponse'),
    newImportMigrationTaskResponse,

    -- ** ListApplicationStates (Paginated)
    ListApplicationStates (ListApplicationStates'),
    newListApplicationStates,
    ListApplicationStatesResponse (ListApplicationStatesResponse'),
    newListApplicationStatesResponse,

    -- ** ListCreatedArtifacts (Paginated)
    ListCreatedArtifacts (ListCreatedArtifacts'),
    newListCreatedArtifacts,
    ListCreatedArtifactsResponse (ListCreatedArtifactsResponse'),
    newListCreatedArtifactsResponse,

    -- ** ListDiscoveredResources (Paginated)
    ListDiscoveredResources (ListDiscoveredResources'),
    newListDiscoveredResources,
    ListDiscoveredResourcesResponse (ListDiscoveredResourcesResponse'),
    newListDiscoveredResourcesResponse,

    -- ** ListMigrationTasks (Paginated)
    ListMigrationTasks (ListMigrationTasks'),
    newListMigrationTasks,
    ListMigrationTasksResponse (ListMigrationTasksResponse'),
    newListMigrationTasksResponse,

    -- ** ListProgressUpdateStreams (Paginated)
    ListProgressUpdateStreams (ListProgressUpdateStreams'),
    newListProgressUpdateStreams,
    ListProgressUpdateStreamsResponse (ListProgressUpdateStreamsResponse'),
    newListProgressUpdateStreamsResponse,

    -- ** NotifyApplicationState
    NotifyApplicationState (NotifyApplicationState'),
    newNotifyApplicationState,
    NotifyApplicationStateResponse (NotifyApplicationStateResponse'),
    newNotifyApplicationStateResponse,

    -- ** NotifyMigrationTaskState
    NotifyMigrationTaskState (NotifyMigrationTaskState'),
    newNotifyMigrationTaskState,
    NotifyMigrationTaskStateResponse (NotifyMigrationTaskStateResponse'),
    newNotifyMigrationTaskStateResponse,

    -- ** PutResourceAttributes
    PutResourceAttributes (PutResourceAttributes'),
    newPutResourceAttributes,
    PutResourceAttributesResponse (PutResourceAttributesResponse'),
    newPutResourceAttributesResponse,

    -- * Types

    -- ** ApplicationStatus
    ApplicationStatus (..),

    -- ** MigrationStatus
    MigrationStatus (..),

    -- ** ResourceAttributeType
    ResourceAttributeType (..),

    -- ** ApplicationState
    ApplicationState (ApplicationState'),
    newApplicationState,

    -- ** CreatedArtifact
    CreatedArtifact (CreatedArtifact'),
    newCreatedArtifact,

    -- ** DiscoveredResource
    DiscoveredResource (DiscoveredResource'),
    newDiscoveredResource,

    -- ** MigrationTask
    MigrationTask (MigrationTask'),
    newMigrationTask,

    -- ** MigrationTaskSummary
    MigrationTaskSummary (MigrationTaskSummary'),
    newMigrationTaskSummary,

    -- ** ProgressUpdateStreamSummary
    ProgressUpdateStreamSummary (ProgressUpdateStreamSummary'),
    newProgressUpdateStreamSummary,

    -- ** ResourceAttribute
    ResourceAttribute (ResourceAttribute'),
    newResourceAttribute,

    -- ** Task
    Task (Task'),
    newTask,
  )
where

import Amazonka.MigrationHub.AssociateCreatedArtifact
import Amazonka.MigrationHub.AssociateDiscoveredResource
import Amazonka.MigrationHub.CreateProgressUpdateStream
import Amazonka.MigrationHub.DeleteProgressUpdateStream
import Amazonka.MigrationHub.DescribeApplicationState
import Amazonka.MigrationHub.DescribeMigrationTask
import Amazonka.MigrationHub.DisassociateCreatedArtifact
import Amazonka.MigrationHub.DisassociateDiscoveredResource
import Amazonka.MigrationHub.ImportMigrationTask
import Amazonka.MigrationHub.Lens
import Amazonka.MigrationHub.ListApplicationStates
import Amazonka.MigrationHub.ListCreatedArtifacts
import Amazonka.MigrationHub.ListDiscoveredResources
import Amazonka.MigrationHub.ListMigrationTasks
import Amazonka.MigrationHub.ListProgressUpdateStreams
import Amazonka.MigrationHub.NotifyApplicationState
import Amazonka.MigrationHub.NotifyMigrationTaskState
import Amazonka.MigrationHub.PutResourceAttributes
import Amazonka.MigrationHub.Types
import Amazonka.MigrationHub.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MigrationHub'.

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
