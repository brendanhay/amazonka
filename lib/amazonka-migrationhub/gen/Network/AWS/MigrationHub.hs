{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The AWS Migration Hub API methods help to obtain server and application migration status and integrate your resource-specific migration tool by providing a programmatic interface to Migration Hub.
--
-- Remember that you must set your AWS Migration Hub home region before you call any of these APIs, or a @HomeRegionNotSetException@ error will be returned. Also, you must make the API calls while in your home region.
module Network.AWS.MigrationHub
  ( -- * Service configuration
    migrationHubService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateDiscoveredResource
    module Network.AWS.MigrationHub.AssociateDiscoveredResource,

    -- ** CreateProgressUpdateStream
    module Network.AWS.MigrationHub.CreateProgressUpdateStream,

    -- ** ListApplicationStates (Paginated)
    module Network.AWS.MigrationHub.ListApplicationStates,

    -- ** ListCreatedArtifacts (Paginated)
    module Network.AWS.MigrationHub.ListCreatedArtifacts,

    -- ** DisassociateDiscoveredResource
    module Network.AWS.MigrationHub.DisassociateDiscoveredResource,

    -- ** NotifyApplicationState
    module Network.AWS.MigrationHub.NotifyApplicationState,

    -- ** DeleteProgressUpdateStream
    module Network.AWS.MigrationHub.DeleteProgressUpdateStream,

    -- ** ListProgressUpdateStreams (Paginated)
    module Network.AWS.MigrationHub.ListProgressUpdateStreams,

    -- ** DisassociateCreatedArtifact
    module Network.AWS.MigrationHub.DisassociateCreatedArtifact,

    -- ** ImportMigrationTask
    module Network.AWS.MigrationHub.ImportMigrationTask,

    -- ** DescribeMigrationTask
    module Network.AWS.MigrationHub.DescribeMigrationTask,

    -- ** PutResourceAttributes
    module Network.AWS.MigrationHub.PutResourceAttributes,

    -- ** AssociateCreatedArtifact
    module Network.AWS.MigrationHub.AssociateCreatedArtifact,

    -- ** NotifyMigrationTaskState
    module Network.AWS.MigrationHub.NotifyMigrationTaskState,

    -- ** DescribeApplicationState
    module Network.AWS.MigrationHub.DescribeApplicationState,

    -- ** ListMigrationTasks (Paginated)
    module Network.AWS.MigrationHub.ListMigrationTasks,

    -- ** ListDiscoveredResources (Paginated)
    module Network.AWS.MigrationHub.ListDiscoveredResources,

    -- * Types

    -- ** ApplicationStatus
    ApplicationStatus (..),

    -- ** MigrationStatus
    MigrationStatus (..),

    -- ** ResourceAttributeType
    ResourceAttributeType (..),

    -- ** ApplicationState
    ApplicationState (..),
    mkApplicationState,
    asLastUpdatedTime,
    asApplicationId,
    asApplicationStatus,

    -- ** CreatedArtifact
    CreatedArtifact (..),
    mkCreatedArtifact,
    caDescription,
    caName,

    -- ** DiscoveredResource
    DiscoveredResource (..),
    mkDiscoveredResource,
    drDescription,
    drConfigurationId,

    -- ** MigrationTask
    MigrationTask (..),
    mkMigrationTask,
    mtUpdateDateTime,
    mtResourceAttributeList,
    mtTask,
    mtProgressUpdateStream,
    mtMigrationTaskName,

    -- ** MigrationTaskSummary
    MigrationTaskSummary (..),
    mkMigrationTaskSummary,
    mtsStatus,
    mtsUpdateDateTime,
    mtsProgressPercent,
    mtsStatusDetail,
    mtsProgressUpdateStream,
    mtsMigrationTaskName,

    -- ** ProgressUpdateStreamSummary
    ProgressUpdateStreamSummary (..),
    mkProgressUpdateStreamSummary,
    pussProgressUpdateStreamName,

    -- ** ResourceAttribute
    ResourceAttribute (..),
    mkResourceAttribute,
    raType,
    raValue,

    -- ** Task
    Task (..),
    mkTask,
    tProgressPercent,
    tStatusDetail,
    tStatus,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
  )
where

import Network.AWS.MigrationHub.AssociateCreatedArtifact
import Network.AWS.MigrationHub.AssociateDiscoveredResource
import Network.AWS.MigrationHub.CreateProgressUpdateStream
import Network.AWS.MigrationHub.DeleteProgressUpdateStream
import Network.AWS.MigrationHub.DescribeApplicationState
import Network.AWS.MigrationHub.DescribeMigrationTask
import Network.AWS.MigrationHub.DisassociateCreatedArtifact
import Network.AWS.MigrationHub.DisassociateDiscoveredResource
import Network.AWS.MigrationHub.ImportMigrationTask
import Network.AWS.MigrationHub.ListApplicationStates
import Network.AWS.MigrationHub.ListCreatedArtifacts
import Network.AWS.MigrationHub.ListDiscoveredResources
import Network.AWS.MigrationHub.ListMigrationTasks
import Network.AWS.MigrationHub.ListProgressUpdateStreams
import Network.AWS.MigrationHub.NotifyApplicationState
import Network.AWS.MigrationHub.NotifyMigrationTaskState
import Network.AWS.MigrationHub.PutResourceAttributes
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Waiters
import qualified Network.AWS.Prelude as Lude

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
