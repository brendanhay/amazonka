{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHub.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHub.Lens
  ( -- * Operations

    -- ** AssociateCreatedArtifact
    associateCreatedArtifact_dryRun,
    associateCreatedArtifact_progressUpdateStream,
    associateCreatedArtifact_migrationTaskName,
    associateCreatedArtifact_createdArtifact,
    associateCreatedArtifactResponse_httpStatus,

    -- ** AssociateDiscoveredResource
    associateDiscoveredResource_dryRun,
    associateDiscoveredResource_progressUpdateStream,
    associateDiscoveredResource_migrationTaskName,
    associateDiscoveredResource_discoveredResource,
    associateDiscoveredResourceResponse_httpStatus,

    -- ** CreateProgressUpdateStream
    createProgressUpdateStream_dryRun,
    createProgressUpdateStream_progressUpdateStreamName,
    createProgressUpdateStreamResponse_httpStatus,

    -- ** DeleteProgressUpdateStream
    deleteProgressUpdateStream_dryRun,
    deleteProgressUpdateStream_progressUpdateStreamName,
    deleteProgressUpdateStreamResponse_httpStatus,

    -- ** DescribeApplicationState
    describeApplicationState_applicationId,
    describeApplicationStateResponse_applicationStatus,
    describeApplicationStateResponse_lastUpdatedTime,
    describeApplicationStateResponse_httpStatus,

    -- ** DescribeMigrationTask
    describeMigrationTask_progressUpdateStream,
    describeMigrationTask_migrationTaskName,
    describeMigrationTaskResponse_migrationTask,
    describeMigrationTaskResponse_httpStatus,

    -- ** DisassociateCreatedArtifact
    disassociateCreatedArtifact_dryRun,
    disassociateCreatedArtifact_progressUpdateStream,
    disassociateCreatedArtifact_migrationTaskName,
    disassociateCreatedArtifact_createdArtifactName,
    disassociateCreatedArtifactResponse_httpStatus,

    -- ** DisassociateDiscoveredResource
    disassociateDiscoveredResource_dryRun,
    disassociateDiscoveredResource_progressUpdateStream,
    disassociateDiscoveredResource_migrationTaskName,
    disassociateDiscoveredResource_configurationId,
    disassociateDiscoveredResourceResponse_httpStatus,

    -- ** ImportMigrationTask
    importMigrationTask_dryRun,
    importMigrationTask_progressUpdateStream,
    importMigrationTask_migrationTaskName,
    importMigrationTaskResponse_httpStatus,

    -- ** ListApplicationStates
    listApplicationStates_applicationIds,
    listApplicationStates_maxResults,
    listApplicationStates_nextToken,
    listApplicationStatesResponse_applicationStateList,
    listApplicationStatesResponse_nextToken,
    listApplicationStatesResponse_httpStatus,

    -- ** ListCreatedArtifacts
    listCreatedArtifacts_maxResults,
    listCreatedArtifacts_nextToken,
    listCreatedArtifacts_progressUpdateStream,
    listCreatedArtifacts_migrationTaskName,
    listCreatedArtifactsResponse_createdArtifactList,
    listCreatedArtifactsResponse_nextToken,
    listCreatedArtifactsResponse_httpStatus,

    -- ** ListDiscoveredResources
    listDiscoveredResources_maxResults,
    listDiscoveredResources_nextToken,
    listDiscoveredResources_progressUpdateStream,
    listDiscoveredResources_migrationTaskName,
    listDiscoveredResourcesResponse_discoveredResourceList,
    listDiscoveredResourcesResponse_nextToken,
    listDiscoveredResourcesResponse_httpStatus,

    -- ** ListMigrationTasks
    listMigrationTasks_maxResults,
    listMigrationTasks_nextToken,
    listMigrationTasks_resourceName,
    listMigrationTasksResponse_migrationTaskSummaryList,
    listMigrationTasksResponse_nextToken,
    listMigrationTasksResponse_httpStatus,

    -- ** ListProgressUpdateStreams
    listProgressUpdateStreams_maxResults,
    listProgressUpdateStreams_nextToken,
    listProgressUpdateStreamsResponse_nextToken,
    listProgressUpdateStreamsResponse_progressUpdateStreamSummaryList,
    listProgressUpdateStreamsResponse_httpStatus,

    -- ** NotifyApplicationState
    notifyApplicationState_dryRun,
    notifyApplicationState_updateDateTime,
    notifyApplicationState_applicationId,
    notifyApplicationState_status,
    notifyApplicationStateResponse_httpStatus,

    -- ** NotifyMigrationTaskState
    notifyMigrationTaskState_dryRun,
    notifyMigrationTaskState_progressUpdateStream,
    notifyMigrationTaskState_migrationTaskName,
    notifyMigrationTaskState_task,
    notifyMigrationTaskState_updateDateTime,
    notifyMigrationTaskState_nextUpdateSeconds,
    notifyMigrationTaskStateResponse_httpStatus,

    -- ** PutResourceAttributes
    putResourceAttributes_dryRun,
    putResourceAttributes_progressUpdateStream,
    putResourceAttributes_migrationTaskName,
    putResourceAttributes_resourceAttributeList,
    putResourceAttributesResponse_httpStatus,

    -- * Types

    -- ** ApplicationState
    applicationState_applicationId,
    applicationState_applicationStatus,
    applicationState_lastUpdatedTime,

    -- ** CreatedArtifact
    createdArtifact_description,
    createdArtifact_name,

    -- ** DiscoveredResource
    discoveredResource_description,
    discoveredResource_configurationId,

    -- ** MigrationTask
    migrationTask_migrationTaskName,
    migrationTask_progressUpdateStream,
    migrationTask_resourceAttributeList,
    migrationTask_task,
    migrationTask_updateDateTime,

    -- ** MigrationTaskSummary
    migrationTaskSummary_migrationTaskName,
    migrationTaskSummary_progressPercent,
    migrationTaskSummary_progressUpdateStream,
    migrationTaskSummary_status,
    migrationTaskSummary_statusDetail,
    migrationTaskSummary_updateDateTime,

    -- ** ProgressUpdateStreamSummary
    progressUpdateStreamSummary_progressUpdateStreamName,

    -- ** ResourceAttribute
    resourceAttribute_type,
    resourceAttribute_value,

    -- ** Task
    task_progressPercent,
    task_statusDetail,
    task_status,
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
import Amazonka.MigrationHub.ListApplicationStates
import Amazonka.MigrationHub.ListCreatedArtifacts
import Amazonka.MigrationHub.ListDiscoveredResources
import Amazonka.MigrationHub.ListMigrationTasks
import Amazonka.MigrationHub.ListProgressUpdateStreams
import Amazonka.MigrationHub.NotifyApplicationState
import Amazonka.MigrationHub.NotifyMigrationTaskState
import Amazonka.MigrationHub.PutResourceAttributes
import Amazonka.MigrationHub.Types.ApplicationState
import Amazonka.MigrationHub.Types.CreatedArtifact
import Amazonka.MigrationHub.Types.DiscoveredResource
import Amazonka.MigrationHub.Types.MigrationTask
import Amazonka.MigrationHub.Types.MigrationTaskSummary
import Amazonka.MigrationHub.Types.ProgressUpdateStreamSummary
import Amazonka.MigrationHub.Types.ResourceAttribute
import Amazonka.MigrationHub.Types.Task
