{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHub.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    describeApplicationStateResponse_lastUpdatedTime,
    describeApplicationStateResponse_applicationStatus,
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
    listApplicationStates_nextToken,
    listApplicationStates_applicationIds,
    listApplicationStates_maxResults,
    listApplicationStatesResponse_nextToken,
    listApplicationStatesResponse_applicationStateList,
    listApplicationStatesResponse_httpStatus,

    -- ** ListCreatedArtifacts
    listCreatedArtifacts_nextToken,
    listCreatedArtifacts_maxResults,
    listCreatedArtifacts_progressUpdateStream,
    listCreatedArtifacts_migrationTaskName,
    listCreatedArtifactsResponse_nextToken,
    listCreatedArtifactsResponse_createdArtifactList,
    listCreatedArtifactsResponse_httpStatus,

    -- ** ListDiscoveredResources
    listDiscoveredResources_nextToken,
    listDiscoveredResources_maxResults,
    listDiscoveredResources_progressUpdateStream,
    listDiscoveredResources_migrationTaskName,
    listDiscoveredResourcesResponse_nextToken,
    listDiscoveredResourcesResponse_discoveredResourceList,
    listDiscoveredResourcesResponse_httpStatus,

    -- ** ListMigrationTasks
    listMigrationTasks_nextToken,
    listMigrationTasks_resourceName,
    listMigrationTasks_maxResults,
    listMigrationTasksResponse_nextToken,
    listMigrationTasksResponse_migrationTaskSummaryList,
    listMigrationTasksResponse_httpStatus,

    -- ** ListProgressUpdateStreams
    listProgressUpdateStreams_nextToken,
    listProgressUpdateStreams_maxResults,
    listProgressUpdateStreamsResponse_nextToken,
    listProgressUpdateStreamsResponse_progressUpdateStreamSummaryList,
    listProgressUpdateStreamsResponse_httpStatus,

    -- ** NotifyApplicationState
    notifyApplicationState_updateDateTime,
    notifyApplicationState_dryRun,
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
    applicationState_lastUpdatedTime,
    applicationState_applicationStatus,
    applicationState_applicationId,

    -- ** CreatedArtifact
    createdArtifact_description,
    createdArtifact_name,

    -- ** DiscoveredResource
    discoveredResource_description,
    discoveredResource_configurationId,

    -- ** MigrationTask
    migrationTask_updateDateTime,
    migrationTask_task,
    migrationTask_migrationTaskName,
    migrationTask_progressUpdateStream,
    migrationTask_resourceAttributeList,

    -- ** MigrationTaskSummary
    migrationTaskSummary_updateDateTime,
    migrationTaskSummary_migrationTaskName,
    migrationTaskSummary_statusDetail,
    migrationTaskSummary_status,
    migrationTaskSummary_progressUpdateStream,
    migrationTaskSummary_progressPercent,

    -- ** ProgressUpdateStreamSummary
    progressUpdateStreamSummary_progressUpdateStreamName,

    -- ** ResourceAttribute
    resourceAttribute_type,
    resourceAttribute_value,

    -- ** Task
    task_statusDetail,
    task_progressPercent,
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
