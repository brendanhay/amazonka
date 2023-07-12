{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GamesParks.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Lens
  ( -- * Operations

    -- ** CreateGame
    createGame_clientToken,
    createGame_description,
    createGame_tags,
    createGame_gameName,
    createGameResponse_game,
    createGameResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_description,
    createSnapshot_gameName,
    createSnapshotResponse_snapshot,
    createSnapshotResponse_httpStatus,

    -- ** CreateStage
    createStage_clientToken,
    createStage_description,
    createStage_tags,
    createStage_gameName,
    createStage_role,
    createStage_stageName,
    createStageResponse_stage,
    createStageResponse_httpStatus,

    -- ** DeleteGame
    deleteGame_gameName,
    deleteGameResponse_httpStatus,

    -- ** DeleteStage
    deleteStage_gameName,
    deleteStage_stageName,
    deleteStageResponse_httpStatus,

    -- ** DisconnectPlayer
    disconnectPlayer_gameName,
    disconnectPlayer_playerId,
    disconnectPlayer_stageName,
    disconnectPlayerResponse_disconnectFailures,
    disconnectPlayerResponse_disconnectSuccesses,
    disconnectPlayerResponse_httpStatus,

    -- ** ExportSnapshot
    exportSnapshot_gameName,
    exportSnapshot_snapshotId,
    exportSnapshotResponse_s3Url,
    exportSnapshotResponse_httpStatus,

    -- ** GetExtension
    getExtension_name,
    getExtension_namespace,
    getExtensionResponse_extension,
    getExtensionResponse_httpStatus,

    -- ** GetExtensionVersion
    getExtensionVersion_extensionVersion,
    getExtensionVersion_name,
    getExtensionVersion_namespace,
    getExtensionVersionResponse_extensionVersion,
    getExtensionVersionResponse_httpStatus,

    -- ** GetGame
    getGame_gameName,
    getGameResponse_game,
    getGameResponse_httpStatus,

    -- ** GetGameConfiguration
    getGameConfiguration_sections,
    getGameConfiguration_gameName,
    getGameConfigurationResponse_gameConfiguration,
    getGameConfigurationResponse_httpStatus,

    -- ** GetGeneratedCodeJob
    getGeneratedCodeJob_gameName,
    getGeneratedCodeJob_jobId,
    getGeneratedCodeJob_snapshotId,
    getGeneratedCodeJobResponse_generatedCodeJob,
    getGeneratedCodeJobResponse_httpStatus,

    -- ** GetPlayerConnectionStatus
    getPlayerConnectionStatus_gameName,
    getPlayerConnectionStatus_playerId,
    getPlayerConnectionStatus_stageName,
    getPlayerConnectionStatusResponse_connections,
    getPlayerConnectionStatusResponse_httpStatus,

    -- ** GetSnapshot
    getSnapshot_sections,
    getSnapshot_gameName,
    getSnapshot_snapshotId,
    getSnapshotResponse_snapshot,
    getSnapshotResponse_httpStatus,

    -- ** GetStage
    getStage_gameName,
    getStage_stageName,
    getStageResponse_stage,
    getStageResponse_httpStatus,

    -- ** GetStageDeployment
    getStageDeployment_deploymentId,
    getStageDeployment_gameName,
    getStageDeployment_stageName,
    getStageDeploymentResponse_stageDeployment,
    getStageDeploymentResponse_httpStatus,

    -- ** ImportGameConfiguration
    importGameConfiguration_gameName,
    importGameConfiguration_importSource,
    importGameConfigurationResponse_gameConfiguration,
    importGameConfigurationResponse_httpStatus,

    -- ** ListExtensionVersions
    listExtensionVersions_maxResults,
    listExtensionVersions_nextToken,
    listExtensionVersions_name,
    listExtensionVersions_namespace,
    listExtensionVersionsResponse_extensionVersions,
    listExtensionVersionsResponse_nextToken,
    listExtensionVersionsResponse_httpStatus,

    -- ** ListExtensions
    listExtensions_maxResults,
    listExtensions_nextToken,
    listExtensionsResponse_extensions,
    listExtensionsResponse_nextToken,
    listExtensionsResponse_httpStatus,

    -- ** ListGames
    listGames_maxResults,
    listGames_nextToken,
    listGamesResponse_games,
    listGamesResponse_nextToken,
    listGamesResponse_httpStatus,

    -- ** ListGeneratedCodeJobs
    listGeneratedCodeJobs_maxResults,
    listGeneratedCodeJobs_nextToken,
    listGeneratedCodeJobs_gameName,
    listGeneratedCodeJobs_snapshotId,
    listGeneratedCodeJobsResponse_generatedCodeJobs,
    listGeneratedCodeJobsResponse_nextToken,
    listGeneratedCodeJobsResponse_httpStatus,

    -- ** ListSnapshots
    listSnapshots_maxResults,
    listSnapshots_nextToken,
    listSnapshots_gameName,
    listSnapshotsResponse_nextToken,
    listSnapshotsResponse_snapshots,
    listSnapshotsResponse_httpStatus,

    -- ** ListStageDeployments
    listStageDeployments_maxResults,
    listStageDeployments_nextToken,
    listStageDeployments_gameName,
    listStageDeployments_stageName,
    listStageDeploymentsResponse_nextToken,
    listStageDeploymentsResponse_stageDeployments,
    listStageDeploymentsResponse_httpStatus,

    -- ** ListStages
    listStages_maxResults,
    listStages_nextToken,
    listStages_gameName,
    listStagesResponse_nextToken,
    listStagesResponse_stages,
    listStagesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartGeneratedCodeJob
    startGeneratedCodeJob_gameName,
    startGeneratedCodeJob_generator,
    startGeneratedCodeJob_snapshotId,
    startGeneratedCodeJobResponse_generatedCodeJobId,
    startGeneratedCodeJobResponse_httpStatus,

    -- ** StartStageDeployment
    startStageDeployment_clientToken,
    startStageDeployment_gameName,
    startStageDeployment_snapshotId,
    startStageDeployment_stageName,
    startStageDeploymentResponse_stageDeployment,
    startStageDeploymentResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateGame
    updateGame_description,
    updateGame_gameName,
    updateGameResponse_game,
    updateGameResponse_httpStatus,

    -- ** UpdateGameConfiguration
    updateGameConfiguration_gameName,
    updateGameConfiguration_modifications,
    updateGameConfigurationResponse_gameConfiguration,
    updateGameConfigurationResponse_httpStatus,

    -- ** UpdateSnapshot
    updateSnapshot_description,
    updateSnapshot_gameName,
    updateSnapshot_snapshotId,
    updateSnapshotResponse_snapshot,
    updateSnapshotResponse_httpStatus,

    -- ** UpdateStage
    updateStage_description,
    updateStage_role,
    updateStage_gameName,
    updateStage_stageName,
    updateStageResponse_stage,
    updateStageResponse_httpStatus,

    -- * Types

    -- ** Connection
    connection_created,
    connection_id,

    -- ** DeploymentResult
    deploymentResult_message,
    deploymentResult_resultCode,

    -- ** Document

    -- ** ExtensionDetails
    extensionDetails_description,
    extensionDetails_name,
    extensionDetails_namespace,

    -- ** ExtensionVersionDetails
    extensionVersionDetails_name,
    extensionVersionDetails_namespace,
    extensionVersionDetails_schema,
    extensionVersionDetails_version,

    -- ** GameConfigurationDetails
    gameConfigurationDetails_created,
    gameConfigurationDetails_lastUpdated,
    gameConfigurationDetails_sections,

    -- ** GameDetails
    gameDetails_arn,
    gameDetails_created,
    gameDetails_description,
    gameDetails_enableTerminationProtection,
    gameDetails_lastUpdated,
    gameDetails_name,
    gameDetails_state,
    gameDetails_tags,

    -- ** GameSummary
    gameSummary_description,
    gameSummary_name,
    gameSummary_state,
    gameSummary_tags,

    -- ** GeneratedCodeJobDetails
    generatedCodeJobDetails_description,
    generatedCodeJobDetails_expirationTime,
    generatedCodeJobDetails_generatedCodeJobId,
    generatedCodeJobDetails_s3Url,
    generatedCodeJobDetails_status,

    -- ** Generator
    generator_gameSdkVersion,
    generator_language,
    generator_targetPlatform,

    -- ** ImportGameConfigurationSource
    importGameConfigurationSource_file,

    -- ** Section
    section_attributes,
    section_name,
    section_size,

    -- ** SectionModification
    sectionModification_value,
    sectionModification_operation,
    sectionModification_path,
    sectionModification_section,

    -- ** SnapshotDetails
    snapshotDetails_created,
    snapshotDetails_description,
    snapshotDetails_id,
    snapshotDetails_lastUpdated,
    snapshotDetails_sections,

    -- ** SnapshotSummary
    snapshotSummary_created,
    snapshotSummary_description,
    snapshotSummary_id,
    snapshotSummary_lastUpdated,

    -- ** StageDeploymentDetails
    stageDeploymentDetails_created,
    stageDeploymentDetails_deploymentAction,
    stageDeploymentDetails_deploymentId,
    stageDeploymentDetails_deploymentResult,
    stageDeploymentDetails_deploymentState,
    stageDeploymentDetails_lastUpdated,
    stageDeploymentDetails_snapshotId,

    -- ** StageDeploymentSummary
    stageDeploymentSummary_deploymentAction,
    stageDeploymentSummary_deploymentId,
    stageDeploymentSummary_deploymentResult,
    stageDeploymentSummary_deploymentState,
    stageDeploymentSummary_lastUpdated,
    stageDeploymentSummary_snapshotId,

    -- ** StageDetails
    stageDetails_arn,
    stageDetails_created,
    stageDetails_description,
    stageDetails_gameKey,
    stageDetails_lastUpdated,
    stageDetails_logGroup,
    stageDetails_name,
    stageDetails_role,
    stageDetails_state,
    stageDetails_tags,

    -- ** StageSummary
    stageSummary_description,
    stageSummary_gameKey,
    stageSummary_name,
    stageSummary_state,
    stageSummary_tags,
  )
where

import Amazonka.GamesParks.CreateGame
import Amazonka.GamesParks.CreateSnapshot
import Amazonka.GamesParks.CreateStage
import Amazonka.GamesParks.DeleteGame
import Amazonka.GamesParks.DeleteStage
import Amazonka.GamesParks.DisconnectPlayer
import Amazonka.GamesParks.ExportSnapshot
import Amazonka.GamesParks.GetExtension
import Amazonka.GamesParks.GetExtensionVersion
import Amazonka.GamesParks.GetGame
import Amazonka.GamesParks.GetGameConfiguration
import Amazonka.GamesParks.GetGeneratedCodeJob
import Amazonka.GamesParks.GetPlayerConnectionStatus
import Amazonka.GamesParks.GetSnapshot
import Amazonka.GamesParks.GetStage
import Amazonka.GamesParks.GetStageDeployment
import Amazonka.GamesParks.ImportGameConfiguration
import Amazonka.GamesParks.ListExtensionVersions
import Amazonka.GamesParks.ListExtensions
import Amazonka.GamesParks.ListGames
import Amazonka.GamesParks.ListGeneratedCodeJobs
import Amazonka.GamesParks.ListSnapshots
import Amazonka.GamesParks.ListStageDeployments
import Amazonka.GamesParks.ListStages
import Amazonka.GamesParks.ListTagsForResource
import Amazonka.GamesParks.StartGeneratedCodeJob
import Amazonka.GamesParks.StartStageDeployment
import Amazonka.GamesParks.TagResource
import Amazonka.GamesParks.Types.Connection
import Amazonka.GamesParks.Types.DeploymentResult
import Amazonka.GamesParks.Types.Document
import Amazonka.GamesParks.Types.ExtensionDetails
import Amazonka.GamesParks.Types.ExtensionVersionDetails
import Amazonka.GamesParks.Types.GameConfigurationDetails
import Amazonka.GamesParks.Types.GameDetails
import Amazonka.GamesParks.Types.GameSummary
import Amazonka.GamesParks.Types.GeneratedCodeJobDetails
import Amazonka.GamesParks.Types.Generator
import Amazonka.GamesParks.Types.ImportGameConfigurationSource
import Amazonka.GamesParks.Types.Section
import Amazonka.GamesParks.Types.SectionModification
import Amazonka.GamesParks.Types.SnapshotDetails
import Amazonka.GamesParks.Types.SnapshotSummary
import Amazonka.GamesParks.Types.StageDeploymentDetails
import Amazonka.GamesParks.Types.StageDeploymentSummary
import Amazonka.GamesParks.Types.StageDetails
import Amazonka.GamesParks.Types.StageSummary
import Amazonka.GamesParks.UntagResource
import Amazonka.GamesParks.UpdateGame
import Amazonka.GamesParks.UpdateGameConfiguration
import Amazonka.GamesParks.UpdateSnapshot
import Amazonka.GamesParks.UpdateStage
