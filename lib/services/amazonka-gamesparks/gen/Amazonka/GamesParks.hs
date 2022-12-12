{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.GamesParks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-08-17@ of the AWS service descriptions, licensed under Apache 2.0.
module Amazonka.GamesParks
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateGame
    CreateGame (CreateGame'),
    newCreateGame,
    CreateGameResponse (CreateGameResponse'),
    newCreateGameResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    CreateSnapshotResponse (CreateSnapshotResponse'),
    newCreateSnapshotResponse,

    -- ** CreateStage
    CreateStage (CreateStage'),
    newCreateStage,
    CreateStageResponse (CreateStageResponse'),
    newCreateStageResponse,

    -- ** DeleteGame
    DeleteGame (DeleteGame'),
    newDeleteGame,
    DeleteGameResponse (DeleteGameResponse'),
    newDeleteGameResponse,

    -- ** DeleteStage
    DeleteStage (DeleteStage'),
    newDeleteStage,
    DeleteStageResponse (DeleteStageResponse'),
    newDeleteStageResponse,

    -- ** DisconnectPlayer
    DisconnectPlayer (DisconnectPlayer'),
    newDisconnectPlayer,
    DisconnectPlayerResponse (DisconnectPlayerResponse'),
    newDisconnectPlayerResponse,

    -- ** ExportSnapshot
    ExportSnapshot (ExportSnapshot'),
    newExportSnapshot,
    ExportSnapshotResponse (ExportSnapshotResponse'),
    newExportSnapshotResponse,

    -- ** GetExtension
    GetExtension (GetExtension'),
    newGetExtension,
    GetExtensionResponse (GetExtensionResponse'),
    newGetExtensionResponse,

    -- ** GetExtensionVersion
    GetExtensionVersion (GetExtensionVersion'),
    newGetExtensionVersion,
    GetExtensionVersionResponse (GetExtensionVersionResponse'),
    newGetExtensionVersionResponse,

    -- ** GetGame
    GetGame (GetGame'),
    newGetGame,
    GetGameResponse (GetGameResponse'),
    newGetGameResponse,

    -- ** GetGameConfiguration
    GetGameConfiguration (GetGameConfiguration'),
    newGetGameConfiguration,
    GetGameConfigurationResponse (GetGameConfigurationResponse'),
    newGetGameConfigurationResponse,

    -- ** GetGeneratedCodeJob
    GetGeneratedCodeJob (GetGeneratedCodeJob'),
    newGetGeneratedCodeJob,
    GetGeneratedCodeJobResponse (GetGeneratedCodeJobResponse'),
    newGetGeneratedCodeJobResponse,

    -- ** GetPlayerConnectionStatus
    GetPlayerConnectionStatus (GetPlayerConnectionStatus'),
    newGetPlayerConnectionStatus,
    GetPlayerConnectionStatusResponse (GetPlayerConnectionStatusResponse'),
    newGetPlayerConnectionStatusResponse,

    -- ** GetSnapshot
    GetSnapshot (GetSnapshot'),
    newGetSnapshot,
    GetSnapshotResponse (GetSnapshotResponse'),
    newGetSnapshotResponse,

    -- ** GetStage
    GetStage (GetStage'),
    newGetStage,
    GetStageResponse (GetStageResponse'),
    newGetStageResponse,

    -- ** GetStageDeployment
    GetStageDeployment (GetStageDeployment'),
    newGetStageDeployment,
    GetStageDeploymentResponse (GetStageDeploymentResponse'),
    newGetStageDeploymentResponse,

    -- ** ImportGameConfiguration
    ImportGameConfiguration (ImportGameConfiguration'),
    newImportGameConfiguration,
    ImportGameConfigurationResponse (ImportGameConfigurationResponse'),
    newImportGameConfigurationResponse,

    -- ** ListExtensionVersions (Paginated)
    ListExtensionVersions (ListExtensionVersions'),
    newListExtensionVersions,
    ListExtensionVersionsResponse (ListExtensionVersionsResponse'),
    newListExtensionVersionsResponse,

    -- ** ListExtensions (Paginated)
    ListExtensions (ListExtensions'),
    newListExtensions,
    ListExtensionsResponse (ListExtensionsResponse'),
    newListExtensionsResponse,

    -- ** ListGames (Paginated)
    ListGames (ListGames'),
    newListGames,
    ListGamesResponse (ListGamesResponse'),
    newListGamesResponse,

    -- ** ListGeneratedCodeJobs (Paginated)
    ListGeneratedCodeJobs (ListGeneratedCodeJobs'),
    newListGeneratedCodeJobs,
    ListGeneratedCodeJobsResponse (ListGeneratedCodeJobsResponse'),
    newListGeneratedCodeJobsResponse,

    -- ** ListSnapshots (Paginated)
    ListSnapshots (ListSnapshots'),
    newListSnapshots,
    ListSnapshotsResponse (ListSnapshotsResponse'),
    newListSnapshotsResponse,

    -- ** ListStageDeployments (Paginated)
    ListStageDeployments (ListStageDeployments'),
    newListStageDeployments,
    ListStageDeploymentsResponse (ListStageDeploymentsResponse'),
    newListStageDeploymentsResponse,

    -- ** ListStages (Paginated)
    ListStages (ListStages'),
    newListStages,
    ListStagesResponse (ListStagesResponse'),
    newListStagesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartGeneratedCodeJob
    StartGeneratedCodeJob (StartGeneratedCodeJob'),
    newStartGeneratedCodeJob,
    StartGeneratedCodeJobResponse (StartGeneratedCodeJobResponse'),
    newStartGeneratedCodeJobResponse,

    -- ** StartStageDeployment
    StartStageDeployment (StartStageDeployment'),
    newStartStageDeployment,
    StartStageDeploymentResponse (StartStageDeploymentResponse'),
    newStartStageDeploymentResponse,

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

    -- ** UpdateGame
    UpdateGame (UpdateGame'),
    newUpdateGame,
    UpdateGameResponse (UpdateGameResponse'),
    newUpdateGameResponse,

    -- ** UpdateGameConfiguration
    UpdateGameConfiguration (UpdateGameConfiguration'),
    newUpdateGameConfiguration,
    UpdateGameConfigurationResponse (UpdateGameConfigurationResponse'),
    newUpdateGameConfigurationResponse,

    -- ** UpdateSnapshot
    UpdateSnapshot (UpdateSnapshot'),
    newUpdateSnapshot,
    UpdateSnapshotResponse (UpdateSnapshotResponse'),
    newUpdateSnapshotResponse,

    -- ** UpdateStage
    UpdateStage (UpdateStage'),
    newUpdateStage,
    UpdateStageResponse (UpdateStageResponse'),
    newUpdateStageResponse,

    -- * Types

    -- ** DeploymentAction
    DeploymentAction (..),

    -- ** DeploymentState
    DeploymentState (..),

    -- ** GameState
    GameState (..),

    -- ** GeneratedCodeJobState
    GeneratedCodeJobState (..),

    -- ** Operation
    Operation (..),

    -- ** ResultCode
    ResultCode (..),

    -- ** StageState
    StageState (..),

    -- ** Connection
    Connection (Connection'),
    newConnection,

    -- ** DeploymentResult
    DeploymentResult (DeploymentResult'),
    newDeploymentResult,

    -- ** Document
    Document (Document'),
    newDocument,

    -- ** ExtensionDetails
    ExtensionDetails (ExtensionDetails'),
    newExtensionDetails,

    -- ** ExtensionVersionDetails
    ExtensionVersionDetails (ExtensionVersionDetails'),
    newExtensionVersionDetails,

    -- ** GameConfigurationDetails
    GameConfigurationDetails (GameConfigurationDetails'),
    newGameConfigurationDetails,

    -- ** GameDetails
    GameDetails (GameDetails'),
    newGameDetails,

    -- ** GameSummary
    GameSummary (GameSummary'),
    newGameSummary,

    -- ** GeneratedCodeJobDetails
    GeneratedCodeJobDetails (GeneratedCodeJobDetails'),
    newGeneratedCodeJobDetails,

    -- ** Generator
    Generator (Generator'),
    newGenerator,

    -- ** ImportGameConfigurationSource
    ImportGameConfigurationSource (ImportGameConfigurationSource'),
    newImportGameConfigurationSource,

    -- ** Section
    Section (Section'),
    newSection,

    -- ** SectionModification
    SectionModification (SectionModification'),
    newSectionModification,

    -- ** SnapshotDetails
    SnapshotDetails (SnapshotDetails'),
    newSnapshotDetails,

    -- ** SnapshotSummary
    SnapshotSummary (SnapshotSummary'),
    newSnapshotSummary,

    -- ** StageDeploymentDetails
    StageDeploymentDetails (StageDeploymentDetails'),
    newStageDeploymentDetails,

    -- ** StageDeploymentSummary
    StageDeploymentSummary (StageDeploymentSummary'),
    newStageDeploymentSummary,

    -- ** StageDetails
    StageDetails (StageDetails'),
    newStageDetails,

    -- ** StageSummary
    StageSummary (StageSummary'),
    newStageSummary,
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
import Amazonka.GamesParks.Lens
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
import Amazonka.GamesParks.Types
import Amazonka.GamesParks.UntagResource
import Amazonka.GamesParks.UpdateGame
import Amazonka.GamesParks.UpdateGameConfiguration
import Amazonka.GamesParks.UpdateSnapshot
import Amazonka.GamesParks.UpdateStage
import Amazonka.GamesParks.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'GamesParks'.

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
