{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.LookoutVision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-11-20@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the Amazon Lookout for Vision API Reference. It provides
-- descriptions of actions, data types, common parameters, and common
-- errors.
--
-- Amazon Lookout for Vision enables you to find visual defects in
-- industrial products, accurately and at scale. It uses computer vision to
-- identify missing components in an industrial product, damage to vehicles
-- or structures, irregularities in production lines, and even minuscule
-- defects in silicon wafers — or any other physical item where quality is
-- important such as a missing capacitor on printed circuit boards.
module Amazonka.LookoutVision
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListProjects (Paginated)
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** DescribeDataset
    DescribeDataset (DescribeDataset'),
    newDescribeDataset,
    DescribeDatasetResponse (DescribeDatasetResponse'),
    newDescribeDatasetResponse,

    -- ** StopModel
    StopModel (StopModel'),
    newStopModel,
    StopModelResponse (StopModelResponse'),
    newStopModelResponse,

    -- ** ListDatasetEntries (Paginated)
    ListDatasetEntries (ListDatasetEntries'),
    newListDatasetEntries,
    ListDatasetEntriesResponse (ListDatasetEntriesResponse'),
    newListDatasetEntriesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeProject
    DescribeProject (DescribeProject'),
    newDescribeProject,
    DescribeProjectResponse (DescribeProjectResponse'),
    newDescribeProjectResponse,

    -- ** CreateModel
    CreateModel (CreateModel'),
    newCreateModel,
    CreateModelResponse (CreateModelResponse'),
    newCreateModelResponse,

    -- ** DeleteDataset
    DeleteDataset (DeleteDataset'),
    newDeleteDataset,
    DeleteDatasetResponse (DeleteDatasetResponse'),
    newDeleteDatasetResponse,

    -- ** CreateDataset
    CreateDataset (CreateDataset'),
    newCreateDataset,
    CreateDatasetResponse (CreateDatasetResponse'),
    newCreateDatasetResponse,

    -- ** DeleteModel
    DeleteModel (DeleteModel'),
    newDeleteModel,
    DeleteModelResponse (DeleteModelResponse'),
    newDeleteModelResponse,

    -- ** ListModels (Paginated)
    ListModels (ListModels'),
    newListModels,
    ListModelsResponse (ListModelsResponse'),
    newListModelsResponse,

    -- ** StartModel
    StartModel (StartModel'),
    newStartModel,
    StartModelResponse (StartModelResponse'),
    newStartModelResponse,

    -- ** DescribeModel
    DescribeModel (DescribeModel'),
    newDescribeModel,
    DescribeModelResponse (DescribeModelResponse'),
    newDescribeModelResponse,

    -- ** DetectAnomalies
    DetectAnomalies (DetectAnomalies'),
    newDetectAnomalies,
    DetectAnomaliesResponse (DetectAnomaliesResponse'),
    newDetectAnomaliesResponse,

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

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- ** UpdateDatasetEntries
    UpdateDatasetEntries (UpdateDatasetEntries'),
    newUpdateDatasetEntries,
    UpdateDatasetEntriesResponse (UpdateDatasetEntriesResponse'),
    newUpdateDatasetEntriesResponse,

    -- * Types

    -- ** DatasetStatus
    DatasetStatus (..),

    -- ** ModelHostingStatus
    ModelHostingStatus (..),

    -- ** ModelStatus
    ModelStatus (..),

    -- ** DatasetDescription
    DatasetDescription (DatasetDescription'),
    newDatasetDescription,

    -- ** DatasetGroundTruthManifest
    DatasetGroundTruthManifest (DatasetGroundTruthManifest'),
    newDatasetGroundTruthManifest,

    -- ** DatasetImageStats
    DatasetImageStats (DatasetImageStats'),
    newDatasetImageStats,

    -- ** DatasetMetadata
    DatasetMetadata (DatasetMetadata'),
    newDatasetMetadata,

    -- ** DatasetSource
    DatasetSource (DatasetSource'),
    newDatasetSource,

    -- ** DetectAnomalyResult
    DetectAnomalyResult (DetectAnomalyResult'),
    newDetectAnomalyResult,

    -- ** ImageSource
    ImageSource (ImageSource'),
    newImageSource,

    -- ** InputS3Object
    InputS3Object (InputS3Object'),
    newInputS3Object,

    -- ** ModelDescription
    ModelDescription (ModelDescription'),
    newModelDescription,

    -- ** ModelMetadata
    ModelMetadata (ModelMetadata'),
    newModelMetadata,

    -- ** ModelPerformance
    ModelPerformance (ModelPerformance'),
    newModelPerformance,

    -- ** OutputConfig
    OutputConfig (OutputConfig'),
    newOutputConfig,

    -- ** OutputS3Object
    OutputS3Object (OutputS3Object'),
    newOutputS3Object,

    -- ** ProjectDescription
    ProjectDescription (ProjectDescription'),
    newProjectDescription,

    -- ** ProjectMetadata
    ProjectMetadata (ProjectMetadata'),
    newProjectMetadata,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.LookoutVision.CreateDataset
import Amazonka.LookoutVision.CreateModel
import Amazonka.LookoutVision.CreateProject
import Amazonka.LookoutVision.DeleteDataset
import Amazonka.LookoutVision.DeleteModel
import Amazonka.LookoutVision.DeleteProject
import Amazonka.LookoutVision.DescribeDataset
import Amazonka.LookoutVision.DescribeModel
import Amazonka.LookoutVision.DescribeProject
import Amazonka.LookoutVision.DetectAnomalies
import Amazonka.LookoutVision.Lens
import Amazonka.LookoutVision.ListDatasetEntries
import Amazonka.LookoutVision.ListModels
import Amazonka.LookoutVision.ListProjects
import Amazonka.LookoutVision.ListTagsForResource
import Amazonka.LookoutVision.StartModel
import Amazonka.LookoutVision.StopModel
import Amazonka.LookoutVision.TagResource
import Amazonka.LookoutVision.Types
import Amazonka.LookoutVision.UntagResource
import Amazonka.LookoutVision.UpdateDatasetEntries
import Amazonka.LookoutVision.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'LookoutVision'.

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
