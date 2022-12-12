{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.LookoutEquipment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-12-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Lookout for Equipment is a machine learning service that uses
-- advanced analytics to identify anomalies in machines from sensor data
-- for use in predictive maintenance.
module Amazonka.LookoutEquipment
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

    -- ** CreateDataset
    CreateDataset (CreateDataset'),
    newCreateDataset,
    CreateDatasetResponse (CreateDatasetResponse'),
    newCreateDatasetResponse,

    -- ** CreateInferenceScheduler
    CreateInferenceScheduler (CreateInferenceScheduler'),
    newCreateInferenceScheduler,
    CreateInferenceSchedulerResponse (CreateInferenceSchedulerResponse'),
    newCreateInferenceSchedulerResponse,

    -- ** CreateLabel
    CreateLabel (CreateLabel'),
    newCreateLabel,
    CreateLabelResponse (CreateLabelResponse'),
    newCreateLabelResponse,

    -- ** CreateLabelGroup
    CreateLabelGroup (CreateLabelGroup'),
    newCreateLabelGroup,
    CreateLabelGroupResponse (CreateLabelGroupResponse'),
    newCreateLabelGroupResponse,

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

    -- ** DeleteInferenceScheduler
    DeleteInferenceScheduler (DeleteInferenceScheduler'),
    newDeleteInferenceScheduler,
    DeleteInferenceSchedulerResponse (DeleteInferenceSchedulerResponse'),
    newDeleteInferenceSchedulerResponse,

    -- ** DeleteLabel
    DeleteLabel (DeleteLabel'),
    newDeleteLabel,
    DeleteLabelResponse (DeleteLabelResponse'),
    newDeleteLabelResponse,

    -- ** DeleteLabelGroup
    DeleteLabelGroup (DeleteLabelGroup'),
    newDeleteLabelGroup,
    DeleteLabelGroupResponse (DeleteLabelGroupResponse'),
    newDeleteLabelGroupResponse,

    -- ** DeleteModel
    DeleteModel (DeleteModel'),
    newDeleteModel,
    DeleteModelResponse (DeleteModelResponse'),
    newDeleteModelResponse,

    -- ** DescribeDataIngestionJob
    DescribeDataIngestionJob (DescribeDataIngestionJob'),
    newDescribeDataIngestionJob,
    DescribeDataIngestionJobResponse (DescribeDataIngestionJobResponse'),
    newDescribeDataIngestionJobResponse,

    -- ** DescribeDataset
    DescribeDataset (DescribeDataset'),
    newDescribeDataset,
    DescribeDatasetResponse (DescribeDatasetResponse'),
    newDescribeDatasetResponse,

    -- ** DescribeInferenceScheduler
    DescribeInferenceScheduler (DescribeInferenceScheduler'),
    newDescribeInferenceScheduler,
    DescribeInferenceSchedulerResponse (DescribeInferenceSchedulerResponse'),
    newDescribeInferenceSchedulerResponse,

    -- ** DescribeLabel
    DescribeLabel (DescribeLabel'),
    newDescribeLabel,
    DescribeLabelResponse (DescribeLabelResponse'),
    newDescribeLabelResponse,

    -- ** DescribeLabelGroup
    DescribeLabelGroup (DescribeLabelGroup'),
    newDescribeLabelGroup,
    DescribeLabelGroupResponse (DescribeLabelGroupResponse'),
    newDescribeLabelGroupResponse,

    -- ** DescribeModel
    DescribeModel (DescribeModel'),
    newDescribeModel,
    DescribeModelResponse (DescribeModelResponse'),
    newDescribeModelResponse,

    -- ** ListDataIngestionJobs
    ListDataIngestionJobs (ListDataIngestionJobs'),
    newListDataIngestionJobs,
    ListDataIngestionJobsResponse (ListDataIngestionJobsResponse'),
    newListDataIngestionJobsResponse,

    -- ** ListDatasets
    ListDatasets (ListDatasets'),
    newListDatasets,
    ListDatasetsResponse (ListDatasetsResponse'),
    newListDatasetsResponse,

    -- ** ListInferenceEvents
    ListInferenceEvents (ListInferenceEvents'),
    newListInferenceEvents,
    ListInferenceEventsResponse (ListInferenceEventsResponse'),
    newListInferenceEventsResponse,

    -- ** ListInferenceExecutions
    ListInferenceExecutions (ListInferenceExecutions'),
    newListInferenceExecutions,
    ListInferenceExecutionsResponse (ListInferenceExecutionsResponse'),
    newListInferenceExecutionsResponse,

    -- ** ListInferenceSchedulers
    ListInferenceSchedulers (ListInferenceSchedulers'),
    newListInferenceSchedulers,
    ListInferenceSchedulersResponse (ListInferenceSchedulersResponse'),
    newListInferenceSchedulersResponse,

    -- ** ListLabelGroups
    ListLabelGroups (ListLabelGroups'),
    newListLabelGroups,
    ListLabelGroupsResponse (ListLabelGroupsResponse'),
    newListLabelGroupsResponse,

    -- ** ListLabels
    ListLabels (ListLabels'),
    newListLabels,
    ListLabelsResponse (ListLabelsResponse'),
    newListLabelsResponse,

    -- ** ListModels
    ListModels (ListModels'),
    newListModels,
    ListModelsResponse (ListModelsResponse'),
    newListModelsResponse,

    -- ** ListSensorStatistics
    ListSensorStatistics (ListSensorStatistics'),
    newListSensorStatistics,
    ListSensorStatisticsResponse (ListSensorStatisticsResponse'),
    newListSensorStatisticsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartDataIngestionJob
    StartDataIngestionJob (StartDataIngestionJob'),
    newStartDataIngestionJob,
    StartDataIngestionJobResponse (StartDataIngestionJobResponse'),
    newStartDataIngestionJobResponse,

    -- ** StartInferenceScheduler
    StartInferenceScheduler (StartInferenceScheduler'),
    newStartInferenceScheduler,
    StartInferenceSchedulerResponse (StartInferenceSchedulerResponse'),
    newStartInferenceSchedulerResponse,

    -- ** StopInferenceScheduler
    StopInferenceScheduler (StopInferenceScheduler'),
    newStopInferenceScheduler,
    StopInferenceSchedulerResponse (StopInferenceSchedulerResponse'),
    newStopInferenceSchedulerResponse,

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

    -- ** UpdateInferenceScheduler
    UpdateInferenceScheduler (UpdateInferenceScheduler'),
    newUpdateInferenceScheduler,
    UpdateInferenceSchedulerResponse (UpdateInferenceSchedulerResponse'),
    newUpdateInferenceSchedulerResponse,

    -- ** UpdateLabelGroup
    UpdateLabelGroup (UpdateLabelGroup'),
    newUpdateLabelGroup,
    UpdateLabelGroupResponse (UpdateLabelGroupResponse'),
    newUpdateLabelGroupResponse,

    -- * Types

    -- ** DataUploadFrequency
    DataUploadFrequency (..),

    -- ** DatasetStatus
    DatasetStatus (..),

    -- ** InferenceExecutionStatus
    InferenceExecutionStatus (..),

    -- ** InferenceSchedulerStatus
    InferenceSchedulerStatus (..),

    -- ** IngestionJobStatus
    IngestionJobStatus (..),

    -- ** LabelRating
    LabelRating (..),

    -- ** LatestInferenceResult
    LatestInferenceResult (..),

    -- ** ModelStatus
    ModelStatus (..),

    -- ** Monotonicity
    Monotonicity (..),

    -- ** StatisticalIssueStatus
    StatisticalIssueStatus (..),

    -- ** TargetSamplingRate
    TargetSamplingRate (..),

    -- ** CategoricalValues
    CategoricalValues (CategoricalValues'),
    newCategoricalValues,

    -- ** CountPercent
    CountPercent (CountPercent'),
    newCountPercent,

    -- ** DataIngestionJobSummary
    DataIngestionJobSummary (DataIngestionJobSummary'),
    newDataIngestionJobSummary,

    -- ** DataPreProcessingConfiguration
    DataPreProcessingConfiguration (DataPreProcessingConfiguration'),
    newDataPreProcessingConfiguration,

    -- ** DataQualitySummary
    DataQualitySummary (DataQualitySummary'),
    newDataQualitySummary,

    -- ** DatasetSchema
    DatasetSchema (DatasetSchema'),
    newDatasetSchema,

    -- ** DatasetSummary
    DatasetSummary (DatasetSummary'),
    newDatasetSummary,

    -- ** DuplicateTimestamps
    DuplicateTimestamps (DuplicateTimestamps'),
    newDuplicateTimestamps,

    -- ** InferenceEventSummary
    InferenceEventSummary (InferenceEventSummary'),
    newInferenceEventSummary,

    -- ** InferenceExecutionSummary
    InferenceExecutionSummary (InferenceExecutionSummary'),
    newInferenceExecutionSummary,

    -- ** InferenceInputConfiguration
    InferenceInputConfiguration (InferenceInputConfiguration'),
    newInferenceInputConfiguration,

    -- ** InferenceInputNameConfiguration
    InferenceInputNameConfiguration (InferenceInputNameConfiguration'),
    newInferenceInputNameConfiguration,

    -- ** InferenceOutputConfiguration
    InferenceOutputConfiguration (InferenceOutputConfiguration'),
    newInferenceOutputConfiguration,

    -- ** InferenceS3InputConfiguration
    InferenceS3InputConfiguration (InferenceS3InputConfiguration'),
    newInferenceS3InputConfiguration,

    -- ** InferenceS3OutputConfiguration
    InferenceS3OutputConfiguration (InferenceS3OutputConfiguration'),
    newInferenceS3OutputConfiguration,

    -- ** InferenceSchedulerSummary
    InferenceSchedulerSummary (InferenceSchedulerSummary'),
    newInferenceSchedulerSummary,

    -- ** IngestedFilesSummary
    IngestedFilesSummary (IngestedFilesSummary'),
    newIngestedFilesSummary,

    -- ** IngestionInputConfiguration
    IngestionInputConfiguration (IngestionInputConfiguration'),
    newIngestionInputConfiguration,

    -- ** IngestionS3InputConfiguration
    IngestionS3InputConfiguration (IngestionS3InputConfiguration'),
    newIngestionS3InputConfiguration,

    -- ** InsufficientSensorData
    InsufficientSensorData (InsufficientSensorData'),
    newInsufficientSensorData,

    -- ** InvalidSensorData
    InvalidSensorData (InvalidSensorData'),
    newInvalidSensorData,

    -- ** LabelGroupSummary
    LabelGroupSummary (LabelGroupSummary'),
    newLabelGroupSummary,

    -- ** LabelSummary
    LabelSummary (LabelSummary'),
    newLabelSummary,

    -- ** LabelsInputConfiguration
    LabelsInputConfiguration (LabelsInputConfiguration'),
    newLabelsInputConfiguration,

    -- ** LabelsS3InputConfiguration
    LabelsS3InputConfiguration (LabelsS3InputConfiguration'),
    newLabelsS3InputConfiguration,

    -- ** LargeTimestampGaps
    LargeTimestampGaps (LargeTimestampGaps'),
    newLargeTimestampGaps,

    -- ** MissingCompleteSensorData
    MissingCompleteSensorData (MissingCompleteSensorData'),
    newMissingCompleteSensorData,

    -- ** MissingSensorData
    MissingSensorData (MissingSensorData'),
    newMissingSensorData,

    -- ** ModelSummary
    ModelSummary (ModelSummary'),
    newModelSummary,

    -- ** MonotonicValues
    MonotonicValues (MonotonicValues'),
    newMonotonicValues,

    -- ** MultipleOperatingModes
    MultipleOperatingModes (MultipleOperatingModes'),
    newMultipleOperatingModes,

    -- ** S3Object
    S3Object (S3Object'),
    newS3Object,

    -- ** SensorStatisticsSummary
    SensorStatisticsSummary (SensorStatisticsSummary'),
    newSensorStatisticsSummary,

    -- ** SensorsWithShortDateRange
    SensorsWithShortDateRange (SensorsWithShortDateRange'),
    newSensorsWithShortDateRange,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UnsupportedTimestamps
    UnsupportedTimestamps (UnsupportedTimestamps'),
    newUnsupportedTimestamps,
  )
where

import Amazonka.LookoutEquipment.CreateDataset
import Amazonka.LookoutEquipment.CreateInferenceScheduler
import Amazonka.LookoutEquipment.CreateLabel
import Amazonka.LookoutEquipment.CreateLabelGroup
import Amazonka.LookoutEquipment.CreateModel
import Amazonka.LookoutEquipment.DeleteDataset
import Amazonka.LookoutEquipment.DeleteInferenceScheduler
import Amazonka.LookoutEquipment.DeleteLabel
import Amazonka.LookoutEquipment.DeleteLabelGroup
import Amazonka.LookoutEquipment.DeleteModel
import Amazonka.LookoutEquipment.DescribeDataIngestionJob
import Amazonka.LookoutEquipment.DescribeDataset
import Amazonka.LookoutEquipment.DescribeInferenceScheduler
import Amazonka.LookoutEquipment.DescribeLabel
import Amazonka.LookoutEquipment.DescribeLabelGroup
import Amazonka.LookoutEquipment.DescribeModel
import Amazonka.LookoutEquipment.Lens
import Amazonka.LookoutEquipment.ListDataIngestionJobs
import Amazonka.LookoutEquipment.ListDatasets
import Amazonka.LookoutEquipment.ListInferenceEvents
import Amazonka.LookoutEquipment.ListInferenceExecutions
import Amazonka.LookoutEquipment.ListInferenceSchedulers
import Amazonka.LookoutEquipment.ListLabelGroups
import Amazonka.LookoutEquipment.ListLabels
import Amazonka.LookoutEquipment.ListModels
import Amazonka.LookoutEquipment.ListSensorStatistics
import Amazonka.LookoutEquipment.ListTagsForResource
import Amazonka.LookoutEquipment.StartDataIngestionJob
import Amazonka.LookoutEquipment.StartInferenceScheduler
import Amazonka.LookoutEquipment.StopInferenceScheduler
import Amazonka.LookoutEquipment.TagResource
import Amazonka.LookoutEquipment.Types
import Amazonka.LookoutEquipment.UntagResource
import Amazonka.LookoutEquipment.UpdateInferenceScheduler
import Amazonka.LookoutEquipment.UpdateLabelGroup
import Amazonka.LookoutEquipment.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'LookoutEquipment'.

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
