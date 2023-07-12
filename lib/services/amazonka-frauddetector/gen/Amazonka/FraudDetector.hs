{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.FraudDetector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-11-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the Amazon Fraud Detector API Reference. This guide is for
-- developers who need detailed information about Amazon Fraud Detector API
-- actions, data types, and errors. For more information about Amazon Fraud
-- Detector features, see the
-- <https://docs.aws.amazon.com/frauddetector/latest/ug/ Amazon Fraud Detector User Guide>.
--
-- We provide the Query API as well as AWS software development kits (SDK)
-- for Amazon Fraud Detector in Java and Python programming languages.
--
-- The Amazon Fraud Detector Query API provides HTTPS requests that use the
-- HTTP verb GET or POST and a Query parameter @Action@. AWS SDK provides
-- libraries, sample code, tutorials, and other resources for software
-- developers who prefer to build applications using language-specific APIs
-- instead of submitting a request over HTTP or HTTPS. These libraries
-- provide basic functions that automatically take care of tasks such as
-- cryptographically signing your requests, retrying requests, and handling
-- error responses, so that it is easier for you to get started. For more
-- information about the AWS SDKs, see
-- <https://docs.aws.amazon.com/https:/aws.amazon.com/tools/ Tools to build on AWS>.
module Amazonka.FraudDetector
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

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchCreateVariable
    BatchCreateVariable (BatchCreateVariable'),
    newBatchCreateVariable,
    BatchCreateVariableResponse (BatchCreateVariableResponse'),
    newBatchCreateVariableResponse,

    -- ** BatchGetVariable
    BatchGetVariable (BatchGetVariable'),
    newBatchGetVariable,
    BatchGetVariableResponse (BatchGetVariableResponse'),
    newBatchGetVariableResponse,

    -- ** CancelBatchImportJob
    CancelBatchImportJob (CancelBatchImportJob'),
    newCancelBatchImportJob,
    CancelBatchImportJobResponse (CancelBatchImportJobResponse'),
    newCancelBatchImportJobResponse,

    -- ** CancelBatchPredictionJob
    CancelBatchPredictionJob (CancelBatchPredictionJob'),
    newCancelBatchPredictionJob,
    CancelBatchPredictionJobResponse (CancelBatchPredictionJobResponse'),
    newCancelBatchPredictionJobResponse,

    -- ** CreateBatchImportJob
    CreateBatchImportJob (CreateBatchImportJob'),
    newCreateBatchImportJob,
    CreateBatchImportJobResponse (CreateBatchImportJobResponse'),
    newCreateBatchImportJobResponse,

    -- ** CreateBatchPredictionJob
    CreateBatchPredictionJob (CreateBatchPredictionJob'),
    newCreateBatchPredictionJob,
    CreateBatchPredictionJobResponse (CreateBatchPredictionJobResponse'),
    newCreateBatchPredictionJobResponse,

    -- ** CreateDetectorVersion
    CreateDetectorVersion (CreateDetectorVersion'),
    newCreateDetectorVersion,
    CreateDetectorVersionResponse (CreateDetectorVersionResponse'),
    newCreateDetectorVersionResponse,

    -- ** CreateModel
    CreateModel (CreateModel'),
    newCreateModel,
    CreateModelResponse (CreateModelResponse'),
    newCreateModelResponse,

    -- ** CreateModelVersion
    CreateModelVersion (CreateModelVersion'),
    newCreateModelVersion,
    CreateModelVersionResponse (CreateModelVersionResponse'),
    newCreateModelVersionResponse,

    -- ** CreateRule
    CreateRule (CreateRule'),
    newCreateRule,
    CreateRuleResponse (CreateRuleResponse'),
    newCreateRuleResponse,

    -- ** CreateVariable
    CreateVariable (CreateVariable'),
    newCreateVariable,
    CreateVariableResponse (CreateVariableResponse'),
    newCreateVariableResponse,

    -- ** DeleteBatchImportJob
    DeleteBatchImportJob (DeleteBatchImportJob'),
    newDeleteBatchImportJob,
    DeleteBatchImportJobResponse (DeleteBatchImportJobResponse'),
    newDeleteBatchImportJobResponse,

    -- ** DeleteBatchPredictionJob
    DeleteBatchPredictionJob (DeleteBatchPredictionJob'),
    newDeleteBatchPredictionJob,
    DeleteBatchPredictionJobResponse (DeleteBatchPredictionJobResponse'),
    newDeleteBatchPredictionJobResponse,

    -- ** DeleteDetector
    DeleteDetector (DeleteDetector'),
    newDeleteDetector,
    DeleteDetectorResponse (DeleteDetectorResponse'),
    newDeleteDetectorResponse,

    -- ** DeleteDetectorVersion
    DeleteDetectorVersion (DeleteDetectorVersion'),
    newDeleteDetectorVersion,
    DeleteDetectorVersionResponse (DeleteDetectorVersionResponse'),
    newDeleteDetectorVersionResponse,

    -- ** DeleteEntityType
    DeleteEntityType (DeleteEntityType'),
    newDeleteEntityType,
    DeleteEntityTypeResponse (DeleteEntityTypeResponse'),
    newDeleteEntityTypeResponse,

    -- ** DeleteEvent
    DeleteEvent (DeleteEvent'),
    newDeleteEvent,
    DeleteEventResponse (DeleteEventResponse'),
    newDeleteEventResponse,

    -- ** DeleteEventType
    DeleteEventType (DeleteEventType'),
    newDeleteEventType,
    DeleteEventTypeResponse (DeleteEventTypeResponse'),
    newDeleteEventTypeResponse,

    -- ** DeleteEventsByEventType
    DeleteEventsByEventType (DeleteEventsByEventType'),
    newDeleteEventsByEventType,
    DeleteEventsByEventTypeResponse (DeleteEventsByEventTypeResponse'),
    newDeleteEventsByEventTypeResponse,

    -- ** DeleteExternalModel
    DeleteExternalModel (DeleteExternalModel'),
    newDeleteExternalModel,
    DeleteExternalModelResponse (DeleteExternalModelResponse'),
    newDeleteExternalModelResponse,

    -- ** DeleteLabel
    DeleteLabel (DeleteLabel'),
    newDeleteLabel,
    DeleteLabelResponse (DeleteLabelResponse'),
    newDeleteLabelResponse,

    -- ** DeleteModel
    DeleteModel (DeleteModel'),
    newDeleteModel,
    DeleteModelResponse (DeleteModelResponse'),
    newDeleteModelResponse,

    -- ** DeleteModelVersion
    DeleteModelVersion (DeleteModelVersion'),
    newDeleteModelVersion,
    DeleteModelVersionResponse (DeleteModelVersionResponse'),
    newDeleteModelVersionResponse,

    -- ** DeleteOutcome
    DeleteOutcome (DeleteOutcome'),
    newDeleteOutcome,
    DeleteOutcomeResponse (DeleteOutcomeResponse'),
    newDeleteOutcomeResponse,

    -- ** DeleteRule
    DeleteRule (DeleteRule'),
    newDeleteRule,
    DeleteRuleResponse (DeleteRuleResponse'),
    newDeleteRuleResponse,

    -- ** DeleteVariable
    DeleteVariable (DeleteVariable'),
    newDeleteVariable,
    DeleteVariableResponse (DeleteVariableResponse'),
    newDeleteVariableResponse,

    -- ** DescribeDetector
    DescribeDetector (DescribeDetector'),
    newDescribeDetector,
    DescribeDetectorResponse (DescribeDetectorResponse'),
    newDescribeDetectorResponse,

    -- ** DescribeModelVersions
    DescribeModelVersions (DescribeModelVersions'),
    newDescribeModelVersions,
    DescribeModelVersionsResponse (DescribeModelVersionsResponse'),
    newDescribeModelVersionsResponse,

    -- ** GetBatchImportJobs
    GetBatchImportJobs (GetBatchImportJobs'),
    newGetBatchImportJobs,
    GetBatchImportJobsResponse (GetBatchImportJobsResponse'),
    newGetBatchImportJobsResponse,

    -- ** GetBatchPredictionJobs
    GetBatchPredictionJobs (GetBatchPredictionJobs'),
    newGetBatchPredictionJobs,
    GetBatchPredictionJobsResponse (GetBatchPredictionJobsResponse'),
    newGetBatchPredictionJobsResponse,

    -- ** GetDeleteEventsByEventTypeStatus
    GetDeleteEventsByEventTypeStatus (GetDeleteEventsByEventTypeStatus'),
    newGetDeleteEventsByEventTypeStatus,
    GetDeleteEventsByEventTypeStatusResponse (GetDeleteEventsByEventTypeStatusResponse'),
    newGetDeleteEventsByEventTypeStatusResponse,

    -- ** GetDetectorVersion
    GetDetectorVersion (GetDetectorVersion'),
    newGetDetectorVersion,
    GetDetectorVersionResponse (GetDetectorVersionResponse'),
    newGetDetectorVersionResponse,

    -- ** GetDetectors
    GetDetectors (GetDetectors'),
    newGetDetectors,
    GetDetectorsResponse (GetDetectorsResponse'),
    newGetDetectorsResponse,

    -- ** GetEntityTypes
    GetEntityTypes (GetEntityTypes'),
    newGetEntityTypes,
    GetEntityTypesResponse (GetEntityTypesResponse'),
    newGetEntityTypesResponse,

    -- ** GetEvent
    GetEvent (GetEvent'),
    newGetEvent,
    GetEventResponse (GetEventResponse'),
    newGetEventResponse,

    -- ** GetEventPrediction
    GetEventPrediction (GetEventPrediction'),
    newGetEventPrediction,
    GetEventPredictionResponse (GetEventPredictionResponse'),
    newGetEventPredictionResponse,

    -- ** GetEventPredictionMetadata
    GetEventPredictionMetadata (GetEventPredictionMetadata'),
    newGetEventPredictionMetadata,
    GetEventPredictionMetadataResponse (GetEventPredictionMetadataResponse'),
    newGetEventPredictionMetadataResponse,

    -- ** GetEventTypes
    GetEventTypes (GetEventTypes'),
    newGetEventTypes,
    GetEventTypesResponse (GetEventTypesResponse'),
    newGetEventTypesResponse,

    -- ** GetExternalModels
    GetExternalModels (GetExternalModels'),
    newGetExternalModels,
    GetExternalModelsResponse (GetExternalModelsResponse'),
    newGetExternalModelsResponse,

    -- ** GetKMSEncryptionKey
    GetKMSEncryptionKey (GetKMSEncryptionKey'),
    newGetKMSEncryptionKey,
    GetKMSEncryptionKeyResponse (GetKMSEncryptionKeyResponse'),
    newGetKMSEncryptionKeyResponse,

    -- ** GetLabels
    GetLabels (GetLabels'),
    newGetLabels,
    GetLabelsResponse (GetLabelsResponse'),
    newGetLabelsResponse,

    -- ** GetModelVersion
    GetModelVersion (GetModelVersion'),
    newGetModelVersion,
    GetModelVersionResponse (GetModelVersionResponse'),
    newGetModelVersionResponse,

    -- ** GetModels
    GetModels (GetModels'),
    newGetModels,
    GetModelsResponse (GetModelsResponse'),
    newGetModelsResponse,

    -- ** GetOutcomes
    GetOutcomes (GetOutcomes'),
    newGetOutcomes,
    GetOutcomesResponse (GetOutcomesResponse'),
    newGetOutcomesResponse,

    -- ** GetRules
    GetRules (GetRules'),
    newGetRules,
    GetRulesResponse (GetRulesResponse'),
    newGetRulesResponse,

    -- ** GetVariables
    GetVariables (GetVariables'),
    newGetVariables,
    GetVariablesResponse (GetVariablesResponse'),
    newGetVariablesResponse,

    -- ** ListEventPredictions
    ListEventPredictions (ListEventPredictions'),
    newListEventPredictions,
    ListEventPredictionsResponse (ListEventPredictionsResponse'),
    newListEventPredictionsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutDetector
    PutDetector (PutDetector'),
    newPutDetector,
    PutDetectorResponse (PutDetectorResponse'),
    newPutDetectorResponse,

    -- ** PutEntityType
    PutEntityType (PutEntityType'),
    newPutEntityType,
    PutEntityTypeResponse (PutEntityTypeResponse'),
    newPutEntityTypeResponse,

    -- ** PutEventType
    PutEventType (PutEventType'),
    newPutEventType,
    PutEventTypeResponse (PutEventTypeResponse'),
    newPutEventTypeResponse,

    -- ** PutExternalModel
    PutExternalModel (PutExternalModel'),
    newPutExternalModel,
    PutExternalModelResponse (PutExternalModelResponse'),
    newPutExternalModelResponse,

    -- ** PutKMSEncryptionKey
    PutKMSEncryptionKey (PutKMSEncryptionKey'),
    newPutKMSEncryptionKey,
    PutKMSEncryptionKeyResponse (PutKMSEncryptionKeyResponse'),
    newPutKMSEncryptionKeyResponse,

    -- ** PutLabel
    PutLabel (PutLabel'),
    newPutLabel,
    PutLabelResponse (PutLabelResponse'),
    newPutLabelResponse,

    -- ** PutOutcome
    PutOutcome (PutOutcome'),
    newPutOutcome,
    PutOutcomeResponse (PutOutcomeResponse'),
    newPutOutcomeResponse,

    -- ** SendEvent
    SendEvent (SendEvent'),
    newSendEvent,
    SendEventResponse (SendEventResponse'),
    newSendEventResponse,

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

    -- ** UpdateDetectorVersion
    UpdateDetectorVersion (UpdateDetectorVersion'),
    newUpdateDetectorVersion,
    UpdateDetectorVersionResponse (UpdateDetectorVersionResponse'),
    newUpdateDetectorVersionResponse,

    -- ** UpdateDetectorVersionMetadata
    UpdateDetectorVersionMetadata (UpdateDetectorVersionMetadata'),
    newUpdateDetectorVersionMetadata,
    UpdateDetectorVersionMetadataResponse (UpdateDetectorVersionMetadataResponse'),
    newUpdateDetectorVersionMetadataResponse,

    -- ** UpdateDetectorVersionStatus
    UpdateDetectorVersionStatus (UpdateDetectorVersionStatus'),
    newUpdateDetectorVersionStatus,
    UpdateDetectorVersionStatusResponse (UpdateDetectorVersionStatusResponse'),
    newUpdateDetectorVersionStatusResponse,

    -- ** UpdateEventLabel
    UpdateEventLabel (UpdateEventLabel'),
    newUpdateEventLabel,
    UpdateEventLabelResponse (UpdateEventLabelResponse'),
    newUpdateEventLabelResponse,

    -- ** UpdateModel
    UpdateModel (UpdateModel'),
    newUpdateModel,
    UpdateModelResponse (UpdateModelResponse'),
    newUpdateModelResponse,

    -- ** UpdateModelVersion
    UpdateModelVersion (UpdateModelVersion'),
    newUpdateModelVersion,
    UpdateModelVersionResponse (UpdateModelVersionResponse'),
    newUpdateModelVersionResponse,

    -- ** UpdateModelVersionStatus
    UpdateModelVersionStatus (UpdateModelVersionStatus'),
    newUpdateModelVersionStatus,
    UpdateModelVersionStatusResponse (UpdateModelVersionStatusResponse'),
    newUpdateModelVersionStatusResponse,

    -- ** UpdateRuleMetadata
    UpdateRuleMetadata (UpdateRuleMetadata'),
    newUpdateRuleMetadata,
    UpdateRuleMetadataResponse (UpdateRuleMetadataResponse'),
    newUpdateRuleMetadataResponse,

    -- ** UpdateRuleVersion
    UpdateRuleVersion (UpdateRuleVersion'),
    newUpdateRuleVersion,
    UpdateRuleVersionResponse (UpdateRuleVersionResponse'),
    newUpdateRuleVersionResponse,

    -- ** UpdateVariable
    UpdateVariable (UpdateVariable'),
    newUpdateVariable,
    UpdateVariableResponse (UpdateVariableResponse'),
    newUpdateVariableResponse,

    -- * Types

    -- ** AsyncJobStatus
    AsyncJobStatus (..),

    -- ** DataSource
    DataSource (..),

    -- ** DataType
    DataType (..),

    -- ** DetectorVersionStatus
    DetectorVersionStatus (..),

    -- ** EventIngestion
    EventIngestion (..),

    -- ** Language
    Language (..),

    -- ** ModelEndpointStatus
    ModelEndpointStatus (..),

    -- ** ModelInputDataFormat
    ModelInputDataFormat (..),

    -- ** ModelOutputDataFormat
    ModelOutputDataFormat (..),

    -- ** ModelSource
    ModelSource (..),

    -- ** ModelTypeEnum
    ModelTypeEnum (..),

    -- ** ModelVersionStatus
    ModelVersionStatus (..),

    -- ** RuleExecutionMode
    RuleExecutionMode (..),

    -- ** TrainingDataSourceEnum
    TrainingDataSourceEnum (..),

    -- ** UnlabeledEventsTreatment
    UnlabeledEventsTreatment (..),

    -- ** ATIMetricDataPoint
    ATIMetricDataPoint (ATIMetricDataPoint'),
    newATIMetricDataPoint,

    -- ** ATIModelPerformance
    ATIModelPerformance (ATIModelPerformance'),
    newATIModelPerformance,

    -- ** ATITrainingMetricsValue
    ATITrainingMetricsValue (ATITrainingMetricsValue'),
    newATITrainingMetricsValue,

    -- ** AggregatedLogOddsMetric
    AggregatedLogOddsMetric (AggregatedLogOddsMetric'),
    newAggregatedLogOddsMetric,

    -- ** AggregatedVariablesImpactExplanation
    AggregatedVariablesImpactExplanation (AggregatedVariablesImpactExplanation'),
    newAggregatedVariablesImpactExplanation,

    -- ** AggregatedVariablesImportanceMetrics
    AggregatedVariablesImportanceMetrics (AggregatedVariablesImportanceMetrics'),
    newAggregatedVariablesImportanceMetrics,

    -- ** BatchCreateVariableError
    BatchCreateVariableError (BatchCreateVariableError'),
    newBatchCreateVariableError,

    -- ** BatchGetVariableError
    BatchGetVariableError (BatchGetVariableError'),
    newBatchGetVariableError,

    -- ** BatchImport
    BatchImport (BatchImport'),
    newBatchImport,

    -- ** BatchPrediction
    BatchPrediction (BatchPrediction'),
    newBatchPrediction,

    -- ** DataValidationMetrics
    DataValidationMetrics (DataValidationMetrics'),
    newDataValidationMetrics,

    -- ** Detector
    Detector (Detector'),
    newDetector,

    -- ** DetectorVersionSummary
    DetectorVersionSummary (DetectorVersionSummary'),
    newDetectorVersionSummary,

    -- ** Entity
    Entity (Entity'),
    newEntity,

    -- ** EntityType
    EntityType (EntityType'),
    newEntityType,

    -- ** EvaluatedExternalModel
    EvaluatedExternalModel (EvaluatedExternalModel'),
    newEvaluatedExternalModel,

    -- ** EvaluatedModelVersion
    EvaluatedModelVersion (EvaluatedModelVersion'),
    newEvaluatedModelVersion,

    -- ** EvaluatedRule
    EvaluatedRule (EvaluatedRule'),
    newEvaluatedRule,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** EventPredictionSummary
    EventPredictionSummary (EventPredictionSummary'),
    newEventPredictionSummary,

    -- ** EventType
    EventType (EventType'),
    newEventType,

    -- ** EventVariableSummary
    EventVariableSummary (EventVariableSummary'),
    newEventVariableSummary,

    -- ** ExternalEventsDetail
    ExternalEventsDetail (ExternalEventsDetail'),
    newExternalEventsDetail,

    -- ** ExternalModel
    ExternalModel (ExternalModel'),
    newExternalModel,

    -- ** ExternalModelOutputs
    ExternalModelOutputs (ExternalModelOutputs'),
    newExternalModelOutputs,

    -- ** ExternalModelSummary
    ExternalModelSummary (ExternalModelSummary'),
    newExternalModelSummary,

    -- ** FieldValidationMessage
    FieldValidationMessage (FieldValidationMessage'),
    newFieldValidationMessage,

    -- ** FileValidationMessage
    FileValidationMessage (FileValidationMessage'),
    newFileValidationMessage,

    -- ** FilterCondition
    FilterCondition (FilterCondition'),
    newFilterCondition,

    -- ** IngestedEventStatistics
    IngestedEventStatistics (IngestedEventStatistics'),
    newIngestedEventStatistics,

    -- ** IngestedEventsDetail
    IngestedEventsDetail (IngestedEventsDetail'),
    newIngestedEventsDetail,

    -- ** IngestedEventsTimeWindow
    IngestedEventsTimeWindow (IngestedEventsTimeWindow'),
    newIngestedEventsTimeWindow,

    -- ** KMSKey
    KMSKey (KMSKey'),
    newKMSKey,

    -- ** Label
    Label (Label'),
    newLabel,

    -- ** LabelSchema
    LabelSchema (LabelSchema'),
    newLabelSchema,

    -- ** LogOddsMetric
    LogOddsMetric (LogOddsMetric'),
    newLogOddsMetric,

    -- ** MetricDataPoint
    MetricDataPoint (MetricDataPoint'),
    newMetricDataPoint,

    -- ** Model
    Model (Model'),
    newModel,

    -- ** ModelEndpointDataBlob
    ModelEndpointDataBlob (ModelEndpointDataBlob'),
    newModelEndpointDataBlob,

    -- ** ModelInputConfiguration
    ModelInputConfiguration (ModelInputConfiguration'),
    newModelInputConfiguration,

    -- ** ModelOutputConfiguration
    ModelOutputConfiguration (ModelOutputConfiguration'),
    newModelOutputConfiguration,

    -- ** ModelScores
    ModelScores (ModelScores'),
    newModelScores,

    -- ** ModelVersion
    ModelVersion (ModelVersion'),
    newModelVersion,

    -- ** ModelVersionDetail
    ModelVersionDetail (ModelVersionDetail'),
    newModelVersionDetail,

    -- ** ModelVersionEvaluation
    ModelVersionEvaluation (ModelVersionEvaluation'),
    newModelVersionEvaluation,

    -- ** OFIMetricDataPoint
    OFIMetricDataPoint (OFIMetricDataPoint'),
    newOFIMetricDataPoint,

    -- ** OFIModelPerformance
    OFIModelPerformance (OFIModelPerformance'),
    newOFIModelPerformance,

    -- ** OFITrainingMetricsValue
    OFITrainingMetricsValue (OFITrainingMetricsValue'),
    newOFITrainingMetricsValue,

    -- ** Outcome
    Outcome (Outcome'),
    newOutcome,

    -- ** PredictionExplanations
    PredictionExplanations (PredictionExplanations'),
    newPredictionExplanations,

    -- ** PredictionTimeRange
    PredictionTimeRange (PredictionTimeRange'),
    newPredictionTimeRange,

    -- ** Rule
    Rule (Rule'),
    newRule,

    -- ** RuleDetail
    RuleDetail (RuleDetail'),
    newRuleDetail,

    -- ** RuleResult
    RuleResult (RuleResult'),
    newRuleResult,

    -- ** TFIMetricDataPoint
    TFIMetricDataPoint (TFIMetricDataPoint'),
    newTFIMetricDataPoint,

    -- ** TFIModelPerformance
    TFIModelPerformance (TFIModelPerformance'),
    newTFIModelPerformance,

    -- ** TFITrainingMetricsValue
    TFITrainingMetricsValue (TFITrainingMetricsValue'),
    newTFITrainingMetricsValue,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TrainingDataSchema
    TrainingDataSchema (TrainingDataSchema'),
    newTrainingDataSchema,

    -- ** TrainingMetrics
    TrainingMetrics (TrainingMetrics'),
    newTrainingMetrics,

    -- ** TrainingMetricsV2
    TrainingMetricsV2 (TrainingMetricsV2'),
    newTrainingMetricsV2,

    -- ** TrainingResult
    TrainingResult (TrainingResult'),
    newTrainingResult,

    -- ** TrainingResultV2
    TrainingResultV2 (TrainingResultV2'),
    newTrainingResultV2,

    -- ** Variable
    Variable (Variable'),
    newVariable,

    -- ** VariableEntry
    VariableEntry (VariableEntry'),
    newVariableEntry,

    -- ** VariableImpactExplanation
    VariableImpactExplanation (VariableImpactExplanation'),
    newVariableImpactExplanation,

    -- ** VariableImportanceMetrics
    VariableImportanceMetrics (VariableImportanceMetrics'),
    newVariableImportanceMetrics,
  )
where

import Amazonka.FraudDetector.BatchCreateVariable
import Amazonka.FraudDetector.BatchGetVariable
import Amazonka.FraudDetector.CancelBatchImportJob
import Amazonka.FraudDetector.CancelBatchPredictionJob
import Amazonka.FraudDetector.CreateBatchImportJob
import Amazonka.FraudDetector.CreateBatchPredictionJob
import Amazonka.FraudDetector.CreateDetectorVersion
import Amazonka.FraudDetector.CreateModel
import Amazonka.FraudDetector.CreateModelVersion
import Amazonka.FraudDetector.CreateRule
import Amazonka.FraudDetector.CreateVariable
import Amazonka.FraudDetector.DeleteBatchImportJob
import Amazonka.FraudDetector.DeleteBatchPredictionJob
import Amazonka.FraudDetector.DeleteDetector
import Amazonka.FraudDetector.DeleteDetectorVersion
import Amazonka.FraudDetector.DeleteEntityType
import Amazonka.FraudDetector.DeleteEvent
import Amazonka.FraudDetector.DeleteEventType
import Amazonka.FraudDetector.DeleteEventsByEventType
import Amazonka.FraudDetector.DeleteExternalModel
import Amazonka.FraudDetector.DeleteLabel
import Amazonka.FraudDetector.DeleteModel
import Amazonka.FraudDetector.DeleteModelVersion
import Amazonka.FraudDetector.DeleteOutcome
import Amazonka.FraudDetector.DeleteRule
import Amazonka.FraudDetector.DeleteVariable
import Amazonka.FraudDetector.DescribeDetector
import Amazonka.FraudDetector.DescribeModelVersions
import Amazonka.FraudDetector.GetBatchImportJobs
import Amazonka.FraudDetector.GetBatchPredictionJobs
import Amazonka.FraudDetector.GetDeleteEventsByEventTypeStatus
import Amazonka.FraudDetector.GetDetectorVersion
import Amazonka.FraudDetector.GetDetectors
import Amazonka.FraudDetector.GetEntityTypes
import Amazonka.FraudDetector.GetEvent
import Amazonka.FraudDetector.GetEventPrediction
import Amazonka.FraudDetector.GetEventPredictionMetadata
import Amazonka.FraudDetector.GetEventTypes
import Amazonka.FraudDetector.GetExternalModels
import Amazonka.FraudDetector.GetKMSEncryptionKey
import Amazonka.FraudDetector.GetLabels
import Amazonka.FraudDetector.GetModelVersion
import Amazonka.FraudDetector.GetModels
import Amazonka.FraudDetector.GetOutcomes
import Amazonka.FraudDetector.GetRules
import Amazonka.FraudDetector.GetVariables
import Amazonka.FraudDetector.Lens
import Amazonka.FraudDetector.ListEventPredictions
import Amazonka.FraudDetector.ListTagsForResource
import Amazonka.FraudDetector.PutDetector
import Amazonka.FraudDetector.PutEntityType
import Amazonka.FraudDetector.PutEventType
import Amazonka.FraudDetector.PutExternalModel
import Amazonka.FraudDetector.PutKMSEncryptionKey
import Amazonka.FraudDetector.PutLabel
import Amazonka.FraudDetector.PutOutcome
import Amazonka.FraudDetector.SendEvent
import Amazonka.FraudDetector.TagResource
import Amazonka.FraudDetector.Types
import Amazonka.FraudDetector.UntagResource
import Amazonka.FraudDetector.UpdateDetectorVersion
import Amazonka.FraudDetector.UpdateDetectorVersionMetadata
import Amazonka.FraudDetector.UpdateDetectorVersionStatus
import Amazonka.FraudDetector.UpdateEventLabel
import Amazonka.FraudDetector.UpdateModel
import Amazonka.FraudDetector.UpdateModelVersion
import Amazonka.FraudDetector.UpdateModelVersionStatus
import Amazonka.FraudDetector.UpdateRuleMetadata
import Amazonka.FraudDetector.UpdateRuleVersion
import Amazonka.FraudDetector.UpdateVariable
import Amazonka.FraudDetector.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'FraudDetector'.

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
