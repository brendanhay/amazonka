{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Omics.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _RangeNotSatisfiableException,
    _RequestTimeoutException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AnnotationType
    AnnotationType (..),

    -- * EncryptionType
    EncryptionType (..),

    -- * FileType
    FileType (..),

    -- * FormatToHeaderKey
    FormatToHeaderKey (..),

    -- * JobStatus
    JobStatus (..),

    -- * ReadSetActivationJobItemStatus
    ReadSetActivationJobItemStatus (..),

    -- * ReadSetActivationJobStatus
    ReadSetActivationJobStatus (..),

    -- * ReadSetExportJobItemStatus
    ReadSetExportJobItemStatus (..),

    -- * ReadSetExportJobStatus
    ReadSetExportJobStatus (..),

    -- * ReadSetFile
    ReadSetFile (..),

    -- * ReadSetImportJobItemStatus
    ReadSetImportJobItemStatus (..),

    -- * ReadSetImportJobStatus
    ReadSetImportJobStatus (..),

    -- * ReadSetStatus
    ReadSetStatus (..),

    -- * ReferenceFile
    ReferenceFile (..),

    -- * ReferenceImportJobItemStatus
    ReferenceImportJobItemStatus (..),

    -- * ReferenceImportJobStatus
    ReferenceImportJobStatus (..),

    -- * ReferenceStatus
    ReferenceStatus (..),

    -- * RunExport
    RunExport (..),

    -- * RunLogLevel
    RunLogLevel (..),

    -- * RunStatus
    RunStatus (..),

    -- * SchemaValueType
    SchemaValueType (..),

    -- * StoreFormat
    StoreFormat (..),

    -- * StoreStatus
    StoreStatus (..),

    -- * TaskStatus
    TaskStatus (..),

    -- * WorkflowEngine
    WorkflowEngine (..),

    -- * WorkflowExport
    WorkflowExport (..),

    -- * WorkflowStatus
    WorkflowStatus (..),

    -- * WorkflowType
    WorkflowType (..),

    -- * ActivateReadSetFilter
    ActivateReadSetFilter (..),
    newActivateReadSetFilter,
    activateReadSetFilter_createdAfter,
    activateReadSetFilter_createdBefore,
    activateReadSetFilter_status,

    -- * ActivateReadSetJobItem
    ActivateReadSetJobItem (..),
    newActivateReadSetJobItem,
    activateReadSetJobItem_completionTime,
    activateReadSetJobItem_creationTime,
    activateReadSetJobItem_id,
    activateReadSetJobItem_sequenceStoreId,
    activateReadSetJobItem_status,

    -- * ActivateReadSetSourceItem
    ActivateReadSetSourceItem (..),
    newActivateReadSetSourceItem,
    activateReadSetSourceItem_statusMessage,
    activateReadSetSourceItem_readSetId,
    activateReadSetSourceItem_status,

    -- * AnnotationImportItemDetail
    AnnotationImportItemDetail (..),
    newAnnotationImportItemDetail,
    annotationImportItemDetail_jobStatus,
    annotationImportItemDetail_source,

    -- * AnnotationImportItemSource
    AnnotationImportItemSource (..),
    newAnnotationImportItemSource,
    annotationImportItemSource_source,

    -- * AnnotationImportJobItem
    AnnotationImportJobItem (..),
    newAnnotationImportJobItem,
    annotationImportJobItem_completionTime,
    annotationImportJobItem_runLeftNormalization,
    annotationImportJobItem_creationTime,
    annotationImportJobItem_destinationName,
    annotationImportJobItem_id,
    annotationImportJobItem_roleArn,
    annotationImportJobItem_status,
    annotationImportJobItem_updateTime,

    -- * AnnotationStoreItem
    AnnotationStoreItem (..),
    newAnnotationStoreItem,
    annotationStoreItem_creationTime,
    annotationStoreItem_description,
    annotationStoreItem_id,
    annotationStoreItem_name,
    annotationStoreItem_reference,
    annotationStoreItem_sseConfig,
    annotationStoreItem_status,
    annotationStoreItem_statusMessage,
    annotationStoreItem_storeArn,
    annotationStoreItem_storeFormat,
    annotationStoreItem_storeSizeBytes,
    annotationStoreItem_updateTime,

    -- * ExportReadSet
    ExportReadSet (..),
    newExportReadSet,
    exportReadSet_readSetId,

    -- * ExportReadSetDetail
    ExportReadSetDetail (..),
    newExportReadSetDetail,
    exportReadSetDetail_statusMessage,
    exportReadSetDetail_id,
    exportReadSetDetail_status,

    -- * ExportReadSetFilter
    ExportReadSetFilter (..),
    newExportReadSetFilter,
    exportReadSetFilter_createdAfter,
    exportReadSetFilter_createdBefore,
    exportReadSetFilter_status,

    -- * ExportReadSetJobDetail
    ExportReadSetJobDetail (..),
    newExportReadSetJobDetail,
    exportReadSetJobDetail_completionTime,
    exportReadSetJobDetail_creationTime,
    exportReadSetJobDetail_destination,
    exportReadSetJobDetail_id,
    exportReadSetJobDetail_sequenceStoreId,
    exportReadSetJobDetail_status,

    -- * FileInformation
    FileInformation (..),
    newFileInformation,
    fileInformation_contentLength,
    fileInformation_partSize,
    fileInformation_totalParts,

    -- * FormatOptions
    FormatOptions (..),
    newFormatOptions,
    formatOptions_tsvOptions,
    formatOptions_vcfOptions,

    -- * ImportReadSetFilter
    ImportReadSetFilter (..),
    newImportReadSetFilter,
    importReadSetFilter_createdAfter,
    importReadSetFilter_createdBefore,
    importReadSetFilter_status,

    -- * ImportReadSetJobItem
    ImportReadSetJobItem (..),
    newImportReadSetJobItem,
    importReadSetJobItem_completionTime,
    importReadSetJobItem_creationTime,
    importReadSetJobItem_id,
    importReadSetJobItem_roleArn,
    importReadSetJobItem_sequenceStoreId,
    importReadSetJobItem_status,

    -- * ImportReadSetSourceItem
    ImportReadSetSourceItem (..),
    newImportReadSetSourceItem,
    importReadSetSourceItem_description,
    importReadSetSourceItem_generatedFrom,
    importReadSetSourceItem_name,
    importReadSetSourceItem_referenceArn,
    importReadSetSourceItem_statusMessage,
    importReadSetSourceItem_tags,
    importReadSetSourceItem_sampleId,
    importReadSetSourceItem_sourceFileType,
    importReadSetSourceItem_sourceFiles,
    importReadSetSourceItem_status,
    importReadSetSourceItem_subjectId,

    -- * ImportReferenceFilter
    ImportReferenceFilter (..),
    newImportReferenceFilter,
    importReferenceFilter_createdAfter,
    importReferenceFilter_createdBefore,
    importReferenceFilter_status,

    -- * ImportReferenceJobItem
    ImportReferenceJobItem (..),
    newImportReferenceJobItem,
    importReferenceJobItem_completionTime,
    importReferenceJobItem_creationTime,
    importReferenceJobItem_id,
    importReferenceJobItem_referenceStoreId,
    importReferenceJobItem_roleArn,
    importReferenceJobItem_status,

    -- * ImportReferenceSourceItem
    ImportReferenceSourceItem (..),
    newImportReferenceSourceItem,
    importReferenceSourceItem_description,
    importReferenceSourceItem_name,
    importReferenceSourceItem_sourceFile,
    importReferenceSourceItem_statusMessage,
    importReferenceSourceItem_tags,
    importReferenceSourceItem_status,

    -- * ListAnnotationImportJobsFilter
    ListAnnotationImportJobsFilter (..),
    newListAnnotationImportJobsFilter,
    listAnnotationImportJobsFilter_status,
    listAnnotationImportJobsFilter_storeName,

    -- * ListAnnotationStoresFilter
    ListAnnotationStoresFilter (..),
    newListAnnotationStoresFilter,
    listAnnotationStoresFilter_status,

    -- * ListVariantImportJobsFilter
    ListVariantImportJobsFilter (..),
    newListVariantImportJobsFilter,
    listVariantImportJobsFilter_status,
    listVariantImportJobsFilter_storeName,

    -- * ListVariantStoresFilter
    ListVariantStoresFilter (..),
    newListVariantStoresFilter,
    listVariantStoresFilter_status,

    -- * ReadOptions
    ReadOptions (..),
    newReadOptions,
    readOptions_comment,
    readOptions_encoding,
    readOptions_escape,
    readOptions_escapeQuotes,
    readOptions_header,
    readOptions_lineSep,
    readOptions_quote,
    readOptions_quoteAll,
    readOptions_sep,

    -- * ReadSetBatchError
    ReadSetBatchError (..),
    newReadSetBatchError,
    readSetBatchError_code,
    readSetBatchError_id,
    readSetBatchError_message,

    -- * ReadSetFiles
    ReadSetFiles (..),
    newReadSetFiles,
    readSetFiles_index,
    readSetFiles_source1,
    readSetFiles_source2,

    -- * ReadSetFilter
    ReadSetFilter (..),
    newReadSetFilter,
    readSetFilter_createdAfter,
    readSetFilter_createdBefore,
    readSetFilter_name,
    readSetFilter_referenceArn,
    readSetFilter_status,

    -- * ReadSetListItem
    ReadSetListItem (..),
    newReadSetListItem,
    readSetListItem_description,
    readSetListItem_name,
    readSetListItem_referenceArn,
    readSetListItem_sampleId,
    readSetListItem_sequenceInformation,
    readSetListItem_subjectId,
    readSetListItem_arn,
    readSetListItem_creationTime,
    readSetListItem_fileType,
    readSetListItem_id,
    readSetListItem_sequenceStoreId,
    readSetListItem_status,

    -- * ReferenceFiles
    ReferenceFiles (..),
    newReferenceFiles,
    referenceFiles_index,
    referenceFiles_source,

    -- * ReferenceFilter
    ReferenceFilter (..),
    newReferenceFilter,
    referenceFilter_createdAfter,
    referenceFilter_createdBefore,
    referenceFilter_md5,
    referenceFilter_name,

    -- * ReferenceItem
    ReferenceItem (..),
    newReferenceItem,
    referenceItem_referenceArn,

    -- * ReferenceListItem
    ReferenceListItem (..),
    newReferenceListItem,
    referenceListItem_description,
    referenceListItem_name,
    referenceListItem_status,
    referenceListItem_arn,
    referenceListItem_creationTime,
    referenceListItem_id,
    referenceListItem_md5,
    referenceListItem_referenceStoreId,
    referenceListItem_updateTime,

    -- * ReferenceStoreDetail
    ReferenceStoreDetail (..),
    newReferenceStoreDetail,
    referenceStoreDetail_description,
    referenceStoreDetail_name,
    referenceStoreDetail_sseConfig,
    referenceStoreDetail_arn,
    referenceStoreDetail_creationTime,
    referenceStoreDetail_id,

    -- * ReferenceStoreFilter
    ReferenceStoreFilter (..),
    newReferenceStoreFilter,
    referenceStoreFilter_createdAfter,
    referenceStoreFilter_createdBefore,
    referenceStoreFilter_name,

    -- * RunGroupListItem
    RunGroupListItem (..),
    newRunGroupListItem,
    runGroupListItem_arn,
    runGroupListItem_creationTime,
    runGroupListItem_id,
    runGroupListItem_maxCpus,
    runGroupListItem_maxDuration,
    runGroupListItem_maxRuns,
    runGroupListItem_name,

    -- * RunListItem
    RunListItem (..),
    newRunListItem,
    runListItem_arn,
    runListItem_creationTime,
    runListItem_id,
    runListItem_name,
    runListItem_priority,
    runListItem_startTime,
    runListItem_status,
    runListItem_stopTime,
    runListItem_storageCapacity,
    runListItem_workflowId,

    -- * RunParameters
    RunParameters (..),
    newRunParameters,

    -- * SequenceInformation
    SequenceInformation (..),
    newSequenceInformation,
    sequenceInformation_alignment,
    sequenceInformation_generatedFrom,
    sequenceInformation_totalBaseCount,
    sequenceInformation_totalReadCount,

    -- * SequenceStoreDetail
    SequenceStoreDetail (..),
    newSequenceStoreDetail,
    sequenceStoreDetail_description,
    sequenceStoreDetail_name,
    sequenceStoreDetail_sseConfig,
    sequenceStoreDetail_arn,
    sequenceStoreDetail_creationTime,
    sequenceStoreDetail_id,

    -- * SequenceStoreFilter
    SequenceStoreFilter (..),
    newSequenceStoreFilter,
    sequenceStoreFilter_createdAfter,
    sequenceStoreFilter_createdBefore,
    sequenceStoreFilter_name,

    -- * SourceFiles
    SourceFiles (..),
    newSourceFiles,
    sourceFiles_source2,
    sourceFiles_source1,

    -- * SseConfig
    SseConfig (..),
    newSseConfig,
    sseConfig_keyArn,
    sseConfig_type,

    -- * StartReadSetActivationJobSourceItem
    StartReadSetActivationJobSourceItem (..),
    newStartReadSetActivationJobSourceItem,
    startReadSetActivationJobSourceItem_readSetId,

    -- * StartReadSetImportJobSourceItem
    StartReadSetImportJobSourceItem (..),
    newStartReadSetImportJobSourceItem,
    startReadSetImportJobSourceItem_description,
    startReadSetImportJobSourceItem_generatedFrom,
    startReadSetImportJobSourceItem_name,
    startReadSetImportJobSourceItem_tags,
    startReadSetImportJobSourceItem_referenceArn,
    startReadSetImportJobSourceItem_sampleId,
    startReadSetImportJobSourceItem_sourceFileType,
    startReadSetImportJobSourceItem_sourceFiles,
    startReadSetImportJobSourceItem_subjectId,

    -- * StartReferenceImportJobSourceItem
    StartReferenceImportJobSourceItem (..),
    newStartReferenceImportJobSourceItem,
    startReferenceImportJobSourceItem_description,
    startReferenceImportJobSourceItem_tags,
    startReferenceImportJobSourceItem_name,
    startReferenceImportJobSourceItem_sourceFile,

    -- * StoreOptions
    StoreOptions (..),
    newStoreOptions,
    storeOptions_tsvStoreOptions,

    -- * TaskListItem
    TaskListItem (..),
    newTaskListItem,
    taskListItem_cpus,
    taskListItem_creationTime,
    taskListItem_memory,
    taskListItem_name,
    taskListItem_startTime,
    taskListItem_status,
    taskListItem_stopTime,
    taskListItem_taskId,

    -- * TsvOptions
    TsvOptions (..),
    newTsvOptions,
    tsvOptions_readOptions,

    -- * TsvStoreOptions
    TsvStoreOptions (..),
    newTsvStoreOptions,
    tsvStoreOptions_annotationType,
    tsvStoreOptions_formatToHeader,
    tsvStoreOptions_schema,

    -- * VariantImportItemDetail
    VariantImportItemDetail (..),
    newVariantImportItemDetail,
    variantImportItemDetail_jobStatus,
    variantImportItemDetail_source,

    -- * VariantImportItemSource
    VariantImportItemSource (..),
    newVariantImportItemSource,
    variantImportItemSource_source,

    -- * VariantImportJobItem
    VariantImportJobItem (..),
    newVariantImportJobItem,
    variantImportJobItem_completionTime,
    variantImportJobItem_runLeftNormalization,
    variantImportJobItem_creationTime,
    variantImportJobItem_destinationName,
    variantImportJobItem_id,
    variantImportJobItem_roleArn,
    variantImportJobItem_status,
    variantImportJobItem_updateTime,

    -- * VariantStoreItem
    VariantStoreItem (..),
    newVariantStoreItem,
    variantStoreItem_creationTime,
    variantStoreItem_description,
    variantStoreItem_id,
    variantStoreItem_name,
    variantStoreItem_reference,
    variantStoreItem_sseConfig,
    variantStoreItem_status,
    variantStoreItem_statusMessage,
    variantStoreItem_storeArn,
    variantStoreItem_storeSizeBytes,
    variantStoreItem_updateTime,

    -- * VcfOptions
    VcfOptions (..),
    newVcfOptions,
    vcfOptions_ignoreFilterField,
    vcfOptions_ignoreQualField,

    -- * WorkflowListItem
    WorkflowListItem (..),
    newWorkflowListItem,
    workflowListItem_arn,
    workflowListItem_creationTime,
    workflowListItem_digest,
    workflowListItem_id,
    workflowListItem_name,
    workflowListItem_status,
    workflowListItem_type,

    -- * WorkflowParameter
    WorkflowParameter (..),
    newWorkflowParameter,
    workflowParameter_description,
    workflowParameter_optional,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Omics.Types.ActivateReadSetFilter
import Amazonka.Omics.Types.ActivateReadSetJobItem
import Amazonka.Omics.Types.ActivateReadSetSourceItem
import Amazonka.Omics.Types.AnnotationImportItemDetail
import Amazonka.Omics.Types.AnnotationImportItemSource
import Amazonka.Omics.Types.AnnotationImportJobItem
import Amazonka.Omics.Types.AnnotationStoreItem
import Amazonka.Omics.Types.AnnotationType
import Amazonka.Omics.Types.EncryptionType
import Amazonka.Omics.Types.ExportReadSet
import Amazonka.Omics.Types.ExportReadSetDetail
import Amazonka.Omics.Types.ExportReadSetFilter
import Amazonka.Omics.Types.ExportReadSetJobDetail
import Amazonka.Omics.Types.FileInformation
import Amazonka.Omics.Types.FileType
import Amazonka.Omics.Types.FormatOptions
import Amazonka.Omics.Types.FormatToHeaderKey
import Amazonka.Omics.Types.ImportReadSetFilter
import Amazonka.Omics.Types.ImportReadSetJobItem
import Amazonka.Omics.Types.ImportReadSetSourceItem
import Amazonka.Omics.Types.ImportReferenceFilter
import Amazonka.Omics.Types.ImportReferenceJobItem
import Amazonka.Omics.Types.ImportReferenceSourceItem
import Amazonka.Omics.Types.JobStatus
import Amazonka.Omics.Types.ListAnnotationImportJobsFilter
import Amazonka.Omics.Types.ListAnnotationStoresFilter
import Amazonka.Omics.Types.ListVariantImportJobsFilter
import Amazonka.Omics.Types.ListVariantStoresFilter
import Amazonka.Omics.Types.ReadOptions
import Amazonka.Omics.Types.ReadSetActivationJobItemStatus
import Amazonka.Omics.Types.ReadSetActivationJobStatus
import Amazonka.Omics.Types.ReadSetBatchError
import Amazonka.Omics.Types.ReadSetExportJobItemStatus
import Amazonka.Omics.Types.ReadSetExportJobStatus
import Amazonka.Omics.Types.ReadSetFile
import Amazonka.Omics.Types.ReadSetFiles
import Amazonka.Omics.Types.ReadSetFilter
import Amazonka.Omics.Types.ReadSetImportJobItemStatus
import Amazonka.Omics.Types.ReadSetImportJobStatus
import Amazonka.Omics.Types.ReadSetListItem
import Amazonka.Omics.Types.ReadSetStatus
import Amazonka.Omics.Types.ReferenceFile
import Amazonka.Omics.Types.ReferenceFiles
import Amazonka.Omics.Types.ReferenceFilter
import Amazonka.Omics.Types.ReferenceImportJobItemStatus
import Amazonka.Omics.Types.ReferenceImportJobStatus
import Amazonka.Omics.Types.ReferenceItem
import Amazonka.Omics.Types.ReferenceListItem
import Amazonka.Omics.Types.ReferenceStatus
import Amazonka.Omics.Types.ReferenceStoreDetail
import Amazonka.Omics.Types.ReferenceStoreFilter
import Amazonka.Omics.Types.RunExport
import Amazonka.Omics.Types.RunGroupListItem
import Amazonka.Omics.Types.RunListItem
import Amazonka.Omics.Types.RunLogLevel
import Amazonka.Omics.Types.RunParameters
import Amazonka.Omics.Types.RunStatus
import Amazonka.Omics.Types.SchemaValueType
import Amazonka.Omics.Types.SequenceInformation
import Amazonka.Omics.Types.SequenceStoreDetail
import Amazonka.Omics.Types.SequenceStoreFilter
import Amazonka.Omics.Types.SourceFiles
import Amazonka.Omics.Types.SseConfig
import Amazonka.Omics.Types.StartReadSetActivationJobSourceItem
import Amazonka.Omics.Types.StartReadSetImportJobSourceItem
import Amazonka.Omics.Types.StartReferenceImportJobSourceItem
import Amazonka.Omics.Types.StoreFormat
import Amazonka.Omics.Types.StoreOptions
import Amazonka.Omics.Types.StoreStatus
import Amazonka.Omics.Types.TaskListItem
import Amazonka.Omics.Types.TaskStatus
import Amazonka.Omics.Types.TsvOptions
import Amazonka.Omics.Types.TsvStoreOptions
import Amazonka.Omics.Types.VariantImportItemDetail
import Amazonka.Omics.Types.VariantImportItemSource
import Amazonka.Omics.Types.VariantImportJobItem
import Amazonka.Omics.Types.VariantStoreItem
import Amazonka.Omics.Types.VcfOptions
import Amazonka.Omics.Types.WorkflowEngine
import Amazonka.Omics.Types.WorkflowExport
import Amazonka.Omics.Types.WorkflowListItem
import Amazonka.Omics.Types.WorkflowParameter
import Amazonka.Omics.Types.WorkflowStatus
import Amazonka.Omics.Types.WorkflowType
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2022-11-28@ of the Amazon Omics SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Omics",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "omics",
      Core.signingName = "omics",
      Core.version = "2022-11-28",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Omics",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request cannot be applied to the target resource in its current
-- state.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An unexpected error occurred. Try the request again.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The ranges specified in the request are not valid.
_RangeNotSatisfiableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RangeNotSatisfiableException =
  Core._MatchServiceError
    defaultService
    "RangeNotSatisfiableException"
    Prelude.. Core.hasStatus 416

-- | The request timed out.
_RequestTimeoutException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RequestTimeoutException =
  Core._MatchServiceError
    defaultService
    "RequestTimeoutException"
    Prelude.. Core.hasStatus 408

-- | The target resource was not found in the current Region.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request exceeds a service quota.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
