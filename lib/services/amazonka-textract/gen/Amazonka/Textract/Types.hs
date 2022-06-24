{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Textract.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidKMSKeyException,
    _AccessDeniedException,
    _ProvisionedThroughputExceededException,
    _BadDocumentException,
    _HumanLoopQuotaExceededException,
    _LimitExceededException,
    _DocumentTooLargeException,
    _InternalServerError,
    _ThrottlingException,
    _InvalidS3ObjectException,
    _UnsupportedDocumentException,
    _InvalidJobIdException,
    _IdempotentParameterMismatchException,
    _InvalidParameterException,

    -- * BlockType
    BlockType (..),

    -- * ContentClassifier
    ContentClassifier (..),

    -- * EntityType
    EntityType (..),

    -- * FeatureType
    FeatureType (..),

    -- * JobStatus
    JobStatus (..),

    -- * RelationshipType
    RelationshipType (..),

    -- * SelectionStatus
    SelectionStatus (..),

    -- * TextType
    TextType (..),

    -- * Block
    Block (..),
    newBlock,
    block_blockType,
    block_rowSpan,
    block_columnIndex,
    block_entityTypes,
    block_columnSpan,
    block_confidence,
    block_rowIndex,
    block_selectionStatus,
    block_id,
    block_page,
    block_textType,
    block_relationships,
    block_text,
    block_geometry,

    -- * BoundingBox
    BoundingBox (..),
    newBoundingBox,
    boundingBox_width,
    boundingBox_top,
    boundingBox_left,
    boundingBox_height,

    -- * Document
    Document (..),
    newDocument,
    document_bytes,
    document_s3Object,

    -- * DocumentLocation
    DocumentLocation (..),
    newDocumentLocation,
    documentLocation_s3Object,

    -- * DocumentMetadata
    DocumentMetadata (..),
    newDocumentMetadata,
    documentMetadata_pages,

    -- * ExpenseDetection
    ExpenseDetection (..),
    newExpenseDetection,
    expenseDetection_confidence,
    expenseDetection_text,
    expenseDetection_geometry,

    -- * ExpenseDocument
    ExpenseDocument (..),
    newExpenseDocument,
    expenseDocument_lineItemGroups,
    expenseDocument_summaryFields,
    expenseDocument_expenseIndex,

    -- * ExpenseField
    ExpenseField (..),
    newExpenseField,
    expenseField_type,
    expenseField_pageNumber,
    expenseField_labelDetection,
    expenseField_valueDetection,

    -- * ExpenseType
    ExpenseType (..),
    newExpenseType,
    expenseType_confidence,
    expenseType_text,

    -- * Geometry
    Geometry (..),
    newGeometry,
    geometry_polygon,
    geometry_boundingBox,

    -- * HumanLoopActivationOutput
    HumanLoopActivationOutput (..),
    newHumanLoopActivationOutput,
    humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults,
    humanLoopActivationOutput_humanLoopArn,
    humanLoopActivationOutput_humanLoopActivationReasons,

    -- * HumanLoopConfig
    HumanLoopConfig (..),
    newHumanLoopConfig,
    humanLoopConfig_dataAttributes,
    humanLoopConfig_humanLoopName,
    humanLoopConfig_flowDefinitionArn,

    -- * HumanLoopDataAttributes
    HumanLoopDataAttributes (..),
    newHumanLoopDataAttributes,
    humanLoopDataAttributes_contentClassifiers,

    -- * LineItemFields
    LineItemFields (..),
    newLineItemFields,
    lineItemFields_lineItemExpenseFields,

    -- * LineItemGroup
    LineItemGroup (..),
    newLineItemGroup,
    lineItemGroup_lineItems,
    lineItemGroup_lineItemGroupIndex,

    -- * NotificationChannel
    NotificationChannel (..),
    newNotificationChannel,
    notificationChannel_sNSTopicArn,
    notificationChannel_roleArn,

    -- * OutputConfig
    OutputConfig (..),
    newOutputConfig,
    outputConfig_s3Prefix,
    outputConfig_s3Bucket,

    -- * Point
    Point (..),
    newPoint,
    point_x,
    point_y,

    -- * Relationship
    Relationship (..),
    newRelationship,
    relationship_type,
    relationship_ids,

    -- * S3Object
    S3Object (..),
    newS3Object,
    s3Object_name,
    s3Object_bucket,
    s3Object_version,

    -- * Warning
    Warning (..),
    newWarning,
    warning_errorCode,
    warning_pages,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.Textract.Types.Block
import Amazonka.Textract.Types.BlockType
import Amazonka.Textract.Types.BoundingBox
import Amazonka.Textract.Types.ContentClassifier
import Amazonka.Textract.Types.Document
import Amazonka.Textract.Types.DocumentLocation
import Amazonka.Textract.Types.DocumentMetadata
import Amazonka.Textract.Types.EntityType
import Amazonka.Textract.Types.ExpenseDetection
import Amazonka.Textract.Types.ExpenseDocument
import Amazonka.Textract.Types.ExpenseField
import Amazonka.Textract.Types.ExpenseType
import Amazonka.Textract.Types.FeatureType
import Amazonka.Textract.Types.Geometry
import Amazonka.Textract.Types.HumanLoopActivationOutput
import Amazonka.Textract.Types.HumanLoopConfig
import Amazonka.Textract.Types.HumanLoopDataAttributes
import Amazonka.Textract.Types.JobStatus
import Amazonka.Textract.Types.LineItemFields
import Amazonka.Textract.Types.LineItemGroup
import Amazonka.Textract.Types.NotificationChannel
import Amazonka.Textract.Types.OutputConfig
import Amazonka.Textract.Types.Point
import Amazonka.Textract.Types.Relationship
import Amazonka.Textract.Types.RelationshipType
import Amazonka.Textract.Types.S3Object
import Amazonka.Textract.Types.SelectionStatus
import Amazonka.Textract.Types.TextType
import Amazonka.Textract.Types.Warning

-- | API version @2018-06-27@ of the Amazon Textract SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Textract",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "textract",
      Core._serviceSigningName = "textract",
      Core._serviceVersion = "2018-06-27",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Textract",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Indicates you do not have decrypt permissions with the KMS key entered,
-- or the KMS key was entered incorrectly.
_InvalidKMSKeyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidKMSKeyException =
  Core._MatchServiceError
    defaultService
    "InvalidKMSKeyException"

-- | You aren\'t authorized to perform the action. Use the Amazon Resource
-- Name (ARN) of an authorized user or IAM role to perform the operation.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The number of requests exceeded your throughput limit. If you want to
-- increase this limit, contact Amazon Textract.
_ProvisionedThroughputExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ProvisionedThroughputExceededException =
  Core._MatchServiceError
    defaultService
    "ProvisionedThroughputExceededException"

-- | Amazon Textract isn\'t able to read the document. For more information
-- on the document limits in Amazon Textract, see limits.
_BadDocumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadDocumentException =
  Core._MatchServiceError
    defaultService
    "BadDocumentException"

-- | Indicates you have exceeded the maximum number of active human in the
-- loop workflows available
_HumanLoopQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HumanLoopQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "HumanLoopQuotaExceededException"

-- | An Amazon Textract service limit was exceeded. For example, if you start
-- too many asynchronous jobs concurrently, calls to start operations
-- (@StartDocumentTextDetection@, for example) raise a
-- LimitExceededException exception (HTTP status code: 400) until the
-- number of concurrently running jobs is below the Amazon Textract service
-- limit.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The document can\'t be processed because it\'s too large. The maximum
-- document size for synchronous operations 10 MB. The maximum document
-- size for asynchronous operations is 500 MB for PDF files.
_DocumentTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentTooLargeException =
  Core._MatchServiceError
    defaultService
    "DocumentTooLargeException"

-- | Amazon Textract experienced a service issue. Try your call again.
_InternalServerError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | Amazon Textract is temporarily unable to process the request. Try your
-- call again.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | Amazon Textract is unable to access the S3 object that\'s specified in
-- the request. for more information,
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Configure Access to Amazon S3>
-- For troubleshooting information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/troubleshooting.html Troubleshooting Amazon S3>
_InvalidS3ObjectException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidS3ObjectException =
  Core._MatchServiceError
    defaultService
    "InvalidS3ObjectException"

-- | The format of the input document isn\'t supported. Documents for
-- synchronous operations can be in PNG or JPEG format. Documents for
-- asynchronous operations can also be in PDF format.
_UnsupportedDocumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedDocumentException =
  Core._MatchServiceError
    defaultService
    "UnsupportedDocumentException"

-- | An invalid job identifier was passed to GetDocumentAnalysis or to
-- GetDocumentAnalysis.
_InvalidJobIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidJobIdException =
  Core._MatchServiceError
    defaultService
    "InvalidJobIdException"

-- | A @ClientRequestToken@ input parameter was reused with an operation, but
-- at least one of the other input parameters is different from the
-- previous call to the operation.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"

-- | An input parameter violated a constraint. For example, in synchronous
-- operations, an @InvalidParameterException@ exception occurs when neither
-- of the @S3Object@ or @Bytes@ values are supplied in the @Document@
-- request parameter. Validate your parameter before calling the API
-- operation again.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
