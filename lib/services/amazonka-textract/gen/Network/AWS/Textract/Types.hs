{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Textract.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Textract.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadDocumentException,
    _DocumentTooLargeException,
    _HumanLoopQuotaExceededException,
    _InvalidS3ObjectException,
    _InternalServerError,
    _ThrottlingException,
    _InvalidParameterException,
    _AccessDeniedException,
    _InvalidKMSKeyException,
    _InvalidJobIdException,
    _LimitExceededException,
    _ProvisionedThroughputExceededException,
    _IdempotentParameterMismatchException,
    _UnsupportedDocumentException,

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
    block_relationships,
    block_selectionStatus,
    block_blockType,
    block_rowSpan,
    block_page,
    block_id,
    block_textType,
    block_columnSpan,
    block_rowIndex,
    block_confidence,
    block_columnIndex,
    block_entityTypes,
    block_text,
    block_geometry,

    -- * BoundingBox
    BoundingBox (..),
    newBoundingBox,
    boundingBox_height,
    boundingBox_width,
    boundingBox_left,
    boundingBox_top,

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
    expenseDocument_expenseIndex,
    expenseDocument_summaryFields,

    -- * ExpenseField
    ExpenseField (..),
    newExpenseField,
    expenseField_labelDetection,
    expenseField_valueDetection,
    expenseField_pageNumber,
    expenseField_type,

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
    humanLoopActivationOutput_humanLoopActivationReasons,
    humanLoopActivationOutput_humanLoopArn,
    humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults,

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
    point_y,
    point_x,

    -- * Relationship
    Relationship (..),
    newRelationship,
    relationship_ids,
    relationship_type,

    -- * S3Object
    S3Object (..),
    newS3Object,
    s3Object_version,
    s3Object_name,
    s3Object_bucket,

    -- * Warning
    Warning (..),
    newWarning,
    warning_pages,
    warning_errorCode,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Textract.Types.Block
import Network.AWS.Textract.Types.BlockType
import Network.AWS.Textract.Types.BoundingBox
import Network.AWS.Textract.Types.ContentClassifier
import Network.AWS.Textract.Types.Document
import Network.AWS.Textract.Types.DocumentLocation
import Network.AWS.Textract.Types.DocumentMetadata
import Network.AWS.Textract.Types.EntityType
import Network.AWS.Textract.Types.ExpenseDetection
import Network.AWS.Textract.Types.ExpenseDocument
import Network.AWS.Textract.Types.ExpenseField
import Network.AWS.Textract.Types.ExpenseType
import Network.AWS.Textract.Types.FeatureType
import Network.AWS.Textract.Types.Geometry
import Network.AWS.Textract.Types.HumanLoopActivationOutput
import Network.AWS.Textract.Types.HumanLoopConfig
import Network.AWS.Textract.Types.HumanLoopDataAttributes
import Network.AWS.Textract.Types.JobStatus
import Network.AWS.Textract.Types.LineItemFields
import Network.AWS.Textract.Types.LineItemGroup
import Network.AWS.Textract.Types.NotificationChannel
import Network.AWS.Textract.Types.OutputConfig
import Network.AWS.Textract.Types.Point
import Network.AWS.Textract.Types.Relationship
import Network.AWS.Textract.Types.RelationshipType
import Network.AWS.Textract.Types.S3Object
import Network.AWS.Textract.Types.SelectionStatus
import Network.AWS.Textract.Types.TextType
import Network.AWS.Textract.Types.Warning

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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Amazon Textract isn\'t able to read the document. For more information
-- on the document limits in Amazon Textract, see limits.
_BadDocumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadDocumentException =
  Core._MatchServiceError
    defaultService
    "BadDocumentException"

-- | The document can\'t be processed because it\'s too large. The maximum
-- document size for synchronous operations 10 MB. The maximum document
-- size for asynchronous operations is 500 MB for PDF files.
_DocumentTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DocumentTooLargeException =
  Core._MatchServiceError
    defaultService
    "DocumentTooLargeException"

-- | Indicates you have exceeded the maximum number of active human in the
-- loop workflows available
_HumanLoopQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HumanLoopQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "HumanLoopQuotaExceededException"

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

-- | You aren\'t authorized to perform the action. Use the Amazon Resource
-- Name (ARN) of an authorized user or IAM role to perform the operation.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Indicates you do not have decrypt permissions with the KMS key entered,
-- or the KMS key was entered incorrectly.
_InvalidKMSKeyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidKMSKeyException =
  Core._MatchServiceError
    defaultService
    "InvalidKMSKeyException"

-- | An invalid job identifier was passed to GetDocumentAnalysis or to
-- GetDocumentAnalysis.
_InvalidJobIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidJobIdException =
  Core._MatchServiceError
    defaultService
    "InvalidJobIdException"

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

-- | The number of requests exceeded your throughput limit. If you want to
-- increase this limit, contact Amazon Textract.
_ProvisionedThroughputExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ProvisionedThroughputExceededException =
  Core._MatchServiceError
    defaultService
    "ProvisionedThroughputExceededException"

-- | A @ClientRequestToken@ input parameter was reused with an operation, but
-- at least one of the other input parameters is different from the
-- previous call to the operation.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"

-- | The format of the input document isn\'t supported. Documents for
-- synchronous operations can be in PNG or JPEG format. Documents for
-- asynchronous operations can also be in PDF format.
_UnsupportedDocumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedDocumentException =
  Core._MatchServiceError
    defaultService
    "UnsupportedDocumentException"
