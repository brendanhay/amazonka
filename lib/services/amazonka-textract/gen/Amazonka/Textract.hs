{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Textract
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-06-27@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Textract detects and analyzes text in documents and converts it
-- into machine-readable text. This is the API reference documentation for
-- Amazon Textract.
module Amazonka.Textract
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BadDocumentException
    _BadDocumentException,

    -- ** DocumentTooLargeException
    _DocumentTooLargeException,

    -- ** HumanLoopQuotaExceededException
    _HumanLoopQuotaExceededException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** InvalidJobIdException
    _InvalidJobIdException,

    -- ** InvalidKMSKeyException
    _InvalidKMSKeyException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidS3ObjectException
    _InvalidS3ObjectException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ProvisionedThroughputExceededException
    _ProvisionedThroughputExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** UnsupportedDocumentException
    _UnsupportedDocumentException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AnalyzeDocument
    AnalyzeDocument (AnalyzeDocument'),
    newAnalyzeDocument,
    AnalyzeDocumentResponse (AnalyzeDocumentResponse'),
    newAnalyzeDocumentResponse,

    -- ** AnalyzeExpense
    AnalyzeExpense (AnalyzeExpense'),
    newAnalyzeExpense,
    AnalyzeExpenseResponse (AnalyzeExpenseResponse'),
    newAnalyzeExpenseResponse,

    -- ** AnalyzeID
    AnalyzeID (AnalyzeID'),
    newAnalyzeID,
    AnalyzeIDResponse (AnalyzeIDResponse'),
    newAnalyzeIDResponse,

    -- ** DetectDocumentText
    DetectDocumentText (DetectDocumentText'),
    newDetectDocumentText,
    DetectDocumentTextResponse (DetectDocumentTextResponse'),
    newDetectDocumentTextResponse,

    -- ** GetDocumentAnalysis
    GetDocumentAnalysis (GetDocumentAnalysis'),
    newGetDocumentAnalysis,
    GetDocumentAnalysisResponse (GetDocumentAnalysisResponse'),
    newGetDocumentAnalysisResponse,

    -- ** GetDocumentTextDetection
    GetDocumentTextDetection (GetDocumentTextDetection'),
    newGetDocumentTextDetection,
    GetDocumentTextDetectionResponse (GetDocumentTextDetectionResponse'),
    newGetDocumentTextDetectionResponse,

    -- ** GetExpenseAnalysis
    GetExpenseAnalysis (GetExpenseAnalysis'),
    newGetExpenseAnalysis,
    GetExpenseAnalysisResponse (GetExpenseAnalysisResponse'),
    newGetExpenseAnalysisResponse,

    -- ** GetLendingAnalysis
    GetLendingAnalysis (GetLendingAnalysis'),
    newGetLendingAnalysis,
    GetLendingAnalysisResponse (GetLendingAnalysisResponse'),
    newGetLendingAnalysisResponse,

    -- ** GetLendingAnalysisSummary
    GetLendingAnalysisSummary (GetLendingAnalysisSummary'),
    newGetLendingAnalysisSummary,
    GetLendingAnalysisSummaryResponse (GetLendingAnalysisSummaryResponse'),
    newGetLendingAnalysisSummaryResponse,

    -- ** StartDocumentAnalysis
    StartDocumentAnalysis (StartDocumentAnalysis'),
    newStartDocumentAnalysis,
    StartDocumentAnalysisResponse (StartDocumentAnalysisResponse'),
    newStartDocumentAnalysisResponse,

    -- ** StartDocumentTextDetection
    StartDocumentTextDetection (StartDocumentTextDetection'),
    newStartDocumentTextDetection,
    StartDocumentTextDetectionResponse (StartDocumentTextDetectionResponse'),
    newStartDocumentTextDetectionResponse,

    -- ** StartExpenseAnalysis
    StartExpenseAnalysis (StartExpenseAnalysis'),
    newStartExpenseAnalysis,
    StartExpenseAnalysisResponse (StartExpenseAnalysisResponse'),
    newStartExpenseAnalysisResponse,

    -- ** StartLendingAnalysis
    StartLendingAnalysis (StartLendingAnalysis'),
    newStartLendingAnalysis,
    StartLendingAnalysisResponse (StartLendingAnalysisResponse'),
    newStartLendingAnalysisResponse,

    -- * Types

    -- ** BlockType
    BlockType (..),

    -- ** ContentClassifier
    ContentClassifier (..),

    -- ** EntityType
    EntityType (..),

    -- ** FeatureType
    FeatureType (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** RelationshipType
    RelationshipType (..),

    -- ** SelectionStatus
    SelectionStatus (..),

    -- ** TextType
    TextType (..),

    -- ** ValueType
    ValueType (..),

    -- ** AnalyzeIDDetections
    AnalyzeIDDetections (AnalyzeIDDetections'),
    newAnalyzeIDDetections,

    -- ** Block
    Block (Block'),
    newBlock,

    -- ** BoundingBox
    BoundingBox (BoundingBox'),
    newBoundingBox,

    -- ** DetectedSignature
    DetectedSignature (DetectedSignature'),
    newDetectedSignature,

    -- ** Document
    Document (Document'),
    newDocument,

    -- ** DocumentGroup
    DocumentGroup (DocumentGroup'),
    newDocumentGroup,

    -- ** DocumentLocation
    DocumentLocation (DocumentLocation'),
    newDocumentLocation,

    -- ** DocumentMetadata
    DocumentMetadata (DocumentMetadata'),
    newDocumentMetadata,

    -- ** ExpenseCurrency
    ExpenseCurrency (ExpenseCurrency'),
    newExpenseCurrency,

    -- ** ExpenseDetection
    ExpenseDetection (ExpenseDetection'),
    newExpenseDetection,

    -- ** ExpenseDocument
    ExpenseDocument (ExpenseDocument'),
    newExpenseDocument,

    -- ** ExpenseField
    ExpenseField (ExpenseField'),
    newExpenseField,

    -- ** ExpenseGroupProperty
    ExpenseGroupProperty (ExpenseGroupProperty'),
    newExpenseGroupProperty,

    -- ** ExpenseType
    ExpenseType (ExpenseType'),
    newExpenseType,

    -- ** Extraction
    Extraction (Extraction'),
    newExtraction,

    -- ** Geometry
    Geometry (Geometry'),
    newGeometry,

    -- ** HumanLoopActivationOutput
    HumanLoopActivationOutput (HumanLoopActivationOutput'),
    newHumanLoopActivationOutput,

    -- ** HumanLoopConfig
    HumanLoopConfig (HumanLoopConfig'),
    newHumanLoopConfig,

    -- ** HumanLoopDataAttributes
    HumanLoopDataAttributes (HumanLoopDataAttributes'),
    newHumanLoopDataAttributes,

    -- ** IdentityDocument
    IdentityDocument (IdentityDocument'),
    newIdentityDocument,

    -- ** IdentityDocumentField
    IdentityDocumentField (IdentityDocumentField'),
    newIdentityDocumentField,

    -- ** LendingDetection
    LendingDetection (LendingDetection'),
    newLendingDetection,

    -- ** LendingDocument
    LendingDocument (LendingDocument'),
    newLendingDocument,

    -- ** LendingField
    LendingField (LendingField'),
    newLendingField,

    -- ** LendingResult
    LendingResult (LendingResult'),
    newLendingResult,

    -- ** LendingSummary
    LendingSummary (LendingSummary'),
    newLendingSummary,

    -- ** LineItemFields
    LineItemFields (LineItemFields'),
    newLineItemFields,

    -- ** LineItemGroup
    LineItemGroup (LineItemGroup'),
    newLineItemGroup,

    -- ** NormalizedValue
    NormalizedValue (NormalizedValue'),
    newNormalizedValue,

    -- ** NotificationChannel
    NotificationChannel (NotificationChannel'),
    newNotificationChannel,

    -- ** OutputConfig
    OutputConfig (OutputConfig'),
    newOutputConfig,

    -- ** PageClassification
    PageClassification (PageClassification'),
    newPageClassification,

    -- ** Point
    Point (Point'),
    newPoint,

    -- ** Prediction
    Prediction (Prediction'),
    newPrediction,

    -- ** QueriesConfig
    QueriesConfig (QueriesConfig'),
    newQueriesConfig,

    -- ** Query
    Query (Query'),
    newQuery,

    -- ** Relationship
    Relationship (Relationship'),
    newRelationship,

    -- ** S3Object
    S3Object (S3Object'),
    newS3Object,

    -- ** SignatureDetection
    SignatureDetection (SignatureDetection'),
    newSignatureDetection,

    -- ** SplitDocument
    SplitDocument (SplitDocument'),
    newSplitDocument,

    -- ** UndetectedSignature
    UndetectedSignature (UndetectedSignature'),
    newUndetectedSignature,

    -- ** Warning
    Warning (Warning'),
    newWarning,
  )
where

import Amazonka.Textract.AnalyzeDocument
import Amazonka.Textract.AnalyzeExpense
import Amazonka.Textract.AnalyzeID
import Amazonka.Textract.DetectDocumentText
import Amazonka.Textract.GetDocumentAnalysis
import Amazonka.Textract.GetDocumentTextDetection
import Amazonka.Textract.GetExpenseAnalysis
import Amazonka.Textract.GetLendingAnalysis
import Amazonka.Textract.GetLendingAnalysisSummary
import Amazonka.Textract.Lens
import Amazonka.Textract.StartDocumentAnalysis
import Amazonka.Textract.StartDocumentTextDetection
import Amazonka.Textract.StartExpenseAnalysis
import Amazonka.Textract.StartLendingAnalysis
import Amazonka.Textract.Types
import Amazonka.Textract.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Textract'.

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
