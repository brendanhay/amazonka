{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Textract
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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

    -- ** InvalidKMSKeyException
    _InvalidKMSKeyException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ProvisionedThroughputExceededException
    _ProvisionedThroughputExceededException,

    -- ** BadDocumentException
    _BadDocumentException,

    -- ** HumanLoopQuotaExceededException
    _HumanLoopQuotaExceededException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** DocumentTooLargeException
    _DocumentTooLargeException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InvalidS3ObjectException
    _InvalidS3ObjectException,

    -- ** UnsupportedDocumentException
    _UnsupportedDocumentException,

    -- ** InvalidJobIdException
    _InvalidJobIdException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** InvalidParameterException
    _InvalidParameterException,

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

    -- ** Document
    Document (Document'),
    newDocument,

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

    -- ** Point
    Point (Point'),
    newPoint,

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
import Amazonka.Textract.Lens
import Amazonka.Textract.StartDocumentAnalysis
import Amazonka.Textract.StartDocumentTextDetection
import Amazonka.Textract.StartExpenseAnalysis
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
