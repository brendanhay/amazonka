{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Textract
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Textract
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidJobIdException
    _InvalidJobIdException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BadDocumentException
    _BadDocumentException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** UnsupportedDocumentException
    _UnsupportedDocumentException,

    -- ** InvalidS3ObjectException
    _InvalidS3ObjectException,

    -- ** ProvisionedThroughputExceededException
    _ProvisionedThroughputExceededException,

    -- ** InvalidKMSKeyException
    _InvalidKMSKeyException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerError
    _InternalServerError,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** HumanLoopQuotaExceededException
    _HumanLoopQuotaExceededException,

    -- ** DocumentTooLargeException
    _DocumentTooLargeException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DetectDocumentText
    DetectDocumentText (DetectDocumentText'),
    newDetectDocumentText,
    DetectDocumentTextResponse (DetectDocumentTextResponse'),
    newDetectDocumentTextResponse,

    -- ** StartDocumentAnalysis
    StartDocumentAnalysis (StartDocumentAnalysis'),
    newStartDocumentAnalysis,
    StartDocumentAnalysisResponse (StartDocumentAnalysisResponse'),
    newStartDocumentAnalysisResponse,

    -- ** AnalyzeDocument
    AnalyzeDocument (AnalyzeDocument'),
    newAnalyzeDocument,
    AnalyzeDocumentResponse (AnalyzeDocumentResponse'),
    newAnalyzeDocumentResponse,

    -- ** GetDocumentTextDetection
    GetDocumentTextDetection (GetDocumentTextDetection'),
    newGetDocumentTextDetection,
    GetDocumentTextDetectionResponse (GetDocumentTextDetectionResponse'),
    newGetDocumentTextDetectionResponse,

    -- ** AnalyzeExpense
    AnalyzeExpense (AnalyzeExpense'),
    newAnalyzeExpense,
    AnalyzeExpenseResponse (AnalyzeExpenseResponse'),
    newAnalyzeExpenseResponse,

    -- ** StartDocumentTextDetection
    StartDocumentTextDetection (StartDocumentTextDetection'),
    newStartDocumentTextDetection,
    StartDocumentTextDetectionResponse (StartDocumentTextDetectionResponse'),
    newStartDocumentTextDetectionResponse,

    -- ** GetDocumentAnalysis
    GetDocumentAnalysis (GetDocumentAnalysis'),
    newGetDocumentAnalysis,
    GetDocumentAnalysisResponse (GetDocumentAnalysisResponse'),
    newGetDocumentAnalysisResponse,

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

    -- ** ExpenseDetection
    ExpenseDetection (ExpenseDetection'),
    newExpenseDetection,

    -- ** ExpenseDocument
    ExpenseDocument (ExpenseDocument'),
    newExpenseDocument,

    -- ** ExpenseField
    ExpenseField (ExpenseField'),
    newExpenseField,

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

    -- ** LineItemFields
    LineItemFields (LineItemFields'),
    newLineItemFields,

    -- ** LineItemGroup
    LineItemGroup (LineItemGroup'),
    newLineItemGroup,

    -- ** NotificationChannel
    NotificationChannel (NotificationChannel'),
    newNotificationChannel,

    -- ** OutputConfig
    OutputConfig (OutputConfig'),
    newOutputConfig,

    -- ** Point
    Point (Point'),
    newPoint,

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

import Network.AWS.Textract.AnalyzeDocument
import Network.AWS.Textract.AnalyzeExpense
import Network.AWS.Textract.DetectDocumentText
import Network.AWS.Textract.GetDocumentAnalysis
import Network.AWS.Textract.GetDocumentTextDetection
import Network.AWS.Textract.Lens
import Network.AWS.Textract.StartDocumentAnalysis
import Network.AWS.Textract.StartDocumentTextDetection
import Network.AWS.Textract.Types
import Network.AWS.Textract.Waiters

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
