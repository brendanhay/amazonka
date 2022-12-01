{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Textract.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Lens
  ( -- * Operations

    -- ** AnalyzeDocument
    analyzeDocument_humanLoopConfig,
    analyzeDocument_queriesConfig,
    analyzeDocument_document,
    analyzeDocument_featureTypes,
    analyzeDocumentResponse_humanLoopActivationOutput,
    analyzeDocumentResponse_documentMetadata,
    analyzeDocumentResponse_analyzeDocumentModelVersion,
    analyzeDocumentResponse_blocks,
    analyzeDocumentResponse_httpStatus,

    -- ** AnalyzeExpense
    analyzeExpense_document,
    analyzeExpenseResponse_documentMetadata,
    analyzeExpenseResponse_expenseDocuments,
    analyzeExpenseResponse_httpStatus,

    -- ** AnalyzeID
    analyzeID_documentPages,
    analyzeIDResponse_identityDocuments,
    analyzeIDResponse_documentMetadata,
    analyzeIDResponse_analyzeIDModelVersion,
    analyzeIDResponse_httpStatus,

    -- ** DetectDocumentText
    detectDocumentText_document,
    detectDocumentTextResponse_documentMetadata,
    detectDocumentTextResponse_detectDocumentTextModelVersion,
    detectDocumentTextResponse_blocks,
    detectDocumentTextResponse_httpStatus,

    -- ** GetDocumentAnalysis
    getDocumentAnalysis_nextToken,
    getDocumentAnalysis_maxResults,
    getDocumentAnalysis_jobId,
    getDocumentAnalysisResponse_nextToken,
    getDocumentAnalysisResponse_jobStatus,
    getDocumentAnalysisResponse_documentMetadata,
    getDocumentAnalysisResponse_warnings,
    getDocumentAnalysisResponse_analyzeDocumentModelVersion,
    getDocumentAnalysisResponse_statusMessage,
    getDocumentAnalysisResponse_blocks,
    getDocumentAnalysisResponse_httpStatus,

    -- ** GetDocumentTextDetection
    getDocumentTextDetection_nextToken,
    getDocumentTextDetection_maxResults,
    getDocumentTextDetection_jobId,
    getDocumentTextDetectionResponse_nextToken,
    getDocumentTextDetectionResponse_jobStatus,
    getDocumentTextDetectionResponse_documentMetadata,
    getDocumentTextDetectionResponse_warnings,
    getDocumentTextDetectionResponse_detectDocumentTextModelVersion,
    getDocumentTextDetectionResponse_statusMessage,
    getDocumentTextDetectionResponse_blocks,
    getDocumentTextDetectionResponse_httpStatus,

    -- ** GetExpenseAnalysis
    getExpenseAnalysis_nextToken,
    getExpenseAnalysis_maxResults,
    getExpenseAnalysis_jobId,
    getExpenseAnalysisResponse_analyzeExpenseModelVersion,
    getExpenseAnalysisResponse_nextToken,
    getExpenseAnalysisResponse_jobStatus,
    getExpenseAnalysisResponse_documentMetadata,
    getExpenseAnalysisResponse_expenseDocuments,
    getExpenseAnalysisResponse_warnings,
    getExpenseAnalysisResponse_statusMessage,
    getExpenseAnalysisResponse_httpStatus,

    -- ** StartDocumentAnalysis
    startDocumentAnalysis_clientRequestToken,
    startDocumentAnalysis_queriesConfig,
    startDocumentAnalysis_kmsKeyId,
    startDocumentAnalysis_jobTag,
    startDocumentAnalysis_outputConfig,
    startDocumentAnalysis_notificationChannel,
    startDocumentAnalysis_documentLocation,
    startDocumentAnalysis_featureTypes,
    startDocumentAnalysisResponse_jobId,
    startDocumentAnalysisResponse_httpStatus,

    -- ** StartDocumentTextDetection
    startDocumentTextDetection_clientRequestToken,
    startDocumentTextDetection_kmsKeyId,
    startDocumentTextDetection_jobTag,
    startDocumentTextDetection_outputConfig,
    startDocumentTextDetection_notificationChannel,
    startDocumentTextDetection_documentLocation,
    startDocumentTextDetectionResponse_jobId,
    startDocumentTextDetectionResponse_httpStatus,

    -- ** StartExpenseAnalysis
    startExpenseAnalysis_clientRequestToken,
    startExpenseAnalysis_kmsKeyId,
    startExpenseAnalysis_jobTag,
    startExpenseAnalysis_outputConfig,
    startExpenseAnalysis_notificationChannel,
    startExpenseAnalysis_documentLocation,
    startExpenseAnalysisResponse_jobId,
    startExpenseAnalysisResponse_httpStatus,

    -- * Types

    -- ** AnalyzeIDDetections
    analyzeIDDetections_confidence,
    analyzeIDDetections_normalizedValue,
    analyzeIDDetections_text,

    -- ** Block
    block_blockType,
    block_rowSpan,
    block_columnIndex,
    block_entityTypes,
    block_columnSpan,
    block_confidence,
    block_rowIndex,
    block_selectionStatus,
    block_id,
    block_query,
    block_page,
    block_textType,
    block_relationships,
    block_text,
    block_geometry,

    -- ** BoundingBox
    boundingBox_width,
    boundingBox_top,
    boundingBox_left,
    boundingBox_height,

    -- ** Document
    document_bytes,
    document_s3Object,

    -- ** DocumentLocation
    documentLocation_s3Object,

    -- ** DocumentMetadata
    documentMetadata_pages,

    -- ** ExpenseCurrency
    expenseCurrency_code,
    expenseCurrency_confidence,

    -- ** ExpenseDetection
    expenseDetection_confidence,
    expenseDetection_text,
    expenseDetection_geometry,

    -- ** ExpenseDocument
    expenseDocument_lineItemGroups,
    expenseDocument_summaryFields,
    expenseDocument_expenseIndex,
    expenseDocument_blocks,

    -- ** ExpenseField
    expenseField_type,
    expenseField_groupProperties,
    expenseField_pageNumber,
    expenseField_labelDetection,
    expenseField_currency,
    expenseField_valueDetection,

    -- ** ExpenseGroupProperty
    expenseGroupProperty_id,
    expenseGroupProperty_types,

    -- ** ExpenseType
    expenseType_confidence,
    expenseType_text,

    -- ** Geometry
    geometry_polygon,
    geometry_boundingBox,

    -- ** HumanLoopActivationOutput
    humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults,
    humanLoopActivationOutput_humanLoopArn,
    humanLoopActivationOutput_humanLoopActivationReasons,

    -- ** HumanLoopConfig
    humanLoopConfig_dataAttributes,
    humanLoopConfig_humanLoopName,
    humanLoopConfig_flowDefinitionArn,

    -- ** HumanLoopDataAttributes
    humanLoopDataAttributes_contentClassifiers,

    -- ** IdentityDocument
    identityDocument_documentIndex,
    identityDocument_identityDocumentFields,
    identityDocument_blocks,

    -- ** IdentityDocumentField
    identityDocumentField_type,
    identityDocumentField_valueDetection,

    -- ** LineItemFields
    lineItemFields_lineItemExpenseFields,

    -- ** LineItemGroup
    lineItemGroup_lineItems,
    lineItemGroup_lineItemGroupIndex,

    -- ** NormalizedValue
    normalizedValue_valueType,
    normalizedValue_value,

    -- ** NotificationChannel
    notificationChannel_sNSTopicArn,
    notificationChannel_roleArn,

    -- ** OutputConfig
    outputConfig_s3Prefix,
    outputConfig_s3Bucket,

    -- ** Point
    point_x,
    point_y,

    -- ** QueriesConfig
    queriesConfig_queries,

    -- ** Query
    query_alias,
    query_pages,
    query_text,

    -- ** Relationship
    relationship_type,
    relationship_ids,

    -- ** S3Object
    s3Object_name,
    s3Object_bucket,
    s3Object_version,

    -- ** Warning
    warning_errorCode,
    warning_pages,
  )
where

import Amazonka.Textract.AnalyzeDocument
import Amazonka.Textract.AnalyzeExpense
import Amazonka.Textract.AnalyzeID
import Amazonka.Textract.DetectDocumentText
import Amazonka.Textract.GetDocumentAnalysis
import Amazonka.Textract.GetDocumentTextDetection
import Amazonka.Textract.GetExpenseAnalysis
import Amazonka.Textract.StartDocumentAnalysis
import Amazonka.Textract.StartDocumentTextDetection
import Amazonka.Textract.StartExpenseAnalysis
import Amazonka.Textract.Types.AnalyzeIDDetections
import Amazonka.Textract.Types.Block
import Amazonka.Textract.Types.BoundingBox
import Amazonka.Textract.Types.Document
import Amazonka.Textract.Types.DocumentLocation
import Amazonka.Textract.Types.DocumentMetadata
import Amazonka.Textract.Types.ExpenseCurrency
import Amazonka.Textract.Types.ExpenseDetection
import Amazonka.Textract.Types.ExpenseDocument
import Amazonka.Textract.Types.ExpenseField
import Amazonka.Textract.Types.ExpenseGroupProperty
import Amazonka.Textract.Types.ExpenseType
import Amazonka.Textract.Types.Geometry
import Amazonka.Textract.Types.HumanLoopActivationOutput
import Amazonka.Textract.Types.HumanLoopConfig
import Amazonka.Textract.Types.HumanLoopDataAttributes
import Amazonka.Textract.Types.IdentityDocument
import Amazonka.Textract.Types.IdentityDocumentField
import Amazonka.Textract.Types.LineItemFields
import Amazonka.Textract.Types.LineItemGroup
import Amazonka.Textract.Types.NormalizedValue
import Amazonka.Textract.Types.NotificationChannel
import Amazonka.Textract.Types.OutputConfig
import Amazonka.Textract.Types.Point
import Amazonka.Textract.Types.QueriesConfig
import Amazonka.Textract.Types.Query
import Amazonka.Textract.Types.Relationship
import Amazonka.Textract.Types.S3Object
import Amazonka.Textract.Types.Warning
