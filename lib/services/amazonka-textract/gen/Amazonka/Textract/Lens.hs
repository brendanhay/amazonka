{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Textract.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Lens
  ( -- * Operations

    -- ** AnalyzeDocument
    analyzeDocument_humanLoopConfig,
    analyzeDocument_queriesConfig,
    analyzeDocument_document,
    analyzeDocument_featureTypes,
    analyzeDocumentResponse_analyzeDocumentModelVersion,
    analyzeDocumentResponse_blocks,
    analyzeDocumentResponse_documentMetadata,
    analyzeDocumentResponse_humanLoopActivationOutput,
    analyzeDocumentResponse_httpStatus,

    -- ** AnalyzeExpense
    analyzeExpense_document,
    analyzeExpenseResponse_documentMetadata,
    analyzeExpenseResponse_expenseDocuments,
    analyzeExpenseResponse_httpStatus,

    -- ** AnalyzeID
    analyzeID_documentPages,
    analyzeIDResponse_analyzeIDModelVersion,
    analyzeIDResponse_documentMetadata,
    analyzeIDResponse_identityDocuments,
    analyzeIDResponse_httpStatus,

    -- ** DetectDocumentText
    detectDocumentText_document,
    detectDocumentTextResponse_blocks,
    detectDocumentTextResponse_detectDocumentTextModelVersion,
    detectDocumentTextResponse_documentMetadata,
    detectDocumentTextResponse_httpStatus,

    -- ** GetDocumentAnalysis
    getDocumentAnalysis_maxResults,
    getDocumentAnalysis_nextToken,
    getDocumentAnalysis_jobId,
    getDocumentAnalysisResponse_analyzeDocumentModelVersion,
    getDocumentAnalysisResponse_blocks,
    getDocumentAnalysisResponse_documentMetadata,
    getDocumentAnalysisResponse_jobStatus,
    getDocumentAnalysisResponse_nextToken,
    getDocumentAnalysisResponse_statusMessage,
    getDocumentAnalysisResponse_warnings,
    getDocumentAnalysisResponse_httpStatus,

    -- ** GetDocumentTextDetection
    getDocumentTextDetection_maxResults,
    getDocumentTextDetection_nextToken,
    getDocumentTextDetection_jobId,
    getDocumentTextDetectionResponse_blocks,
    getDocumentTextDetectionResponse_detectDocumentTextModelVersion,
    getDocumentTextDetectionResponse_documentMetadata,
    getDocumentTextDetectionResponse_jobStatus,
    getDocumentTextDetectionResponse_nextToken,
    getDocumentTextDetectionResponse_statusMessage,
    getDocumentTextDetectionResponse_warnings,
    getDocumentTextDetectionResponse_httpStatus,

    -- ** GetExpenseAnalysis
    getExpenseAnalysis_maxResults,
    getExpenseAnalysis_nextToken,
    getExpenseAnalysis_jobId,
    getExpenseAnalysisResponse_analyzeExpenseModelVersion,
    getExpenseAnalysisResponse_documentMetadata,
    getExpenseAnalysisResponse_expenseDocuments,
    getExpenseAnalysisResponse_jobStatus,
    getExpenseAnalysisResponse_nextToken,
    getExpenseAnalysisResponse_statusMessage,
    getExpenseAnalysisResponse_warnings,
    getExpenseAnalysisResponse_httpStatus,

    -- ** GetLendingAnalysis
    getLendingAnalysis_maxResults,
    getLendingAnalysis_nextToken,
    getLendingAnalysis_jobId,
    getLendingAnalysisResponse_analyzeLendingModelVersion,
    getLendingAnalysisResponse_documentMetadata,
    getLendingAnalysisResponse_jobStatus,
    getLendingAnalysisResponse_nextToken,
    getLendingAnalysisResponse_results,
    getLendingAnalysisResponse_statusMessage,
    getLendingAnalysisResponse_warnings,
    getLendingAnalysisResponse_httpStatus,

    -- ** GetLendingAnalysisSummary
    getLendingAnalysisSummary_jobId,
    getLendingAnalysisSummaryResponse_analyzeLendingModelVersion,
    getLendingAnalysisSummaryResponse_documentMetadata,
    getLendingAnalysisSummaryResponse_jobStatus,
    getLendingAnalysisSummaryResponse_statusMessage,
    getLendingAnalysisSummaryResponse_summary,
    getLendingAnalysisSummaryResponse_warnings,
    getLendingAnalysisSummaryResponse_httpStatus,

    -- ** StartDocumentAnalysis
    startDocumentAnalysis_clientRequestToken,
    startDocumentAnalysis_jobTag,
    startDocumentAnalysis_kmsKeyId,
    startDocumentAnalysis_notificationChannel,
    startDocumentAnalysis_outputConfig,
    startDocumentAnalysis_queriesConfig,
    startDocumentAnalysis_documentLocation,
    startDocumentAnalysis_featureTypes,
    startDocumentAnalysisResponse_jobId,
    startDocumentAnalysisResponse_httpStatus,

    -- ** StartDocumentTextDetection
    startDocumentTextDetection_clientRequestToken,
    startDocumentTextDetection_jobTag,
    startDocumentTextDetection_kmsKeyId,
    startDocumentTextDetection_notificationChannel,
    startDocumentTextDetection_outputConfig,
    startDocumentTextDetection_documentLocation,
    startDocumentTextDetectionResponse_jobId,
    startDocumentTextDetectionResponse_httpStatus,

    -- ** StartExpenseAnalysis
    startExpenseAnalysis_clientRequestToken,
    startExpenseAnalysis_jobTag,
    startExpenseAnalysis_kmsKeyId,
    startExpenseAnalysis_notificationChannel,
    startExpenseAnalysis_outputConfig,
    startExpenseAnalysis_documentLocation,
    startExpenseAnalysisResponse_jobId,
    startExpenseAnalysisResponse_httpStatus,

    -- ** StartLendingAnalysis
    startLendingAnalysis_clientRequestToken,
    startLendingAnalysis_jobTag,
    startLendingAnalysis_kmsKeyId,
    startLendingAnalysis_notificationChannel,
    startLendingAnalysis_outputConfig,
    startLendingAnalysis_documentLocation,
    startLendingAnalysisResponse_jobId,
    startLendingAnalysisResponse_httpStatus,

    -- * Types

    -- ** AnalyzeIDDetections
    analyzeIDDetections_confidence,
    analyzeIDDetections_normalizedValue,
    analyzeIDDetections_text,

    -- ** Block
    block_blockType,
    block_columnIndex,
    block_columnSpan,
    block_confidence,
    block_entityTypes,
    block_geometry,
    block_id,
    block_page,
    block_query,
    block_relationships,
    block_rowIndex,
    block_rowSpan,
    block_selectionStatus,
    block_text,
    block_textType,

    -- ** BoundingBox
    boundingBox_height,
    boundingBox_left,
    boundingBox_top,
    boundingBox_width,

    -- ** DetectedSignature
    detectedSignature_page,

    -- ** Document
    document_bytes,
    document_s3Object,

    -- ** DocumentGroup
    documentGroup_detectedSignatures,
    documentGroup_splitDocuments,
    documentGroup_type,
    documentGroup_undetectedSignatures,

    -- ** DocumentLocation
    documentLocation_s3Object,

    -- ** DocumentMetadata
    documentMetadata_pages,

    -- ** ExpenseCurrency
    expenseCurrency_code,
    expenseCurrency_confidence,

    -- ** ExpenseDetection
    expenseDetection_confidence,
    expenseDetection_geometry,
    expenseDetection_text,

    -- ** ExpenseDocument
    expenseDocument_blocks,
    expenseDocument_expenseIndex,
    expenseDocument_lineItemGroups,
    expenseDocument_summaryFields,

    -- ** ExpenseField
    expenseField_currency,
    expenseField_groupProperties,
    expenseField_labelDetection,
    expenseField_pageNumber,
    expenseField_type,
    expenseField_valueDetection,

    -- ** ExpenseGroupProperty
    expenseGroupProperty_id,
    expenseGroupProperty_types,

    -- ** ExpenseType
    expenseType_confidence,
    expenseType_text,

    -- ** Extraction
    extraction_expenseDocument,
    extraction_identityDocument,
    extraction_lendingDocument,

    -- ** Geometry
    geometry_boundingBox,
    geometry_polygon,

    -- ** HumanLoopActivationOutput
    humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults,
    humanLoopActivationOutput_humanLoopActivationReasons,
    humanLoopActivationOutput_humanLoopArn,

    -- ** HumanLoopConfig
    humanLoopConfig_dataAttributes,
    humanLoopConfig_humanLoopName,
    humanLoopConfig_flowDefinitionArn,

    -- ** HumanLoopDataAttributes
    humanLoopDataAttributes_contentClassifiers,

    -- ** IdentityDocument
    identityDocument_blocks,
    identityDocument_documentIndex,
    identityDocument_identityDocumentFields,

    -- ** IdentityDocumentField
    identityDocumentField_type,
    identityDocumentField_valueDetection,

    -- ** LendingDetection
    lendingDetection_confidence,
    lendingDetection_geometry,
    lendingDetection_selectionStatus,
    lendingDetection_text,

    -- ** LendingDocument
    lendingDocument_lendingFields,
    lendingDocument_signatureDetections,

    -- ** LendingField
    lendingField_keyDetection,
    lendingField_type,
    lendingField_valueDetections,

    -- ** LendingResult
    lendingResult_extractions,
    lendingResult_page,
    lendingResult_pageClassification,

    -- ** LendingSummary
    lendingSummary_documentGroups,
    lendingSummary_undetectedDocumentTypes,

    -- ** LineItemFields
    lineItemFields_lineItemExpenseFields,

    -- ** LineItemGroup
    lineItemGroup_lineItemGroupIndex,
    lineItemGroup_lineItems,

    -- ** NormalizedValue
    normalizedValue_value,
    normalizedValue_valueType,

    -- ** NotificationChannel
    notificationChannel_sNSTopicArn,
    notificationChannel_roleArn,

    -- ** OutputConfig
    outputConfig_s3Prefix,
    outputConfig_s3Bucket,

    -- ** PageClassification
    pageClassification_pageType,
    pageClassification_pageNumber,

    -- ** Point
    point_x,
    point_y,

    -- ** Prediction
    prediction_confidence,
    prediction_value,

    -- ** QueriesConfig
    queriesConfig_queries,

    -- ** Query
    query_alias,
    query_pages,
    query_text,

    -- ** Relationship
    relationship_ids,
    relationship_type,

    -- ** S3Object
    s3Object_bucket,
    s3Object_name,
    s3Object_version,

    -- ** SignatureDetection
    signatureDetection_confidence,
    signatureDetection_geometry,

    -- ** SplitDocument
    splitDocument_index,
    splitDocument_pages,

    -- ** UndetectedSignature
    undetectedSignature_page,

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
import Amazonka.Textract.GetLendingAnalysis
import Amazonka.Textract.GetLendingAnalysisSummary
import Amazonka.Textract.StartDocumentAnalysis
import Amazonka.Textract.StartDocumentTextDetection
import Amazonka.Textract.StartExpenseAnalysis
import Amazonka.Textract.StartLendingAnalysis
import Amazonka.Textract.Types.AnalyzeIDDetections
import Amazonka.Textract.Types.Block
import Amazonka.Textract.Types.BoundingBox
import Amazonka.Textract.Types.DetectedSignature
import Amazonka.Textract.Types.Document
import Amazonka.Textract.Types.DocumentGroup
import Amazonka.Textract.Types.DocumentLocation
import Amazonka.Textract.Types.DocumentMetadata
import Amazonka.Textract.Types.ExpenseCurrency
import Amazonka.Textract.Types.ExpenseDetection
import Amazonka.Textract.Types.ExpenseDocument
import Amazonka.Textract.Types.ExpenseField
import Amazonka.Textract.Types.ExpenseGroupProperty
import Amazonka.Textract.Types.ExpenseType
import Amazonka.Textract.Types.Extraction
import Amazonka.Textract.Types.Geometry
import Amazonka.Textract.Types.HumanLoopActivationOutput
import Amazonka.Textract.Types.HumanLoopConfig
import Amazonka.Textract.Types.HumanLoopDataAttributes
import Amazonka.Textract.Types.IdentityDocument
import Amazonka.Textract.Types.IdentityDocumentField
import Amazonka.Textract.Types.LendingDetection
import Amazonka.Textract.Types.LendingDocument
import Amazonka.Textract.Types.LendingField
import Amazonka.Textract.Types.LendingResult
import Amazonka.Textract.Types.LendingSummary
import Amazonka.Textract.Types.LineItemFields
import Amazonka.Textract.Types.LineItemGroup
import Amazonka.Textract.Types.NormalizedValue
import Amazonka.Textract.Types.NotificationChannel
import Amazonka.Textract.Types.OutputConfig
import Amazonka.Textract.Types.PageClassification
import Amazonka.Textract.Types.Point
import Amazonka.Textract.Types.Prediction
import Amazonka.Textract.Types.QueriesConfig
import Amazonka.Textract.Types.Query
import Amazonka.Textract.Types.Relationship
import Amazonka.Textract.Types.S3Object
import Amazonka.Textract.Types.SignatureDetection
import Amazonka.Textract.Types.SplitDocument
import Amazonka.Textract.Types.UndetectedSignature
import Amazonka.Textract.Types.Warning
