{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Textract.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Textract.Lens
  ( -- * Operations

    -- ** GetDocumentTextDetection
    getDocumentTextDetection_nextToken,
    getDocumentTextDetection_maxResults,
    getDocumentTextDetection_jobId,
    getDocumentTextDetectionResponse_statusMessage,
    getDocumentTextDetectionResponse_nextToken,
    getDocumentTextDetectionResponse_warnings,
    getDocumentTextDetectionResponse_jobStatus,
    getDocumentTextDetectionResponse_blocks,
    getDocumentTextDetectionResponse_documentMetadata,
    getDocumentTextDetectionResponse_detectDocumentTextModelVersion,
    getDocumentTextDetectionResponse_httpStatus,

    -- ** GetDocumentAnalysis
    getDocumentAnalysis_nextToken,
    getDocumentAnalysis_maxResults,
    getDocumentAnalysis_jobId,
    getDocumentAnalysisResponse_statusMessage,
    getDocumentAnalysisResponse_nextToken,
    getDocumentAnalysisResponse_warnings,
    getDocumentAnalysisResponse_analyzeDocumentModelVersion,
    getDocumentAnalysisResponse_jobStatus,
    getDocumentAnalysisResponse_blocks,
    getDocumentAnalysisResponse_documentMetadata,
    getDocumentAnalysisResponse_httpStatus,

    -- ** StartDocumentAnalysis
    startDocumentAnalysis_notificationChannel,
    startDocumentAnalysis_outputConfig,
    startDocumentAnalysis_kmsKeyId,
    startDocumentAnalysis_clientRequestToken,
    startDocumentAnalysis_jobTag,
    startDocumentAnalysis_documentLocation,
    startDocumentAnalysis_featureTypes,
    startDocumentAnalysisResponse_jobId,
    startDocumentAnalysisResponse_httpStatus,

    -- ** StartDocumentTextDetection
    startDocumentTextDetection_notificationChannel,
    startDocumentTextDetection_outputConfig,
    startDocumentTextDetection_kmsKeyId,
    startDocumentTextDetection_clientRequestToken,
    startDocumentTextDetection_jobTag,
    startDocumentTextDetection_documentLocation,
    startDocumentTextDetectionResponse_jobId,
    startDocumentTextDetectionResponse_httpStatus,

    -- ** AnalyzeExpense
    analyzeExpense_document,
    analyzeExpenseResponse_documentMetadata,
    analyzeExpenseResponse_expenseDocuments,
    analyzeExpenseResponse_httpStatus,

    -- ** AnalyzeDocument
    analyzeDocument_humanLoopConfig,
    analyzeDocument_document,
    analyzeDocument_featureTypes,
    analyzeDocumentResponse_analyzeDocumentModelVersion,
    analyzeDocumentResponse_blocks,
    analyzeDocumentResponse_documentMetadata,
    analyzeDocumentResponse_humanLoopActivationOutput,
    analyzeDocumentResponse_httpStatus,

    -- ** DetectDocumentText
    detectDocumentText_document,
    detectDocumentTextResponse_blocks,
    detectDocumentTextResponse_documentMetadata,
    detectDocumentTextResponse_detectDocumentTextModelVersion,
    detectDocumentTextResponse_httpStatus,

    -- * Types

    -- ** Block
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

    -- ** BoundingBox
    boundingBox_height,
    boundingBox_width,
    boundingBox_left,
    boundingBox_top,

    -- ** Document
    document_bytes,
    document_s3Object,

    -- ** DocumentLocation
    documentLocation_s3Object,

    -- ** DocumentMetadata
    documentMetadata_pages,

    -- ** ExpenseDetection
    expenseDetection_confidence,
    expenseDetection_text,
    expenseDetection_geometry,

    -- ** ExpenseDocument
    expenseDocument_lineItemGroups,
    expenseDocument_expenseIndex,
    expenseDocument_summaryFields,

    -- ** ExpenseField
    expenseField_labelDetection,
    expenseField_valueDetection,
    expenseField_pageNumber,
    expenseField_type,

    -- ** ExpenseType
    expenseType_confidence,
    expenseType_text,

    -- ** Geometry
    geometry_polygon,
    geometry_boundingBox,

    -- ** HumanLoopActivationOutput
    humanLoopActivationOutput_humanLoopActivationReasons,
    humanLoopActivationOutput_humanLoopArn,
    humanLoopActivationOutput_humanLoopActivationConditionsEvaluationResults,

    -- ** HumanLoopConfig
    humanLoopConfig_dataAttributes,
    humanLoopConfig_humanLoopName,
    humanLoopConfig_flowDefinitionArn,

    -- ** HumanLoopDataAttributes
    humanLoopDataAttributes_contentClassifiers,

    -- ** LineItemFields
    lineItemFields_lineItemExpenseFields,

    -- ** LineItemGroup
    lineItemGroup_lineItems,
    lineItemGroup_lineItemGroupIndex,

    -- ** NotificationChannel
    notificationChannel_sNSTopicArn,
    notificationChannel_roleArn,

    -- ** OutputConfig
    outputConfig_s3Prefix,
    outputConfig_s3Bucket,

    -- ** Point
    point_y,
    point_x,

    -- ** Relationship
    relationship_ids,
    relationship_type,

    -- ** S3Object
    s3Object_version,
    s3Object_name,
    s3Object_bucket,

    -- ** Warning
    warning_pages,
    warning_errorCode,
  )
where

import Network.AWS.Textract.AnalyzeDocument
import Network.AWS.Textract.AnalyzeExpense
import Network.AWS.Textract.DetectDocumentText
import Network.AWS.Textract.GetDocumentAnalysis
import Network.AWS.Textract.GetDocumentTextDetection
import Network.AWS.Textract.StartDocumentAnalysis
import Network.AWS.Textract.StartDocumentTextDetection
import Network.AWS.Textract.Types.Block
import Network.AWS.Textract.Types.BoundingBox
import Network.AWS.Textract.Types.Document
import Network.AWS.Textract.Types.DocumentLocation
import Network.AWS.Textract.Types.DocumentMetadata
import Network.AWS.Textract.Types.ExpenseDetection
import Network.AWS.Textract.Types.ExpenseDocument
import Network.AWS.Textract.Types.ExpenseField
import Network.AWS.Textract.Types.ExpenseType
import Network.AWS.Textract.Types.Geometry
import Network.AWS.Textract.Types.HumanLoopActivationOutput
import Network.AWS.Textract.Types.HumanLoopConfig
import Network.AWS.Textract.Types.HumanLoopDataAttributes
import Network.AWS.Textract.Types.LineItemFields
import Network.AWS.Textract.Types.LineItemGroup
import Network.AWS.Textract.Types.NotificationChannel
import Network.AWS.Textract.Types.OutputConfig
import Network.AWS.Textract.Types.Point
import Network.AWS.Textract.Types.Relationship
import Network.AWS.Textract.Types.S3Object
import Network.AWS.Textract.Types.Warning
