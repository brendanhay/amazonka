{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Textract.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Lens
  ( -- * Operations

    -- ** DetectDocumentText
    detectDocumentText_document,
    detectDocumentTextResponse_documentMetadata,
    detectDocumentTextResponse_blocks,
    detectDocumentTextResponse_detectDocumentTextModelVersion,
    detectDocumentTextResponse_httpStatus,

    -- ** StartDocumentAnalysis
    startDocumentAnalysis_jobTag,
    startDocumentAnalysis_notificationChannel,
    startDocumentAnalysis_kmsKeyId,
    startDocumentAnalysis_outputConfig,
    startDocumentAnalysis_clientRequestToken,
    startDocumentAnalysis_documentLocation,
    startDocumentAnalysis_featureTypes,
    startDocumentAnalysisResponse_jobId,
    startDocumentAnalysisResponse_httpStatus,

    -- ** AnalyzeDocument
    analyzeDocument_humanLoopConfig,
    analyzeDocument_document,
    analyzeDocument_featureTypes,
    analyzeDocumentResponse_documentMetadata,
    analyzeDocumentResponse_blocks,
    analyzeDocumentResponse_humanLoopActivationOutput,
    analyzeDocumentResponse_analyzeDocumentModelVersion,
    analyzeDocumentResponse_httpStatus,

    -- ** GetDocumentTextDetection
    getDocumentTextDetection_nextToken,
    getDocumentTextDetection_maxResults,
    getDocumentTextDetection_jobId,
    getDocumentTextDetectionResponse_documentMetadata,
    getDocumentTextDetectionResponse_blocks,
    getDocumentTextDetectionResponse_warnings,
    getDocumentTextDetectionResponse_nextToken,
    getDocumentTextDetectionResponse_statusMessage,
    getDocumentTextDetectionResponse_detectDocumentTextModelVersion,
    getDocumentTextDetectionResponse_jobStatus,
    getDocumentTextDetectionResponse_httpStatus,

    -- ** AnalyzeExpense
    analyzeExpense_document,
    analyzeExpenseResponse_documentMetadata,
    analyzeExpenseResponse_expenseDocuments,
    analyzeExpenseResponse_httpStatus,

    -- ** StartDocumentTextDetection
    startDocumentTextDetection_jobTag,
    startDocumentTextDetection_notificationChannel,
    startDocumentTextDetection_kmsKeyId,
    startDocumentTextDetection_outputConfig,
    startDocumentTextDetection_clientRequestToken,
    startDocumentTextDetection_documentLocation,
    startDocumentTextDetectionResponse_jobId,
    startDocumentTextDetectionResponse_httpStatus,

    -- ** GetDocumentAnalysis
    getDocumentAnalysis_nextToken,
    getDocumentAnalysis_maxResults,
    getDocumentAnalysis_jobId,
    getDocumentAnalysisResponse_documentMetadata,
    getDocumentAnalysisResponse_blocks,
    getDocumentAnalysisResponse_analyzeDocumentModelVersion,
    getDocumentAnalysisResponse_warnings,
    getDocumentAnalysisResponse_nextToken,
    getDocumentAnalysisResponse_statusMessage,
    getDocumentAnalysisResponse_jobStatus,
    getDocumentAnalysisResponse_httpStatus,

    -- * Types

    -- ** Block
    block_columnSpan,
    block_text,
    block_entityTypes,
    block_columnIndex,
    block_page,
    block_rowSpan,
    block_selectionStatus,
    block_rowIndex,
    block_confidence,
    block_relationships,
    block_geometry,
    block_textType,
    block_id,
    block_blockType,

    -- ** BoundingBox
    boundingBox_height,
    boundingBox_left,
    boundingBox_width,
    boundingBox_top,

    -- ** Document
    document_s3Object,
    document_bytes,

    -- ** DocumentLocation
    documentLocation_s3Object,

    -- ** DocumentMetadata
    documentMetadata_pages,

    -- ** ExpenseDetection
    expenseDetection_text,
    expenseDetection_confidence,
    expenseDetection_geometry,

    -- ** ExpenseDocument
    expenseDocument_lineItemGroups,
    expenseDocument_summaryFields,
    expenseDocument_expenseIndex,

    -- ** ExpenseField
    expenseField_labelDetection,
    expenseField_valueDetection,
    expenseField_type,
    expenseField_pageNumber,

    -- ** ExpenseType
    expenseType_text,
    expenseType_confidence,

    -- ** Geometry
    geometry_boundingBox,
    geometry_polygon,

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
    point_x,
    point_y,

    -- ** Relationship
    relationship_ids,
    relationship_type,

    -- ** S3Object
    s3Object_bucket,
    s3Object_name,
    s3Object_version,

    -- ** Warning
    warning_pages,
    warning_errorCode,
  )
where

import Amazonka.Textract.AnalyzeDocument
import Amazonka.Textract.AnalyzeExpense
import Amazonka.Textract.DetectDocumentText
import Amazonka.Textract.GetDocumentAnalysis
import Amazonka.Textract.GetDocumentTextDetection
import Amazonka.Textract.StartDocumentAnalysis
import Amazonka.Textract.StartDocumentTextDetection
import Amazonka.Textract.Types.Block
import Amazonka.Textract.Types.BoundingBox
import Amazonka.Textract.Types.Document
import Amazonka.Textract.Types.DocumentLocation
import Amazonka.Textract.Types.DocumentMetadata
import Amazonka.Textract.Types.ExpenseDetection
import Amazonka.Textract.Types.ExpenseDocument
import Amazonka.Textract.Types.ExpenseField
import Amazonka.Textract.Types.ExpenseType
import Amazonka.Textract.Types.Geometry
import Amazonka.Textract.Types.HumanLoopActivationOutput
import Amazonka.Textract.Types.HumanLoopConfig
import Amazonka.Textract.Types.HumanLoopDataAttributes
import Amazonka.Textract.Types.LineItemFields
import Amazonka.Textract.Types.LineItemGroup
import Amazonka.Textract.Types.NotificationChannel
import Amazonka.Textract.Types.OutputConfig
import Amazonka.Textract.Types.Point
import Amazonka.Textract.Types.Relationship
import Amazonka.Textract.Types.S3Object
import Amazonka.Textract.Types.Warning
