{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Textract
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Textract where

import Amazonka.Textract
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Textract.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAnalyzeDocument $
--             newAnalyzeDocument
--
--         , requestAnalyzeExpense $
--             newAnalyzeExpense
--
--         , requestAnalyzeID $
--             newAnalyzeID
--
--         , requestDetectDocumentText $
--             newDetectDocumentText
--
--         , requestGetDocumentAnalysis $
--             newGetDocumentAnalysis
--
--         , requestGetDocumentTextDetection $
--             newGetDocumentTextDetection
--
--         , requestGetExpenseAnalysis $
--             newGetExpenseAnalysis
--
--         , requestGetLendingAnalysis $
--             newGetLendingAnalysis
--
--         , requestGetLendingAnalysisSummary $
--             newGetLendingAnalysisSummary
--
--         , requestStartDocumentAnalysis $
--             newStartDocumentAnalysis
--
--         , requestStartDocumentTextDetection $
--             newStartDocumentTextDetection
--
--         , requestStartExpenseAnalysis $
--             newStartExpenseAnalysis
--
--         , requestStartLendingAnalysis $
--             newStartLendingAnalysis
--
--           ]

--     , testGroup "response"
--         [ responseAnalyzeDocument $
--             newAnalyzeDocumentResponse
--
--         , responseAnalyzeExpense $
--             newAnalyzeExpenseResponse
--
--         , responseAnalyzeID $
--             newAnalyzeIDResponse
--
--         , responseDetectDocumentText $
--             newDetectDocumentTextResponse
--
--         , responseGetDocumentAnalysis $
--             newGetDocumentAnalysisResponse
--
--         , responseGetDocumentTextDetection $
--             newGetDocumentTextDetectionResponse
--
--         , responseGetExpenseAnalysis $
--             newGetExpenseAnalysisResponse
--
--         , responseGetLendingAnalysis $
--             newGetLendingAnalysisResponse
--
--         , responseGetLendingAnalysisSummary $
--             newGetLendingAnalysisSummaryResponse
--
--         , responseStartDocumentAnalysis $
--             newStartDocumentAnalysisResponse
--
--         , responseStartDocumentTextDetection $
--             newStartDocumentTextDetectionResponse
--
--         , responseStartExpenseAnalysis $
--             newStartExpenseAnalysisResponse
--
--         , responseStartLendingAnalysis $
--             newStartLendingAnalysisResponse
--
--           ]
--     ]

-- Requests

requestAnalyzeDocument :: AnalyzeDocument -> TestTree
requestAnalyzeDocument =
  req
    "AnalyzeDocument"
    "fixture/AnalyzeDocument.yaml"

requestAnalyzeExpense :: AnalyzeExpense -> TestTree
requestAnalyzeExpense =
  req
    "AnalyzeExpense"
    "fixture/AnalyzeExpense.yaml"

requestAnalyzeID :: AnalyzeID -> TestTree
requestAnalyzeID =
  req
    "AnalyzeID"
    "fixture/AnalyzeID.yaml"

requestDetectDocumentText :: DetectDocumentText -> TestTree
requestDetectDocumentText =
  req
    "DetectDocumentText"
    "fixture/DetectDocumentText.yaml"

requestGetDocumentAnalysis :: GetDocumentAnalysis -> TestTree
requestGetDocumentAnalysis =
  req
    "GetDocumentAnalysis"
    "fixture/GetDocumentAnalysis.yaml"

requestGetDocumentTextDetection :: GetDocumentTextDetection -> TestTree
requestGetDocumentTextDetection =
  req
    "GetDocumentTextDetection"
    "fixture/GetDocumentTextDetection.yaml"

requestGetExpenseAnalysis :: GetExpenseAnalysis -> TestTree
requestGetExpenseAnalysis =
  req
    "GetExpenseAnalysis"
    "fixture/GetExpenseAnalysis.yaml"

requestGetLendingAnalysis :: GetLendingAnalysis -> TestTree
requestGetLendingAnalysis =
  req
    "GetLendingAnalysis"
    "fixture/GetLendingAnalysis.yaml"

requestGetLendingAnalysisSummary :: GetLendingAnalysisSummary -> TestTree
requestGetLendingAnalysisSummary =
  req
    "GetLendingAnalysisSummary"
    "fixture/GetLendingAnalysisSummary.yaml"

requestStartDocumentAnalysis :: StartDocumentAnalysis -> TestTree
requestStartDocumentAnalysis =
  req
    "StartDocumentAnalysis"
    "fixture/StartDocumentAnalysis.yaml"

requestStartDocumentTextDetection :: StartDocumentTextDetection -> TestTree
requestStartDocumentTextDetection =
  req
    "StartDocumentTextDetection"
    "fixture/StartDocumentTextDetection.yaml"

requestStartExpenseAnalysis :: StartExpenseAnalysis -> TestTree
requestStartExpenseAnalysis =
  req
    "StartExpenseAnalysis"
    "fixture/StartExpenseAnalysis.yaml"

requestStartLendingAnalysis :: StartLendingAnalysis -> TestTree
requestStartLendingAnalysis =
  req
    "StartLendingAnalysis"
    "fixture/StartLendingAnalysis.yaml"

-- Responses

responseAnalyzeDocument :: AnalyzeDocumentResponse -> TestTree
responseAnalyzeDocument =
  res
    "AnalyzeDocumentResponse"
    "fixture/AnalyzeDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AnalyzeDocument)

responseAnalyzeExpense :: AnalyzeExpenseResponse -> TestTree
responseAnalyzeExpense =
  res
    "AnalyzeExpenseResponse"
    "fixture/AnalyzeExpenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AnalyzeExpense)

responseAnalyzeID :: AnalyzeIDResponse -> TestTree
responseAnalyzeID =
  res
    "AnalyzeIDResponse"
    "fixture/AnalyzeIDResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AnalyzeID)

responseDetectDocumentText :: DetectDocumentTextResponse -> TestTree
responseDetectDocumentText =
  res
    "DetectDocumentTextResponse"
    "fixture/DetectDocumentTextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectDocumentText)

responseGetDocumentAnalysis :: GetDocumentAnalysisResponse -> TestTree
responseGetDocumentAnalysis =
  res
    "GetDocumentAnalysisResponse"
    "fixture/GetDocumentAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentAnalysis)

responseGetDocumentTextDetection :: GetDocumentTextDetectionResponse -> TestTree
responseGetDocumentTextDetection =
  res
    "GetDocumentTextDetectionResponse"
    "fixture/GetDocumentTextDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentTextDetection)

responseGetExpenseAnalysis :: GetExpenseAnalysisResponse -> TestTree
responseGetExpenseAnalysis =
  res
    "GetExpenseAnalysisResponse"
    "fixture/GetExpenseAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExpenseAnalysis)

responseGetLendingAnalysis :: GetLendingAnalysisResponse -> TestTree
responseGetLendingAnalysis =
  res
    "GetLendingAnalysisResponse"
    "fixture/GetLendingAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLendingAnalysis)

responseGetLendingAnalysisSummary :: GetLendingAnalysisSummaryResponse -> TestTree
responseGetLendingAnalysisSummary =
  res
    "GetLendingAnalysisSummaryResponse"
    "fixture/GetLendingAnalysisSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLendingAnalysisSummary)

responseStartDocumentAnalysis :: StartDocumentAnalysisResponse -> TestTree
responseStartDocumentAnalysis =
  res
    "StartDocumentAnalysisResponse"
    "fixture/StartDocumentAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDocumentAnalysis)

responseStartDocumentTextDetection :: StartDocumentTextDetectionResponse -> TestTree
responseStartDocumentTextDetection =
  res
    "StartDocumentTextDetectionResponse"
    "fixture/StartDocumentTextDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDocumentTextDetection)

responseStartExpenseAnalysis :: StartExpenseAnalysisResponse -> TestTree
responseStartExpenseAnalysis =
  res
    "StartExpenseAnalysisResponse"
    "fixture/StartExpenseAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartExpenseAnalysis)

responseStartLendingAnalysis :: StartLendingAnalysisResponse -> TestTree
responseStartLendingAnalysis =
  res
    "StartLendingAnalysisResponse"
    "fixture/StartLendingAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartLendingAnalysis)
