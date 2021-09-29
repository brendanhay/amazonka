{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Textract
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Textract where

import Data.Proxy
import Network.AWS.Textract
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Textract.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetDocumentTextDetection $
--             newGetDocumentTextDetection
--
--         , requestGetDocumentAnalysis $
--             newGetDocumentAnalysis
--
--         , requestStartDocumentAnalysis $
--             newStartDocumentAnalysis
--
--         , requestStartDocumentTextDetection $
--             newStartDocumentTextDetection
--
--         , requestAnalyzeExpense $
--             newAnalyzeExpense
--
--         , requestAnalyzeDocument $
--             newAnalyzeDocument
--
--         , requestDetectDocumentText $
--             newDetectDocumentText
--
--           ]

--     , testGroup "response"
--         [ responseGetDocumentTextDetection $
--             newGetDocumentTextDetectionResponse
--
--         , responseGetDocumentAnalysis $
--             newGetDocumentAnalysisResponse
--
--         , responseStartDocumentAnalysis $
--             newStartDocumentAnalysisResponse
--
--         , responseStartDocumentTextDetection $
--             newStartDocumentTextDetectionResponse
--
--         , responseAnalyzeExpense $
--             newAnalyzeExpenseResponse
--
--         , responseAnalyzeDocument $
--             newAnalyzeDocumentResponse
--
--         , responseDetectDocumentText $
--             newDetectDocumentTextResponse
--
--           ]
--     ]

-- Requests

requestGetDocumentTextDetection :: GetDocumentTextDetection -> TestTree
requestGetDocumentTextDetection =
  req
    "GetDocumentTextDetection"
    "fixture/GetDocumentTextDetection.yaml"

requestGetDocumentAnalysis :: GetDocumentAnalysis -> TestTree
requestGetDocumentAnalysis =
  req
    "GetDocumentAnalysis"
    "fixture/GetDocumentAnalysis.yaml"

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

requestAnalyzeExpense :: AnalyzeExpense -> TestTree
requestAnalyzeExpense =
  req
    "AnalyzeExpense"
    "fixture/AnalyzeExpense.yaml"

requestAnalyzeDocument :: AnalyzeDocument -> TestTree
requestAnalyzeDocument =
  req
    "AnalyzeDocument"
    "fixture/AnalyzeDocument.yaml"

requestDetectDocumentText :: DetectDocumentText -> TestTree
requestDetectDocumentText =
  req
    "DetectDocumentText"
    "fixture/DetectDocumentText.yaml"

-- Responses

responseGetDocumentTextDetection :: GetDocumentTextDetectionResponse -> TestTree
responseGetDocumentTextDetection =
  res
    "GetDocumentTextDetectionResponse"
    "fixture/GetDocumentTextDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentTextDetection)

responseGetDocumentAnalysis :: GetDocumentAnalysisResponse -> TestTree
responseGetDocumentAnalysis =
  res
    "GetDocumentAnalysisResponse"
    "fixture/GetDocumentAnalysisResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentAnalysis)

responseStartDocumentAnalysis :: StartDocumentAnalysisResponse -> TestTree
responseStartDocumentAnalysis =
  res
    "StartDocumentAnalysisResponse"
    "fixture/StartDocumentAnalysisResponse.proto"
    defaultService
    (Proxy :: Proxy StartDocumentAnalysis)

responseStartDocumentTextDetection :: StartDocumentTextDetectionResponse -> TestTree
responseStartDocumentTextDetection =
  res
    "StartDocumentTextDetectionResponse"
    "fixture/StartDocumentTextDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy StartDocumentTextDetection)

responseAnalyzeExpense :: AnalyzeExpenseResponse -> TestTree
responseAnalyzeExpense =
  res
    "AnalyzeExpenseResponse"
    "fixture/AnalyzeExpenseResponse.proto"
    defaultService
    (Proxy :: Proxy AnalyzeExpense)

responseAnalyzeDocument :: AnalyzeDocumentResponse -> TestTree
responseAnalyzeDocument =
  res
    "AnalyzeDocumentResponse"
    "fixture/AnalyzeDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy AnalyzeDocument)

responseDetectDocumentText :: DetectDocumentTextResponse -> TestTree
responseDetectDocumentText =
  res
    "DetectDocumentTextResponse"
    "fixture/DetectDocumentTextResponse.proto"
    defaultService
    (Proxy :: Proxy DetectDocumentText)
